#' BKG batch (reverse) geocoding
#'
#' \code{bkg_geocode} provides an interface for geocoding dataframes holding
#' structured or unstructured address data. \code{bkg_geocode} finds addresses
#' that can be associated to a dataframe of input geometries. Both functions
#' require access to the \code{gdz_geokodierung} endpoint of the BKG.
#'
#' @param .data \code{[data.frame]}
#' 
#' For \code{bkg_geocode}, a dataframe containing address data. The dataframe
#' must all columns specified with the \code{cols} argument. For
#' \code{bkg_reverse}, .data is expected to be an \code{sf} data.frame
#' containing geometries of a single geometry type. Geometry types correspond to
#' the allowed geometry types in \code{\link[bkggeocoder]{bkg_reverse_single}}.
#' @param cols \code{[numeric/character]}
#' 
#' Names or indices of the columns containing relevant geocoding information.
#' Must be of length 3 or 4. If a length-3 vector is passed, the first column is
#' interpreted as a single character string containing street and house number.
#' By default, interprets the first four columns as street, house number, zip
#' code and municipality (in this order).
#' If \code{structured = FALSE}, only one column is accepted which holds query
#' strings for unstructured geocoding.
#' @param epsg \code{[numeric/character]}
#' 
#' Numeric or character string containing an EPSG code for the requested CRS.
#' @param ... Further arguments passed to
#' \code{\link[bkggeocoder]{bkg_geocode_single}} or
#' \code{\link[bkggeocoder]{bkg_reverse_single}}
#' @param structured \code{[logical]}
#' 
#' If \code{TRUE}, activates structured geocoding. Structured geocoding accepts
#' up to six columns describing different elements of an address. If
#' \code{FALSE}, activates unstructured geocoding. Unstructured geocoding
#' accepts only a single column containing an address string (and, optionally,
#' query operators as described in
#' \code{\link[bkggeocoder]{bkg_geocode_single}}). Generally, structured
#' geocoding is the safer option, but unstructured geocoding offers much more
#' flexibility and requires less data preparation.
#' @param target_quality \code{[numeric]}
#' 
#' Targeted quality of the geocoding result. Only results are returned that
#' lie above this threshold.

#' @inheritParams bkg_geocode_offline
#'
#' @return \code{bkg_geocode} returns a nested list of class GeocodingResults
#' containing an \code{sf} dataframe of the geocoding results (\code{$geocoded})
#' and a dataframe with addresses with non-matched places (\code{$not_geocoded}).
#' Since the BKG geocoder does not need to subset the data using place matching,
#' the output does not contain dataframes on unmatched places like in
#' \code{\link{bkg_geocode_offline}}. The object also includes a call object.
#' Please note that original columns retrieve the suffix \code{"_input"}.
#'
#' @examples
#'
#' \dontrun{
#' # dataset with addresses
#' address_data <- tibble::tribble(
#'  ~street, ~house_number, ~zip_code, ~place,
#'  "B2", "1", "68159", "Mannheim",
#'  "Unter Sachsenhausen", "6-8", "50667", "Köln"
#' )
#'
#' bkg_geocode(data = address_data, epsg  = 4326)
#' }
#'
#' @encoding UTF-8
#'
#' @export
bkg_geocode <- function(
  .data,
  cols = 1:4,
  epsg = 3035,
  ...,
  structured = TRUE,
  join_with_original = FALSE,
  identifiers = "rs",
  target_quality = 0.9,
  verbose = TRUE
) {
  cols <- names(.data[cols])
  
  query <- if (!structured) .data[[cols[1]]] else NULL
  street <- if (structured) .data[[cols[1]]] else NULL
  house_number <- if (structured && length(cols) == 4) .data[[cols[2]]] else NULL
  zip_code <- if (structured && length(cols) == 4) .data[[cols[3]]] else .data[[cols[2]]]
  place <- if (structured && length(cols) == 4) .data[[cols[4]]] else .data[[cols[3]]]

  args <- as.list(environment())
  args$.data <- NULL
    
  if (!length(.data[cols])) {
    cli::cli_abort("The provided column indices are invalid.")
  }
    
  if (isTRUE(verbose)) {
    cli::cli_progress_bar(
      name = "Geocoding addresses",
      total = nrow(.data),
      format = paste(
        "{cli::pb_name} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} |",
        "ETA {cli::pb_eta}"
      ),
      format_failed = "Failed at address {cli::pb_current}/{cli::pb_total}."
    )
  }
  
  .data <- cbind(data.frame(.iid = row.names(.data)), .data)
  
  geocoded_data <- lapply(seq_len(nrow(.data)), function(i) {
    if (isTRUE(verbose)) {
      cli::cli_progress_update(.envir = parent.frame(2))
    }
    
    if (structured) {
      res <- bkg_geocode_single(
        street = if (length(cols) == 4) street[i] else NULL,
        house_number = house_number[i],
        zip_code = zip_code[i],
        place = place[i],
        street_house = if (length(cols) == 3) street[i] else NULL,
        epsg = epsg,
        count = 1L,
        clean = FALSE,
        ...
      )
    } else {
      res <- bkg_geocode_single(
        query = query[i],
        epsg = epsg,
        count = 1L,
        clean = FALSE,
        ...
      )
    }

    res$.iid <- i
    res
  })

  geocoded_data <- rbind_list(geocoded_data)
  geocoded_data <- sf::st_sf(geocoded_data, crs = epsg, sf_column_name = "geometry")
  
  geocoded_data <- clean_geocode(
    geocoded_data,
    query = query,
    street = street,
    house_number = house_number,
    zip_code = zip_code,
    place = place,
    identifiers = identifiers
  )
  
  if (isTRUE(join_with_original)) {
    geocoded_data <- merge(
      .data,
      geocoded_data,
      by = ".iid",
      all.x = TRUE,
      sort = TRUE
    )
    
    geocoded_data <- sf::st_as_sf(tibble::as_tibble(geocoded_data))
  }
  
  # Remove internal identifier
  geocoded_data$.iid <- NULL

  geocoded_data_na <- geocoded_data[
    is.na(geocoded_data$RS) |
      geocoded_data$score < target_quality, 
  ]
  geocoded_data <- geocoded_data[
    !is.na(geocoded_data$RS) &
      geocoded_data$score >= target_quality,
  ]

  output_list <- structure(
    list(
      geocoded = geocoded_data,
      not_geocoded = geocoded_data_na,
      call = match.call()
    ),
    type = "bkg",
    args = args,
    class = "GeocodingResults"
  )
  
  output_list
}
