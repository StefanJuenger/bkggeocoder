#' BKG geocoding interface
#'
#' Geocoding of single or multiple addresses using the BKG geocoding WFS
#' interface (\code{wfs_geokodierung_bund}).
#'
#' @param epsg Numeric or character string containing an EPSG code for the
#' requested CRS.
#' @param target_quality Numeric; targeted quality. All geocoded addresses with
#' scores below this value will be dropped in the output.
#' @inheritParams bkg_geocode_offline
#'
#' @return \code{bkg_geocode}r eturns a nested list of class GeocodingResults
#' containing an \code{sf} dataframe of the geocoding results (\code{$geocoded})
#' and a dataframe with addresses with non-matched places (\code{$not_geocoded}).
#' Since the BKG geocoder does not need to subset the data using place matching,
#' the output does not contain dataframes on unmatched places like in
#' \code{\link{bkg_geocode_offline}}. The object also includes a call object and
#' descriptive summary statistics. Please note that original columns
#' retrieve the suffix \code{"_input"}.
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

bkg_geocode <- function (
  data,
  cols = 1L:4L,
  epsg = 3035,
  join_with_original = FALSE,
  target_quality = 0.9,
  verbose = TRUE
) {
  cols <- names(data[cols])
    
  if (!length(data)) {
    cli::cli_abort(paste(
      "Either {.var data} or the alternative arguments {.var street},",
      "{.var house_number}, {.var zip_code} and {.var place} must be",
      "specified."
    ))
  }
    
  if (isTRUE(verbose)) {
    cli::cli_progress_bar(
      name = "Geocoding addresses",
      total = nrow(data),
      format = paste(
        "{cli::pb_name} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} |",
        "ETA {cli::pb_eta}"
      ),
      format_failed = "Failed at address {cli::pb_current}/{cli::pb_total}."
    )
  }
  
  data <- cbind(data.frame(.iid = row.names(data)), data)
  
  geocoded_data <- lapply(1:nrow(data), function(i) {
    if (isTRUE(verbose)) {
      cli::cli_progress_update(.envir = parent.frame(2))
    }
    
    res <- bkg_geocode_single_address(
      street = data[[cols[1]]][i],
      house_number = data[[cols[2]]][i],
      zip_code = data[[cols[3]]][i],
      place = data[[cols[4]]][i],
      epsg = epsg
    )

    res$.iid <- i
    res
  })

  geocoded_data <- do.call(rbind, geocoded_data)
  geocoded_data <- sf::st_sf(geocoded_data, crs = epsg, sf_column_name = "geometry")
  
  if (isTRUE(join_with_original)) {
    geocoded_data <- merge(
      data,
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
    type = "bkg"
  )
  
  class(output_list) <- c("GeocodingResults", class(output_list))
  
  output_list
}
