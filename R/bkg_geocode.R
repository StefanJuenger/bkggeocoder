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
#' Ignored if \code{structured = FALSE}.
#' 
#' @param query \code{[character]}
#' A character vector containing OpenSearch query strings. These types of
#' queries follow their own query language documented in the
#' [\code{gdz_geokodierung} documentation](https://sg.geodatenzentrum.de/web_public/gdz/dokumentation/deu/geokodierungsdienst.pdf).
#' For details refer to the details. Ignored if \code{structured = TRUE}.
#' 
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
#' @param target_quality \code{[numeric]}
#' 
#' Targeted quality of the geocoding result. Only results are returned that
#' lie above this threshold. If \code{structured = TRUE}, this value represents
#' a probability where 1 denotes absolute certainty and values below 0.9
#' bad results. If \code{structured = FALSE}, this value is absolute, meaning
#' that it has no maximum limit. Higher values simply denote better results.
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
#' @section Queries: 
#' If \code{structured = FALSE}, the geocoding service will perform unstructured
#' geocoding which is solely based on a query string provided by the
#' \code{query} argument. Such queries follow a unique query language which can
#' contain terms and operators. There are two types of terms: single terms and
#' phrases. Single terms consist of single words like "Leipzig" or "Sachsen".
#' Phrases can consist of multiple words and are linked through double quotes.
#' Terms can be combined using boolean operators.
#' 
#' Five boolean operators are recognized: \code{AND}, \code{+}, \code{OR},
#' \code{NOT}, \code{-}. \code{AND}, \code{OR}, and \code{+} are identical. To
#' control boolean logics, terms can be grouped using paranetheses, e.g.
#' \code{(Leipzig OR Halle) AND typ:Ort}. Parantheses can also be used to
#' assign multiple terms to the same attribute, e.g.
#' \code{strasse:(karl +rothe straße)}.
#' 
#' By default, terms are looked for in the `text` attribute. Other attributes
#' can be searched by specifying the attribute name followed by a colon and
#' the term, e.g. \code{ort:Leipzig AND strasse:Karl-Rothe-Straße}.
#' 
#' The query syntax for terms supports the use of wildcards. Wildcards for
#' single characters are specified using the \code{?} character. Wildcards for
#' multiple characters are specified using the \code{*} character. Both wildcards
#' are special characters and must be escaped with a backslash \code{\\}.
#' Wildcards cannot be used as the first character in a string.
#' 
#' Fuzzy searches are supported through the tilde operator \code{~}. 
#' Fuzzy searches use the Levenshtein string distance to increase the search
#' tolerance. A tolerance level can be inserted right after the tilde operator,
#' e.g. \code{Leipzig~0.6}. The default tolerance is 0.5. Higher values indicate
#' more similar terms. In case of phrases, the tilde operator denotes the
#' distance between the terms inside the phrase. A value of 1 would indicate
#' a 1-word distance between the terms inside a phrase, e.g.
#' \code{"karl straße"~1}.
#' 
#' Terms can be weighted through the \code{^} operator. This weight can be
#' ignored in case of relative scoring. For example,
#' \code{ort:Leipzig^5.3 typ:Strasse^0.5} would assign a higher weight to
#' Leipzig than to the place type street.
#' 
#' The following special characters are reserved by the query syntax:
#' 
#' \preformatted{+ - && || ! ( ) { } [ ] ^ " ~ * = : \\}
#'
#' @encoding UTF-8
#' @export
#' @examples
#' \dontrun{
#' # structured search
#' address_data <- tibble::tribble(
#'  ~street, ~house_number, ~zip_code, ~place,
#'  "B2", "1", "68159", "Mannheim",
#'  "Unter Sachsenhausen", "6-8", "50667", "Köln"
#' )
#'
#' bkg_geocode(data = address_data, epsg  = 4326)
#' 
#' # stricter geocoding
#' bkg_geocode(data = address_data, target_quality = 0.99)
#' 
#' # unstructured search
#' bkg_geocode(query = "Unter Sachsenhausen 6-8 50667 Köln")
#' 
#' # wildcard query
#' bkg_geocode(query = c("Freib\\?rg", "Frank\\*"))
#' 
#' # without boolean operator: returns state
#' bkg_geocode(query = "Nordrhein Westfalen")
#' 
#' # with NOT operator: returns street
#' bkg_geocode(query = "Nordrhein NOT Westfalen")
#' }
bkg_geocode <- function(.data = NULL,
                        query = NULL,
                        cols = 1:4,
                        epsg = 3035,
                        ...,
                        join_with_original = FALSE,
                        identifiers = "rs",
                        target_quality = 0.9,
                        verbose = TRUE) {
  args <- as.list(environment())
  do.call(check_online_args, args)
  
  geocoded_data <- if (is.null(.data)) {
    bkg_geocode_query(
      query = query,
      epsg = epsg,
      identifiers = identifiers,
      target_quality = target_quality,
      verbose = verbose,
      ...
    )
  } else {
    bkg_geocode_structured(
      .data = .data,
      cols = cols,
      epsg = epsg,
      identifiers = identifiers,
      target_quality = target_quality,
      verbose = verbose,
      ...
    )
  }
  
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

  # Split into geocoded and non-geocoded
  geocoded_data_na <- geocoded_data[
    is.na(geocoded_data$score) | geocoded_data$score < target_quality, 
  ]
  geocoded_data <- geocoded_data[
    !is.na(geocoded_data$score) & geocoded_data$score >= target_quality,
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


bkg_geocode_structured <- function(.data,
                                   cols,
                                   epsg,
                                   identifiers,
                                   target_quality,
                                   verbose,
                                   ...) {
  cols <- names(.data[cols])
  street <- .data[[cols[1]]]
  house_number <- if (length(cols) == 4) .data[[cols[2]]] else NULL
  zip_code <- if (length(cols) == 4) .data[[cols[3]]] else .data[[cols[2]]]
  place <- if (length(cols) == 4) .data[[cols[4]]] else .data[[cols[3]]]
  
  if (!length(.data[cols])) {
    cli::cli_abort("The provided column indices are invalid.")
  }
  
  if (verbose) {
    env <- environment()
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
  
  geocoded <- lapply(seq_len(nrow(.data)), function(i) {
    if (verbose) {
      cli::cli_progress_update(.envir = env)
    }
    
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
    
    res$.iid <- i
    res
  })
  
  geocoded <- rbind_list(geocoded)
  geocoded <- sf::st_sf(geocoded, crs = epsg, sf_column_name = "geometry")
  
  clean_geocode(
    geocoded,
    query = query,
    street = street,
    house_number = house_number,
    zip_code = zip_code,
    place = place,
    identifiers = identifiers
  )
}


bkg_geocode_query <- function(query,
                              epsg,
                              identifiers,
                              target_quality,
                              verbose,
                              ...) {
  if (isTRUE(verbose)) {
    env <- environment()
    cli::cli_progress_bar(
      name = "Geocoding addresses",
      total = length(query),
      format = paste(
        "{cli::pb_name} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} |",
        "ETA {cli::pb_eta}"
      ),
      format_failed = "Failed at address {cli::pb_current}/{cli::pb_total}."
    )
  }
  
  geocoded <- lapply(seq_along(query), function(i) {
    if (isTRUE(verbose)) {
      cli::cli_progress_update(.envir = env)
    }
    
    bkg_geocode_single(
      query = query[i],
      epsg = epsg,
      count = 1L,
      clean = FALSE,
      ...
    )
  })
  
  geocoded <- rbind_list(geocoded)
  geocoded <- sf::st_sf(geocoded, crs = epsg, sf_column_name = "geometry")
  
  clean_geocode(
    geocoded,
    query = query,
    street = NULL,
    house_number = NULL,
    zip_code = NULL,
    place = NULL,
    identifiers = identifiers
  )
}


check_online_args <- function(.data = NULL,
                              query = NULL,
                              cols = 1:4,
                              epsg = 3035,
                              ...,
                              join_with_original = FALSE,
                              identifiers = "rs",
                              target_quality = 0.9,
                              verbose = TRUE) {
  structured <- !is.null(.data)
  check_lgl(verbose)
  check_lgl(join_with_original)
  
  if (structured && is.null(.data)) {
    cli::cli_abort("If structured = TRUE, .data must not be NULL.")
  }
  
  if (!structured && is.null(query)) {
    cli::cli_abort("If structured = FALSE, query must not be NULL.")
  }
  
  if (!is.character(cols) && !is.numeric(cols)) {
    cli::cli_abort("The cols argument must contain either column names or column indices.")
  }
  
  if (structured && !length(.data[cols])) {
    cli::cli_abort("The provided column indices are invalid.")
  }
  
  if (!is.character(epsg) && !is.numeric(epsg)) {
    cli::cli_abort("The epsg argument must contain a valid EPSG code.")
  }
  
  if (!is.numeric(target_quality) && !target_quality > 0) {
    cli::cli_abort("The target_quality must be a valid non-negative number.")
  }
  
  if (structured && target_quality > 1) {
    cli::cli_abort("If structured = TRUE, target_quality must not be greater than 1.")
  }
}