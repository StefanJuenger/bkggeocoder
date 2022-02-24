#' Geocoding of a multiple addresses (BKG offline version)
#'
#' @description Geocoding of a multiple addresses using record linkage and an
#' address/coordinate database (provided by the BKG)
#'
#' @param data Dataframe containing address data. The dataframe must contain
#' at least four columns carrying the street name, house number, zip code and
#' municipality name. The corresponding column names or indices can be specified
#' using the \code{cols} argument.
#' @param cols Numeric or character of length 4; names or indices of the columns
#' containing relevant geocoding information. By default, interprets the
#' first four columns as street, house number, zip code and municipality (in
#' this order).
#' @param data_from_server Logical; shall the address data be downloaded from
#' GESIS internal server? Requires access to the GESIS net (default is
#' \code{FALSE})
#' @param data_path Path to the address data provided by Stefan Jünger. Ignored
#' if \code{data_from_server = TRUE}.
#' @param credentials_path Path to credentials package provided by Stefan Jünger
#' @param join_with_original Logical; should the output be joined with the
#' input data?
#' @param crs Any kind of object that can be parsed by \code{\link[sf]{st_crs}}
#' that the output data should be transformed to (e.g. EPSG code, WKT/PROJ4
#' character string or object of class \code{crs}). Defaults to EPSG:3035.
#' @param place_match_quality Numeric; targeted quality of first record linkage
#' round (see details). Corresponds to the threshold value of
#' \code{\link[reclin2]{jaro_winkler}}.
#' @param target_quality Numeric; targeted quality of second record linkage
#' round (see details). Corresponds to the threshold value of
#' \code{\link[reclin2]{jaro_winkler}} and \code{\link[reclin2]{select_greedy}}.
#' @param verbose Whether to print informative messages and progress bars during
#' the geocoding process.
#'
#' @returns Returns a nested list of class GeocodingResult containing an
#' \code{sf} dataframe of the geocoding results (\code{$geocoded_data}) as well
#' as a dataframe with non-matched place names (\code{$unmatched_places}),
#' addresses with non-matched places (\code{$non_geocoded_data}) and non-matched
#' addresses (\code{$geocoded_data_na}). The object also includes a call
#' object and descriptive summary statistics. Please note that original columns
#' retrieved the suffix \code{"_input"}.
#'
#' @details The function first matches the zip code and place information from
#' the data against the official names in the address/geocoordinate database
#' (first round of record linkage). This is done to filter out address datasets
#' that are not needed and lower the data size. You can play with the quality by
#' adjusting the \code{place_match_quality} parameter. In a second step, the
#' input addresses together with the matched results are then again matched
#' against the addresses in the address/geocoordinate database (second round of
#' record linkage). Again, you can play with the quality by adjusting
#' the \code{target_quality} parameter.
#' 
#' The overall quality of the geocoding can be evaluated by looking at the
#' values of the column \code{score} (ranging from 0 to 1), which is based on
#' the second round of record linkage. In general, for both rounds of record
#' linkage, a score of above 0.9 can be considered a good match. If the score
#' falls below 0.6, the result should be thoroughly scrutinized.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr .data
#' @import data.table
#' 
#' @encoding UTF-8
#'
#' @export

bkg_geocode_offline <- function(
  data,
  cols = 1L:4L,
  data_from_server = FALSE,
  data_path = "../bkgdata/",
  credentials_path = "../bkgcredentials/",
  join_with_original = TRUE,
  crs = 3035L,
  place_match_quality = 0.9,
  target_quality = 0.9,
  verbose = TRUE
) {
  stopifnot(is.data.frame(data))
  stopifnot(is.logical(data_from_server))
  stopifnot(is.logical(join_with_original))
  stopifnot(is.numeric(place_match_quality))
  stopifnot(is.numeric(target_quality))
  if (isFALSE(data_from_server)) {
    stopifnot(dir.exists(data_path))
    stopifnot(dir.exists(credentials_path))
  }
  crs_err <- function(e) {
    cli::cli_abort("{.var crs} must be parsable by {.fn sf::st_crs}")
  }
  tryCatch(expr = sf::st_crs(crs), error = crs_err, warning = crs_err)

  if (isTRUE(verbose)) {
    cli::cli_h1("Starting offline geocoding")
    cli::cat_line()
    cli::cli_inform(c(
      "i" = "Number of distinct addresses: {.val {nrow(data)}}",
      "i" = "Targeted quality of place-matching: {.val {place_match_quality}}",
      "i" = "Targeted quality of geocoding: {.val {target_quality}}")
    )
    
    cli::cli_h2("Subsetting data")
  }
  
  cols <- names(data[cols])
  
  data <- cbind(data.frame(id = row.names(data)), data)

  # Place Matching ----
  data_edited <- bkg_match_places(
    data,
    cols,
    data_from_server,
    data_path,
    credentials_path,
    place_match_quality,
    verbose
  )

  # Querying Database ----
  house_coordinates <- bkg_query_ga(
    unique(data_edited$matched$place_matched),
    data_from_server,
    data_path,
    credentials_path,
    verbose
  )

  data.table::setkeyv(house_coordinates, c("zip_code", "place"))

  # Retrieving Geocoordinates ----
  if (isTRUE(verbose)) {
    cli::cli_h2("Geocoding input data")
  }

  fuzzy_joined_data <- bkg_match_addresses(
    data_edited,
    cols,
    house_coordinates,
    target_quality,
    verbose
  )

  # Data Cleaning ----
  fuzzy_joined_data <- bkg_clean_matched_addresses(
    fuzzy_joined_data,
    cols,
    verbose
  )

  if (isTRUE(join_with_original)) {
    fuzzy_joined_data <- dplyr::left_join(data, fuzzy_joined_data, by = "id") %>%
      sf::st_as_sf()
  }
  
  if (!missing(crs)) {
    fuzzy_joined_data <- sf::st_transform(fuzzy_joined_data, crs = crs)
  }

  # Create Output ----
  geocoded_data    <- dplyr::filter(fuzzy_joined_data, !is.na(.data$RS))
  geocoded_data_na <- dplyr::filter(fuzzy_joined_data, is.na(.data$RS))

  output_list <- list(
    geocoded_data = geocoded_data,
    geocoded_data_na = geocoded_data_na,
    non_geocoded_data = data_edited$data_unmatched,
    unmatched_places = data_edited$unmatched_places,
    summary_statistics = tibble::tibble(
      n_input = nrow(data),
      n_entering = nrow(data_edited$matched),
      n_geocoded = nrow(geocoded_data),
      n_geocoded_error = nrow(geocoded_data_na),
      mean_score = mean(geocoded_data$score, na.rm = TRUE),
      sd_score = stats::sd(geocoded_data$score, na.rm = TRUE),
      min_score = min(geocoded_data$score, na.rm = TRUE)
    ),
    call = match.call()
  )

  class(output_list) <- c("GeocodingResults", class(output_list))

  output_list
}

