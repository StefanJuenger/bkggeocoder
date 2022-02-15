#' Geocoding of a multiple addresses (BKG offline version)
#'
#' Geocoding of a multiple addresses using record linkage and an
#' address/coordinate database (provided by the BKG)
#'
#' @param data Dataframe containing address data. The dataframe must contain
#' at least four columns carrying the street name, house number, zip code and
#' municipality name. The corresponding column names or indices can be specified
#' using the \code{cols} argument.
#' @param cols Numeric or character; names or indices of the columns containing
#' relevant geocoding the input data. By default, interprets the first four
#' columns as street, house number, zip code and municipality (in this order).
#' @param data_from_server Logical; shall the address data be downloaded from
#' GESIS internal server? Requires access to the GESIS net (default is
#' \code{FALSE})
#' @param data_path Path to the address data provided by Stefan Jünger
#' @param credentials_path Path to credentials package provided by Stefan Jünger
#' @param join_with_original Logical; should the output be joined with the
#' input data?
#' @param crs EPSG code, WKT/PROJ4 character string or object of class \code{crs}
#' that the output should be transformed to. Defaults to EPSG:3035.
#' @param place_match_quality Numeric; targeted quality of first record linkage
#' round (see details). Corresponds to the threshold value of
#' \code{\link[reclin2]{jaro_winkler}} and \code{\link[reclin2]{select_greedy}}.
#' @param target_quality Numeric; targeted quality of second record linkage
#' round (see details). Corresponds to the threshold value of
#' \code{\link[reclin2]{jaro_winkler}} and \code{\link[reclin2]{select_greedy}}.
#'
#' @return Returns a nested list of class GeocodingResult containing an
#' \code{sf} dataframe of the geocoding results (\code{$geocoded_data}) as well
#' as a dataframe with non-matched place names (\code{$unmatched_places}),
#' non-matched addresses (\code{$non_geocoded_data}) and matches with unresolved
#' regional key (\code{$geocoded_data_na}). The object also includes a call
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
#' The overall quality of the geocoding can be evaluated by looking at the
#' values of the column \code{score} (ranging from 0 to 1), which is based on
#' the second round of record linkage.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
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
  crs = NULL,
  place_match_quality = .5,
  target_quality = .5,
  verbose = TRUE
) {

  if (isTRUE(verbose)) {
    message(
      paste(
        "** Starting offline geocoding **\n",
        sprintf("Number of distinct addresses: %s", nrow(data)),
        sprintf("Targeted quality of geocoding: %s %%", target_quality),
        sep = "\n"
      )
    )
    message("\n--- Subsetting data ", strrep("-", 20))
  }
  
  cols <- names(data)[1:4]
  
  data <- cbind(data.frame(id = row.names(data)), data)

  # Place Matching ----
  data_edited <-
    bkg_match_places(
      data,
      cols,
      data_from_server,
      data_path,
      credentials_path,
      place_match_quality,
      verbose
    )

  # Querying Database ----
  if (isTRUE(verbose)) {
    message("\n--- Preparing database ", strrep("-", 20))
  }

  house_coordinates <-
    bkg_query_ga(
      data_edited$matched$place_matched %>% unique(),
      data_from_server,
      data_path,
      credentials_path,
      verbose
    )

  data.table::setkeyv(house_coordinates, c("zip_code", "place"))

  # Retrieving Geocoordinates ----
  if (isTRUE(verbose)) {
    message("\n--- Geocoding input ", strrep("-", 20))
  }

  fuzzy_joined_data <-
    bkg_match_addresses(
      data_edited,
      cols,
      house_coordinates,
      target_quality,
      verbose
    )

  # Data Cleaning ----
  fuzzy_joined_data <-
    bkg_clean_matched_addresses(fuzzy_joined_data, cols, verbose)

  if (isTRUE(join_with_original)) {
    fuzzy_joined_data <-
      dplyr::left_join(data, fuzzy_joined_data, by = "id")
  }

  fuzzy_joined_data <- sf::st_as_sf(fuzzy_joined_data, crs = 3035)
  
  if (!missing(crs)) {
    fuzzy_joined_data <- sf::st_transform(fuzzy_joined_data, crs = crs)
  }

  # prepare data
  geocoded_data    <- fuzzy_joined_data %>% dplyr::filter(!is.na(RS))
  geocoded_data_na <- fuzzy_joined_data %>% dplyr::filter(is.na(RS))

  # create list
  output_list <-
    list(
      geocoded_data = geocoded_data,
      geocoded_data_na = geocoded_data_na,
      non_geocoded_data = data_edited$data_unmatched,
      unmatched_places = data_edited$unmatched_places,
      summary_statistics =
        tibble::tibble(
          n_input = nrow(data),
          n_entering = nrow(data_edited$matched),
          n_geocoded = nrow(geocoded_data),
          n_geocoded_error = nrow(geocoded_data_na),
          mean_score = mean(geocoded_data$score, na.rm = TRUE),
          sd_score = sd(geocoded_data$score, na.rm = TRUE),
          min_score = min(geocoded_data$score, na.rm = TRUE)
        ),
      call = match.call()
    )

  class(output_list) <- c("GeocodingResults", class(output_list))

  output_list
}

