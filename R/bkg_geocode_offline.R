#' Geocoding of a multiple addresses (BKG offline version)
#'
#' Geocoding of a multiple addresses using record linkage and an
#' address/coordinate database (provided by the BKG)
#'
#' @param data Data object that contains the columns for the parameters defined
#' next
#' @param street Character string for the street name
#' @param house_number Character string for the house number
#' @param zip_code Character string for the zip code
#' @param place Character string for the place (i.e., municipality/city)
#' @param credentials_path path to credentials package provided by Stefan Jünger
#' @param join_with_original logical; join with original data or only output
#' results
#' @param epsg Character string for the requested CSR
#' @param place_match_quality numeric; targeted quality of first record linkage
#' round (see details)
#' @param target_quality numeric; targeted quality of second record linkage
#' round (see details)
#'
#' @return Returns a tidy simple features data frame of the original data object
#' and the requested information from the geocoding. Please note that
#' original columns retrieved the suffix "_input".
#'
#' @details The function first matches the zip code and place information from
#' the data against the official names in the address/geocoordinate database
#' (first round of record linkage). You can play with the quality by adjusting
#' the \code{place_match_quality} parameter. In a second step, the input
#' addresses together with the matched results are then again matched against
#' the addresses in the address/geocoordinate database (second round of record
#' linkage). Again, you can play with the quality by adjusting
#' the \code{target_quality} parameter. The overall quality of
#' the geocoding can be evaluated by looking at the values of the column
#' \code{score} (ranging from 0 to 1), which is based on the second round of
#' record linkage.¸
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
#'
#' @export

bkg_geocode_offline <- function(
  data,
  id_variable = "id",
  street = "street",
  house_number = "house_number",
  zip_code = "zip_code",
  place = "place",
  data_path = "../bkgdata/",
  credentials_path = "../bkgcredentials/",
  join_with_original = FALSE,
  epsg = NULL,
  place_match_quality = .5,
  target_quality = .5
) {

  message(
    paste0(
      "** Starting offline geocoding service **\n",
      "\n",
      "Number of distinct addresses: ", nrow(data), "\n",
      "Targeted quality of geocoding: ", target_quality, " %",
      "\n"
    )
  )

  # Place Matching ----
  data_edited <-
    bkg_match_places(
      data,
      id_variable,
      data_path,
      credentials_path,
      place_match_quality
    )

  # Quering Database ----
  message("\nPreparing database:")

  house_coordinates <-
    bkg_query_ga(
      data_edited$matched$place_matched %>% unique(),
      data_path,
      credentials_path
    )

  data.table::setkey(house_coordinates, place)

  # Retrieving Geocoordinates ----
  message("\nStarting geocoding:")

  fuzzy_joined_data <-
    bkg_match_addresses(
      data_edited,
      house_coordinates,
      target_quality
    )

  # Data Cleaning ----
  fuzzy_joined_data <-
    bkg_clean_matched_addresses(fuzzy_joined_data, id_variable)

  if (isTRUE(join_with_original)) {
    fuzzy_joined_data <-
      dplyr::left_join(data, fuzzy_joined_data, by = id_variable)
  }

  if (!is.null(epsg)) {
    fuzzy_joined_data <- sf::st_transform(fuzzy_joined_data, crs = epsg)
  }

  # prepare data
  geocoded_data    <- fuzzy_joined_data %>% filter(!is.na(RS))
  geocoded_data_na <- fuzzy_joined_data %>% filter(is.na(RS))

  output_message <-
    paste0(
      "\n",
      "*** SUMMARY *** \n\n",
      "Addresses in input data:         ", nrow(data), "\n",
      "Addresses entering geocoding:    ", nrow(data_edited$matched), "\n",
      "Addresses left out:              ", nrow(data_edited$unmatched), "\n",
      "Addressses geocoded:             ", nrow(geocoded_data), "\n",
      "Addressees geocoded with errors: ", nrow(geocoded_data_na), "\n",
      "Mean score:                      ", mean(geocoded_data$score, na.rm = TRUE), "\n",
      "Standard deviation of score:     ", sd(geocoded_data$score, na.rm = TRUE), "\n",
      "Minimum score:                   ", min(geocoded_data$score, na.rm = TRUE)
    )

  message(output_message)

  # create list
  output_list <-
    list(
      geocoded_data = geocoded_data,
      geocoded_data_na = geocoded_data_na,
      non_geocoded_data = data_edited$data_unmatched,
      unmatched_places = data_edited$unmatched_places
    )

  output_list
}
