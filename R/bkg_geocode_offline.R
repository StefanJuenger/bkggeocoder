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
#' @param data_from_server logical; shall the address data be downloaded from
#' GESIS internal server? Requires access to the GESIS net (default is FALSE)
#' @param data_path path to the address data provided by Stefan Jünger
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
  data_from_server = FALSE,
  data_path = "../bkgdata/",
  credentials_path = "../bkgcredentials/",
  join_with_original = TRUE,
  epsg = NULL,
  place_match_quality = .5,
  target_quality = .5,
  echo = TRUE
) {

  if (isTRUE(echo)) {
    message(
      paste0(
        "** Starting offline geocoding **\n",
        "\n",
        "Number of distinct addresses: ", nrow(data), "\n",
        "Targeted quality of geocoding: ", target_quality, " %",
        "\n"
      )
    )
  }

  # Place Matching ----
  data_edited <-
    bkg_match_places(
      data,
      id_variable,
      place,
      zip_code,
      data_path,
      credentials_path,
      place_match_quality,
      echo
    )

  # Quering Database ----
  if (isTRUE(echo)) {
    message("\nPreparing database:")
  }

  house_coordinates <-
    bkg_query_ga(
      data_edited$matched$place_matched %>% unique(),
      data_path,
      credentials_path,
      echo
    )

  data.table::setkeyv(house_coordinates, c("zip_code", "place"))

  # Retrieving Geocoordinates ----
  if (isTRUE(echo)) {
    message("\nStarting geocoding:")
  }

  fuzzy_joined_data <-
    bkg_match_addresses(
      data_edited,
      street,
      house_number,
      zip_code,
      place,
      house_coordinates,
      target_quality,
      echo
    )

  # Data Cleaning ----
  fuzzy_joined_data <-
    bkg_clean_matched_addresses(
      fuzzy_joined_data,
      zip_code,
      place,
      id_variable
      )

  if (isTRUE(join_with_original)) {
    fuzzy_joined_data <-
      dplyr::left_join(data, fuzzy_joined_data, by = id_variable)
  }

  if (!is.null(epsg)) {
    fuzzy_joined_data <-
      fuzzy_joined_data %>%
      sf::st_as_sf() %>%
      sf::st_transform(fuzzy_joined_data, crs = epsg)
  } else {
    epsg <- 3035

    fuzzy_joined_data %>%
      sf::st_as_sf(crs = epsg)
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
      set_parameters =
        tibble::tibble(
          place_match_quality = place_match_quality,
          target_quality = target_quality,
          target_epsg = epsg,
          id_name = id_variable,
          street_name = street,
          house_number_name = house_number,
          zip_code_name = zip_code,
          place_name = place,
          data_name = deparse(substitute(data)),
          join_with_original = join_with_original
        )
    )

  class(output_list) <- c("GeocodingResults", class(output_list))

  output_list
}

