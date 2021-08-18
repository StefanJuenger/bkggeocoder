#' Geocoding of a multiple addresses to a geo-coordinate
#'
#' Geocoding of a multiple addresses to a geo-coordinate through the BKG Geocoder
#'
#' @param data Data object that contains the columns for the parameters defined
#' next
#' @param street Character string for the street name
#' @param house_number Character string for the house number
#' @param zip_code Character string for the zip code
#' @param place Character string for the place (i.e., municipality/city)
#' @param epsg Character string for the requested CSR
#'
#' @return Returns a tidy simple features data frame of the original data object
#' and the requested information from the geocoding service. Please note that
#' all columns are converted to character strings.
#'
#' @examples
#'
#' \dontrun{
#' # single address
#' bkg_geocode_single_address(
#'   street       = "Unter Sachsenhausen",
#'   house_number = "6-8",
#'   zip_code     = 50667,
#'   place        = "Köln",
#'   epsg         = "3035"
#' )
#'
#' # dataset with addresses
#' address_data <-
#'   tibble::tribble(
#'     ~street, ~house_number, ~zip_code, ~place,
#'     "B2", "1", "68159", "Mannheim",
#'     "Unter Sachsenhausen", "6-8", "50667", "Köln"
#'   )
#'
#' address_data_geocoded <-
#'   bkg_geocode(
#'     data         = address_data,
#'     street       = "street",
#'     house_number = "house_number",
#'     zip_code     = "zip_code",
#'     place        = "place",
#'     epsg         = "3035"
#'   )
#' }
#'
#' @importFrom magrittr %>%
#'
#' @export

bkg_geocode <- function (
  data = NULL,
  street = "street",
  house_number = "house_number",
  zip_code = "zip_code",
  place = "place",
  epsg = "3035"
) {

  if (is.null(data)) {
    bkg_geocode_single_address(
      street = street,
      house_number = house_number,
      zip_code = zip_code,
      place = place,
      epsg = epsg
    )
  } else {
    lapply(1:nrow(data), function (i) {

      message(paste0("Turning to row ", i, " from ", nrow(data)))

      bkg_geocode_single_address(
        street = data[[street]][i],
        house_number = data[[house_number]][i],
        zip_code = data[[zip_code]][i],
        place = data[[place]][i],
        epsg = epsg
      )

    }) %>%
      dplyr::bind_rows(.) %>%
      dplyr::bind_cols(
        data,
        .
      ) %>%
      sf::st_sf(crs = eval(parse(text = epsg)), sf_column_name = "geometry")
  }
}
