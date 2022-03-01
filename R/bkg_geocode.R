#' Geocoding of a multiple addresses to a geo-coordinate
#'
#' Geocoding of a multiple addresses to a geo-coordinate through the BKG Geocoder
#'
#' @param epsg Character string for the requested CSR
#' @inheritParams bkg_geocode_offline
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
#' @export

bkg_geocode <- function (
  data = NULL,
  cols = 1L:4L,
  epsg = "3035",
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
  
  geocoded_data <- lapply(1:nrow(data), function(i) {
    if (isTRUE(verbose)) {
      cli::cli_progress_update(.envir = parent.frame(2))
    }
    
    bkg_geocode_single_address(
      street = data[[cols[1]]][i],
      house_number = data[[cols[2]]][i],
      zip_code = data[[cols[3]]][i],
      place = data[[cols[4]]][i],
      epsg = epsg
    )

  })
  
  geocoded_data <-  dplyr::bind_rows(geocoded_data) %>%
    dplyr::bind_cols(data, .) %>%
    sf::st_sf(crs = eval(parse(text = epsg)), sf_column_name = "geometry")
}
