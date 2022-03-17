#' BKG geocoding interface
#'
#' Geocoding of single or multiple addresses using the BKG geocoding WFS
#' interface (\code{wfs_geokodierung_bund}).
#'
#' @param epsg Numeric or character string containing an EPSG code for the
#' requested CRS.
#' @inheritParams bkg_geocode_offline
#'
#' @return Returns a tidy simple features data frame of the original data object
#' and the requested information from the geocoding service. Please note that
#' all columns are converted to character strings.
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
#' @export

bkg_geocode <- function (
  data,
  cols = 1L:4L,
  epsg = 3035,
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

  geocoded_data <- do.call(rbind, geocoded_data)
  geocoded_data <- sf::st_sf(geocoded_data, crs = epsg, sf_column_name = "geometry")

  geocoded_data_na <- geocoded_data[
    is.na(geocoded_data$AGS) |
      geocoded_data$score < target_quality, 
  ]
  geocoded_data <- geocoded_data[
    !is.na(geocoded_data$AGS) &
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
