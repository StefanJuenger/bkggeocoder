#' Exporting the results of the geocoding
#'
#' Exporting the results of the geocoding for further checking
#'
#' @param data R object of class GeocodingResults; the geocoded addresses
#' @param which character string; what kind of results would you like to export
#' (default is the successful and na ones appended together)
#' @param file character string; the file path resp. name either with an .csv or
#' .xlsx extension
#' @param overwrite logical; define whether exported file get overwritten
#' (default is true)
#'
#' @importFrom magrittr %>%
#'
#' @export

bkg_export_geocodes <-
  function(
    data,
    which = c("all", "successful", "na", "unmatched_places"),
    file,
    overwrite = TRUE
  ) {

    which <- match.arg(which)

    if (!("GeocodingResults" %in% class(data))) {
      stop("Not an object of class GeocodingResults")
    }

    export_function <- function (x, file) {
      if (stringr::str_detect(file, ".csv")) {
        readr::write_csv(x, file)
      } else if (stringr::str_detect(file, ".xlsx")) {
        openxlsx::write.xlsx(x, file, overwrite = overwrite)
      }
    }

    data_to_export <-
      if (which == "all") {
        dplyr::bind_rows(
          data$geocoded_data %>%
            dplyr::select(-.data$geometry),
          data$geocoded_data_na %>%
            dplyr::select(-.data$geometry)
        ) %>%
          dplyr::select(-.data$geometry)
      } else if (which == "successful") {
        data$geocoded_data %>%
          dplyr::select(-.data$geometry)
      } else if (which == "na") {
        data$geocoded_data_na %>%
          dplyr::select(-.data$geometry)
      } else if (which == "unmatched_places") {
        data$unmatched_places
      } else {
        stop("No valid which argument.")
      }

    export_function(data_to_export, file)
  }
