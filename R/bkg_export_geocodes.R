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
#' @export

bkg_export_geocodes <- function(
  data,
  which = c("all", "successful", "na", "unmatched_places"),
  file,
  overwrite = TRUE
  ) {
  which <- match.arg(which)

  if (!inherits(data, "GeocodingResults")) {
    cli::cli_abort(
      "i" = "Expected object of class {.cls GeocodingResults}",
      "x" = "Got object of class {.cls {class(data)}}"
    )
  }

  export_function <- function (x, file) {
    if (stringr::str_detect(file, ".csv")) {
      if (!file.exists(file) || isTRUE(overwrite)) {
        readr::write_csv(x, file)
      }
    } else if (stringr::str_detect(file, ".xlsx")) {
      openxlsx::write.xlsx(x, file, overwrite = isTRUE(overwrite))
    }
  }

  data_to_export <- if (which == "all") {
    dplyr::bind_rows(
      dplyr::select(data$geocoded_data, -.data$geometry),
      dplyr::select(data$geocoded_data_na, -.data$geometry)
    ) %>%
      dplyr::select(-.data$geometry)
    } else if (which == "successful") {
       dplyr::select(data$geocoded_data, -.data$geometry)
    } else if (which == "na") {
      dplyr::select(data$geocoded_data_na, -.data$geometry)
    } else if (which == "unmatched_places") {
      data$unmatched_places
    }

  export_function(data_to_export, file)
  
  invisible()
}
