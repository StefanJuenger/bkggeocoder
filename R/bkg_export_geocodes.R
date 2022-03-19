#' Export geocoding results
#'
#' Export the output of \code{\link[bkggeocoder]{bkg_geocode_offline}} and
#' \code{\link[bkggeocoder]{bkg_geocode}}.
#'
#' @param data R object of class GeocodingResults; the geocoded addresses
#' @param which Character string; what kind of results would you like to export
#' (default is the successful and na ones appended together)
#' @param file Path to the output file. The file type is guessed by
#' \code{\link[sf]{st_write}} and must be supported by
#' \code{\link[sf]{st_drivers}}.
#' @param overwrite Logical; define whether exported file get overwritten
#' (default is true)
#' @param ... Further arguments passed to \code{\link[utils]{write.table}} or
#' \code{\link[sf]{st_write}}.
#' 
#' @returns \code{file}, invisibly.
#'
#' @export

bkg_export_geocodes <- function(
  data,
  which = c("all", "geocoded", "not_geocoded",
            "not_place_matched", "unmatched_places"),
  file,
  overwrite = TRUE,
  ...
  ) {
  which <- match.arg(which)

  if (!inherits(data, "GeocodingResults")) {
    cli::cli_abort(
      "i" = "Expected object of class {.cls GeocodingResults}",
      "x" = "Got object of class {.cls {class(data)}}"
    )
  }

  data_to_export <- if (which == "all") {
    do.call(rbind, list(data$geocoded, data$not_geocoded))
  } else {
    data[[which]]
  }
  
  if (!file.exists(file) || isTRUE(overwrite)) {
    if (grepl(".csv", file, fixed = TRUE)) {
      coords <- sf::st_coordinates(data_to_export)
      data_to_export <- cbind.data.frame(coords, data_to_export)
      data_to_export$geometry <- NULL
      utils::write.table(data_to_export, file, ...)
    } else {
      sf::st_write(data_to_export, file, ...)
    }
  }
  
  invisible(file)
}
