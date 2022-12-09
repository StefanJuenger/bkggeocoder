#' Export geocoding results
#'
#' Export the output of \code{\link[bkggeocoder]{bkg_geocode_offline}} and
#' \code{\link[bkggeocoder]{bkg_geocode}}.
#'
#' @param .data \code{[GeocodingResults]}
#' 
#' Output of \code{\link[bkggeocoder]{bkg_geocode}} or
#' \code{\link[bkggeocoder]{bkg_geocode_offline}} that should be exported.
#' 
#' @param which \code{[character]}
#' 
#' Names of \code{.data} that should be exported. One of \code{"all"},
#' \code{"place_matched"}, \code{"geocoded"}, \code{"not_geocoded"},
#' \code{"not_place_matched"} or \code{"unmatched_places"}. Defaults to
#' \code{"all"}.
#' 
#' If \code{"all"} and \code{file} is an \code{xlsx} file, saves each dataset
#' to a distinct sheet. Otherwise, creates multiple files that are appended with
#' the corresponding list name. For geospatial file extensions, only \code{sf}
#' objects can be exported. Regular data.frames are skipped.
#' 
#' If \code{"place_matched"}, appends the \code{geocoded} and \code{not_geocoded}
#' datasets and saves both in a single file.
#' 
#' If \code{"not_place_matched"} or \code{"unmatched_places"} and \code{file} is
#' a geospatial file (like \code{shp} or \code{geojson}), an error is thrown.
#' 
#' @param file \code{[character]}
#' 
#' Path to the output file. The file type is guessed based on the file extension.
#' If the file extension is \code{csv}, the data is exported using
#' \code{\link[readr]{write_delim}}. If it is \code{xlsx}, the data is exported
#' using \code{\link[openxlsx]{writeData}}. If is is anything else, the file type
#' is guessed by \code{\link[sf]{st_write}} and must be supported by
#' \code{\link[sf]{st_drivers}}.
#' 
#' @param overwrite \code{[logical]}
#' 
#' Whether to overwrite \code{file}, if it already exists. Defaults to \code{TRUE}.
#' 
#' @param ... Further arguments passed to \code{\link[readr]{write_delim}},
#' \code{\link[openxlsx]{writeData}} or \code{\link[sf]{st_write}}.
#' 
#' @returns \code{file}, invisibly.
#'
#' @export

bkg_export_geocodes <- function(
  .data,
  which = c("all", "place_matched", "geocoded", "not_geocoded",
            "not_place_matched", "unmatched_places"),
  file,
  overwrite = TRUE,
  ...
  ) {
  which <- match.arg(which)

  if (!inherits(.data, "GeocodingResults")) {
    cli::cli_abort(
      "i" = "Expected object of class {.cls GeocodingResults}",
      "x" = "Got object of class {.cls {class(data)}}"
    )
  }
  
  .data <- .data[names(.data) != "call"]

  data_to_export <- if (which == "place_matched") {
    do.call(rbind.data.frame, list(.data$geocoded, .data$not_geocoded))
  } else if (which == "all") {
    .data
  } else {
    .data[[which]]
  }
  
  geometry_to_xy <- function(data) {
    if (inherits(data, "sf")) {
      coords <- sf::st_coordinates(data)
      data <- cbind.data.frame(coords, data)
      data$geometry <- NULL
      data
    } else data
  }
  
  if (!file.exists(file) || isTRUE(overwrite)) {
    if (grepl("\\.csv$", file)) {
      if (!requireNamespace("readr")) {
        cli::cli_abort(c(
          "The {.pkg readr} package is required to export to csv.",
          "i" = "Install it using {.code install.packages(\"readr\")}"
        ))
      }
      if (which == "all") {
        for (ds in names(data_to_export)) {
          if (!is.null(data_to_export[[ds]]) && nrow(data_to_export[[ds]])) {
            filename <- gsub(
              "(.+)(\\.[[:alpha:]]+$)",
              sprintf("\\1_%s\\2", ds), 
              file,
              perl = TRUE
            )
            data_to_export[[ds]] <- geometry_to_xy(data_to_export[[ds]])
            readr::write_delim(data_to_export[[ds]], filename, ...)
          }
        }
      } else {
        data_to_export <- geometry_to_xy(data_to_export)
        readr::write_delim(data_to_export, file, ...)
      }

    } else if (grepl("\\.xlsx?$", file)) {
      if (!requireNamespace("openxlsx")) {
        cli::cli_abort(c(
          "The {.pkg openxlsx} package is required to export to Excel.",
          "i" = "Install it using {.code install.packages(\"openxlsx\")}"
        ))
      }
      if (which == "all") {
        wb <- openxlsx::createWorkbook()
        for (ds in names(data_to_export)) {
          if (!is.null(data_to_export[[ds]]) && nrow(data_to_export[[ds]])) {
            data_to_export[[ds]] <- geometry_to_xy(data_to_export[[ds]])
            openxlsx::addWorksheet(wb, ds)
            openxlsx::writeData(wb, sheet = ds, data_to_export[[ds]], ...)
          }
        }
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        openxlsx::write.xlsx(data_to_export, file)
      }
    } else {
      if (which == "all") {
        for (ds in names(data_to_export)) {
          if (inherits(data_to_export[[ds]], "sf") && nrow(data_to_export[[ds]])) {
            filename <- gsub(
              "(.+)(\\.[[:alpha:]]+$)",
              sprintf("\\1_%s\\2", ds), 
              file,
              perl = TRUE
            )
            sf::st_write(data_to_export[[ds]], filename, delete_dsn = TRUE, ...)
          }
        }
      } else if (which %in% c("place_matched", "geocoded", "not_geocoded")) {
        sf::st_write(data_to_export, file, delete_dsn = TRUE, ...)
      } else {
        cli::cli_abort(c(
          paste(
            "For objects that are not of class {.code sf}, only {.code .xlsx}",
            "and {.code .csv} are supported."
          ),
          "i" = paste(
            "Either pass a different file extension, or export only geocoding results",
            "of class {.code sf}."
          )
        ))
      }
      
    }
  }
  
  invisible(file)
}
