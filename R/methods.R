#' Print the output of the GeocodingResults class
#'
#' @param x R object of class 'GeocodingResults'
#' @param which character string; choose which type of the results should be
#' displayed
#'
#' @importFrom magrittr %>%
#'
#' @export

print.GeocodingResults <-
  function(x, which = c("successful", "na", "unmatched_places")) {

    which <- match.arg(which)

    if (which == "successful") {
      x$geocoded_data %>%
        sf::st_sf(crs = x$set_parameters$target_epsg)
    } else if (which == "na") {
      x$geocoded_data_na
    } else if (which == "unmatched_places") {
      x$unmatched_places
    }
  }

#' Get a summary of the GeocodingResults class
#'
#' @param x R object of class 'GeocodingResults'
#'
#' @export
#'
summary.GeocodingResults <- function(x) {
  message_to_print <-
    paste0(
      "Geocoding results\n\n",
      "Structure of input data\n",
      x$set_parameters$id_name, "\t", x$set_parameters$street_name, "\t",
      x$set_parameters$house_number_name, "\t",
      x$set_parameters$zip_code_name, "\t", x$set_parameters$place_name, "\n\n",
      "Set parameters\n",
      "Requested CRS:                                     ",
      x$set_parameters$target_epsg, "\n",
      "Target Quality (everything below is filtered out): ",
      x$set_parameters$target_quality, "\n",
      "Target Quality for place matching:                 ",
      x$set_parameters$place_match_quality, "\n\n",
      "Statistics\n",
      "Addresses in input data:         ", x$summary_statistics$n_input, "\n",
      "Addresses entering geocoding:    ", x$summary_statistics$n_entering, "\n",
      "Addresses geocoded:              ", x$summary_statistics$n_geocoded, "\n",
      "Addresses geocoded with errors:  ", x$summary_statistics$n_geocoded_error, "\n",
      "Mean score:                      ",
      round(x$summary_statistics$mean_score, 3), "\n",
      "Standard deviation of score:     ",
      round(x$summary_statistics$sd_score, 3), "\n",
      "Minimum score:                   ",
      round(x$summary_statistics$min_score, 3)
    )

  message(message_to_print)
}



#' Get a simple results plot of the GeocodingResults class
#'
#' @param x R object of class 'GeocodingResults'
#'
#' @export
#'
plot.GeocodingResults <- function(x) {
  hist(
    x$geocoded_data$score,
    main = "Distribution of Scores",
    xlab = "Score",
    xlim = c(0, 1)
  )
}