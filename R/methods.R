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
  function(x, which = c("successful", "na", "unmatched_places"), ...) {
    
    sum <- x$summary_statistics
    cat("Class:", strrep(" ", 5), "GeocodingResults", "\n")
    cat("Geocoded:   ", sum$n_geocoded, "/", sum$n_input, "\n")
    cat("Mean score: ", round(sum$mean_score, 3), "\n\n")
    
    which <- match.arg(which)

    if (which == "successful") {
      print(x$geocoded_data[, 2:6])
    } else if (which == "na") {
      print(x$geocoded_data_na[, 2:6])
    } else if (which == "unmatched_places") {
      print(x$unmatched_places[, 2:6])
    }
  }

#' Get a summary of the GeocodingResults class
#'
#' @param x R object of class 'GeocodingResults'
#'
#' @export
#'
summary.GeocodingResults <- function(x, ...) {
  msg <-
    paste0(
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

  cat(msg)
}



#' Get a simple results plot of the GeocodingResults class
#'
#' @param x R object of class 'GeocodingResults'
#'
#' @export
#'
plot.GeocodingResults <- function(x, ...) {
  hist(
    x$geocoded_data$score,
    main = "Distribution of Scores",
    xlab = "Score",
    xlim = c(0, 1)
  )
}