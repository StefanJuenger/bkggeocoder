#' Print the output of the GeocodingResults class
#'
#' @param x Object of class \code{GeocodingResults}
#' @param which character string; choose which type of the results should be
#' displayed
#' @param ... Further arguments passed on to \code{\link[print.data.frame]{base::print.data.frame()}}
#' or \code{\link[sf:sf]{sf::print.sf()}}
#'
#' @export

print.GeocodingResults <- function(
  x,
  which = c("geocoded", "not_geocoded", "not_place_matched", "unmatched_places"),
  ...
) {
  n_input <- nrow(x$geocoded) + nrow(x$not_geocoded)
  which <- match.arg(which)
  if (!which %in% names(x) && attr(x, "type") == "bkg") {
    cli::cli_abort("{.code which = {which}} is not applicable for BKG geocoding results.")
  }
  indices <- c("score", "address_input", "address_output")
  
  cat("Class:", strrep(" ", 5), "GeocodingResults", "\n")
  cat("Geocoded:   ", nrow(x$geocoded), "/", n_input, "\n")
  cat("Mean score: ", round(mean(x$geocoded$score), 3), "\n")
  cat("Type:", strrep(" ", 6), attr(x, "type"), "\n\n")

  print(x[[which]][indices], ...)
}


#' Get a summary of the GeocodingResults class
#'
#' @param object Object of class \code{GeocodingResults}
#' @param ... Ignored.
#'
#' @export

summary.GeocodingResults <- function(object, ...) {
  n_input <- nrow(object$geocoded) + nrow(object$not_geocoded)
  n_matched <- if (!is.null(object$not_place_matched)) {
    n_input - nrow(object$not_place_matched)
  } else NA
  n_geocoded <- nrow(object$geocoded)
  n_not_geocoded <- nrow(object$not_geocoded)
  
  msg <- paste0(
    "Addresses in input data:         ", n_input, "\n",
    "Addresses entering geocoding:    ", n_matched, "\n",
    "Addresses geocoded:              ", n_geocoded, "\n",
    "Addresses geocoded with errors:  ", n_not_geocoded, "\n\n",
    "Mean score:                      ", round(mean(object$geocoded$score), 3), "\n",
    "Median score:                    ", round(stats::median(object$geocoded$score), 3), "\n",
    "Standard deviation of score:     ", round(stats::sd(object$geocoded$score), 3), "\n",
    "Minimum score:                   ", round(min(object$geocoded$score), 3), "\n",
    "Maximum score:                   ", round(max(object$geocoded$score), 3), "\n"
  )

  cat(msg)
}



#' Get a simple results plot of the GeocodingResults class
#'
#' @param x Object of class \code{GeocodingResults}
#' @param ... Further arguments passed on to \code{\link[graphics]{hist}}
#'
#' @export

plot.GeocodingResults <- function(x, ...) {
  graphics::hist(
    x$geocoded$score,
    main = "Distribution of geocoding scores",
    xlab = "Score",
    xlim = c(0, 1),
    ...
  )
}