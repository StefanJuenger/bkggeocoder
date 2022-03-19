#' Dynamic comparator function
#' 
#' @description Return a comparator function that can be used in
#' \code{reclin2::compare_pairs}.
#' 
#' @param threshold Threshold for creating binary comparisons, e.g. as needed
#' by \code{\link[reclin2]{problink_em}}.
#' @param opts Further options to customize the comparator function.
#' 
#' @noRd
dyn_comparator <- function(threshold, opts) {
  function(x, y) {
    if (!missing(y)) {
      if (is.null(opts$method)) opts$method <- "jw"
      if (is.null(opts$weight)) opts$weight <- c(d = 1, i = 1, s = 1, t = 1)
      if (is.null(opts$q)) opts$q <- 1
      if (is.null(opts$p)) opts$p <- 0
      if (is.null(opts$bt)) opts$bt <- 0
      
      d <- stringdist::stringdist(
        a = x,
        b = y,
        method = opts$method,
        weight = opts$weight,
        q = opts$q,
        p = opts$p,
        bt = opts$bt
      )
      
      d <- if (opts$method %in% c("lcs", "osa")) {
        d / (nchar(x) + nchar(y))
      } else if (opts$method %in% c("lv", "dl")) {
        d / max(nchar(x), nchar(y))
      } else if (opts$method == "hamming") {
        d / nchar(x)
      } else if (opts$method == "qgram") {
        d / (nchar(x) + nchar(y) - 2 * opts$q - 2)
      } else d
      1 - d
    } else {
      (x > threshold) & !is.na(x)
    }
  }
}