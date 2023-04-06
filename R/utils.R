#' Binds list of (sf) data.frames to a single data.frame. If the number of
#' columns differs, fills empty columns with NA
#' @param args List of data.frames or sf objects
#' @returns data.frame or sf data.frame
#' @noRd
rbind_list <- function(args) {
  nam <- lapply(args, names)
  unam <- unique(unlist(nam))
  len <- vapply(args, length, numeric(1))
  out <- vector("list", length(len))
  for (i in seq_along(len)) {
    if (nrow(args[[i]])) {
      nam_diff <- setdiff(unam, nam[[i]])
      if (length(nam_diff)) {
        args[[i]][nam_diff] <- NA
      }
    } else {
      next
    }
  }
  out <- do.call(rbind, args)
  rownames(out) <- NULL
  out
}


#' Simpler wrapper for regexec and regmatches
#' 
#' @param x A character vector to be matched.
#' @param pattern Regex expression or term to be looked up in x
#' @param ... Further arguments passed to regexec.
#' 
#' @noRd
match_regex <- function(x, pattern, ...) {
  matches <- regexec(pattern, x, ...)
  regmatches(x, matches)
}