#' Match data's places and zip codes against official ones
#'
#' @param data data.frame or tibble comprising both a column named \code{place}
#' and \code{zip_code}
#'
#' @noRd

bkg_match_places <- function(
  data,
  cols,
  data_from_server,
  data_path,
  credentials_path,
  place_match_quality,
  opts,
  verbose
) {
  place <- ifelse(length(cols) == 4, cols[4], cols[3])
  zip_code <- ifelse(length(cols) == 4, cols[3], cols[2])

  # Prepare place data ----
  data_mun <- unique(data[c(place, zip_code)])
  data_mun[, zip_code] <- vapply(data_mun[, zip_code], function(zip) {
    zip <- as.character(zip)
    if (nchar(zip) == 4) {
      paste0(0, zip)
    } else zip
  }, FUN.VALUE = character(1))
  data_mun$az_group <- substr(data_mun[, place], 1, 3)
  data_mun$plz_group <- substr(data_mun[, zip_code], 1, 6)

  if (isTRUE(verbose)) {
    cli::cli_inform("Found {.val {nrow(data_mun)}} distinct place{?s}.")
    cli::cli_progress_step(
      msg = "Retrieving place names from database...",
      msg_done = "Retrieved place names from database.",
      msg_failed = "Could not retrieve place names from database."
    )
  }

  # Acquire place data from BKG ----
  bkg_zip_places <- bkg_read(
    what = "places",
    data_from_server = data_from_server,
    data_path = data_path,
    credentials_path = credentials_path
  )

  names(bkg_zip_places) <- c(place, zip_code)

  if (isTRUE(verbose)) {
    cli::cli_progress_done()
    cli::cli_progress_step(
      msg = "Pairing place records...",
      msg_done = "Paired place records..",
      msg_failed = "Could not pair place records."
    )
  }

  bkg_zip_places$az_group <- substr(bkg_zip_places[[place]], 1, 3)
  bkg_zip_places$plz_group <- substr(bkg_zip_places[[zip_code]], 1, 6)

  # Match data (record linkage) ----
  suppressWarnings({
    data_mun_pairs <- reclin2::pair_blocking(
      data_mun,
      bkg_zip_places,
      on = c("plz_group"),
    )

    reclin2::compare_pairs(
      data_mun_pairs,
      on = c(place, zip_code),
      minsim = 1,
      default_comparator = dyn_comparator(place_match_quality, opts),
      inplace = TRUE
    )

    if (isTRUE(verbose)) {
      cli::cli_progress_step(
        msg = "Calculating place matching scores...",
        msg_done = "Calculated place matching scores.",
        msg_failed = "Could not calculate place matching scores."
      )
    }

    formula_chr <- paste0("~", place, " + ", zip_code)
    fun_env <- environment()
    est <- reclin2::problink_em(
      formula = stats::as.formula(formula_chr, env = fun_env),
      data = data_mun_pairs
    )

    stats::predict(
      est,
      pairs = data_mun_pairs,
      add = TRUE,
      type = "all"
    )

    if (isTRUE(verbose)) {
      cli::cli_progress_step(
        msg = "Linking and selecting records...",
        msg_done = "Place record linkage finished.",
        msg_failed = "Could not link data."
      )
    }

    reclin2::select_greedy(
      pairs = data_mun_pairs,
      variable = "threshold",
      score = "mpost",
      threshold = place_match_quality,
      inplace = TRUE
    )

    data_mun_real <- reclin2::link(
      pairs = data_mun_pairs[data_mun_pairs$threshold],
      all_x = TRUE,
      all_y = FALSE
    )

    data_mun_real <- structure(
      data.frame(
        data_mun_real[[3L]],
        as.numeric(data_mun_real[[4L]]),
        data_mun_real[[7L]],
        data_mun_real[[8L]]
      ),
      names = c(place, zip_code, "place_matched", "zip_code_matched")
    )
  })

  unmatched_places <- data_mun_real[is.na(data_mun_real$place_matched), ]
  n_unmatched <- nrow(unmatched_places)

  if (isTRUE(verbose)) {
    if (n_unmatched) {
      cli::cli_inform(c("!" = "WARNING: {.val {n_unmatched}} place{?s} left unmatched."))
    }
    cli::cli_progress_done()
  }

  # Combine with input ----
  data_matched <- merge(
    data,
    data_mun_real,
    by = c(place, zip_code),
    all.x = TRUE,
    sort = FALSE
  )

  data_matched <- unique(data_matched[!is.na(data_matched$place_matched), ])
  data_unmatched <- data[!data$.iid %in% data_matched$.iid, ]

  if (isTRUE(verbose)) {
    if (nrow(data_unmatched) && !nrow(data_matched)) {
      cli::cli_abort("No address could be matched with any place. Check your input!")
    } else if (!nrow(data_unmatched) && nrow(data_matched)) {
      cli::cli_alert_success("All addresses could be place-matched.")
    } else if (nrow(data_unmatched) && nrow(data_matched)) {
      cli::cli_inform(c("i" = paste(
        "{.val {nrow(data_matched)}} out of {.val {nrow(data)}}",
        "address{?es} could be place-matched."
      )))
    }
  }

  list(
    matched = data_matched,
    unmatched = data_unmatched,
    unmatched_places = unmatched_places
  )
}
