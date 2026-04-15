#' Match data's places and zip codes against official ones
#'
#' @param data data.frame or tibble comprising both a column named \code{place}
#' and \code{zip_code}
#'
#' @noRd
bkg_match_places <- function(
  .data,
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
  # Handle leading zeroes in zip codes
  .data[, zip_code] <- vapply(.data[, zip_code], function(zip) {
    zip <- as.character(zip)
    if (nchar(zip) == 4) {
      paste0(0, zip)
    } else zip
  }, FUN.VALUE = character(1))
  
  # Remove duplicates of place-zipcode combinations
  data_mun <- unique(.data[c(place, zip_code)])
  
  # Create AZ and PLZ groups
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
  
  place_simple <- bkg_zip_places$place
  bkg_zip_places$place <- paste(bkg_zip_places$place, bkg_zip_places$place_add)
  
  bkg_zip_places$place_add <- NULL
  
  bkg_zip_places_simple <-
    data.frame(place_matched = bkg_zip_places$place, place_simple)
  
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
  
  # strip whitespace at the end
  bkg_zip_places[] <- lapply(bkg_zip_places, function(x) {
    if (is.character(x)) trimws(x) else x
  })

  bkg_zip_places_simple[] <- lapply(bkg_zip_places_simple, function(x) {
    if (is.character(x)) trimws(x) else x
  })
  # Match data (record linkage) ----
  # suppressWarnings({
  #   data_mun_real <- lapply(seq_len(nrow(data_mun)), function(i) {
  #     plz_pairs <- 
  #       reclin2::pair(
  #         x = data_mun[i, ],
  #         y = bkg_zip_places
  #       )
  #     
  #     reclin2::compare_pairs(
  #       plz_pairs,
  #       on = c(zip_code),
  #       default_comparator = bkggeocoder:::dyn_comparator(target_quality, opts),
  #       inplace = TRUE
  #     )
  #     
  #     weight_plz <- max(plz_pairs[[zip_code]])
  #     
  #     if (!nrow(plz_pairs)) {
  #       cli::cli_abort(c(
  #         "!" = paste(
  #           "None of the zip codes in the {.var {zip_code}} column",
  #           "seems to be a valid zip code.")
  #       ), call = NULL)
  #     }
  #     
  #     # Select data below threshold using a greedy selection algorithm
  #     reclin2::select_greedy(
  #       plz_pairs,
  #       variable = "threshold",
  #       score = c(zip_code),
  #       threshold = 0,
  #       inplace = TRUE
  #     )
  #     
  #     selection_plz <- 
  #       plz_pairs[plz_pairs$threshold]
  #     
  #     # within plz
  #     matched_plz <- 
  #       reclin2::link(
  #         selection_plz,
  #         all_x = TRUE,
  #         all_y = FALSE
  #       ) |> 
  #       cbind(score = weight_plz) |> 
  #       dplyr::pull(paste0(zip_code, ".y"))
  #     
  #     within_plz <- bkg_zip_places[bkg_zip_places[[zip_code]] == matched_plz,]
  #     
  #     mun_pairs <- 
  #       reclin2::pair(
  #         x = data_mun[i, ],
  #         y = within_plz
  #       )
  #     
  #     # Compute string distance scores of the pairs
  #     reclin2::compare_pairs(
  #       mun_pairs,
  #       on = c(place),
  #       # on = "whole_address",
  #       default_comparator = bkggeocoder:::dyn_comparator(target_quality, opts),
  #       inplace = TRUE
  #     )
  #     
  #     weight_place <- max(mun_pairs[[place]])
  #     
  #     # Select data below threshold using a greedy selection algorithm
  #     reclin2::select_greedy(
  #       mun_pairs,
  #       variable = "threshold",
  #       score = c(place),
  #       threshold = 0,
  #       inplace = TRUE
  #     )
  #     
  #     selection_place <- 
  #       mun_pairs[mun_pairs$threshold]
  #     
  #     # Link results with original data
  #     data_linked <- reclin2::link(
  #       selection_place,
  #       all_x = TRUE,
  #       all_y = FALSE
  #     )
  #     
  #     cbind(data_linked, score = (weight_plz + weight_place) / 2)
  #   })
  # })
  
  # data_mun_bak <- data_mun
  # data_mun <- data_mun_bak
  
  suppressWarnings({
    data_mun_pairs <- reclin2::pair_blocking(
      data_mun,
      bkg_zip_places,
      on = c("plz_group")
    )
    
    if (!nrow(data_mun_pairs)) {
      cli::cli_abort(c(
        "!" = paste(
          "None of the zip codes in the {.var {zip_code}} column",
          "seems to be a valid zip code.")
      ), call = NULL)
    }

    reclin2::compare_pairs(
      data_mun_pairs,
      on = c(place, zip_code),
      default_comparator = bkggeocoder:::dyn_comparator(place_match_quality, opts),
      inplace = TRUE
    )

    if (isTRUE(verbose)) {
      cli::cli_progress_step(
        msg = "Calculating place matching scores...",
        msg_done = "Calculated place matching scores.",
        msg_failed = "Could not calculate place matching scores."
      )
    }
    
    # data_mun_pairs <-
    #   reclin2::score_simple(
    #     data_mun_pairs,
    #     "score",
    #     on = c(place, zip_code)
    #   )

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
      type = "all",
      inplace = TRUE
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
      # score = c(zip_code),
      threshold = 0,
      inplace = TRUE
    )

    # Sometimes select_greedy is not greedy enough and leaves out some values
    fix_greedy <- function(x, score) {
      if (!sum(x)) {
        x <- max(score) == score
      }
      x
    }

    data_mun_pairs$threshold <- data_mun_pairs[,
      .(threshold = fix_greedy(threshold, mpost)), by = ".x"
    ][["threshold"]]

    scores <- data_mun_pairs[data_mun_pairs$threshold, c(".x", "mprob")]
    
    # scores <- 
    #   dplyr::tibble(
    #     .x = data_mun_pairs$.x,
    #     score = data_mun_pairs$score / 2
    #   ) |> 
    #   dplyr::group_by(.x) |> 
    #   dplyr::arrange(score) |> 
    #   dplyr::slice_tail(n = 1)

    data_mun_real <- reclin2::link(
      pairs = data_mun_pairs[data_mun_pairs$threshold],
      all_x = TRUE,
      all_y = FALSE
    )

    data_mun_real <- merge(data_mun_real, scores, by = ".x")
      
    data_mun_real <- structure(
      tibble::tibble(
        data_mun_real[[paste0(zip_code, ".x")]],
        data_mun_real[[paste0(place, ".x")]],
        data_mun_real[[paste0(zip_code, ".y")]],
        trimws(data_mun_real[[paste0(place, ".y")]], which = "right"),
        data_mun_real[["mprob"]]
        # data_mun_real[["score"]]
      ),
      names = c(zip_code, place, "zip_code_matched", "place_matched", "score")
    )
  })

  # Combine with input ----
  # data_mun_real <- merge(
  #   .data,
  #   data_mun_real,
  #   by = c(place, zip_code),
  #   all.x = TRUE,
  #   sort = FALSE
  # )
  
  data_mun_real <- merge(
    data_mun_real, bkg_zip_places_simple, by = c("place_matched")
  )
  
  data_mun_real$place_matched <- data_mun_real$place_simple
  data_mun_real$place_simple <- NULL
  
  data_mun_real <- data_mun_real[!duplicated(data_mun_real),]
  
  data_mun_real <- merge(
    .data,
    data_mun_real,
    by = c(place, zip_code),
    all.x = TRUE,
    sort = FALSE
  )

  # Format matched data
  data_matched <- data_mun_real[data_mun_real$score >= place_match_quality &
                                  !is.na(data_mun_real$score), ]
  data_unmatched <- data_mun_real[data_mun_real$score < place_match_quality |
                                    is.na(data_mun_real$score), ]
  unmatched_places <- data_unmatched[, c(zip_code, place)]
  unmatched_places <- unmatched_places[!duplicated(unmatched_places), ]
  rownames(unmatched_places) <- NULL

  if (isTRUE(verbose)) {
    cli::cli_progress_done()
    n_unmatched <- nrow(unmatched_places)
    if (n_unmatched) {
      cli::cli_inform(c("!" = "WARNING: {.val {n_unmatched}} place{?s} left unmatched."))
    }
    if (nrow(data_unmatched) && !nrow(data_matched)) {
      cli::cli_abort("No address could be matched with any place. Check your input!")
    } else if (!nrow(data_unmatched) && nrow(data_matched)) {
      cli::cli_alert_success("All addresses could be place-matched.")
    } else if (nrow(data_unmatched) && nrow(data_matched)) {
      cli::cli_inform(c("i" = paste(
        "{.val {nrow(data_matched)}} out of {.val {nrow(.data)}}",
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
