#' Match data's places and zip codes against official ones
#'
#' @param data data.frame or tibble comprising both a column named \code{place}
#' and \code{zip_code}
#'
#' @noRd

bkg_match_places <-
  function (data,
            cols,
            data_from_server,
            data_path,
            credentials_path,
            place_match_quality,
            verbose) {
    place <- cols[4]
    zip_code <- cols[3]

    data_municipalities <-
      data %>%
      dplyr::select(place := !!place, zip_code := !!zip_code) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        az_group = stringr::str_sub(place, 1, 3),
        plz_group = stringr::str_sub(zip_code, 1, 6)
      )

    if (isTRUE(verbose)) {
      cli::cli_inform("Found {.val {nrow(data_municipalities)}} distinct place{?s}.")
      cli::cli_progress_step(
        msg = "Retrieving place names from database...",
        msg_done = "Retrieved place names from database.",
        msg_failed = "Couldn't retrieve place names from database.")
    }

    places_file <- "zip_places/ga_zip_places.csv.encryptr.bin"
    
    if (isTRUE(data_from_server)) {
      .crypt <- url(file.path("http://10.6.13.132:8000", places_file))
    } else {
      .crypt <- file.path(data_path, places_file)
    }
    
    .crypt <- tryCatch(
      expr = readRDS(.crypt),
      error = function(e) {
        if (isTRUE(data_from_server)) {
          cli::cli_abort(c(
            "Cannot access local server under {.path http://10.6.13.132:8000/}.",
            "!" = "Verify if you are inside the GESIS intranet or set {.var data_from_server = FALSE}"
          ))
        } else {
          cli::cli_abort("Cannot read place data from {.path {data_path}}")
        }
      }
    )

    tmp_out_file <- file("tmp_out_file.csv", "wb") # out file

    openssl::decrypt_envelope(
      .crypt$data, .crypt$iv,
      .crypt$session,
      key = file.path(credentials_path, "id_rsa"),
      password = readLines(file.path(credentials_path, "pwd"))
    ) %>%
      writeBin(tmp_out_file)

    close(tmp_out_file)

    bkg_zip_places <-
      data.table::fread(
        "tmp_out_file.csv",
        colClasses = 'character',
        encoding = "UTF-8"
      )

    unlink("tmp_out_file.csv")

    if (isTRUE(verbose)) {
      cli::cli_progress_done()
      cli::cli_progress_step(
        msg = "Linking place records...",
        msg_done = "Place record linkage finished.",
        msg_failed = "Couldn't link place records."
      )
    }
    
    bkg_zip_places <-
      bkg_zip_places %>%
      dplyr::mutate(
        az_group = stringr::str_sub(place, 1, 3),
        plz_group = stringr::str_sub(zip_code, 1, 6)
      )
    
    # match data (record linking)
    suppressWarnings({
      data_municipalities_pairs <- reclin2::pair_blocking(
        data_municipalities,
        bkg_zip_places,
        on = c("az_group", "plz_group")
      )
      
      data_municipalities_pairs <- reclin2::compare_pairs(
        data_municipalities_pairs,
        on = c("place", "zip_code"),
        default_comparator = reclin2::jaro_winkler(threshold = place_match_quality)
      )
      
      estimates <- reclin2::problink_em(
        formula = ~.x + .y + place + zip_code,
        data = data_municipalities_pairs
      )

      prediction <- reclin2:::predict.problink_em(
        estimates,
        pairs = data_municipalities_pairs,
        add = TRUE
      )
      
      selection <- reclin2::select_greedy(
        pairs = prediction,
        variable = "selected",
        score = "weights"
      )

      data_municipalities_real <- reclin2::link(
        pairs = selection,
        all_x = TRUE,
        all_y = FALSE
      ) %>%
        tibble::as_tibble() %>%
        dplyr::select(
          !!place := place.x,
          !!zip_code := zip_code.x,
          place_matched = place.y,
          zip_code_matched = zip_code.y
        )
    })

    unmatched_places <-
      data_municipalities_real %>%
      dplyr::filter(is.na(place_matched))
    
    n_unmatched <- nrow(unmatched_places)
    
    if (isTRUE(verbose)) {
      if (n_unmatched) {
        cli::cli_inform(c("!" = "WARNING: {.val { n_unmatched}} place{?s} left unmatched."))
      }
      cli::cli_progress_done()
    }

    # add to dataset
    data_matched <-
      dplyr::left_join(
        data,
        data_municipalities_real,
        by = c(place, zip_code)
      ) %>%
      dplyr::filter(!is.na(place_matched)) %>%
      dplyr::distinct()

    data_unmatched <-
      dplyr::anti_join(
        data,
        data_matched,
        by = "id"
      )

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
