#' Match data's places and zip codes against official ones
#'
#' @param data data.frame or tibble comprising both a column named \code{place}
#' and \code{zip_code}
#'
#' @noRd

bkg_match_places <-
  function (
    data,
    cols,
    data_from_server,
    data_path,
    credentials_path,
    place_match_quality,
    verbose
  ) {
    if (isTRUE(verbose)) {
      message("Retrieving place names from database...")
    }
    
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
      message(paste0("Found ", nrow(data_municipalities), " distinct places."))
    }

    places_file <- "zip_places/ga_zip_places.csv.encryptr.bin"
    
    if (isTRUE(data_from_server)) {
      .crypt <- file.path("http://10.6.13.132:8000", places_file) %>%
        url()
    } else {
      .crypt <- file.path(data_path, places_file)
    }
    .crypt <- readRDS(.crypt)

    tmp_out_file <- file("tmp_out_file.csv", "wb") # out file

    openssl::decrypt_envelope(
      .crypt$data, .crypt$iv,
      .crypt$session,
      key = paste0(credentials_path, "/id_rsa"),
      password = readLines(paste0(credentials_path, "/pwd"))
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
    if (isTRUE(verbose) && n_unmatched) {
      message(sprintf("WARNING: %s place(s) left unmatched.", n_unmatched))
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
        stop("No address could be matched with any place. Check your input!")
      } else if (!nrow(data_unmatched) && nrow(data_matched)) {
        message("SUCCESS: All addresses could be place-matched.")
      } else if (nrow(data_unmatched) && nrow(data_matched)) {
        message(sprintf(
          "WARNING: %s out of %s address(es) could be place-matched.",
          nrow(data_matched),
          nrow(data)
        ))
      }
    }

    list(
      matched = data_matched,
      unmatched = data_unmatched,
      unmatched_places = unmatched_places
    )
  }
