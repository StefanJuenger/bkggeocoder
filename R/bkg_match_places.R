#' Match data's places and zip codes against official ones
#'
#' @param data data.frame or tibble comprising both a column named \code{place}
#' and \code{zip_code}
#'
#' @noRd

bkg_match_places <-
  function (
    data,
    id_variable,
    place,
    zip_code,
    data_path,
    credentials_path,
    place_match_quality,
    echo
  ) {

    if (isTRUE(echo)) {
      message("Starting retrieving place names as in database...")
    }

    data_municipalities <-
      data %>%
      dplyr::select(place := !!place, zip_code := !!zip_code) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        az_group = stringr::str_sub(place, 1, 3),
        plz_group = stringr::str_sub(zip_code, 1, 6)
      )

    if (isTRUE(echo)) {
      message(paste0("Found ", nrow(data_municipalities), " distinct places."))
    }

    .crypt <-
      paste0(data_path, "/zip_places/ga_zip_places.csv.encryptr.bin") %>%
      readRDS()

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
    suppressWarnings(
      data_municipalities_real <-
        reclin::pair_blocking(
          data_municipalities,
          bkg_zip_places,
          blocking_var = c("az_group", "plz_group"),
          large = FALSE
        ) %>%
        reclin::compare_pairs(
          by = c("place", "zip_code"),
          default_comparator =
            reclin::jaro_winkler(threshold = place_match_quality)
        ) %>%
        reclin::score_problink() %>%
        reclin::select_greedy() %>%
        reclin::link(all_x = TRUE, all_y = FALSE) %>%
        tibble::as_tibble() %>%
        dplyr::select(
          !!place := place.x,
          !!zip_code := zip_code.x,
          place_matched = place.y,
          zip_code_matched = zip_code.y
        )
    )

    unmatched_places <-
      data_municipalities_real %>%
      dplyr::filter(is.na(place_matched))

    if (isTRUE(echo)) {
      message(
        paste0(
          "\nWARNING: ",
          nrow(unmatched_places),
          " place(s) left unmatched."
        )
      )
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
        by = id_variable
      )

    if (isTRUE(echo)) {
      message(
        paste0(
          "WARNING: ",
          nrow(data_unmatched),
          " address(es) cannot be geocoded.\n\n",
          nrow(data_matched),
          " addresses can be geocoded."
        )
      )
    }

    list(
      matched = data_matched,
      unmatched = data_unmatched,
      unmatched_places = unmatched_places
    )

  }
