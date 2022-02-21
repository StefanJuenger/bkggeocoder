bkg_clean_matched_addresses <-
  function(
    fuzzy_joined_data,
    cols,
    verbose
  ) {
    zip_code <- cols[3]
    place <- cols[4]
    
    if (isTRUE(verbose)) {
      cli::cli_progress_step(
        msg = "Cleaning up geocoding output...",
        msg_done = "Cleaned up geocoding output.",
        msg_failed = "Failed to clean up geocoding output."
      )
    }
    
    fuzzy_joined_data <- fuzzy_joined_data %>%
      dplyr::rename_at(
        dplyr::vars(dplyr::contains(".x")),
        ~stringr::str_replace(., ".x", "_input")
      ) %>%
      dplyr::rename_at(
        dplyr::vars(dplyr::contains(".y")),
        ~stringr::str_replace(., ".y", "_output")
      )

    # Set missing coordinates to zero to be compliant with sf
    fuzzy_joined_data$x[is.na(fuzzy_joined_data$x)] <- 0
    fuzzy_joined_data$y[is.na(fuzzy_joined_data$y)] <- 0

    # clean dataset
    fuzzy_joined_data <- fuzzy_joined_data %>%
      sf::st_as_sf(coords = c("x", "y"), crs = 25832) %>%
      dplyr::mutate(
        address_input = paste(.data$whole_address_input,
                              .data$zip_code_input,
                              .data$place_input),
        address_output = .data$whole_address_add,
        GEM = stringr::str_sub(.data$RS, 1, 9),
        KRS = stringr::str_sub(.data$RS, 1, 5),
        RBZ = stringr::str_sub(.data$RS, 1, 3),
        STA = stringr::str_sub(.data$RS, 1, 2),
        Gitter_ID_1km = spt_create_inspire_ids(data = ., type = "1km"),
        Gitter_ID_100m = spt_create_inspire_ids(data = ., type = "100m"),
        source = "\u00a9 GeoBasis-DE / BKG, Deutsche Post Direkt GmbH, Statistisches Bundesamt, Wiesbaden (2021)"
      ) %>%
      tibble::as_tibble() %>%
      dplyr::select(
        .data$id, .data$score, .data$address_input, .data$address_output,
        .data$RS, .data$GEM, .data$KRS, .data$RBZ, .data$STA,
        dplyr::contains("Gitter"), .data$geometry
      ) %>%
      sf::st_as_sf()

    if (isTRUE(verbose)) {
      cli::cli_progress_done()
    }
    
    fuzzy_joined_data
  }
