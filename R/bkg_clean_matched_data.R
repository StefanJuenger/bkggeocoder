bkg_clean_matched_addresses <-
  function(
    fuzzy_joined_data,
    zip_code,
    place,
    id_variable
  ) {

    fuzzy_joined_data <-
      fuzzy_joined_data %>%
      dplyr::rename_at(
        dplyr::vars(dplyr::contains(".x")),
        ~stringr::str_replace(., ".x", "_input")) %>%
      dplyr::rename_at(
        dplyr::vars(dplyr::contains(".y")),
        ~stringr::str_replace(., ".y", "_output"))

    # set missing coordinates to zero to be compliant with sf
    fuzzy_joined_data$x[is.na(fuzzy_joined_data$x)] <- 0
    fuzzy_joined_data$y[is.na(fuzzy_joined_data$y)] <- 0

    # clean dataset
    fuzzy_joined_data <-
      fuzzy_joined_data %>%
      dplyr::mutate(
        address_input =
          paste(whole_address_input, {{zip_code}}, {{place}}),
        address_output =
          paste(whole_address_add)#, zip_code_output, place_output)
      ) %>%
      dplyr::mutate(
        GEM = RS %>% stringr::str_sub(1, 9),
        KRS = RS %>% stringr::str_sub(1, 5),
        RBZ = RS %>% stringr::str_sub(1, 3),
        STA = RS %>% stringr::str_sub(1, 2)
      ) %>%
      sf::st_as_sf(coords = c("x", "y"), crs = 25832) %>%
      sf::st_transform(3035) %>%
      dplyr::mutate(
        Gitter_ID_1km = spt_create_inspire_ids(data = ., type = "1km"),
        Gitter_ID_100m = spt_create_inspire_ids(data = ., type = "100m")
      ) %>%
      tibble::as_tibble() %>%
      dplyr::select(
        {{id_variable}},
        score,
        address_input,
        address_output,
        RS, GEM, KRS, RBZ, STA, dplyr::contains("Gitter"), geometry
      ) %>%
      dplyr::mutate(
        source =
          "© GeoBasis-DE / BKG, Deutsche Post Direkt GmbH, Statistisches Bundesamt, Wiesbaden (2021)"
      )

    fuzzy_joined_data
  }
