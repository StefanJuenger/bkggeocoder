bkg_match_addresses <-
  function(
    data_edited,
    house_coordinates,
    target_quality
  ) {

    data_edited$matched$whole_address <-
      paste0(
        data_edited$matched$street  %>%
          gsub("Str[.]", "Straße", .) %>%
          gsub("str[.]", "straße", .),
        if ("house_number" %in% colnames(data_edited$matched)) {
          paste0(" ", data_edited$matched$house_number)
        } else {
          ""
        }
      ) %>%
      stringr::str_trim(.)

    house_coordinates$whole_address <-
      paste0(
        house_coordinates$street, " ", house_coordinates$house_number,
        house_coordinates$house_number_add
      ) %>%
      stringr::str_trim()

    fuzzy_joined_data <-
      vector(mode = "list", length = nrow(data_edited$matched))

    # initialize progress bar
    pb <-
      progress::progress_bar$new(
        total = nrow(data_edited$matched),
        force = TRUE,
        clear = FALSE
      )

    pb$tick(0)

    for (i in 1:nrow(data_edited$matched)) {

      pb$tick()

      fuzzy_joined_data[[i]] <-
        reclin::pair_blocking(
          data_edited$matched[i,],
          house_coordinates[.(data_edited$matched[i,]$place_matched)],
          large = FALSE
        ) %>%
        reclin::compare_pairs(
          by = "whole_address",
          default_comparator = reclin::jaro_winkler(target_quality),
          overwrite = TRUE
        ) %>%
        reclin::score_simsum() %T>%
        {weight_i <<- max(.$simsum)} %>%
        reclin::select_greedy(threshold = target_quality) %>%
        reclin::link(all_x = TRUE, all_y = FALSE) %>%
        dplyr::bind_cols(score = weight_i)
    }

    fuzzy_joined_data %>%
      do.call(rbind, .)
  }
