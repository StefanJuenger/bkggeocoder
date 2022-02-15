bkg_match_addresses <-
  function(
    data_edited,
    cols,
    house_coordinates,
    target_quality,
    verbose
  ) {
    street <- cols[1]
    house_number <- cols[2]
    zip_code <- cols[3]
    place <- cols[4]

    data_edited$matched$whole_address <-
      paste0(
        data_edited$matched[[street]]  %>%
          gsub("Str[.]", "Straße", .) %>%
          gsub("str[.]", "straße", .),
        if (house_number %in% colnames(data_edited$matched)) {
          paste0(" ", data_edited$matched[[house_number]])
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
    if (isTRUE(verbose)) {
      pb <-
        progress::progress_bar$new(
          total = nrow(data_edited$matched),
          force = TRUE,
          clear = FALSE
        )
      pb$tick(0)
    }

    for (i in 1:nrow(data_edited$matched)) {

      if (isTRUE(verbose)) {
        pb$tick()
      }
        
      data_edited_pairs <- reclin2::pair(
        x = data_edited$matched[i, ],
        y = house_coordinates[
          place == data_edited$matched[i,]$place_matched &
          zip_code == data_edited$matched[i,]$zip_code_matched
        ]
      )
      
      data_edited_pairs <- reclin2::compare_pairs(
        data_edited_pairs,
        on = "whole_address",
        default_comparator = reclin::jaro_winkler(target_quality),
        inplace = TRUE
      )
      
      weight_i <- max(data_edited_pairs$whole_address)
      
      selection <- reclin2::select_greedy(
        data_edited_pairs,
        variable = "threshold",
        score = "whole_address",
        threshold = target_quality
      )
      selection <- selection[selection$threshold]
      
      fuzzy_joined_data[[i]] <- reclin2::link(selection, all_x = TRUE, all_y = FALSE) %>%
        dplyr::bind_cols(score = weight_i)
    }

    fuzzy_joined_data <- do.call(rbind, fuzzy_joined_data)

    # fix scores
    fuzzy_joined_data <-
      fuzzy_joined_data %>%
      dplyr::mutate(
        score = ifelse(
          stringr::str_extract(
            whole_address.x,
            "[0-9]+[a-z]*"
          ) ==
            stringr::str_extract(
              whole_address.y,
              "[0-9]+[a-z]*"
            ),
          score,
          score - .05
        )
      )
    
    if (isTRUE(verbose)) {
      if (!nrow(fuzzy_joined_data)) {
        stop("No address could be geocoded. Check your input!")
      } else if (nrow(fuzzy_joined_data) == nrow(data_edited$matched)) {
        message("SUCCESS: All place-matched addresses could be geocoded.")
      } else {
        message(sprintf(
          "WARNING: %s out of %s place-matched address(es) could be geocoded.",
          nrow(fuzzy_joined_data),
          nrow(data_edited$matched)
        ))
      }
    }
    
    fuzzy_joined_data
  }
