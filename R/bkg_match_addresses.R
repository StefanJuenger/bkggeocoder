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

    # Convert input address data to character strings
    data_edited$matched$whole_address <- paste0(
      data_edited$matched[[street]] %>%
        gsub("Str[.]", "Straße", .) %>%
        gsub("str[.]", "straße", .),
      if (house_number %in% colnames(data_edited$matched)) {
          paste0(" ", data_edited$matched[[house_number]])
        }
      ) %>%
      stringr::str_trim(.)

    # Convert BKG address data to character strings
    house_coordinates$whole_address <- paste0(
      house_coordinates$street, " ", house_coordinates$house_number,
      house_coordinates$house_number_add
    ) %>%
      stringr::str_trim()

    # Initialize progress bar
    if (isTRUE(verbose)) {
      cli::cli_progress_bar(
        name = "Matching address data",
        total = nrow(data_edited$matched)
      )
    }
    
    fuzzy_joined_data <- list()

    # Match each input address to the BKG address data subset
    for (i in 1:nrow(data_edited$matched)) {

      if (isTRUE(verbose)) {
        cli::cli_progress_update()
      }
      
      # Create a pairs object with matching places and zip codes
      data_edited_pairs <- reclin2::pair(
        x = data_edited$matched[i, ],
        y = house_coordinates[
          place == data_edited$matched[i,]$place_matched &
          zip_code == data_edited$matched[i,]$zip_code_matched
        ]
      )
      
      # Compute Jaro-Winkler scores of the pairs
      data_edited_pairs <- reclin2::compare_pairs(
        data_edited_pairs,
        on = "whole_address",
        default_comparator = reclin::jaro_winkler(target_quality),
        inplace = TRUE
      )
      
      weight_i <- max(data_edited_pairs$whole_address)
      
      # Select data below threshold using a greedy selection algorithm
      selection <- reclin2::select_greedy(
        data_edited_pairs,
        variable = "threshold",
        score = "whole_address",
        threshold = target_quality
      )
      selection <- selection[selection$threshold]
      
      # Link results with original data
      fuzzy_joined_data[[i]] <- reclin2::link(
        selection,
        all_x = TRUE,
        all_y = FALSE
      ) %>%
        dplyr::bind_cols(score = weight_i)
    }
    
    if (isTRUE(verbose)) {
      cli::cli_progress_done()
    }

    fuzzy_joined_data <- do.call(rbind, fuzzy_joined_data)

    # Fix scores
    regex_chr <- "[0-9]+[a-z]*"
    fuzzy_joined_data <- fuzzy_joined_data %>%
      dplyr::mutate(
        score = ifelse(
          stringr::str_extract(whole_address.x, regex_chr) ==
          stringr::str_extract(whole_address.y, regex_chr),
          yes = score,
          no = score - .05
        )
      )
    
    geocoded <- dplyr::filter(fuzzy_joined_data, !is.na(RS))
    
    if (isTRUE(verbose)) {
      if (!nrow(fuzzy_joined_data)) {
        cli::cli_abort("No address could be geocoded. Check your input!")
      } else if (nrow(geocoded) == nrow(data_edited$matched)) {
        cli::cli_alert_success("All place-matched addresses could be geocoded.")
      } else {
        cli::cli_alert_info(paste(
          "{.val {nrow(geocoded)}} out of",
          "{.val {nrow(data_edited$matched)}} place-matched address{?es} could be geocoded."
        ))
      }
    }
    
    fuzzy_joined_data
  }
