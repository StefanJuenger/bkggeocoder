bkg_match_addresses <- function(
  data_edited,
  cols,
  house_coordinates,
  target_quality,
  opts,
  verbose
) {
  street <- cols[1]
  house_number <- cols[2]
  zip_code <- cols[3]
  place <- cols[4]

  # Prepare data ----
  data_edited_fixed_street <- gsub("tr[.]", "tra\u00dfe", data_edited$matched[[street]])
  data_edited$matched$whole_address <- trimws(paste0(
    data_edited_fixed_street,
    if (house_number %in% colnames(data_edited$matched)) {
      paste0(" ", data_edited$matched[[house_number]])
    }
  ))

  # Prepare BKG data ----
  house_coordinates$whole_address <- trimws(paste0(
    house_coordinates$street, " ", house_coordinates$house_number,
    house_coordinates$house_number_add
  ))
  
  if (isTRUE(verbose)) {
    cli::cli_progress_bar(
      name = "Matching address data",
      total = nrow(data_edited$matched),
      format = paste(
        "{cli::pb_name} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} addresses |",
        "ETA {cli::pb_eta}"
      ),
      format_failed = "Failed at address {cli::pb_current}/{cli::pb_total}."
    )
  }
  
  joined_data <- list()
  
  # Match data with BKG data (record linkage) ----
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
    data_edited_compare <- reclin2::compare_pairs(
      data_edited_pairs,
      on = "whole_address",
      default_comparator = dyn_comparator(target_quality, opts),
      inplace = TRUE
    )
    
    weight_i <- max(data_edited_compare$whole_address)
    
    # Select data below threshold using a greedy selection algorithm
    selection <- reclin2::select_greedy(
      data_edited_compare,
      variable = "threshold",
      score = "whole_address",
      threshold = target_quality
    )
    selection <- selection[selection$threshold]
    
    # Link results with original data
    joined_data[[i]] <- reclin2::link(
      selection,
      all_x = TRUE,
      all_y = FALSE
    )
    
    joined_data[[i]] <- cbind(joined_data[[i]], score = weight_i)
  }
  
  if (isTRUE(verbose)) {
    cli::cli_progress_done()
  }
  
  joined_data <- do.call(rbind, joined_data)

  # Fix scores ----
  regex_chr <- "[0-9]+[a-z]*"
  hn.x <- unlist(regmatches(
    joined_data$whole_address.x,
    regexec(regex_chr, joined_data$whole_address.x)
  ))
  hn.y <- regmatches(
    joined_data$whole_address.x,
    regexec(regex_chr, joined_data$whole_address.y)
  )
  hn.y <- sapply(hn.y, function(x) if (!length(x)) NA else x)
  hn_mismatch <- !hn.x == hn.y & !is.na(hn.x) & !is.na(hn.y)
  incorrect_scores <- joined_data$score[hn_mismatch]
  joined_data$score[hn_mismatch] <- incorrect_scores - 0.05
  
  geocoded <- joined_data[!is.na(joined_data$RS), ]
  
  if (isTRUE(verbose)) {
    if (!nrow(joined_data)) {
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
  
  joined_data
}
