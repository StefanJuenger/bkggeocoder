bkg_match_addresses <- function(
  data_edited,
  cols,
  house_coordinates,
  opts,
  verbose
) {
  street <- cols[1]
  house_number <- ifelse(length(cols) == 4, cols[2], "")
  zip_code <- ifelse(length(cols) == 4, cols[3], cols[2])
  place <- ifelse(length(cols) == 4, cols[4], cols[3])
  
  data_edited$matched <- data_edited$matched[seq_len(ncol(data_edited$matched) - 1)]

  # Prepare data ----
  if (verbose) {
    cli::cli_progress_step(
      msg = "Preparing data for address matching...",
      msg_done = "Prepared data for address matching.",
      msg_failed = "Could not prepare data for address matching."
    )
  }
  
  # Fix Mannheim square addresses
  data_edited$matched[, street] <- gsub(
    "^([A-Z])([1-9])$", "\\1 \\2",
    data_edited$matched[, street]
  )
  
  # Expand Str. to Straße
  data_edited_fixed_street <- gsub(
    "str[\\.]?\\s", "stra\u00dfe ",
    data_edited$matched[[street]],
    ignore.case = TRUE
  )
  
  # Create address string
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
    if (isTRUE(verbose)) cli::cli_progress_update()
    # Create a pairs object with matching places and zip codes
    within_place <- house_coordinates[
      place == data_edited$matched[i,]$place_matched &
        zip_code == data_edited$matched[i,]$zip_code_matched
    ]
    data_edited_pairs <- reclin2::pair(
      x = data_edited$matched[i, ],
      y = within_place
    )

    # Compute string distance scores of the pairs
    reclin2::compare_pairs(
      data_edited_pairs,
      on = "whole_address",
      default_comparator = dyn_comparator(target_quality, opts),
      inplace = TRUE
    )

    weight_i <- max(data_edited_pairs$whole_address)

    # Select data below threshold using a greedy selection algorithm
    reclin2::select_greedy(
      data_edited_pairs,
      variable = "threshold",
      score = "whole_address",
      threshold = 0,
      inplace = TRUE
    )
    selection <- data_edited_pairs[data_edited_pairs$threshold]

    # Link results with original data
    data_linked <- reclin2::link(
      selection,
      all_x = TRUE,
      all_y = FALSE
    )
    
    joined_data[[i]] <- cbind(data_linked, score = weight_i)
  }

  if (isTRUE(verbose)) {
    cli::cli_progress_done()
  }

  joined_data <- do.call(rbind, joined_data)

  # Fix names
  for (name in c(street, house_number, zip_code, place)) {
    if (name %in% names(joined_data)) {
      names(joined_data)[names(joined_data) == name] <- paste0(name, ".x")
    }
  }
  
  # Fix scores ----
  regex_chr <- "[0-9]+[a-z]*"
  hn.x <- unlist(match_regex(joined_data$whole_address.x, regex_chr))
  hn.y <- match_regex(joined_data$whole_address.y, regex_chr)
  hn.y <- vapply(hn.y, function(x) if (!length(x)) NA_character_ else x, character(1))
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
