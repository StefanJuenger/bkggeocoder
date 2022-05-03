bkg_clean_matched_addresses <- function(
  messy_data,
  cols,
  verbose
) {
  zip_code <- ifelse(length(cols) == 4, cols[3], cols[2])
  place <- ifelse(length(cols) == 4, cols[4], cols[3])

  if (isTRUE(verbose)) {
    cli::cli_progress_step(
      msg = "Cleaning up geocoding output...",
      msg_done = "Cleaned up geocoding output.",
      msg_failed = "Failed to clean up geocoding output."
    )
  }

  is_out <- grepl(".y", names(messy_data), fixed = TRUE)
  is_inp <- grepl(".x", names(messy_data), fixed = TRUE)
  new_out <- gsub(".y", "_output", names(messy_data)[is_out], fixed = TRUE)
  new_in <- gsub(".x", "_input", names(messy_data)[is_inp], fixed = TRUE)
  names(messy_data)[is_out] <- new_out
  names(messy_data)[is_inp] <- new_in

  # Clean dataset
  not_so_messy_data <- tibble::tibble(
    .iid = messy_data$.iid,
    score = messy_data$score,
    address_input = paste(
      messy_data$whole_address_input,
      messy_data[, zip_code_input],
      messy_data[, place_input]
    ),
    street_input = messy_data$street_input,
    house_number_input = messy_data$house_number_input,
    zip_code_input = messy_data$zip_code_input,
    place_input = messy_data$place_input,
    address_output = messy_data$whole_address_add,
    street_output = messy_data$street_output,
    house_number_output = messy_data$house_number_output,
    zip_code_output = messy_data$zip_code_output,
    place_output = messy_data$place_output,
    RS  = messy_data$RS,
    GEM = substr(messy_data$RS, 1, 9),
    KRS = substr(messy_data$RS, 1, 5),
    RBZ = substr(messy_data$RS, 1, 3),
    STA = substr(messy_data$RS, 1, 2),
    x = messy_data$x,
    y = messy_data$y,
    source = "\u00a9 GeoBasis-DE / BKG, Deutsche Post Direkt GmbH, Statistisches Bundesamt, Wiesbaden (2021)"
  )

  not_so_messy_data_sf <- sf::st_as_sf(
    not_so_messy_data,
    coords = c("x", "y"),
    crs = 25832,
    remove = TRUE,
    na.fail = FALSE
  )

  clean_data <- tibble::add_column(
    not_so_messy_data_sf,
    Gitter_ID_1km = spt_create_inspire_ids(data = not_so_messy_data_sf, type = "1km"),
    Gitter_ID_100m = spt_create_inspire_ids(data = not_so_messy_data_sf, type = "100m"),
    .before = 18
  )

  if (isTRUE(verbose)) {
    cli::cli_progress_done()
  }

  clean_data
}
