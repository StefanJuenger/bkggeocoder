bkg_state_dist <- function(.data) {
  zips <- readr::read_delim(
    "https://gist.githubusercontent.com/jbspeakr/4565964/raw/4083f8b8933f0e9a64dafc943ecbae496f9d65d2/German-Zip-Codes.csv",
    delim = ";",
    col_select = c("Plz", "Bundesland"),
    col_types = c("c", "c")
  )
  
  zip_col <- attr(.data, "args")$cols
  zip_col <- if (length(zip_col) == 3) zip_col[2] else zip_col[3]

  if (all(is.numeric(.data$geocoded[[zip_col]]))) {
    .data$geocoded[, zip_col] <- vapply(.data$geocoded[[zip_col]], function(zip) {
      zip <- as.character(zip)
      if (nchar(zip) == 4) {
        paste0(0, zip)
      } else zip
    }, FUN.VALUE = character(1))
  }
  
  by <- c("Plz")
  names(by) <- zip_col
  
  .data_states <- merge(
    sf::st_drop_geometry(rbind(.data$geocoded, .data$not_geocoded)),
    zips[!duplicated(zips$Plz), ],
    by.x = zip_col,
    by.y = "Plz",
    all.x = TRUE
  )
  
  geocoded <- .data_states[!is.na(.data_states$score), ]
  
  state_geocoded <- table(
    geocoded$Bundesland,
    rep("count", nrow(geocoded))
  )
  state_all <- table(
    state = .data_states$Bundesland,
    rep("count", nrow(.data_states))
  )
  
  structure(
    tibble::tibble(
      state = rownames(state_all),
      count = c(state_geocoded) / c(state_all)
    ),
    class = c("state_dist", "tbl_df", "tbl", "data.frame")
  )
}