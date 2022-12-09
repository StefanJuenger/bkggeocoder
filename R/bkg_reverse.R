#' @rdname bkg_geocode
#' 
#' @export
bkg_reverse <- function(
  .data,
  epsg = 3035,
  radius = 1000L,
  target_quality = NULL,
  ...,
  join_with_original = FALSE,
  identifiers = "rs",
  verbose = TRUE
) {
  location <- geometry <- NULL
  geom_type <- unique(sf::st_geometry_type(.data))
  
  args <- as.list(environment())
  
  if (length(geom_type) > 1) {
    cli::cli_abort("Only datasets with a single geometry type can be reverse geocoded.")
  }

  if (geom_type %in% "POINT") {
    location <- sf::st_geometry(.data)
  } else {
    geometry <- sf::st_geometry(.data)
  }
  
  if (inherits(.data, "sfc")) {
    .data <- sf::st_as_sf(.data)
  }
  
  .data <- cbind(data.frame(.iid = row.names(.data)), .data)
  
  if (isTRUE(verbose)) {
    cli::cli_progress_bar(
      name = "Geocoding addresses",
      total = nrow(.data),
      format = paste(
        "{cli::pb_name} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} |",
        "ETA {cli::pb_eta}"
      ),
      format_failed = "Failed at address {cli::pb_current}/{cli::pb_total}."
    )
  }
  
  reversed_data <- lapply(seq_len(nrow(.data)), function(i) {
    if (isTRUE(verbose)) {
      cli::cli_progress_update(.envir = parent.frame(2))
    }
    
    res <- bkg_reverse_single(
      location[i],
      geometry[i],
      radius = radius,
      epsg = epsg,
      count = 1,
      minscore = target_quality,
      clean = FALSE,
      ...
    )
    
    res$.iid <- i
    res
  })
  
  reversed_data <- rbind_list(reversed_data)
  reversed_data <- sf::st_sf(reversed_data, crs = epsg, sf_column_name = "geometry")
  reversed_data <- clean_geocode(reversed_data, NULL, NULL, NULL, NULL, NULL, identifiers)

  if (isTRUE(join_with_original)) {
    reversed_data <- merge(
      .data,
      reversed_data,
      by = ".iid",
      all.x = TRUE,
      sort = TRUE
    )
    
    reversed_data <- sf::st_as_sf(tibble::as_tibble(reversed_data))
  }
  
  missing <- setdiff(as.numeric(row.names(.data)), reversed_data$.iid)
  
  if (length(missing)) {
    not_reversed <- .data[missing, ]
  }
  
  # Remove internal identifier
  reversed_data$.iid <- NULL
  not_reversed$.iid <- NULL
  
  output_list <- structure(
    list(
      reversed = reversed_data,
      not_reversed = not_reversed,
      call = match.call()
    ),
    type = "bkg",
    args = args,
    class = "ReverseResults"
  )
  
  output_list
}
