#' @rdname bkg_geocode
#' 
#' @param .data For \code{bkg_reverse}, .data is expected to be an \code{sf}
#' data.frame containing geometries of a single geometry type. Geometry types
#' correspond to the allowed geometry types in
#' \code{\link[bkggeocoder]{bkg_reverse_single}}
#' 
#' @export
bkg_reverse <- function(
  .data,
  epsg = 3035,
  target_quality = NULL,
  ...,
  join_with_original = FALSE,
  verbose = TRUE
) {
  location <- geometry <- NULL
  geom_type <- unique(sf::st_geometry_type(.data))
  
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
      epsg = epsg,
      count = 1,
      minscore = target_quality,
      ...
    )
    
    res$.iid <- i
    res
  })
  
  reversed_data <- rbind_list(reversed_data)
  reversed_data <- sf::st_sf(reversed_data, crs = epsg, sf_column_name = "geometry")
  
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
  
  # Remove internal identifier
  reversed_data$.iid <- NULL
  
  output_list <- structure(
    list(
      reversed = reversed_data,
      call = match.call()
    ),
    type = "bkg_reverse"
  )
  
  class(output_list) <- c("ReverseResults", class(output_list))
  
  output_list
}