#' Create 1km and 100m INSPIRE IDs
#'
#' Create 1 km² and 100m X 100m INSPIRE IDs from coordinates
#'
#' @param data Object of class \code{sf} containing point geometries
#' @param type Character string for the requested ID type
#' @param column_name Output column name prefix. Defaults to "Gitter_ID_{type}".
#' @param combine Whether to combine the input data with the output values.
#' @return tibble
#'
#' @noRd

spt_create_inspire_ids <- function(
  data,
  type = c("1km", "100m"),
  column_name = "Gitter_ID_",
  combine = FALSE
) {

  if (sf::st_crs(data)$epsg != 3035) {
    data <- sf::st_transform(data, 3035)
  }

  coordinate_pairs <- tibble::as_tibble(sf::st_coordinates(data))
  
  id_name <- paste0(column_name, type)

  inspire_ids <- sprintf(
    "%sN%sE%s",
    type,
    substr(as.character(coordinate_pairs$Y), 1, 4 + (type == "100m")),
    substr(as.character(coordinate_pairs$X), 1, 4 + (type == "100m"))
  )

  if (isTRUE(combine)) {
    inspire_ids <- cbind(data, data.frame(id_name = inspire_ids))
    names(inspire_ids)[names(inspire_ids) == "id_name"] <- id_name
  } else {
    inspire_ids
  }
}
