#' Create 1km and 100m INSPIRE IDs
#'
#' Create 1 km² and 100m X 100m INSPIRE IDs from coordinates
#'
#' @param data Object of class \code{sf} containing point geometries
#' @param type Character string for the requested ID type
#' @return tibble
#'
#' @noRD

spt_create_inspire_ids <- function(
  data,
  type = c("1km", "100m"),
  column_name = "Gitter_ID_",
  combine = FALSE
) {

  if (sf::st_crs(data)$epsg != 3035) {
    data <- sf::st_transform(data, 3035)
  }

  coordinate_pairs <- data %>%
    sf::st_coordinates() %>%
    as_tibble()

  id_name <- glue::glue("{column_name}{type}")

  loop_to_evaluate <- dplyr::case_when(
    type == "1km" ~ glue::glue(
      "1kmN{substr(coordinate_pairs$Y %>% as.character(), 1, 4)}",
      "E{substr(coordinate_pairs$X %>% as.character(), 1, 4)}"
    ) %>%
      as.character(),
    type == "100m" ~ glue::glue(
      "100mN{substr(coordinate_pairs$Y %>% as.character(), 1, 5)}",
      "E{substr(coordinate_pairs$X %>% as.character(), 1, 5)}"
    ) %>%
      as.character()
  )

  expression_to_evaluate <- rlang::expr(!!rlang::sym(id_name) <- loop_to_evaluate)

  eval(expression_to_evaluate)

  if (isTRUE(combine)) {
    dplyr::bind_cols(data, !!id_name := get(id_name))
  } else {
    get(id_name)
  }
}
