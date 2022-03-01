#' Geocoding of a single address to a geo-coordinate
#'
#' @description Geocoding of a single address to a geo-coordinate through the
#' BKG Geocoder WFS
#'
#' @param street Character string for the street name
#' @param house_number Character string for the house number
#' @param zip_code Character string for the zip code
#' @param place Character string for the place (i.e., municipality/city)
#' @param epsg Character string or numeric of an EPSG code describing the
#' requested CRS
#' @param max_features Numeric or character specifying the maximum amount of
#' results to return. If \code{> 1}, returns matches in descending order by
#' BKG score.
#' @returns Object of class \code{sf} containing the geometries of the input
#' address
#'
#' @examples
#'
#' \dontrun{
#' bkg_geocode_single_address(
#'   street       = "Unter Sachsenhausen",
#'   house_number = "6-8",
#'   zip_code     = 50667,
#'   place        = "Köln",
#'   epsg         = "3035"
#' )
#' }

bkg_geocode_single_address <- function (
  street,
  house_number,
  zip_code,
  place,
  epsg = "3035",
  bbox = NULL,
  max_features = 1
) {
  # Create POST body ----
  req_list <- list(
    "wfs:GetFeature" = structure(
      list(
        "wfs:Query" = structure(
          list(
            "ogc:Filter" = list(
              "ogc:And" = list(
                "ogc:PropertyIsLike" = structure(
                  list(
                    "ogc:PropertyName" = list("strasse"),
                    "ogc:Literal" = list(street)
                  ),
                  escapeChar = "\\",
                  wildCard = "*",
                  singleChar = "?"
                ),
                "ogc:PropertyIsLike" = structure(
                  list(
                    "ogc:PropertyName" = list("haus"),
                    "ogc:Literal" = list(house_number)
                  ),
                  escapeChar = "\\",
                  wildCard = "*",
                  singleChar = "?"
                ),
                "ogc:PropertyIsLike" = structure(
                  list(
                    "ogc:PropertyName" = list("plz"),
                    "ogc:Literal" = list(zip_code)
                  ),
                  escapeChar = "\\",
                  wildCard = "*",
                  singleChar = "?"
                ),
                "ogc:PropertyIsLike" = structure(
                  list(
                    "ogc:PropertyName" = list("ort"),
                    "ogc:Literal" = list(place)
                  ),
                  escapeChar = "\\",
                  wildCard = "*",
                  singleChar = "?"
                )
              )
            )
          ),
          typeName = "gdz:Ortsangabe",
          srsName = paste0("EPSG:", epsg)
        )
      ),
      version = "1.1.0",
      service = "WFS",
      maxFeatures = as.character(max_features),
      "xmlns:wfs" = "http://www.opengis.net/wfs",
      "xmlns:ogc" = "http://www.opengis.net/ogc",
      "xmlns:gdz" = "http://www.geodatenzentrum.de/ortsangabe",
      "xmlns:gml" = "http://www.opengis.net/gml",
      "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance"
    )
  )

  post_req <- as.character(xml2::as_xml_document(req_list))

  # Read data ----
  res <- httr::POST(
    "https://sg.geodatenzentrum.de/wfs_geokodierung_bund?outputformat=json",
    body = post_req
  )
  
  res_json <- httr::content(res, type = "application/json")
  
  if (!is.null(res_json$exceptionCode)) {
    cli::cli_abort(c(
      "The WFS server returned an exception:",
      "x" = "{res_json$exceptionCode}: {res_json$exceptionText[[1]]}"
    ))
  }

  res_sf <- sf::read_sf(res)

  # Clean data ----
  res_sf <- dplyr::transmute(
    res_sf,
    street_input = street,
    house_number_input = house_number,
    zip_code_input = zip_code,
    place_input = place,
    street_output = if("strasse" %in% colnames(res_sf)) .data$strasse else NA,
    house_number_output = if("haus" %in% colnames(res_sf)) .data$haus else NA,
    zip_code_output = if("plz" %in% colnames(res_sf)) .data$plz else NA,
    place_output = if("ort" %in% colnames(res_sf)) .data$ort else NA,
    AGS = if("ags" %in% colnames(res_sf)) .data$ags else NA,
    bkg_score = .data$score,
    coordinate_type = .data$typ
  )

  res_sf
}
