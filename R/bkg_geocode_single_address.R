#' Geocoding of a single address to a geo-coordinate
#'
#' @param street Character string for the street name
#' @param house_number Character string for the house number
#' @param zip_code Character string for the zip code
#' @param place Character string for the place (i.e., municipality/city)
#' @param max_features Numeric or character specifying the maximum amount of
#' results to return. If \code{> 1}, returns matches in descending order by
#' BKG score.
#'
#' @examples
#' \dontrun{
#' # single address
#' bkg_geocode_single_address(
#'   street       = "Unter Sachsenhausen",
#'   house_number = "6-8",
#'   zip_code     = 50667,
#'   place        = "Köln",
#'   epsg         = 3035
#' )
#' }
#' 
#' @rdname bkg_geocode
#' 
#' @export

bkg_geocode_single_address <- function (
  street,
  house_number,
  zip_code,
  place,
  epsg = 3035,
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
  res_sf <- tibble::tibble(
    score = res_sf$score,
    quality = if ("qualitaet" %in% res_sf) res_sf$qualitaet else NA,
    quality_class = if ("qkz" %in% res_sf) res_sf$qkz else NA,
    address_input = trimws(paste(street, house_number, zip_code, place)),
    address_output = trimws(paste(
      if("strasse" %in% colnames(res_sf)) res_sf$strasse else NULL,
      if("haus" %in% colnames(res_sf)) res_sf$haus else NULL,
      if("plz" %in% colnames(res_sf)) res_sf$plz else NULL,
      if("ort" %in% colnames(res_sf)) res_sf$ort else NULL
    )),
    RS  = if("rs" %in% colnames(res_sf)) res_sf$rs else NA,
    GEM = if("rs" %in% colnames(res_sf)) substr(res_sf$rs, 1, 9) else NA,
    KRS = if("rs" %in% colnames(res_sf)) substr(res_sf$rs, 1, 5) else NA,
    RBZ = if("rs" %in% colnames(res_sf)) substr(res_sf$rs, 1, 3) else NA,
    STA = if("rs" %in% colnames(res_sf)) substr(res_sf$rs, 1, 2) else NA,
    AGS = if("ags" %in% colnames(res_sf)) res_sf$ags else NA,
    street_input = street,
    house_number_input = house_number,
    zip_code_input = zip_code,
    place_input = place,
    street_output = if("strasse" %in% colnames(res_sf)) res_sf$strasse else NA,
    house_number_output = if("haus" %in% colnames(res_sf)) res_sf$haus else NA,
    zip_code_output = if("plz" %in% colnames(res_sf)) res_sf$plz else NA,
    place_output = if("ort" %in% colnames(res_sf)) res_sf$ort else NA,
    municipality = if ("gemeinde" %in% colnames(res_sf)) res_sf$gemeinde else NA,
    verwaltungsgemeinschaft = if ("verwgem" %in% colnames(res_sf)) res_sf$verwgem else NA,
    district = if ("kreis" %in% colnames(res_sf)) res_sf$kreis else NA,
    governmental_district = if ("regbezirk" %in% colnames(res_sf)) res_sf$regbezirk else NA,
    state = if ("bundesland" %in% colnames(res_sf)) res_sf$bundesland else NA,
    coordinate_type = res_sf$typ,
    GITTER_ID_100m = spt_create_inspire_ids(res_sf, type = "100m"),
    GITTER_ID_1km = spt_create_inspire_ids(res_sf, type = "1km"),
    source = "\u00a9 GeoBasis-DE / BKG, Deutsche Post Direkt GmbH, Statistisches Bundesamt, Wiesbaden (2021)",
    geometry = res_sf$geometry
  )

  res_sf <- sf::st_as_sf(res_sf)
}
