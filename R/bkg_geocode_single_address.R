#' Geocoding of a single address to a geo-coordinate
#'
#' Geocoding of a single address to a geo-coordinate through the BKG Geocoder
#'
#' @param street Character string for the street name
#' @param house_number Character string for the house number
#' @param zip_code Character string for the zip code
#' @param place Character string for the place (i.e., municipality/city)
#' @param epsg Character string for the requested CSR
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
#'
#' @importFrom magrittr %>%

bkg_geocode_single_address <- function (
  street,
  house_number,
  zip_code,
  place,
  epsg = "3035"
) {

  # create POST string for WFS service
  POST_request_string <- paste0(
    '<?xml version="1.0" encoding="UTF-8" ?>
<wfs:GetFeature version="1.1.0" service="WFS" maxFeatures="1"
        xmlns:wfs="http://www.opengis.net/wfs"
        xmlns:ogc="http://www.opengis.net/ogc"
        xmlns:gdz="http://www.geodatenzentrum.de/ortsangabe"
        xmlns:gml="http://www.opengis.net/gml"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <wfs:Query typeName="gdz:Ortsangabe" srsName="EPSG:', epsg, '">
    <ogc:Filter>
      <ogc:And>
        <ogc:PropertyIsLike escapeChar="\" wildCard="*" singleChar="?">
          <ogc:PropertyName>strasse</ogc:PropertyName>
          <ogc:Literal>', street, '</ogc:Literal>
        </ogc:PropertyIsLike>
        <ogc:PropertyIsLike escapeChar="\" wildCard="*" singleChar="?">
          <ogc:PropertyName>haus</ogc:PropertyName>
          <ogc:Literal>', house_number, '</ogc:Literal>
        </ogc:PropertyIsLike>
        <ogc:PropertyIsLike escapeChar="\" wildCard="*" singleChar="?">
          <ogc:PropertyName>plz</ogc:PropertyName>
          <ogc:Literal>', zip_code, '</ogc:Literal>
        </ogc:PropertyIsLike>
        <ogc:PropertyIsLike escapeChar="\" wildCard="*" singleChar="?">
          <ogc:PropertyName>ort</ogc:PropertyName>
          <ogc:Literal>', place, '</ogc:Literal>
        </ogc:PropertyIsLike>
      </ogc:And>
    </ogc:Filter>
  </wfs:Query>
</wfs:GetFeature>'
  )

  # conduct actual request
  POST_request <- httr::POST(
    'https://sg.geodatenzentrum.de/wfs_geokodierung_bund?outputformat=json',
    body = POST_request_string
  )

  # convert to simple features data frame
  POST_sf <-
    sf::read_sf(POST_request)

  # clean the data
  POST_sf <-
    POST_sf %>%
    dplyr::transmute(
      street_input = street,
      house_number_input = house_number,
      zip_code_input = zip_code,
      place_input = place,
      street_output = if("strasse" %in% colnames(.)) strasse else NA,
      house_number_output = if("haus" %in% colnames(.)) haus else NA,
      zip_code_output = if("plz" %in% colnames(.)) plz else NA,
      place_output = if("ort" %in% colnames(.)) ort else NA,
      AGS = if("ags" %in% colnames(.)) ags else NA,
      bkg_score = score,
      coordinate_type = typ
    )

  # return object
  POST_sf
}
