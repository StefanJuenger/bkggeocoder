#' Address data of community center addresses
#' 
#' @description A dataset containing the addresses of community centers in
#' Germany. The dataset was extracted from OpenStreetMap on 2022-05-12 19:25.
#' 
#' @format A tibble with 7976 rows and 6 columns:
#' \describe{
#'   \item{osm_id}{ID of the OSM object linked to the address}
#'   \item{addr.street}{Street name of the address}
#'   \item{addr.housenumber}{House number of the address}
#'   \item{addr.postcode}{Zip code of the address (PLZ)}
#'   \item{addr.city}{Place name of the address (Gemeinde)}
#'   \item{address}{Address string glued together from the other columns}
#' }
#' 
#' @details The following OverpassQL query was used to extract the address data:
#' 
#' \preformatted{
#' [out:csv(::id, "addr:street","addr:housenumber","addr:postcode","addr:city")]
#' [timeout:3000];
#' 
#' area(3600051477)->.searchArea;
#' (
#'   node
#'   ["addr:street"]
#'   ["addr:housenumber"]
#'   ["addr:postcode"]
#'   ["addr:city"]
#'   ["amenity"="community_centre"]
#'   (area.searchArea);
#'   
#'   way
#'   ["addr:street"]
#'   ["addr:housenumber"]
#'   ["addr:postcode"]
#'   ["addr:city"]
#'   ["amenity"="community_centre"]
#'   (area.searchArea);
#'   
#'   relation
#'   ["addr:street"]
#'   ["addr:housenumber"]
#'   ["addr:postcode"]
#'   ["addr:city"]
#'   ["amenity"="community_centre"]
#'   (area.searchArea);
#' );
#' 
#' out tags;
#' }
#' 
#' @source https://www.openstreetmap.org/, https://overpass-turbo.eu/
"commaddr"
