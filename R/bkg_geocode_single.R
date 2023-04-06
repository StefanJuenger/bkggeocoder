#' (Reverse) geocoding for a single element
#' 
#' @description Low-level interface to the OSGTS and WFS geocoding webservices
#' of the BKG. Fires a single geocoding request at the BKG geocoding
#' services. Can handle both structured and unstructured geocoding requests.
#' \code{bkg_geocode_single} takes a query parameter (for unstructured
#' geocoding) or a set of address elements (for structured geocoding) that
#' are translated into a point geometry. \code{bkg_reverse_single} takes a
#' geometry that is translated into an address.
#'
#' @param query \code{[character]}
#' 
#' Query term that should be geocoded. Terms can be structured
#' using a specialized query language. See details. By providing this argument
#' the GeoSearch endpoint of the BKG geocoder is activated. Otherwise, the
#' Geocode endpoint will be used that only accepts structured queries using
#' the arguments below.
#' @param street,house_number,zip_code,place,district,street_house \code{[character]}
#' 
#' Address components that should be used for geocoding. At least one of these
#' arguments has to be provided. \code{street_house} can be provided as an
#' alternative to street and house_number if these information are tied
#' together in a single character string. Ignored if \code{query} is not
#' \code{NULL}
#' @param epsg \code{[character/numeric]}
#' 
#' EPSG code of the output coordinate reference system. Does not accept
#' PROJ4 or WKT strings.
#' @param properties \code{[character]}
#' 
#' List of property names to include in the output. \code{text}, \code{type},
#' \code{score}, and \code{bbox} are always included.
#' @param count \code{[numeric]} 
#' 
#' Maximum number of results to return. If \code{> 1}, returns matches in
#' descending order by BKG score.
#' @param filter \code{[character]}
#' 
#' Filter query to narrow the search down based on properties. See details.
#' @param bbox \code{[numeric]}
#' 
#' Boundary box to narrow down the search. If provided, only searches for
#' addresses within the boundary box. Requires the format
#' \code{c(xmin, ymin, xmax, ymax)}.
#' 
#' @param geometry \code{[sf/sfc]}
#' 
#' Geometry to narrow down the search. If provided, only searches for addresses
#' for which the binary predicate specified in \code{relation} is \code{TRUE}.
#' Ignored if \code{geometry} is \code{NULL}.
#' 
#' @param relation \code{[character]}
#' 
#' Name of a binary predicate function to test the spatial relationship between
#' a geocoded point and \code{geometry}. Can be one of \code{intersects},
#' \code{contains} and \code{disjoint}.
#' @param focus_point \code{[sf/sfc]}
#' 
#' Point geometry to narrow down the search. If provided, only searches within
#' a perimeter specified in \code{radius} around the focus point.
#' 
#' @param radius \code{[numeric]}
#' 
#' Distance (in meters) from the \code{focus_point}. Results outside of this
#' radius are not included in the output. Ignored if \code{focus_point} is
#' \code{NULL}.
#' @param minscore,maxscore \code{[numeric]}
#' 
#' Lower and upper threshold of scores to be included. Results with a score
#' below or above these thresholds are not included in the output.
#' @param allscores \code{[logical]}
#' 
#' Whether to return the lowest level of granularity for the geocoding scores.
#' If \code{TRUE}, returns scores for all address components. Otherwise, only
#' returns a single score for the whole result.
#' @param clean \code{[logical]}
#' 
#' Whether to clean up the output or provide it as it is returned by the
#' BKG service.
#' 
#' @param interface \code{[character]}
#' 
#' Which interface to use for geocoding. Must be one of \code{osgts} or
#' \code{wfs}. Each interface supports different features, arguments and
#' properties. Defaults to \code{osgts}
#' 
#' @returns A tibble with a maximum of \code{count} rows and containing the
#' address matches in descending order by their assigned quality score.
#' 
#' @details OSGTS requests support a specialized query language, which can be
#' used to structure term and filter queries. By default, entering a query term
#' to the \code{query} or \code{filter} arguments is understood as
#' \code{"text:{term}"}. To further control the output, the BKG geocoder
#' provides a range of operators to configure the query. For example, a query
#' could exclude particular places, restrict geocoding to certain states, or
#' be adjusted to be tolerant towards misspellings.
#' Query terms support the following operators:
#' 
#' \describe{
#'   \item{Double quote (\code{""})}{By default, the query language divides a string
#'   by operators and query terms. Wrapping a term in double quotes allows
#'   the entry of multiple words.}
#'   \item{Colon (\code{:})}{Operator used to seperate attribute and term.
#'   The colon is preceded by an attribute (such as \code{ort} or \code{strasse})
#'   and succeeded by a term. Defaults to the
#'   \code{text} attribute. Geocoding searches are restricted to the specified
#'   attribute-term combinations. For example, \code{"ort:Leipzig"} restricts
#'   geocoding to addresses within the city of Leipzig.}
#'   \item{Question mark (\code{?})}{Singular wildcard operator used to replace a
#'   variable symbol, e.g., \code{"Freib?rg"}.}
#'   \item{Asterisk (\code{*})}{Multiple wildcard operator used to replace an unknown
#'   number of variable symbols, e.g. \code{"Frank*"}.}
#'   \item{Tilde (\code{~})}{Operator to signify an error-tolerant search. Terms that
#'   precede a tilde are matched based on the Levenshtein edit distance to a
#'   potential match. The tilde can be preceded by a numeric which represents
#'   the maximum Levenshtein distance to a match, e.g., \code{"Liepzig~0.6"}.
#'   
#'   If a tilde is preceded by a term within double quotes, the numeric
#'   represents the number of additional words allowed between the phrased
#'   words, e.g., \code{"\"karl straße\"~1"} means that one additional word can
#'   be added between karl and straße.}
#'   \item{Exponent (\code{^})}{Weighting operator. Terms succeeded by an exponent
#'   operator are weighted based on a numeric behind the operator, e.g.,
#'   \code{"ort:Leipzig^5.3 typ:Strasse^0.5"} means that it is much more important
#'   to find an address within Leipzig than it is to find a street.}
#'   \item{Boolean operators}{Boolean operators include \code{AND}, \code{OR},
#'   \code{NOT}, \code{+} and \code{-}. They can be used to combine multiple
#'   query terms. For example, \code{"Leipzig NOT karl-rothe-str"} searches for
#'   all addresses in Leipzig except for the Karl-Rothe-Straße.}
#'   \item{Parantheses}{Parantheses are used to group query terms for the use
#'   with boolean operators. For example,
#'   \code{"(Leipzig OR Halle) AND typ:Ort"} evaluates Leipzig OR Halle before
#'   taking into account \code{typ:Ort}.}
#'   \item{Backslash (\code{\\})}{Can be used to escape operators.}
#' }
#'
#' @examples
#' \dontrun{
#' # Unstructured geocoding:
#' bkg_geocode_single("Unter Sachsen* 6-8 AND ort:Köln AND ortsteil:Altstadt")
#' 
#' # Structured geocoding:
#' bkg_geocode_single(
#'   street       = "Unter Sachsenhausen",
#'   house_number = "6-8",
#'   zip_code     = 50667,
#'   place        = "Köln",
#'   epsg         = 3035
#' )
#'
#' # Reverse geocoding:
#' rand_points <- list(
#'   c(9.162513, 51.02122),
#'   c(10.24401, 53.58412),
#'   c(10.56117, 50.50362),
#'   c(14.46131, 52.19429),
#'   c(10.62503, 48.31571)
#' )
#' rand_points <- sf::st_as_sf(do.call(sf::st_sfc, lapply(rand_points, sf::st_point)))
#' rand_poly <- aggregate(rand_points, list(rep(1, 5)), function(x){
#'   sf::st_cast(sf::st_combine(x), "POLYGON")
#' })
#' rand_poly <- sf::st_convex_hull(rand_poly)
#' 
#' # Reverse geocoding using a random polygon
#' bkg_reverse_single(poly = rand_poly, epsg = 3035)
#' 
#' # Reverse geocoding using a random point geometry
#' bkg_reverse_single(rand_points[1], epsg = 3035, count = 20)
#' }
#' 
#' @export
bkg_geocode_single <- function(
  query = NULL,
  street = NULL,
  house_number = NULL,
  zip_code = NULL,
  place = NULL,
  district = NULL,
  street_house = NULL,
  epsg = 3035,
  properties = NULL,
  count = 1L,
  filter = NULL,
  bbox = NULL,
  geometry = NULL,
  relation = "intersects",
  focus_point = NULL,
  radius = 1000L,
  minscore = NULL,
  maxscore = NULL,
  allscores = FALSE,
  clean = TRUE,
  interface = c("osgts", "wfs")
) {
  args <- as.list(environment())
  args$interface <- NULL
  args$clean <- NULL
  interface <- match.arg(interface)

  # Create POST body ----
  if (interface == "osgts") {
    req <- do.call(osgts_request, unname(args))
  } else {
    req <- do.call(wfs_request, unname(args))
  }

  # Perform request ----
  res <- httr2::req_perform(req)
  res_text <- httr2::resp_body_string(res)
  res_sf <- sf::read_sf(res_text)
  
  # Clean data ----
  if (clean) {
    res_sf <- clean_geocode(res_sf, query, street, house_number, zip_code, place, TRUE)
  }
  
  sf::st_as_sf(res_sf)
}


osgts_request <- function(
  query, strasse, haus, plz, ort, ortsteil, strasse_haus, srsName, propertyNames,
  count, filter, bbox, geometry, relation, focus_point, distance, minScore,
  maxScore, allScore, ...
) {
  args <- as.list(environment())

  if (elen <- ...length()) {
    cli::cli_warn("{elen} argument{?s} {?is/are} not supported by the OSGTS interface.")
  }
  
  if (!is.null(srsName)) {
    if (is.na(srsName)) {
      cli::cli_abort(paste(
        "CRS could not be detected from the input. Please pass it explicitly",
        "using the {.var epsg} argument."
      ))
    }
    epsg <- srsName
    args$srsName <- sprintf("EPSG:%s", srsName)
  }
  
  if (!is.null(geometry)) {
    geometry <- sf::st_transform(geometry, epsg)
    args$geometry <- sf::st_as_text(sf::st_geometry(geometry))
  }
  
  if (!is.null(focus_point)) {
    focus_point <- sf::st_transform(focus_point, epsg)
    focus_point <- sf::st_coordinates(focus_point)
    args$focus_point <- NULL
    args$lon <- focus_point[, 1]
    args$lat <- focus_point[, 2]
  }
  
  url <- paste0(
    "http://sg.geodatenzentrum.de/gdz_geokodierung/",
    ifelse(is.null(query), "geocode", "geosearch")
  )

  req <- httr2::request(url)
  req <- httr2::req_method(req, "POST")
  req <- httr2::req_error(req, is_error = function(res) {
    if (identical(res$headers[["Content-Type"]], "text/xml;charset=UTF-8")) {
      link <- cli::style_hyperlink(
        "gdz_geokodierung",
        "https://gdz.bkg.bund.de/index.php/default/geokodierungsdienst-opensearch-der-adv-fur-adressen-und-geonamen-gdz-geokodierung.html"
      )
      cli::cli_abort(c(
        "Cannot access OSGTS server.",
        "x" = "Do you have access to the {.url {link}} service?"
      ))
    }
    
    res <- httr2::resp_body_json(res)
    if (!is.null(res$exceptionCode)) {
      cli::cli_abort(c(
        "The OSGTS server returned an exception:",
        "x" = "{res$exceptionCode}: {res$exceptionText[[1]]}"
      ))
    } else FALSE
  })
  do.call(httr2::req_url_query, c(.req = list(req), args))
}


wfs_request <- function(street, house_number, zip_code, place, max_features, epsg) {
  req_list <- list(
    "wfs:GetFeature" = structure(
      list(
        "wfs:Query" = structure(
          list(
            "ogc:Filter" = list(
              "ogc:And" = list()
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
  
  cval <- list(street, house_number, zip_code, place)
  cnam <- c("strasse", "haus", "plz", "ort")

  for (i in seq_along(cnam)) {
    if (!is.null(cval[[i]])) {
      req_list[["wfs:GetFeature"]][["wfs:Query"]][["ogc:Filter"]][["ogc:And"]] <- append(
        req_list[["wfs:GetFeature"]][["wfs:Query"]][["ogc:Filter"]][["ogc:And"]],
        list(
          "ogc:PropertyIsLike" = structure(
            list(
              "ogc:PropertyName" = list(cnam[i]),
              "ogc:Literal" = list(cval[[i]])
            ),
            escapeChar = "\\",
            wildCard = "*",
            singleChar = "?"
          )
        )
      )
    }
  }

  as.character(xml2::as_xml_document(req_list))
}


clean_geocode <- function(.data, query, street, house_number, zip_code, place, identifiers) {
  alt_names <- list(
    score = "score", address_input = "address_input", 
    address_output = "address_output", id = "id", text = "text", typ = "type",
    kategorie = "category", name = "name", qkz = "quality_tag", treffer = "hit",
    qualitaet = "quality", score_strasse = "score_street",
    score_haus = "score_house_number", score_plz = "score_zip_code",
    score_ort = "score_place", score_ortsteil = "score_district",
    strasse = "street", haus = "house_number", plz = "zip_code", ort = "place",
    ortsteil = "district", gemeinde = "municipality", verwgem = "verwgem",
    kreis = "county", regbezirk = "gov_district", bundesland = "state",
    schluessel = "schluessel", rs = "RS", ags = "AGS", VWG = "VWG", KRS = "KRS",
    RBZ = "RBZ", STA = "STA", ags = "AGS", nuts1 = "nuts1", nuts2 = "nuts2",
    nuts3 = "nuts3", GITTER_ID_100m = "GITTER_ID_100m",
    GITTER_ID_1km = "GITTER_ID_1km", source = "source", bbox = "bbox",
    .iid = ".iid", geometry = "geometry"
  )
  
  if (isTRUE(identifiers)) {
    identifiers <- c("rs", "nuts", "inspire")
  }

  # Change colnames if necessary
  colnames(.data) <- vapply(colnames(.data), function(nam) alt_names[[nam]], character(1))
  .data_no_geom <- sf::st_drop_geometry(.data)
  
  has_attrib <- function(attrib, i = seq_len(nrow(.data))) {
    exists <- attrib %in% colnames(.data_no_geom)
    if (exists) !any(sapply(.data_no_geom[i, attrib], is.na)) else FALSE
  }

  # Construct input and output addresses
  addrin <- gsub("\\s+", " ", trimws(paste(
    street,
    house_number,
    zip_code,
    place)
  ))
  addrout <- vapply(seq_len(nrow(.data)), function(i) {
    trimws(gsub("\\s+", " ", paste(
      if (has_attrib("street", i)) .data$street[i] else "",
      if (has_attrib("house_number", i)) .data$house_number[i] else  "",
      if (has_attrib("zip_code", i)) .data$zip_code[i] else "",
      if (has_attrib("place", i)) .data$place[i] else ""
    )))
  }, character(1))
  
  VWG <- if (has_attrib("RS")) substr(.data$RS, 1, 9) else NA
  KRS <- if (has_attrib("RS")) substr(.data$RS, 1, 5) else NA
  RBZ <- if (has_attrib("RS")) substr(.data$RS, 1, 3) else NA
  STA <- if (has_attrib("RS")) substr(.data$RS, 1, 2) else NA
  
  nuts <- if (has_attrib("RS")) {
    merge(data.frame(KRS = KRS), nuts_ars, by = "KRS", all.x = TRUE)$nuts
  }

  # Add modified data
  .data <- tibble::add_column(
    .data,
    address_input = if (length(addrin)) {
      addrin
    } else if (length(!is.null(query))) {
      query
    } else NA,
    address_output = if (length(addrout)) addrout else NA,
    VWG = VWG, KRS = KRS, RBZ = RBZ, STA = STA,
    nuts3 = nuts, nuts2 = substr(nuts, 1, 4), nuts1 = substr(nuts, 1, 3),
    GITTER_ID_100m = if (inherits(.data, "sf")) {
      spt_create_inspire_ids(.data, type = "100m")
    } else NA,
    GITTER_ID_1km = if (inherits(.data, "sf")) {
      spt_create_inspire_ids(.data, type = "1km")
    } else NA,
    source = "\u00a9 GeoBasis-DE / BKG, Deutsche Post Direkt GmbH, Statistisches Bundesamt, Wiesbaden (2021)",
    .after = 0
  )

  if (has_attrib("hit")) {
    .data$hit <- if (has_attrib("hit")) .data$hit == "T" else NA
  }
  
  # bbox data is given as a raw geojson string which is not that pretty
  if (has_attrib("bbox")) {
    .data$bbox <- suppressWarnings(lapply(.data$bbox, function(bbox) {
      sf::st_geometry(sf::read_sf(bbox, crs = sf::st_crs(.data)))
    }))
    .data$bbox <- do.call(c, .data$bbox)
  }
  
  if (!"rs" %in% identifiers) {
    .data[c("RS", "AGS", "KRS", "RBZ", "VWG", "STA")] <- NULL
  }
  if (!"nuts" %in% identifiers) {
    .data[grepl("nuts", names(.data))] <- NULL
  }
  if (!"inspire" %in% identifiers) {
    .data[grepl("GITTER", names(.data))] <- NULL
  }

  # Order columns based on the alt_names list order
  name_order <- intersect(
    unlist(alt_names, use.names = FALSE),
    setdiff(colnames(.data), "geometry")
  )
  .data[name_order]
}
