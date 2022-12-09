#' Parse German addresses
#' 
#' @description Attempts to parse German address data as a \code{tibble}
#' consisting of street, house number, zip code and place information.
#' 
#' @param data A \code{data.frame} containing unformatted address strings
#' to be parsed.
#' @param col Column index or column name that contains address strings.
#' @param join Logical; whether to combine output with \code{data}.
#' @param split_place Logical; if \code{FALSE}, returns the entire place
#' description (e.g. Munich, Bavaria, Germany). If \code{TRUE}, attempts to
#' further divide the place description into place, state, country.
#' @param includes_districts Logical; do the addresses in \code{data} generally
#' include districts (e.g. Köln-Deutz)? If \code{TRUE}, attempts to split
#' place and district from each other.
#' 
#' @returns A \code{tibble} with the same number of rows as the input dataset
#' that contains parsed address data. The returned object is in a suitable
#' format to be used for the \code{bkg_geocode} family.
#' 
#' @section How good is it?:
#' Pretty okay for what it's worth. Keep in mind that this function is based on
#' simple regular expressions and addresses (especially German addresses) are
#' not always written in regular language. The input addresses should already be
#' in a somewhat tidy format before entering this function. For 
#' \code{\link[bkggeocoder]{commaddr}} (which is very tidy), around 99.98\% of
#' addresses can be successfully parsed. For untidy data, consider using more
#' advanced software like 
#' \href{https://github.com/openvenues/libpostal}{libpostal} or
#' \href{https://github.com/pelias/parser}{Pelias}.
#' 
#' @examples 
#' data(commaddr, package = "bkggeocoder")
#' 
#' parsed <- parse_addresses(commaddr[1:20, ], col = 5)
#' parsed
#' 
#' @encoding UTF-8
#' 
#' @noRd

parse_addresses <- function(
    data,
    col = 1L,
    join = TRUE,
    split_place = FALSE,
    includes_districts = FALSE
  ) {
  addresses <- data[[col]]

  street <- match_element(addresses, "street", split_place)
  house <- match_element(addresses, "house", split_place)
  zip <- match_element(addresses, "zip", split_place)
  desc <- match_element(addresses, "place", split_place)
  
  if (split_place) {
    desc <- do.call(rbind.data.frame, lapply(desc, structure_place))
  } else desc <- as.data.frame(desc)
  
  parsed <- tibble::tibble(
    street = street,
    house_number = house,
    zip_code = zip,
    place = desc[, 1],
    state = if (split_place) desc[, 2] else NULL,
    country = if (split_place) desc[, 3] else NULL
  )
  
  if (join) {
    tibble::as_tibble(cbind(data, parsed))
  } else parsed
}


match_element <- function(addresses, element, split_place) {
  regex_chr <- sprintf(
    paste0(
      street = "^(%s[[:alnum:]äöüß\\(\\)\\s',.-]+?)\\s*",
      house  = "(%s[[:digit:]]+(?:[[:blank:]]?[;-–|\\+\\/][[:blank:]]?[[:digit:]]+)?\\s*[[:alpha:][:digit:]\\+\\/]*)?",
      zip    = "[,\\s]*(%s\\d{4,5})",
      place  = if (split_place) "\\s*(.+)$" else "\\s*(%s[[:alpha:]ÄÖÜäöüß\\s/-]+)"
    ),
    ifelse(element == "street", "", "?:"),
    ifelse(element == "house", "", "?:"),
    ifelse(element == "zip", "", "?:"),
    ifelse(element == "place", "", "?:")
  )

  matches <- match_regex(addresses, regex_chr, perl = TRUE)
  vapply(matches, function(x) trimws(x[2]), FUN.VALUE = character(1), USE.NAMES = FALSE)
}


structure_place <- function(place_chr, split_place, includes_district) {
  
  country_pattern <- paste(germany_names, collapse = "|")
  country <- unlist(match_regex(place_chr, country_pattern))
  
  land_pattern <- paste(laender, collapse = "|")
  land <- unlist(match_regex(place_chr, land_pattern))
  
  place_pattern <- paste0(paste(country, land, ",", sep = "|"))
  place <- gsub(place_pattern, "\\1", place_chr)

  list(
    place = trimws(place),
    land = if (length(land)) land else NA,
    country = if (length(country)) country else NA
  )
}


laender <- c(
  c("Nordrhein-Westfalen", "Nordrhein Westfalen", "Northrhine Westfalia",
    "Northrhine-Westfalia", "North Rhine Westfalia", "North-Rhine Westfalia",
    "North Rhine Westphalia", "Nordrhein Westphalen", "Nordrhein-Westphalen",
    "NRW"),
  c("Baden Württemberg", "Baden-Württemberg", "Baden Würtemberg",
    "Baden Würtenberg", "BW", "BaWü", "Baden Wurttemberg"),
  c("Schleswig Holstein", "Schleswig-Holstein", "Schleswig Hollstein",
    "Sleswig-Holsteen", "Sleswig-Holsten", "Slaswik-Holstiinij", "SH"),
  c("Rheinland Pfalz", "Rheinland-Pfalz", "Rhineland Palatinate", 
    "Rhineland-Palatinate", "Rhoilond-Palz", "Rhoilond Palz", "RP"),
  c("Mecklenburg Vorpommern", "Mecklenburg-Vorpommern", "MV"),
  c("Sachsen-Anhalt", "Sachsen Anhalt"),
  c("Niedersachsen"),
  c("Thüringen"),
  c("Sachsen"),
  c("Saarland"),
  c("Berlin"),
  c("Hamburg"),
  c("Bremen"),
  c("Bayern", "Bavaria"),
  c("Hessen", "Hesse")
)


germany_names <- c(
  "Deutschland", "Germany", "DE", "GER", "DEU", "BRD", "DDR",
  "germany", "deutschland", "Bundesrepublik Deutschland",
  "Deutsche Demokratische Republik", "bundesrepublik deutschland",
  "deutsche demokratische republik"
)



