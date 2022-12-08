#' Geocoding of a multiple addresses (BKG offline version)
#'
#' @description Geocoding of a multiple addresses using record linkage and an
#' address/coordinate database (provided by the BKG)
#'
#' @param .data \code{[data.frame]}
#'
#' Dataframe containing address data. The dataframe must contain columns carrying
#' the street name, house number, zip code and municipality name. The
#' corresponding column names or indices can be specified using the \code{cols}
#' argument.
#' @param cols \code{[numeric/character]}
#'
#' Names or indices of the columns containing relevant geocoding information.
#' Must be of length 3 or 4. If a length-3 vector is passed, the first column is
#' interpreted as a single character string containing street and house number.
#' By default, interprets the first four columns as street, house number, zip
#' code and municipality (in this order).
#' @param data_from_server \code{[logical]}
#'
#' Whether the address data should be downloaded from GESIS internal server?
#' Requires access to the GESIS net. Defaults to \code{FALSE}.
#' @param data_path \code{[character]}
#'
#' Path to the address data provided by Stefan Jünger. Ignored
#' if \code{data_from_server = TRUE}.
#' @param credentials_path \code{[character]}
#'
#' Path to credentials package provided by Stefan Jünger.
#' @param join_with_original \code{[logical]}
#'
#' Whether the input data should be joined with the output data. If \code{FALSE},
#' input data is discarded. Defaults to \code{TRUE}.
#' @param crs \code{[various]}
#'
#' Any kind of object that can be parsed by \code{\link[sf]{st_crs}}
#' that the output data should be transformed to (e.g. EPSG code, WKT/PROJ4
#' character string or object of class \code{crs}). Defaults to EPSG:3035.
#' @param identifiers \code{[character/logical]}
#'
#' Territorial identifiers to be included in the output. Can be one or several
#' of \code{"rs"}, \code{"nuts"} and \code{"inspire"}. \code{"rs"}
#' is short for Regionalschlüssel and includes all variations of the
#' official municipality key of Germany. \code{"nuts"} includes all NUTS codes
#' from NUTS-1 to NUTS-3. \code{"inspire"} includes identifiers for the 100m
#' and 1km INSPIRE grids. If \code{TRUE}, includes all of the aforementioned
#' identifiers.
#' @param place_match_quality \code{[numeric]}
#'
#' Targeted quality of second record linkage round (see details). Corresponds to
#' the (standardized) string metric that can be specified using
#' \code{target_opts} AND the posterior m-probability that is used to determine
#' a match during record linkage. Values of 1 are interpreted as \code{1 - 5e-8}
#' because reclin2 does not support scores that equal 1.
#' @param place_match_opts \code{[list]}
#'
#' Named list that holds further parameters to customize the first
#' round of record linkage. All list elements are passed as arguments to
#' \code{\link[stringdist]{stringdist}}. Possible values are \code{method},
#' \code{weight}, \code{q}, \code{p}, and \code{bt}.
#' @param target_quality \code{[numeric]}
#'
#' Targeted quality of second record linkage round (see details). Corresponds to
#' the (standardized) string metric that can be specified using
#' \code{target_opts}.
#' @param target_opts \code{[list]}
#'
#' Named list that holds further parameters to customize the second
#' round of record linkage. All list elements are passed as arguments to
#' \code{\link[stringdist]{stringdist}}. Possible values are \code{method},
#' \code{weight}, \code{q}, \code{p}, and \code{bt}.
#' @param verbose \code{[logical]}
#'
#' Whether to print informative messages and progress bars during
#' the geocoding process.
#' @param force_decrypt \code{[logical]}
#'
#' Whether to force the function to read in the encrypted
#' files. This can be useful, if the BKG data should not be stored locally for
#' too long or if the cached data are corrupt or outdated.
#'
#' @returns Returns a nested list of class GeocodingResults containing an
#' \code{sf} dataframe of the geocoding results (\code{$geocoded}) as well
#' as a dataframe with addresses with non-matched places (\code{$not_geocoded}),
#' addresses that didn't pass the first round of record linkage
#' (\code{$not_place_matched}) and the names of non-matched places
#' (\code{$unmatched_places}). The object also includes a call object and
#' descriptive summary statistics. Please note that original columns
#' retrieve the suffix \code{"_input"}.
#'
#' @details The function first matches the zip code and place information from
#' the data against the official names in the address/geocoordinate database
#' (first round of record linkage). This is done to filter out address datasets
#' that are not needed and lower the data size. You can play with the quality by
#' adjusting the \code{place_match_quality} parameter. In a second step, the
#' input addresses together with the matched results are then again matched
#' against the addresses in the address/geocoordinate database (second round of
#' record linkage). Again, you can play with the quality by adjusting
#' the \code{target_quality} parameter.
#'
#' Record linkage is employed using string distance metrics from
#' \code{\link[stringdist]{stringdist}}. By default, scores for both place
#' matching and geocoding are calculated using the standard Jaro distance. This
#' metric can be adjusted by passing a list of options that is then passed on
#' to \code{\link[stringdist]{stringdist}}. This can include the method of
#' choice \code{method} as well as further method-specific parameters like
#' the size of the q-gram (\code{q}), the Jaro-Winkler prefix factor (\code{p}),
#' and the Winkler boost threshold (\code{bt}). To derive a distance index that
#' falls between 0 and 1, where 0 denotes a complete dissimilarity between both
#' address strings and 1 denotes a complete similarity, certain edit and q-gram
#' distance metrics are devided by their maximum possible value as explained in
#' van der Loo (2014). For more details on the method choice, refer to the
#' \code{\link[stringdist]{stringdist-metrics}} documentation from the
#' \code{stringdist} package. Generally and particularly for clean addresses,
#' heuristic distance measures (Jaro/Jaro-Winkler) seem to perform the best,
#' followed by edit distances (e.g. Levenshtein, LCS). Other distance measures
#' (e.g. soundex, cosine) might be useful for different use cases.
#'
#' The overall quality of the geocoding can be evaluated by looking at the
#' values of the column \code{score} (ranging from 0 to 1), which is based on
#' the second round of record linkage. In general, for both rounds of record
#' linkage, a score of above 0.9 can be considered a good match. If the score
#' falls below 0.8, the result might be questionable.
#'
#' Address data loading works using a temporary cache. Before trying to request
#' and decrypt encrypted data chunks, the function will look for each place
#' dataset in a directory named \code{bkg_data_cache} in \code{tempdir()}. If
#' found, data decrypting will be skipped for the respective place which can
#' shorten the processing time significantly for large input datasets with a
#' high number of different places. If needed, this behavior can be suppressed
#' by setting \code{force_decrypt = TRUE}.
#'
#' @references van der Loo, M. P. J. (2014). The stringdist Package for
#' Approximate String Matching. The R Journal, 6(1), 111–122. https://doi.org/10.32614/RJ-2014-011
#'
#' @examples
#' data(commaddr, package = "bkggeocoder")
#'
#' # Basic call with lower quality thresholds
#' gc <- bkg_geocode_offline(commaddr, cols = 2:5, place_match_quality = 0.7, target_quality = 0.7)
#'
#' # Geocoding with different string metrics for address matching
#' gc <- bkg_geocode_offline(commaddr, cols = 2:5, target_opts = list(method = "lv", weight = c(2, 2, 4)))
#'
#' # Geocoding results are transformed to geographic coordinates
#' gc <- bkg_geocode_offline(commaddr, cols = 2:5, crs = 4314)
#'
#' # `cols` argument depends on where the relevant address information is stored
#' commaddr <- commaddr[, 2:5]
#' gc <- bkg_geocode_offline(commaddr, cols = 1:4)
#'
#' # If `cols` is of length 3, street and house_number are assumed to be in a
#' # single string
#' commaddr[, "addr.street"] <- paste(commaddr[["addr.street"]], commaddr[["addr.housenumber"]])
#' gc <- bkg_geocode_offline(commaddr, cols = c("addr.street", "addr.postcode", "addr.city"))
#'
#' @encoding UTF-8
#' @md
#'
#' @export
bkg_geocode_offline <- function(
  .data,
  cols = 1L:4L,
  data_from_server = FALSE,
  data_path = "../bkgdata",
  credentials_path = "../bkgcredentials",
  join_with_original = TRUE,
  crs = 3035L,
  identifiers = "rs",
  place_match_quality = 0.8,
  place_match_opts = list(),
  target_quality = 0.8,
  target_opts = list(),
  verbose = TRUE,
  force_decrypt = FALSE
) {
  
  if (!is.data.frame(.data)) {
    cli::cli_abort("{.var data} must be a dataframe.")
  }
  
  if (length(cols) < 3 || length(cols) > 4) {
    cli::cli_abort("{.var cols} must be of length 3 or 4.")
  }
  
  if (isFALSE(data_from_server) && !dir.exists(data_path)) {
    cli::cli_abort(c(
      "{.path {data_path}} does not exist.",
      "i" = "If you are inside the GESIS intranet, consider setting {.code data_from_server = TRUE}."
    ))
  }
  
  if (!dir.exists(credentials_path)) {
    cli::cli_abort(paste(
      "{.var {credentials_path}} does not exist.",
      "It is needed to decrypt the BKG data."
    ))
  }
  
  if (place_match_quality > 1 || place_match_quality < 0) {
    cli::cli_abort("{.var place_match_quality} needs to be a value between 0 and 1.")
  }
  
  if (target_quality > 1 || target_quality < 0) {
    cli::cli_abort("{.var target_quality} needs to be a value between 0 and 1.")
  }

  if (isTRUE(verbose)) {
    cli::cli_h1("Starting offline geocoding")
    cli::cat_line()
    cli::cli_inform(c(
      "i" = "Number of distinct addresses: {.val {nrow(.data)}}",
      "i" = "Targeted quality of place-matching: {.val {place_match_quality}}",
      "i" = "Targeted quality of geocoding: {.val {target_quality}}")
    )

    cli::cli_h2("Subsetting data")
  }

  cols <- names(.data[cols])
  
  args <- as.list(environment())
  args$.data <- NULL

  .data <- cbind(data.frame(.iid = as.numeric(row.names(.data))), .data)

  # Place Matching ----
  data_edited <- bkg_match_places(
    .data[c(".iid", cols)],
    cols,
    data_from_server,
    data_path,
    credentials_path,
    place_match_quality,
    place_match_opts,
    verbose
  )

  # Querying Database ----
  house_coordinates <- bkg_query_ga(
    unique(data_edited$matched$place_matched),
    data_from_server = data_from_server,
    data_path = data_path,
    credentials_path = credentials_path,
    verbose = verbose,
    force = force_decrypt
  )

  data.table::setkeyv(house_coordinates, c("zip_code", "place"))

  # Retrieving Geocoordinates ----
  if (isTRUE(verbose)) {
    cli::cli_h2("Geocoding input data")
  }

  messy_geocoded_data <- bkg_match_addresses(
    data_edited,
    cols = cols,
    house_coordinates = house_coordinates,
    opts = target_opts,
    verbose = verbose
  )

  # Data Cleaning ----
  cleaned_data <- bkg_clean_matched_addresses(
    messy_geocoded_data,
    cols = cols,
    identifiers = identifiers,
    verbose = verbose
  )

  if (isTRUE(join_with_original)) {
    cleaned_data <- merge(
      cleaned_data,
      .data,
      by = ".iid",
      all.x = TRUE,
      sort = TRUE,
      suffixes = c("", "_input")
    )

    # Remove all '_input' variables since they are already in the original to be
    # merged with
    cleaned_data <- sf::st_as_sf(tibble::as_tibble(cleaned_data))
    cleaned_data <- cleaned_data[!names(cleaned_data) %in% paste0(cols, "_input")]
    
    # Do the merging also for not-place-matched addresses
    data_edited$unmatched <- merge(
      data_edited$unmatched,
      .data,
      by = ".iid",
      all.x = TRUE,
      sort = TRUE,
      suffixes = c("", "_input")
    )
    
    data_edited$unmatched <- data_edited$unmatched[
      !names(data_edited$unmatched) %in% paste0(cols, "_input")
    ]
  }

  # Remove internal id
  cleaned_data$.iid <- NULL
  data_edited$unmatched$.iid <- NULL

  cleaned_data <- sf::st_transform(cleaned_data, crs = crs)

  # Create Output ----
  geocoded_data    <- cleaned_data[which(cleaned_data$score >= target_quality), ]
  geocoded_data_na <- cleaned_data[which(cleaned_data$score <  target_quality), ]

  output_list <- structure(
    list(
      geocoded = geocoded_data,
      not_geocoded = geocoded_data_na,
      not_place_matched = data_edited$unmatched,
      unmatched_places = data_edited$unmatched_places,
      call = match.call()
    ),
    type = "offline",
    args = args,
    class = c("GeocodingResults")
  )

  output_list
}

