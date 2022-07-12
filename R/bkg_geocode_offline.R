#' Geocoding of a multiple addresses (BKG offline version)
#'
#' @description Geocoding of a multiple addresses using record linkage and an
#' address/coordinate database (provided by the BKG)
#'
#' @param data Dataframe containing address data. The dataframe must contain
#' at least four columns carrying the street name, house number, zip code and
#' municipality name. The corresponding column names or indices can be specified
#' using the \code{cols} argument.
#' @param cols Numeric or character of length 4; names or indices of the columns
#' containing relevant geocoding information. By default, interprets the
#' first four columns as street, house number, zip code and municipality (in
#' this order).
#' @param data_from_server Logical; shall the address data be downloaded from
#' GESIS internal server? Requires access to the GESIS net (default is
#' \code{FALSE})
#' @param data_path Path to the address data provided by Stefan Jünger. Ignored
#' if \code{data_from_server = TRUE}.
#' @param credentials_path Path to credentials package provided by Stefan Jünger
#' @param join_with_original Logical; should the output be joined with the
#' input data?
#' @param crs Any kind of object that can be parsed by \code{\link[sf]{st_crs}}
#' that the output data should be transformed to (e.g. EPSG code, WKT/PROJ4
#' character string or object of class \code{crs}). Defaults to EPSG:3035.
#' @param place_match_quality Numeric; targeted quality of first record linkage
#' round (see details). Corresponds to the threshold value of
#' \code{\link[reclin2:select_n_to_m]{reclin::select_greedy()}}.
#' @param place_match_opts Named list; further parameters to customize the first
#' round of record linkage. All list elements are passed as arguments to
#' \code{\link[stringdist]{stringdist}}. Possible values are \code{method},
#' \code{weight}, \code{q}, \code{p}, and \code{bt}.
#' @param target_quality Numeric; targeted quality of second record linkage
#' round (see details). Corresponds to the threshold value of
#' \code{\link[reclin2:select_n_to_m]{reclin::select_greedy()}}.
#' @param target_opts Named list; further parameters to customize the second
#' round of record linkage. All list elements are passed as arguments to
#' \code{\link[stringdist]{stringdist}}. Possible values are \code{method},
#' \code{weight}, \code{q}, \code{p}, and \code{bt}.
#' @param verbose Whether to print informative messages and progress bars during
#' the geocoding process.
#' @param force_decrypt Whether to force the function to read in the encrypted
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
#' van der Loo (2014).
#' For more details on the method
#' choice, refer to the \code{\link[stringdist]{stringdist-metrics}}
#' documentation from the \code{stringdist} package.
#'
#' For geocoding \code{\link[bkggeocoder]{commaddr}} with a target quality of
#' 0.9 and a Jaro-based place matching with a threshold of 0.9, the following
#' metrics apply for different geocoding string distance methods, sorted by
#' their percentage of successfully geocoded addresses:
#' \tabular{lrrrr}{
#'   \strong{method} \tab \strong{geocoded} \tab \strong{spearman} \tab \strong{mean_score} \tab \strong{sd_score} \cr
#'   BKG                     \tab 0.9972 \tab 1.0000 \tab 0.9906 \tab 0.0153\cr
#'   Cosine                  \tab 0.9251 \tab 0.3757 \tab 0.9955 \tab 0.0174\cr
#'   Jaro                    \tab 0.9232 \tab 0.3833 \tab 0.9956 \tab 0.0166\cr
#'   Jaro-Winkler            \tab 0.9232 \tab 0.3833 \tab 0.9956 \tab 0.0166\cr
#'   Optimal string aligment \tab 0.9211 \tab 0.3659 \tab 0.9948 \tab 0.0202\cr
#'   Longest Common String   \tab 0.9182 \tab 0.3668 \tab 0.9948 \tab 0.0198\cr
#'   Levenshtein             \tab 0.9145 \tab 0.3408 \tab 0.9952 \tab 0.0191\cr
#'   Damerau-Levenshtein     \tab 0.9145 \tab 0.3408 \tab 0.9952 \tab 0.0192\cr
#'   Q-gram                  \tab 0.9129 \tab 0.3339 \tab 0.9951 \tab 0.0195\cr
#'   Jaccard                 \tab 0.8976 \tab 0.1779 \tab 0.9931 \tab 0.0226\cr
#'   Hamming                 \tab 0.8866 \tab 0.2711 \tab 0.9969 \tab 0.0157
#' }
#' However, keep in mind that this was tested on a clean dataset without many
#' spelling errors and without setting weights or penalties. Different metrics
#' might be reasonable choices for different use cases.
#'
#' The overall quality of the geocoding can be evaluated by looking at the
#' values of the column \code{score} (ranging from 0 to 1), which is based on
#' the second round of record linkage. In general, for both rounds of record
#' linkage, a score of above 0.9 can be considered a good match. If the score
#' falls below 0.8, the result should be thoroughly scrutinized.
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
  place_match_quality = 0.8,
  place_match_opts = list(),
  target_quality = 0.9,
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
      "i" = "Number of distinct addresses: {.val {nrow(data)}}",
      "i" = "Targeted quality of place-matching: {.val {place_match_quality}}",
      "i" = "Targeted quality of geocoding: {.val {target_quality}}")
    )

    cli::cli_h2("Subsetting data")
  }

  cols <- names(.data[cols])

  .data <- cbind(data.frame(.iid = row.names(.data)), .data)

  # Place Matching ----
  data_edited <- bkg_match_places(
    .data,
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
    data_from_server,
    data_path,
    credentials_path,
    verbose,
    force_decrypt
  )

  data.table::setkeyv(house_coordinates, c("zip_code", "place"))

  # Retrieving Geocoordinates ----
  if (isTRUE(verbose)) {
    cli::cli_h2("Geocoding input data")
  }

  messy_geocoded_data <- bkg_match_addresses(
    data_edited,
    cols,
    house_coordinates,
    target_quality,
    target_opts,
    verbose
  )

  # Data Cleaning ----
  cleaned_data <- bkg_clean_matched_addresses(
    messy_geocoded_data,
    cols,
    verbose
  )

  if (isTRUE(join_with_original)) {
    cleaned_data <- merge(
      .data,
      cleaned_data,
      by = ".iid",
      all.x = TRUE,
      sort = TRUE
    )

    # Remove all '_input' variables since they are already in the original to be
    # merged with
    cleaned_data <- sf::st_as_sf(tibble::as_tibble(cleaned_data))
    cleaned_data <- cleaned_data[!names(cleaned_data) %in% c(
      "street_input",
      "house_number_input",
      "zip_code_input",
      "place_input"
    )]
  }

  # Remove internal id
  cleaned_data$.iid <- NULL

  if (!missing(crs)) {
    cleaned_data <- sf::st_transform(cleaned_data, crs = crs)
  }

  # Create Output ----
  geocoded_data    <- cleaned_data[!is.na(cleaned_data$RS), ]
  geocoded_data_na <- cleaned_data[is.na(cleaned_data$RS), ]

  output_list <- structure(
    list(
      geocoded = geocoded_data,
      not_geocoded = geocoded_data_na,
      not_place_matched = data_edited$unmatched,
      unmatched_places = data_edited$unmatched_places,
      call = match.call()
    ),
    type = "offline",
    class = c("GeocodingResults", "list")
  )

  output_list
}

