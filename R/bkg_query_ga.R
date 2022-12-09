#' Query the address database
#'
#' @param places character vector comprising place names
#'
#' @noRd

bkg_query_ga <- function(
  places,
  data_from_server,
  data_path,
  credentials_path,
  verbose,
  force
) {
  cache_dir <- file.path(tempdir(), "bkg_data_cache")
  
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir)
  }
  
  if (isTRUE(verbose)) {
    cli::cli_h2("Preparing database")
    cli::cli_progress_bar(
      name = "Reading address data",
      total = length(places),
      format = paste(
        "{cli::pb_name} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} places |",
        "ETA {cli::pb_eta}"
      ),
      format_failed = "Failed at address {cli::pb_current}/{cli::pb_total}."
    )
  }
  
  c_count <- 0
  
  # Load address dataset for each unique place ----
  queried_ga <- lapply(places, function(place) {
    if (isTRUE(verbose)) {
      cli::cli_progress_update(.envir = parent.frame(2))
    }
  
    dataset_name <- gsub("/", "_", place)
    is_cached <- dataset_name %in% dir(cache_dir)
    cache_file <- file.path(cache_dir, dataset_name)
    if (is_cached && !isTRUE(force)) {
      c_count <<- c_count + 1
      return(readRDS(cache_file))
    }
    
    place_data <- bkg_read(
      dataset_name,
      what = "addresses",
      data_from_server = data_from_server,
      data_path = data_path,
      credentials_path = credentials_path,
      drop = "place_add"
    )
    
    saveRDS(place_data, file = cache_file, compress = FALSE)
    
    place_data
  })
  
  # Clean data ----
  if (verbose) {
    cli::cli_progress_step(
      msg = "Cleaning BKG data...",
      msg_done = "Cleaned BKG data.",
      msg_failed = "Could not clean BKG data."
    )
  }
  queried_ga <- data.table::rbindlist(queried_ga)

  place <- whole_address <- RS <- x <- y <- NULL
  
  queried_ga[, list(place, whole_address, RS, x, y)]
  
  queried_ga[, x := gsub(",", ".", x)]

  queried_ga[, y := gsub(",", ".", y)]
  
  if (isTRUE(verbose)) {
    cli::cli_alert_success(paste(
      "{.val {c_count}} place{?s} were recovered from the cache."
    ))
    cli::cli_alert_success(paste(
      "Read in {.val {nrow(queried_ga)}} address{?es} within",
      "{.val {length(unique(queried_ga$place))}} municipalit{?y/ies}."
    ))
  }

  queried_ga
}
