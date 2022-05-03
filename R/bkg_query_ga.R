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
  
  c_count <- sum(places %in% gsub("_", "/", dir(cache_dir)))
  
  # Load address dataset for each unique place ----
  queried_ga <- lapply(places, function(place) {
    if (isTRUE(verbose)) {
      cli::cli_progress_update(.envir = parent.frame(2))
    }
  
    dataset_name <- gsub("/", "_", place)
    is_cached <- dataset_name %in% dir(cache_dir)
    cache_file <- file.path(cache_dir, dataset_name)
    if (is_cached && !isTRUE(force)) {
      return(readRDS(cache_file))
    }
    
    

    if(isTRUE(data_from_server)) {
      .crypt_url <- paste0(
        "http://10.6.13.132:8000/ga/",
        utils::URLencode(dataset_name),
        ".csv.encryptr.bin"
      )
      .crypt <- readRDS(url(.crypt_url))
    } else {
      .crypt_path <- paste0(data_path, "/ga/", dataset_name, ".csv.encryptr.bin")
      .crypt <- readRDS(.crypt_path)
    }

    tmp_out_file <- tempfile(pattern = "tmp_out_file", fileext = ".csv")

    .decrypt <- openssl::decrypt_envelope(
      .crypt$data, .crypt$iv,
      .crypt$session,
      key = paste0(credentials_path, "/id_rsa"),
      password = readLines(paste0(credentials_path, "/pwd"))
    )
    
    writeBin(.decrypt, con = tmp_out_file)

    i_data <- data.table::fread(
      tmp_out_file,
      colClasses = 'character',
      encoding = "UTF-8"
    )
    
    closeAllConnections()

    unlink(tmp_out_file)
    
    saveRDS(i_data, file = cache_file, compress = FALSE)
    
    i_data
  })
  
  # Clean data ----
  queried_ga <- data.table::rbindlist(queried_ga)

  place <- whole_address <- RS <- x <- y <- NULL
  
  queried_ga[, list(place, whole_address, RS, x, y)]
  
  queried_ga[, x := gsub(",", ".", x)]

  queried_ga[, y := gsub(",", ".", y)]
  
  if (isTRUE(verbose)) {
    cli::cli_alert_success(paste(
      "{.val {c_count}} places were recovered from the",
      "cache."
    ))
    cli::cli_alert_success(paste(
      "Read in {.val {nrow(queried_ga)}} address{?es} within",
      "{.val {length(unique(queried_ga$place))}} municipalit{?y/ies}."
    ))
  }

  queried_ga
}
