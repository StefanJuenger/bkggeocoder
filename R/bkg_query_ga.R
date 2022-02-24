#' Query the address database
#'
#' @param places character vector comprising place names
#'
#' @noRd

bkg_query_ga <-
  function(places, data_from_server, data_path, credentials_path, verbose) {
  if (isTRUE(verbose)) {
    cli::cli_h2("Preparing database")
    cli::cli_progress_bar(
      name = "Reading address data",
      total = length(places)
    )
  }
  
  # Load address dataset for each unique place ----
  queried_ga <- lapply(places, function(place) {
      if (isTRUE(verbose)) {
        cli::cli_progress_update(.envir = parent.frame(2))
      }
      
      dataset_name <- gsub("/", "_", place)

      if(isTRUE(data_from_server)) {
        .crypt <- paste0(
          "http://10.6.13.132:8000/ga/",
          dataset_name,
          ".csv.encryptr.bin"
        ) %>%
          utils::URLencode() %>%
          url() %>%
          readRDS()
      } else {
        .crypt <- paste0(
          data_path,
          "/ga/",
          dataset_name,
          ".csv.encryptr.bin"
        ) %>%
          readRDS()
      }

      tmp_out_file <- tempfile(pattern = "tmp_out_file", fileext = ".csv")

      openssl::decrypt_envelope(
        .crypt$data, .crypt$iv,
        .crypt$session,
        key = paste0(credentials_path, "/id_rsa"),
        password = readLines(paste0(credentials_path, "/pwd"))
      ) %>%
        writeBin(tmp_out_file)

      i_data <- data.table::fread(
        tmp_out_file,
        colClasses = 'character',
        encoding = "UTF-8"
      )

      unlink(tmp_out_file)

      i_data
    })

  # Clean data ----
  queried_ga <- data.table::rbindlist(queried_ga)

  place <- whole_address <- RS <- x <- y <- NULL
  
  queried_ga[, list(place, whole_address, RS, x, y)]
  
  queried_ga[, x := gsub(",", ".", x)]

  queried_ga[, y := gsub(",", ".", y)]
  
  if (isTRUE(verbose)) {
    cli::cli_alert_info(paste(
      "Read in {.val {nrow(queried_ga)}} address{?es} within",
      "{.val {length(unique(queried_ga$place))}} municipalit{?y/ies}."
    ))
  }
  
  queried_ga
}
