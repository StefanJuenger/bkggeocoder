#' Query the address database
#'
#' @param places character vector comprising place names
#'
#' @noRd

bkg_query_ga <- function(places, data_path, credentials_path, echo) {

  # initialize progress bar
  if (isTRUE(echo)) {
    pb_query <-
      progress::progress_bar$new(
        total = length(places),
        force = TRUE,
        clear = FALSE
      )

    pb_query$tick(0)
  }

  queried_ga <-
    lapply(places, function(i) {

      if (isTRUE(echo)) {
        pb_query$tick()
      }

      dataset_name <-
        i %>%
        gsub("/", "_", .)

      .crypt <-
        paste0(data_path, "/ga/", dataset_name, ".csv.encryptr.bin") %>%
        readRDS()

      tmp_out_file <- file("tmp_out_file.csv", "wb") # out file

      openssl::decrypt_envelope(
        .crypt$data, .crypt$iv,
        .crypt$session,
        key = paste0(credentials_path, "/id_rsa"),
        password = readLines(paste0(credentials_path, "/pwd"))
      ) %>%
        writeBin(tmp_out_file)

      close(tmp_out_file)


      i_data <-
        data.table::fread(
          "tmp_out_file.csv",
          colClasses = 'character',
          encoding = "UTF-8"
        )

      unlink("tmp_out_file.csv")

      i_data
    })

  queried_ga <- data.table::rbindlist(queried_ga)

  queried_ga[, list(place, whole_address, RS, x, y)]

  queried_ga[, x := gsub(",", ".", x)]

  queried_ga[, y := gsub(",", ".", y)]

  queried_ga
}
