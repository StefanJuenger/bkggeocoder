#' Read encrypted envelopes as data.table
#' 
#' @param .crypt Encrypted data to be read
#' @param key Key to decrypt data
#' @param password Password to decrypt data
#' @param ... Further arguments passed to fread
#' 
#' @noRd
fread_encrypted <- function(.crypt, credentials_path, ...) {
  .decrypt <- openssl::decrypt_envelope(
    .crypt$data, .crypt$iv, .crypt$session,
    key = paste0(credentials_path, "/id_rsa"),
    password = readLines(paste0(credentials_path, "/pwd"))
  )
  
  data.table::fread(
    text = readBin(.decrypt, what = "character"),
    colClasses = "character",
    encoding = "UTF-8",
    ...
  )
}


#' Read BKG data
#' 
#' @description Read and decrypt BKG datasets. This includes both address
#' datasets for individual places as well as a dataset containing place names
#' and their zip codes (depending on the \code{what} argument).
#' 
#' @param place \code{[character]}
#' 
#' Name of the place to be read and decrypted, e.g.,
#' \code{"Aachen"} or \code{"Berlin"}. Ignored if \code{what = "places"}.
#' @param what \code{[character]}
#' 
#' Type of dataset to be read. If \code{"places"}, reads a dataset
#' containing all names and zip codes of German places. If \code{"addresses"}
#' reads in all addresses of a place specified in \code{"place"}.
#' @inheritParams bkg_geocode_offline
#' 
#' @returns A data.table containing either names and zip codes of German
#' districts or full addresses of an individual place specified in
#' \code{what}.
#' 
#' @export
bkg_read <- function(
  place,
  what = "addresses",
  data_from_server = FALSE,
  data_path = "../bkgdata",
  credentials_path = "../bkgcredentials",
  ...
) {
  places_file <- "zip_places/ga_zip_places.csv.encryptr.bin"
  
  dataset <- switch(
    what,
    places = "zip_places/ga_zip_places.csv.encryptr.bin",
    addresses = sprintf("ga/%s.csv.encryptr.bin", place)
  )
  
  if (data_from_server) {
    .crypt_file <- url(file.path("http://10.6.13.132:8000", utils::URLencode(dataset), fsep = "/"))
    on.exit(close(.crypt_file))
  } else {
    .crypt_file <- file.path(data_path, dataset)
  }

  .crypt <- tryCatch(
    expr = readRDS(.crypt_file),
    error = function(e) {
      if (data_from_server) {
        cli::cli_abort(c(
          "Cannot access local server under {.path http://10.6.13.132:8000/}.",
          "!" = "Verify that you are inside the GESIS intranet or set {.var data_from_server = FALSE}"
        ))
      } else {
        cli::cli_abort("Cannot read data from {.path {data_path}}")
      }
    },
    warning = function(w) NULL
  )

  fread_encrypted(.crypt, credentials_path, ...)
}


#' Simpler wrapper for regexec and regmatches
#' 
#' @param x A character vector to be matched.
#' @param pattern Regex expression or term to be looked up in x
#' @param ... Further arguments passed to regexec.
#' 
#' @noRd
match_regex <- function(x, pattern, ...) {
  matches <- regexec(pattern, x, ...)
  regmatches(x, matches)
}
