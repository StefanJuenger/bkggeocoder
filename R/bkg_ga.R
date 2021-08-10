#' Database with all German Addresses (extended)
#'
#' A dataset containing....
#'
#' @format A simple features tibble with XXX rows and XX variables
#' \describe{
#' \item{...}{...}
#' }
#' @source © BKG:
# "bkg_house_coordinates"


# library(data.table)
# library(dplyr)
# data.table::setDTthreads(8)
#
# laender_names <-
#   c(
#     "bb", "be", "bw", "by", "hb", "he", "hh", "mv", "ni", "nw", "rp", "sh",
#     "sl", "sn", "st", "th"
#   )
#
# # load all addresses
# bkg_ga <-
#   lapply(laender_names, function(i) {
#
#     message(paste0("Working on ", i))
#
#     tmp <-
#       data.table::fread(
#         paste0(
#           "../bkgdata-raw/300001191_1995.ga/ga/de/ga/ga_", i, ".csv"
#         ),
#         colClasses = 'character',
#         encoding = "UTF-8"
#       )
#
#     # add AGS
#     tmp <-
#       tmp[
#         ,
#         RS := do.call(paste0, .SD), .SDcols = c("V4", "V5", "V6", "V7", "V8")
#       ]
#
#     # select and rename columns
#     tmp <-
#       tmp[
#         ,
#         .(
#           street = V15,
#           house_number = V11,
#           house_number_add = V12,
#           zip_code = V16,
#           place = V20,
#           place_add = V21,
#           RS,
#           x = V13,
#           y = V14
#         )
#       ]
#
#     # make sure all house number additions are lower case
#     tmp <-
#       tmp[, house_number_add := tolower(house_number_add)]
#
#     # make sure all street names have full names
#     tmp <-
#       tmp[, street := stringr::str_replace_all(street, "str[.]$", "straße")]
#
#     tmp <-
#       tmp[, street := stringr::str_replace_all(street, "Str[.]$", "Straße")]
#
#     # remove "Ortsteil unbekannt" string
#     tmp <-
#       tmp[, place_add := stringr::str_replace_all(place_add, "Ortsteil unbekannt", "")]
#
#     # add whole address column
#     tmp <-
#       tmp[, whole_address :=
#             paste0(
#               street, " ", house_number, house_number_add, " ", zip_code, " ",
#               place
#             )
#       ]
#
#     tmp <-
#       tmp[, whole_address_add :=
#             paste0(
#               street, " ", house_number, house_number_add, " ", zip_code, " ",
#               place, " ", place_add
#             )
#       ]
#
#     tmp
#   })
#
# bkg_ga <- data.table::rbindlist(bkg_ga)
#
# data.table::setkey(bkg_ga, place)
#
#
# lapply(names(table(bkg_ga$place)), function(i) {
#
#   message(glue::glue("{i} "), appendLF = FALSE)
#
#   tmp <- bkg_ga[.(i)]
#
#   dataset_name <-
#     i %>%
#     gsub("/", "_", .)
#
#   data.table::fwrite(
#     tmp,
#     paste0("../bkgdata/ga/", dataset_name, ".csv")
#   )
#
#   encryptr::encrypt_file(
#     paste0("../bkgdata/ga/", dataset_name, ".csv"),
#     public_key_path = "../bkgcredentials/id_rsa.pub"
#   )
#
#   unlink(paste0("../bkgdata/ga/", dataset_name, ".csv"))
# })
#
# ga_zip_places <- bkg_ga[, .(place, zip_code)]
#
# ga_zip_places <- unique(ga_zip_places)
#
# ga_zip_places <- ga_zip_places[order(place)]
#
# data.table::fwrite(
#   ga_zip_places, "../bkgdata/zip_places/ga_zip_places.csv"
# )
#
# encryptr::encrypt_file(
#   "../bkgdata/zip_places/ga_zip_places.csv",
#   public_key_path = "../bkgcredentials/id_rsa.pub"
# )
#
# unlink("../bkgdata/zip_places/ga_zip_places.csv")
#
#
#
#
#
