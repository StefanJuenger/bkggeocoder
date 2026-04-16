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
# database_path <- ""
# 
# # load all addresses
# bkg_ga <-
#   lapply(laender_names, function(i) {
#     
#     message(paste0("Working on ", i))
#     
#     tmp <-
#       data.table::fread(
#         paste0(database_path, "/ga_", i, ".csv"
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
#           street = gsub(" \\(.*\\)\\b", "", V15), # remove ortsname in street string
#           house_number = V11,
#           house_number_add = V12,
#           zip_code = V16,
#           place = V20,
#           # place_district = V19,
#           place_add = V19,
#           RS,
#           x = gsub(",", ".", V13),
#           y = gsub(",", ".", V14)
#         )
#       ]
#     
#     # make sure all house number additions are lower case
#     tmp <- tmp[, house_number_add := tolower(house_number_add)]
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
#       tmp[, place_add := 
#             stringr::str_replace_all(place_add, "Ortsteil unbekannt", "")
#       ]
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
# ga_zip_places <- bkg_ga[, .(place, place_add, zip_code)]
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


# save as parquet files
# arrow::write_dataset(
#   bkg_ga,
#   "../bkgdata/ga/addresses_partitioned",
#   partitioning = "place",
#   format = "parquet"
# )








# library(duckdb)
# library(DBI)
# library(arrow)
# library(dplyr)
# 
# laender_names <-
#   c(
#     "bb", "be", "bw", "by", "hb", "he", "hh", "mv", "ni", "nw", "rp", "sh",
#     "sl", "sn", "st", "th"
#   )
# 
# database_path <- "C:/Users/mueller2/Downloads/20250430/ga/"
# 
# ga_path <- "../bkgdata/ga/addresses_partitioned_2"
# 
# zip_places_path <- "../bkgdata/zip_places"
# 
# unlink(base_path, recursive = TRUE)
# unlink(glue::glue("{zip_places_path}/zip_places.duckdb"))
# unlink(glue::glue("{zip_places_path}/zip_places.duckdb.wal"))
# 
# for(i in laender_names) {
#   
#   con <- DBI::dbConnect(duckdb::duckdb())
#   
#   arrow_table <- 
#     arrow::read_csv2_arrow(
#       glue::glue("C:/Users/mueller2/Downloads/20250430/ga/ga_{i}.csv"),
#       col_names = FALSE
#     ) |> 
#     dplyr::mutate(f7 = stringr::str_pad(f7, 3, "left", "0")) |> 
#     dplyr::mutate(
#       place_cleaned = 
#         paste(f19, f20) |>
#         gsub('Ortsteil unbekannt', '', x = _) |> 
#         stringr::str_replace_all(c(
#           "ä" = "ae", "ö" = "oe", "ü" = "ue",
#           "Ä" = "Ae", "Ö" = "Oe", "Ü" = "Ue",
#           "ß" = "ss"
#         )) |> 
#         stringi::stri_trans_general("Latin-ASCII") |>
#         stringr::str_to_lower() |>
#         stringr::str_replace_all("[^a-z0-9_-]", "_") |>
#         stringr::str_replace_all("_+", "_") |>  # collapse multiple underscores
#         stringr::str_replace_all("^_|_$", "") |> 
#         gsub('_na', '', x = _)
#     ) |> 
#     arrow::to_duckdb(con = con)
#   
#   arrow_table <- 
#     arrow_table |> 
#     dplyr::transmute(
#       street =
#         dplyr::sql(
#           "regexp_replace(
#             regexp_replace(
#               CAST(f14 AS VARCHAR), ' \\(.*\\)\\b', ''
#               ), 
#           'tr[.]$', 'straße'
#           )"
#         ),
#       house_number = tolower(paste0(f10, f11)),
#       zip_code = f15,
#       place = 
#         dplyr::sql(
#           "trim(
#             regexp_replace(
#               coalesce(f19, '') || ' ' || coalesce(f20, ''),
#               'Ortsteil unbekannt', ''
#             )
#           )"
#         ),
#       place_cleaned,
#       RS = paste0(f3, f4, f5, f7),
#       x = f12,
#       y = f13
#     ) |> 
#     dplyr::compute(name = "tmp_table", overwrite = TRUE)
#   
#   dir.create(glue::glue('{ga_path}/{i}/'), recursive = TRUE)
#   
#   DBI::dbExecute(con, "PRAGMA threads=1;")
#   
#   DBI::dbExecute(
#     con, 
#     glue::glue("COPY tmp_table TO '{ga_path}/{i}/'
#      (FORMAT 'parquet', PARTITION_BY (place_cleaned))")
#   )
#   
#   zip_place <-
#     arrow_table |> 
#     dplyr::select(zip_code, place, place_cleaned) |> 
#     dplyr::distinct()
#   
#   con2 <- 
#     DBI::dbConnect(
#       duckdb::duckdb(), 
#       dbdir = glue::glue("{zip_places_path}/zip_places.duckdb"), 
#       read_only = FALSE
#     )
#   
#   DBI::dbWriteTable(
#     con2, "zip_places", dplyr::collect(zip_place), append = TRUE
#   )
#   
#   DBI::dbDisconnect(con2, shutdown = TRUE)
#   DBI::dbDisconnect(con, shutdown = TRUE)
#   
# }
# 
# 
# # List all first-level directories (partition folders)
# sub_dirs <- fs::dir_ls(ga_path, type = "directory", recurse = FALSE)
# 
# for (batch in sub_dirs) {
#   subfolders <- fs::dir_ls(batch, full.names = TRUE, recurse = FALSE)
#   
#   for (sub in subfolders) {
#     target <- file.path(base_path, basename(sub))
#     
#     # Only move if the folder doesn't already exist in the target
#     if (!dir.exists(target)) {
#       file.rename(sub, target)
#     } else {
#       message("Skipping existing partition folder: ", target)
#     }
#   }
#   
#   # Remove the empty batch folder
#   unlink(batch, recursive = TRUE)
# }
# 
# # create index   
# fs::dir_ls(ga_path, type = "file", regexp = "parquet", recurse = TRUE) |> 
#   write.table(
#     file = glue::glue("{ga_path}/index"), 
#     row.names = FALSE, 
#     col.names = FALSE, 
#     quote = FALSE
#   )

# sub_dirs <- fs::dir_ls(base_path, type = "directory", recurse = FALSE)

# # Function to decode and rename
# for (old_dir in sub_dirs) {
#   # Extract just the folder name (e.g. gemeinde=M%C3%BCnchen)
#   folder_name <- fs::path_file(old_dir)
#   
#   # URL decode the folder name
#   decoded_name <- utils::URLdecode(folder_name)
#   
#   # Compose new full path
#   new_dir <- fs::path(base_path, decoded_name)
#   
#   # Rename folder only if name changed
#   if (old_dir != new_dir) {
#     file.rename(old_dir, new_dir)
#     message("Renamed ", old_dir, " -> ", new_dir)
#   }
# }


# duckdb::dbWriteTable(con, "arrow_table", arrow_table, overwrite = TRUE)
# 
# DBI::dbExecute(con, "
#   COPY (
#     SELECT * FROM arrow_table
#   )
#   TO '../bkgdata/ga/addresses_partitioned_2'
#   (FORMAT 'parquet', PARTITION_BY (place))
# ")
# 
# 
# 
# duckdb::dbExecute(con, "COPY filtered_result TO 'filtered_output' (FORMAT 'parquet', PARTITION_BY (city))")
# 
# # Step 5: Clean up
# dbDisconnect(con, shutdown = TRUE)

# 
# 
# library(fs)
# 
# # Path to the partitioned dataset folder
# base_path <- "../bkgdata/ga/addresses_partitioned"
# 
# # List all first-level directories (partition folders)
# dirs <- dir_ls(base_path, type = "directory", recurse = FALSE)
# 
# # Function to decode and rename
# for (old_dir in dirs) {
#   # Extract just the folder name (e.g. gemeinde=M%C3%BCnchen)
#   folder_name <- path_file(old_dir)
#   
#   # URL decode the folder name
#   decoded_name <- utils::URLdecode(folder_name)
#   
#   # Compose new full path
#   new_dir <- path(base_path, decoded_name)
#   
#   # Rename folder only if name changed
#   if (old_dir != new_dir) {
#     file.rename(old_dir, new_dir)
#     message("Renamed ", old_dir, " -> ", new_dir)
#   }
# }
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
# ga_zip_places <- bkg_ga[, .(place, place_add, zip_code)]
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





