#' @rdname bkg_geocode_single
#' 
#' @param location \code{[sf/sfc]}
#' 
#' A point geometry to be reverse geocoded.
#' @param poly \code{[sf/sfc]}
#' 
#' A geometry to be reverse geocoded. Can be a geometry of type \code{POINT},
#' \code{LINESTRING}, \code{POLYGON}, \code{MULTIPOINT}, \code{MULTILINESTRING},
#' or \code{MULTIPOLYGON}.
#' @param ... Further arguments as in \code{bkg_geocode_single} except
#' query parameters for structured or unstructured geocoding.
#' 
#' @export
bkg_reverse_single <- function(
  location = NULL,
  poly = NULL,
  radius = 1000L,
  epsg = NULL,
  count = 1,
  ...
) {
  # Prepare arguments ----
  # Construct a list of arguments that is ordered like in bkg_geocode_single
  dep_args <- formals(bkg_geocode_single)
  expl_args <- as.list(environment())
  args <- vector(mode = "list", length = length(dep_args))
  names(args) <- names(dep_args)
  ukeys <- union(names(args), names(list(...)))
  args <- mapply(c, args[ukeys], c(expl_args, list(...))[ukeys], SIMPLIFY = FALSE)
  
  # Remove arguments that cannot be used for reverse geocoding
  foul_args <- c(
    "query", "street", "house_number", "zip_code",
    "place", "district", "street_house"
  )
  args[foul_args] <- list(NULL)

  # Replace arguments that are provided for in this function
  interface <- args$interface
  clean <- args$clean
  args[["interface"]] <- NULL
  args[["clean"]] <- NULL
  args["focus_point"] <- list(location)
  args["geometry"] <- list(poly)
  args[["epsg"]] <- if (is.null(args$epsg)) {
    sf::st_crs(ifelse(is.null(location), poly, location))$epsg
  } else args$epsg
  
  # Create POST body ----
  if (identical(interface, "wfs")) {
    cli::cli_warn(paste(
      "{.fun bkg_reverse_single} currently only works with",
      "{.code interface = \"osgts\"}."
    ))
  }

  req <- do.call(osgts_request, unname(args))
  
  # Perform request ----
  res <- httr2::req_perform(req)
  res_text <- httr2::resp_body_string(res)
  res_sf <- sf::read_sf(res_text)
  
  # Clean data ----
  if (clean) {
    res_sf <- clean_geocode(res_sf, NULL, NULL, NULL, NULL, NULL)
  }
  
  sf::st_as_sf(res_sf)
}