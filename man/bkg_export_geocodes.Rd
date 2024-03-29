% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bkg_export_geocodes.R
\name{bkg_export_geocodes}
\alias{bkg_export_geocodes}
\title{Export geocoding results}
\usage{
bkg_export_geocodes(
  .data,
  which = c("all", "place_matched", "geocoded", "not_geocoded", "not_place_matched",
    "unmatched_places"),
  file,
  overwrite = TRUE,
  ...
)
}
\arguments{
\item{.data}{\code{[GeocodingResults]}

Output of \code{\link[bkggeocoder]{bkg_geocode}} or
\code{\link[bkggeocoder]{bkg_geocode_offline}} that should be exported.}

\item{which}{\code{[character]}

Names of \code{.data} that should be exported. One of \code{"all"},
\code{"place_matched"}, \code{"geocoded"}, \code{"not_geocoded"},
\code{"not_place_matched"} or \code{"unmatched_places"}. Defaults to
\code{"all"}.

If \code{"all"} and \code{file} is an \code{xlsx} file, saves each dataset
to a distinct sheet. Otherwise, creates multiple files that are appended with
the corresponding list name. For geospatial file extensions, only \code{sf}
objects can be exported. Regular data.frames are skipped.

If \code{"place_matched"}, appends the \code{geocoded} and \code{not_geocoded}
datasets and saves both in a single file.

If \code{"not_place_matched"} or \code{"unmatched_places"} and \code{file} is
a geospatial file (like \code{shp} or \code{geojson}), an error is thrown.}

\item{file}{\code{[character]}

Path to the output file. The file type is guessed based on the file extension.
If the file extension is \code{csv}, the data is exported using
\code{\link[readr]{write_delim}}. If it is \code{xlsx}, the data is exported
using \code{\link[openxlsx]{writeData}}. If is is anything else, the file type
is guessed by \code{\link[sf]{st_write}} and must be supported by
\code{\link[sf]{st_drivers}}.}

\item{overwrite}{\code{[logical]}

Whether to overwrite \code{file}, if it already exists. Defaults to \code{TRUE}.}

\item{...}{Further arguments passed to \code{\link[readr]{write_delim}},
\code{\link[openxlsx]{writeData}} or \code{\link[sf]{st_write}}.}
}
\value{
\code{file}, invisibly.
}
\description{
Export the output of \code{\link[bkggeocoder]{bkg_geocode_offline}} and
\code{\link[bkggeocoder]{bkg_geocode}}.
}
