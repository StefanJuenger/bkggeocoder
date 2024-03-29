% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{bkg_read}
\alias{bkg_read}
\title{Read BKG data}
\usage{
bkg_read(
  place,
  what = "addresses",
  data_from_server = FALSE,
  data_path = "../bkgdata",
  credentials_path = "../bkgcredentials",
  ...
)
}
\arguments{
\item{place}{\code{[character]}

Name of the place to be read and decrypted, e.g.,
\code{"Aachen"} or \code{"Berlin"}. Ignored if \code{what = "places"}.}

\item{what}{\code{[character]}

Type of dataset to be read. If \code{"places"}, reads a dataset
containing all names and zip codes of German places. If \code{"addresses"}
reads in all addresses of a place specified in \code{"place"}.}

\item{data_from_server}{\code{[logical]}

Whether the address data should be downloaded from GESIS internal server?
Requires access to the GESIS net. If \code{FALSE}, uses the path provided
by \code{data_path}. Defaults to \code{FALSE}.}

\item{data_path}{\code{[character]}

Path to the address data provided by Stefan Jünger. Ignored
if \code{data_from_server = TRUE}.}

\item{credentials_path}{\code{[character]}

Path to credentials package provided by Stefan Jünger.}

\item{...}{Further arguments passed to \code{\link[data.table]{fread}}}
}
\value{
A data.table containing either names and zip codes of German
districts or full addresses of an individual place specified in
\code{what}.
}
\description{
Read and decrypt BKG datasets. This includes both address
datasets for individual places as well as a dataset containing place names
and their zip codes (depending on the \code{what} argument).
}
