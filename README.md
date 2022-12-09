
# bkggeocoder

<img src="./man/images/hex_bkggeocoder.png" align="right" width="120"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-red.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- badges: end -->

`bkggeocoder` is an R interface to the data and services of the [Federal
Agency of Cartography and Geodesy
(BKG)](https://gdz.bkg.bund.de/index.php/default/webanwendungen/bkg-geocoder.html)
in Germany. The package contains two main features:

- `bkg_geocode_offline`: Offline geocoding based on raw address data
- `bkg_geocode`: Online geocoding through the BKG geocoding API
  (`gdz_geokodierung`)

**WARNING**: The routines for geocoding and also the way the data are
provided are still in the process of testing. Please be aware that some
things may change in the future.

## Requirements

Note that the use of BKG address data is restricted. At the very least,
you need access to the [BKG geocoding
API](https://gdz.bkg.bund.de/index.php/default/geokodierungsdienst-opensearch-der-adv-fur-adressen-und-geonamen-gdz-geokodierung.html)
to use the online geocoding functions. Additionally, access to the raw
encrypted BKG data is required to use the offline geocoding features and
a set of credentials to decrypt the data. Before installing the package,
make sure you fulfill these requirements. In any case, this repository
serves as a means to make geocoding workflows using this package
transparent and reproducible.

When using and publishing the results of the geocoding functions in this
package, always refer to the data source as follows:

© GeoBasis-DE / BKG, Deutsche Post Direkt GmbH, Statistisches Bundesamt,
Wiesbaden (2021)

## Installation

The package is available on GitHub. You can install the latest version
using the following code:

``` r
if (!require(remotes)) install.packages("remotes")

remotes::install_github("StefanJuenger/bkggeocoder")
```
