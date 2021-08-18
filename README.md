
# bkggeocoder - an `R` Package for geocoding based on the data of the Federal Agency of Cartography and Geodesy (BKG) in Germany <img src="./man/images/hex_bkggeocoder.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-red.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

This R Package is probably rather useless to you. But maybe you’ve been
pointed to this site by me - then you might be fine. So what this `R`
package does it so provide an interface to address / geocoordinates data
from Federal Agency of Cartography and Geodesy (BKG) in Germany. It’s an
offline geocoder\!

However, there are two to three things you need to get started (this is
why the package may do nothing for you):

1.  The (encrypted) offline data provided by me
2.  A set of credentials to decrypt the data also provided by me
3.  You have to install the package from GitHub

The latter can easily be done via this command in `R`

``` r
if (!require(remotes)) install.packages("remotes")

remotes::install_github("StefanJuenger/bkggeocoder")
```

**WARNING**: The routines for geocoding and also the way the data are
provided are still in the process of testing. Please be aware that some
things may change in the future.

Enjoy\!

## A few points about the data…

My employer (GESIS - Leibniz Institute for the Social Sciences) is a
licensee of the BKG services, which is why I can work with their data.
But I am not allowed to share the data, and thus I have implemented the
workflow with the decrypted data that must be used offline. Still, I
wanted to make sure that the workflows of geocoding at GESIS are as
transparent as possible. Publishing the code on GitHub seemed to be a
proper choice for that. In any case, please always refer to the source
of data as follows:

© GeoBasis-DE / BKG, Deutsche Post Direkt GmbH, Statistisches Bundesamt,
Wiesbaden (2021)
