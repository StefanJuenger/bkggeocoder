# `bkggeocoder` - an `R` Package for geocoding based on the data of the Federal Agency of Cartography and Geodesy (BKG) in Germany

This R Package is probably rather useless to you. But maybe you've been pointed to this site by me - then you might be fine. So what this `R` package does it so provide an interface to address / geocoordinates data from Federal Agency of Cartography and Geodesy (BKG) in Germany. It's an offline geocoder!

However, there are two to three things you need to get started (this is why the package may do nothing for you):

1. The (encrypted) offline data provided by me
2. A set of credentials to decrypt the data also provided by me
3. You have to install the package from GitHub

The latter can easily be done via this command in `R`

```
remotes::install_github("StefanJuenger/bkggeocoder")
```

WARNING: The routines for geocoding and also the way the data are provided are still in the process of testing. Please be aware that some things may change in the future.

Enjoy!
