
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openlocationcode

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/openlocationcode)](https://CRAN.R-project.org/package=openlocationcode)
<!-- badges: end -->

This is the R Open Location Code package. It allows you to encode and
decode Open Location Codes in the R language.

## Installation

You can install the development version of openlocationcode like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

Hopefully it will be available on CRAN soon.

## Using the package

The following is an example of what you can do with the package.

You can turn the package on with `library()`

``` r
library(openlocationcode)
```

You can turn latitude longitude coordinates into Open Location Codes
with `encode()`

``` r
encode(47.365590, 8.524997)
#> [1] "8FVC9G8F+6X"
```

You can turn Open Location Codes into latitude longitude coordinates
with `decode()`

``` r
decode("8FVC9G8F+6X")
#> [1] "8FVC9G8F6X"
#> Geometry set for 1 feature 
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 8.524875 ymin: 47.3655 xmax: 8.525 ymax: 47.36562
#> Geodetic CRS:  WGS 84
#> POLYGON ((8.524875 47.3655, 8.524875 47.36562, ...
```

## Formatting

Code should be formatted according to the [Google R Style
Guide](http://google.github.io/styleguide/pyguide.html).

You can format your code automatically using
[YAPF](https://github.com/google/yapf/).
