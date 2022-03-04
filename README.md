
<!-- README.md is generated from README.Rmd. Please edit that file -->

# birdseyeview

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/birdseyeview)](https://CRAN.R-project.org/package=birdseyeview)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of birdseyeview is to make it easy to create the types of maps,
plots, and tables used for community plans. This package is designed to
use the [overedge package](https://elipousson.github.io/overedge/) and
data packages like
[mapbaltimore](https://elipousson.github.io/mapbaltimore/) or
[bcpss](https://elipousson.github.io/bcpss/) to create reproducible maps
and tables for a range of needs.

## Installation

You can install the development version of birdseyeview like so:

``` r
remotes::install_github("elipousson/birdseyeview")
```

## Example

``` r
library(birdseyeview)
library(ggplot2)
library(sf)
#> Warning: package 'sf' was built under R version 4.1.1
#> Linking to GEOS 3.9.1, GDAL 3.2.3, PROJ 7.2.1; sf_use_s2() is TRUE
```
