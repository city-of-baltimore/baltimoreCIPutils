
<!-- README.md is generated from README.Rmd. Please edit that file -->

# baltimoreCIPutils

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Purpose

baltimoreCIPutils is an R package with utility functions designed to
support data import, tidying, and transformation using Workday and
Workday Adaptive Planning data sources for the Baltimore City Capital
Improvment Program (CIP) team in the Baltimore City Department of
Planning.

## Installation

You can install the development version of baltimoreCIPutils like so:

``` r
# pak::pkg_install("city-of-baltimore/baltimoreCIPutils")
```

## Example

``` r
library(baltimoreCIPutils)
## basic example code
```

The package includes several reference data sets including labels for
project hierarchy worktags:

``` r
head(wd_proj_hierarchy_xwalk, 10)
#> # A tibble: 10 × 9
#>    source          id    name  entity use   notes createdTime `PHierarchy1 Code`
#>    <chr>           <chr> <chr> <chr>  <chr> <chr> <chr>       <chr>             
#>  1 Adaptive Plann… PJHC… 9510… DOT S… Acti… Adde… 2023-11-21… <NA>              
#>  2 Adaptive Plann… PJH0… Capi… DGS C… Acti… Adde… 2023-11-21… <NA>              
#>  3 Adaptive Plann… PJH6… Park… Parki… Acti… <NA>  2024-10-16… PJH6700           
#>  4 Adaptive Plann… PJH1… Comp… Balti… Acti… <NA>  2023-11-02… PJH1200           
#>  5 Adaptive Plann… PJH6… Park… Parki… Acti… <NA>  2023-11-02… <NA>              
#>  6 Adaptive Plann… PJHC… Capi… Balti… Acti… <NA>  2023-11-02… <NA>              
#>  7 Adaptive Plann… PJH2… Fina… Depar… Acti… <NA>  2023-11-02… PJH2300           
#>  8 Adaptive Plann… PJHC… Capi… Depar… Acti… <NA>  2023-11-02… <NA>              
#>  9 Adaptive Plann… PJH2… Fire  Balti… Acti… <NA>  2023-11-02… PJH2500           
#> 10 Adaptive Plann… PJH2… Gene… Depar… Acti… <NA>  2023-11-02… PJH2600           
#> # ℹ 1 more variable: `PHierarchy2 Code` <chr>
```
