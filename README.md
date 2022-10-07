
<!-- README.md is generated from README.Rmd. Please edit that file -->

# edeaR <a href="https://bupaverse.github.io/edeaR/"><img src="man/figures/logo.png" align="right" height="50" /></a>

<!-- badges: start -->

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/edeaR)](https://cran.r-project.org/package=edeaR)
[![GitHub
version](https://img.shields.io/badge/GitHub-0.9.2.9000-blue)](https://github.com/bupaverse/edeaR)
[![R-CMD-check](https://github.com/bupaverse/edeaR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bupaverse/edeaR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bupaverse/edeaR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/bupaverse/edeaR?branch=master)
[![Lifecycle:
stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable/)
<!-- badges: end -->

This package provides several useful techniques for *Exploratory and
Descriptive Analysis* of event based data in
[`R`](https://www.r-project.org/).

For more information, check the [manual on
GitHub](https://bupaverse.github.io/edeaR/) or the [bupaR
Documentation](https://bupar.net/getting_started.html) website.

## Installation

You can install **edeaR** from [CRAN](https://cran.r-project.org/) with:

``` r
install.packages("edeaR")
```

### Development Version

You can install the development version of **edeaR** from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bupaverse/edeaR")
```

## Example

``` r
library(edeaR)
#> 
#> Attaching package: 'edeaR'
#> The following object is masked from 'package:base':
#> 
#>     setdiff
library(eventdataR)

# Calculate idle times per resource:
patients %>%
  idle_time(level = "resource", units = "days")
#> # A tibble: 7 × 2
#>   employee idle_time    
#>   <fct>    <drtn>       
#> 1 r7       464.4199 days
#> 2 r1       450.2124 days
#> 3 r4       442.6260 days
#> 4 r5       430.1764 days
#> 5 r3       429.1064 days
#> 6 r6       425.5362 days
#> 7 r2       214.7436 days
```
