# edeaR

This package provides several useful techniques for *Exploratory and
Descriptive Analysis* of event based data in
[`R`](https://www.r-project.org/).

For more information, check the [manual on
GitHub](https://bupaverse.github.io/edeaR/) or the [bupaR
Documentation](https://bupaverse.github.io/docs/) website.

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
