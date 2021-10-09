
# covid19india

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/covid19india)](https://CRAN.R-project.org/package=covid19india)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`covid19india` is designed while working with data on COVID-19 in India. [covid19india.org](https://www.covid19india.org/) is 
a data resource with APIs available. It is updated daily and provides count, testing, and vaccine data 
at the national, state, and district level. This package eases the process for R users to obtain data 
ready for analysis in hopes of democratizing data science and speed up public health research.

:warning: **The package is under active development.** :warning: After creating functions for pulling data,
additional functions to calculate *basic* public health metrics and data visualizations will be added.

Please share thoughts and comments with me: [mmsalva@umich.edu](mailto:mmsalva@umich.edu) or 🐦 [@MaxSalTweets](twitter.com/MaxSalTweets)

## Installation

``` r
# Install the CRAN version
install.packages("covid19india")

# Or the development version from GitHub:
# install.packages("remotes")
remotes::install_github("maxsal/covid19india")
```

## Coming soon :movie_camera:

- :racing_car: migrating from `tidyverse` syntax to `data.table`
    - We are moving to `data.table` for speed and reduced dependencies
    - This update will also require R 4.1.0+ (uses native pipe)

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# remotes::install_github("maxsal/covid19india", dependencies = TRUE)
library(covid19india)

# load data from covid19india.org -----------
nat_count   <- get_nat_counts()
state_count <- get_state_counts()
state_tests <- get_state_tests()
state_vax   <- get_state_vax()

# estimate r0 ----------
get_r0(nat_count)
get_r0(state_count)
```
## Versions

### Announcement
* covid19india.org is ceasing to maintain their website at the end of October. We are currently seeking alternate data sources which will be integrated into the package for future use. Expected released with v0.1.5.

### Version 0.1.4
* A rebuild of the package with a `data.table` backend. Improved performance on the order of 4x to 18x. Fewer dependencies. Removed dependencies on R 4.1+ and data.table development version 1.14.3 from earlier version of dev0.1.4.

### Version 0.1.2
* First CRAN submission! Find `covid19india` on CRAN @ [https://cran.r-project.org/package=covid19india](https://cran.r-project.org/package=covid19india)

### Version 0.1.1
* ➕ ADD [`get_all_data()`](R/get_all_data.R) function to pull state and national level time-series count, testing, and vaccine data
* ➕ ADD [`get_district_counts()`](R/get_district_counts.R) function to pull district-level time-series count data
* 🔧 FIX [`get_r0`](R/get_r0.R) function to handle different input variable names
