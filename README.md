
# swo: Survey Workload Optimization

<!-- badges: start -->
<!-- badges: end -->

The goal of swo is to ...

## Installation

You can install the development version of swo from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BenWilliams-NOAA/swo")
```

## Example

This is a basic example which shows you how to query the data for the GOA or AI:

``` r
library(swo)
yrs = 2017
species = c(10110, 21740)
region = 'GOA'
afsc_user = 'your_afsc_username'
afsc_pwd = 'your_afsc_pwd'

query_data(region, species, yrs, afsc_user, afsc_pwd)

```

