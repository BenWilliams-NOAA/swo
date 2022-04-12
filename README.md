
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

cpue <- vroom::vroom(here::here('data', 'cpue_goa.csv'))
lfreq <- vroom::vroom(here::here('data', 'lfreq_goa.csv'))
strata <- vroom::vroom(here::here('data', 'strata_goa.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_goa.csv'))

swo(lfreq, specimen, cpue, strata, yrs = 2017, boot_hauls = TRUE,
    boot_lengths = TRUE, boot_ages = TRUE, length_samples = 100, sex_samples = 50)

```

