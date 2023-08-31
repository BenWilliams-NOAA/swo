
# swo: Survey Workload Optimization

<!-- badges: start -->
<!-- badges: end -->

The goal of swo is to to provide a consistent framework for exploring the effects of reducing sample sizes (age or length, by sex or combined sexes) within AFSC surveys.
The methods are applicable to other fishery independent surveys with similar sampling designs.

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

swo_sim(iters = 2, lfreq, specimen, cpue, strata, yrs = 2017, boot_hauls = TRUE,
    boot_lengths = TRUE, boot_ages = TRUE, length_samples = 100, sex_samples = 50, 
    write_comp = TRUE, save = 'test', region = 'goa')

```

## NOAA README

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.

## NOAA License

Software code created by U.S. Government employees is not subject to
copyright in the United States (17 U.S.C. §105). The United
States/Department of Commerce reserve all rights to seek and obtain
copyright protection in countries other than the United States for
Software authored in its entirety by the Department of Commerce. To this
end, the Department of Commerce hereby grants to Recipient a
royalty-free, nonexclusive license to use, copy, and create derivative
works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" height="75" alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) | [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) |
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
