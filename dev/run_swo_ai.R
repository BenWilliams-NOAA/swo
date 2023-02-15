# devtools::install_github("BenWilliams-NOAA/swo", force = TRUE)
# library(swo)

# load/source libraries/functions for testing
library(purrr)
library(tidyverse)
library(tidytable)
library(psych)
library(vroom)
library(here)

source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)

# get database username/password
db <- vroom::vroom(here::here("database_specs.csv"))
afsc_user = db$username[db$database == "AFSC"]
afsc_pass = db$password[db$database == "AFSC"]

# set number of desired bootstrap iterations (suggested here: 10 for testing, 500 for running)
#iters = 500
iters = 10

# for testing run time
if(iters < 100){
  st <- Sys.time()
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### run for aleutian islands stocks
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

yrs = 2014
species = c(30060, 21740, 10110, 30420, 21921, 21720, 10130, 30020)
region = 'AI'

# query_data(region, species, yrs, afsc_user, afsc_pwd)

cpue <- vroom::vroom(here::here('data', 'cpue_ai.csv'))
lfreq <- vroom::vroom(here::here('data', 'lfreq_ai.csv'))
strata <- vroom::vroom(here::here('data', 'strata_ai.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_ai.csv'))

# base case
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = NULL, age_samples = NULL, sexlen_samples = NULL,
        save = 'base', write_interm = FALSE, region = 'ai')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run sex-length subsampling scenarios (example fcn)

# sub-sample to 50 sexed lengths
# swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
#         boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
#         length_samples = NULL, age_samples = NULL,
#         sexlen_samples = 50,
#         save = 's50', write_interm = FALSE, region = 'ai')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run total-length subsampling scenarios

# sub-sample to 50 lengths
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = 50,
        age_samples = NULL, sexlen_samples = NULL,
        save = 't50', write_interm = FALSE, region = 'ai')

# sub-sample to 100 lengths
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = 100,
        age_samples = NULL, sexlen_samples = NULL,
        save = 't100', write_interm = FALSE, region = 'ai')

# sub-sample to 150 lengths
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = 150,
        age_samples = NULL, sexlen_samples = NULL,
        save = 't150', write_interm = FALSE, region = 'ai')

# sub-sample to 200 lengths
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = 200,
        age_samples = NULL, sexlen_samples = NULL,
        save = 't200', write_interm = FALSE, region = 'ai')

# sub-sample to 250 lengths
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = 250,
        age_samples = NULL, sexlen_samples = NULL,
        save = 't250', write_interm = FALSE, region = 'ai')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run age subsampling scenarios

# sub-sample to 90% of total ages
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, 
        strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = NULL,
        age_samples = 0.9,
        sexlen_samples = NULL,
        save = 'a90', write_interm = FALSE, region = 'ai')

# sub-sample to 75% of total ages
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, 
        strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = NULL,
        age_samples = 0.75,
        sexlen_samples = NULL,
        save = 'a75', write_interm = FALSE, region = 'ai')

# sub-sample to 50% of total ages
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, 
        strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = NULL,
        age_samples = 0.5,
        sexlen_samples = NULL,
        save = 'a50', write_interm = FALSE, region = 'ai')

# sub-sample to 25% of total ages
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, 
        strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = NULL,
        age_samples = 0.25,
        sexlen_samples = NULL,
        save = 'a25', write_interm = FALSE, region = 'ai')


# For testing run time of 500 iterations
if(iters < 100){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}
