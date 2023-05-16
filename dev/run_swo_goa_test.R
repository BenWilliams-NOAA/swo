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
iters = 5

# for testing run time
if(iters < 100){
  st <- Sys.time()
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### run for gulf of alaska stocks
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

yrs = 2017
#species = c(10110, 10130, 10180, 10200, 10261, 10262, 20510, 21720, 21740, 30060, 30420)
species = 21740
region = 'GOA'

# query_data(region, species, yrs, afsc_user, afsc_pass)

cpue <- vroom::vroom(here::here('data', 'cpue_goa.csv'))
lfreq <- vroom::vroom(here::here('data', 'lfreq_goa.csv'))
strata <- vroom::vroom(here::here('data', 'strata_goa.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_goa.csv'))

read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(species_code, region, read_age, test_age) %>% 
  tidytable::rename(age = 'read_age') %>% 
  tidytable::filter(species_code %in% species)

# test swo w/ ae & al at base
swo_sim(iters = iters, 
        lfreq_data = lfreq, 
        specimen_data = specimen, 
        cpue_data = cpue, 
        strata_data = strata,
        r_t = read_test,
        yrs = 2017,
        strata = FALSE, 
        boot_hauls = TRUE, 
        boot_lengths = TRUE, 
        boot_ages = TRUE,
        al_var = TRUE, 
        age_err = TRUE,
        length_samples = NULL, 
        age_samples = NULL, 
        sexlen_samples = NULL, 
        region = region, 
        save_interm = FALSE, 
        match_orig = FALSE,
        srvy_type = NULL, 
        save = 'swo_test')

lfreq_data = lfreq
specimen_data = specimen 
cpue_data = cpue
strata_data = strata
r_t = read_test


# For testing run time of 500 iterations
if(iters < 100){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}
