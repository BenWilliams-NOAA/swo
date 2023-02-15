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
#### run for gulf of alaska stocks
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

yrs = 2017
species = c(10110, 10130, 10180, 10200, 10261, 10262, 20510, 21720, 21740, 30060, 30420)
region = 'GOA'

# query_data(region, species, yrs, afsc_user, afsc_pwd)

cpue <- vroom::vroom(here::here('data', 'cpue_goa.csv'))
lfreq <- vroom::vroom(here::here('data', 'lfreq_goa.csv'))
strata <- vroom::vroom(here::here('data', 'strata_goa.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_goa.csv'))

# base case
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = NULL, age_samples = NULL, sexlen_samples = NULL,
        save = 'base', write_interm = FALSE, region = 'goa')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run sex-length subsampling scenarios (example fcn)

# sub-sample to 50 sexed lengths
# swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
#         boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
#         length_samples = NULL, age_samples = NULL,
#         sexlen_samples = 50,
#         save = 's50', write_interm = FALSE, region = 'goa')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run total-length subsampling scenarios

# sub-sample to 50 lengths
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = 50,
        age_samples = NULL, sexlen_samples = NULL,
        save = 't50', write_interm = FALSE, region = 'goa')

# sub-sample to 100 lengths
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = 100,
        age_samples = NULL, sexlen_samples = NULL,
        save = 't100', write_interm = FALSE, region = 'goa')

# sub-sample to 150 lengths
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = 150,
        age_samples = NULL, sexlen_samples = NULL,
        save = 't150', write_interm = FALSE, region = 'goa')

# sub-sample to 200 lengths
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = 200,
        age_samples = NULL, sexlen_samples = NULL,
        save = 't200', write_interm = FALSE, region = 'goa')

# sub-sample to 250 lengths
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = 250,
        age_samples = NULL, sexlen_samples = NULL,
        save = 't250', write_interm = FALSE, region = 'goa')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run age subsampling scenarios

# sub-sample to 90% of total ages
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, 
        strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = NULL,
        age_samples = 0.9,
        sexlen_samples = NULL,
        save = 'a90', write_interm = FALSE, region = 'goa')

# sub-sample to 75% of total ages
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, 
        strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = NULL,
        age_samples = 0.75,
        sexlen_samples = NULL,
        save = 'a75', write_interm = FALSE, region = 'goa')

# sub-sample to 50% of total ages
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, 
        strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = NULL,
        age_samples = 0.5,
        sexlen_samples = NULL,
        save = 'a50', write_interm = FALSE, region = 'goa')

# sub-sample to 25% of total ages
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, 
        strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = NULL,
        age_samples = 0.25,
        sexlen_samples = NULL,
        save = 'a25', write_interm = FALSE, region = 'goa')


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


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### run for bering sea stocks (shelf survey)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

yrs = 2018
species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740)
region = 'BS'

# query_data(region, species, yrs, afsc_user, afsc_pwd)

cpue <- vroom::vroom(here::here('data', 'cpue_bs.csv'))
lfreq <- vroom::vroom(here::here('data', 'lfreq_bs.csv'))
strata <- vroom::vroom(here::here('data', 'strata_bs_mb.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_bs.csv'))

# base case
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = NULL, age_samples = NULL, sexlen_samples = NULL,
        save = 'base', write_interm = FALSE, region = 'bs')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run sex-length subsampling scenarios (example fcn)

# sub-sample to 50 sexed lengths
# swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
#         boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
#         length_samples = NULL, age_samples = NULL,
#         sexlen_samples = 50,
#         save = 's50', write_interm = FALSE, region = 'bs')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run total-length subsampling scenarios

# sub-sample to 50 lengths
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = 50,
        age_samples = NULL, sexlen_samples = NULL,
        save = 't50', write_interm = FALSE, region = 'bs')

# sub-sample to 100 lengths
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = 100,
        age_samples = NULL, sexlen_samples = NULL,
        save = 't100', write_interm = FALSE, region = 'bs')

# sub-sample to 150 lengths
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = 150,
        age_samples = NULL, sexlen_samples = NULL,
        save = 't150', write_interm = FALSE, region = 'bs')

# sub-sample to 200 lengths
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = 200,
        age_samples = NULL, sexlen_samples = NULL,
        save = 't200', write_interm = FALSE, region = 'bs')

# sub-sample to 250 lengths
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, strata = FALSE,
        boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = 250,
        age_samples = NULL, sexlen_samples = NULL,
        save = 't250', write_interm = FALSE, region = 'bs')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run age subsampling scenarios

# sub-sample to 90% of total ages
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, 
        strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = NULL,
        age_samples = 0.9,
        sexlen_samples = NULL,
        save = 'a90', write_interm = FALSE, region = 'bs')

# sub-sample to 75% of total ages
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, 
        strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = NULL,
        age_samples = 0.75,
        sexlen_samples = NULL,
        save = 'a75', write_interm = FALSE, region = 'bs')

# sub-sample to 50% of total ages
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, 
        strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = NULL,
        age_samples = 0.5,
        sexlen_samples = NULL,
        save = 'a50', write_interm = FALSE, region = 'bs')

# sub-sample to 25% of total ages
swo_sim(iters = iters, lfreq, specimen, cpue, strata, yrs = yrs, 
        strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, boot_ages = TRUE,
        length_samples = NULL,
        age_samples = 0.25,
        sexlen_samples = NULL,
        save = 'a25', write_interm = FALSE, region = 'bs')


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### compile total-length subsampling results (across regions)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# compile bs
vroom::vroom(here::here('output', 'bs', 't50_iss_ag.csv')) %>% 
  rename.('iss_age' = iss) %>% 
  left_join.(vroom::vroom(here::here('output', 'bs', 't50_iss_sz.csv'))) %>% 
  rename.('iss_length' = iss) %>% 
  mutate.(sub_samp = 't50', 
          region = 'bs') %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 't100_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               left_join.(vroom::vroom(here::here('output', 'bs', 't100_iss_sz.csv'))) %>% 
               rename.('iss_length' = iss) %>% 
               mutate.(sub_samp = 't100', 
                       region = 'bs'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 't150_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               left_join.(vroom::vroom(here::here('output', 'bs', 't150_iss_sz.csv'))) %>% 
               rename.('iss_length' = iss) %>% 
               mutate.(sub_samp = 't150', 
                       region = 'bs')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 't200_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               left_join.(vroom::vroom(here::here('output', 'bs', 't200_iss_sz.csv'))) %>% 
               rename.('iss_length' = iss) %>% 
               mutate.(sub_samp = 't200', 
                       region = 'bs')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 't250_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               left_join.(vroom::vroom(here::here('output', 'bs', 't250_iss_sz.csv'))) %>% 
               rename.('iss_length' = iss) %>% 
               mutate.(sub_samp = 't250', 
                       region = 'bs')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'base_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               left_join.(vroom::vroom(here::here('output', 'bs', 'base_iss_sz.csv'))) %>% 
               rename.('iss_length' = iss) %>% 
               mutate.(sub_samp = 'base', 
                       region = 'bs'))-> bs

# compile ai
vroom::vroom(here::here('output', 'ai', 't50_iss_ag.csv')) %>% 
  rename.('iss_age' = iss) %>% 
  left_join.(vroom::vroom(here::here('output', 'ai', 't50_iss_sz.csv'))) %>% 
  rename.('iss_length' = iss) %>% 
  mutate.(sub_samp = 't50', 
          region = 'ai') %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 't100_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               left_join.(vroom::vroom(here::here('output', 'ai', 't100_iss_sz.csv'))) %>% 
               rename.('iss_length' = iss) %>% 
               mutate.(sub_samp = 't100', 
                       region = 'ai'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 't150_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               left_join.(vroom::vroom(here::here('output', 'ai', 't150_iss_sz.csv'))) %>% 
               rename.('iss_length' = iss) %>% 
               mutate.(sub_samp = 't150', 
                       region = 'ai')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 't200_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               left_join.(vroom::vroom(here::here('output', 'ai', 't200_iss_sz.csv'))) %>% 
               rename.('iss_length' = iss) %>% 
               mutate.(sub_samp = 't200', 
                       region = 'ai')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 't250_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               left_join.(vroom::vroom(here::here('output', 'ai', 't250_iss_sz.csv'))) %>% 
               rename.('iss_length' = iss) %>% 
               mutate.(sub_samp = 't250', 
                       region = 'ai')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 'base_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               left_join.(vroom::vroom(here::here('output', 'ai', 'base_iss_sz.csv'))) %>% 
               rename.('iss_length' = iss) %>% 
               mutate.(sub_samp = 'base', 
                       region = 'ai')) -> ai

# compile goa
vroom::vroom(here::here('output', 'goa', 't50_iss_ag.csv')) %>% 
  rename.('iss_age' = iss) %>% 
  left_join.(vroom::vroom(here::here('output', 'goa', 't50_iss_sz.csv'))) %>% 
  rename.('iss_length' = iss) %>% 
  mutate.(sub_samp = 't50', 
          region = 'goa') %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 't100_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               left_join.(vroom::vroom(here::here('output', 'goa', 't100_iss_sz.csv'))) %>% 
               rename.('iss_length' = iss) %>% 
               mutate.(sub_samp = 't100', 
                       region = 'goa'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 't150_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               left_join.(vroom::vroom(here::here('output', 'goa', 't150_iss_sz.csv'))) %>% 
               rename.('iss_length' = iss) %>% 
               mutate.(sub_samp = 't150', 
                       region = 'goa')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 't200_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               left_join.(vroom::vroom(here::here('output', 'goa', 't200_iss_sz.csv'))) %>% 
               rename.('iss_length' = iss) %>% 
               mutate.(sub_samp = 't200', 
                       region = 'goa')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 't250_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               left_join.(vroom::vroom(here::here('output', 'goa', 't250_iss_sz.csv'))) %>% 
               rename.('iss_length' = iss) %>% 
               mutate.(sub_samp = 't250', 
                       region = 'goa')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'base_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               left_join.(vroom::vroom(here::here('output', 'goa', 'base_iss_sz.csv'))) %>% 
               rename.('iss_length' = iss) %>% 
               mutate.(sub_samp = 'base', 
                       region = 'goa')) -> goa

# compile all and write results
goa %>% 
  bind_rows.(ai) %>% 
  bind_rows.(bs) %>% 
  vroom::vroom_write(., 
                     here::here('output', 'totlen_iss.csv'), 
                     delim = ',')


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### compile age subsampling results (across regions)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# compile bs
vroom::vroom(here::here('output', 'bs', 'a25_iss_ag.csv')) %>% 
  rename.('iss_age' = iss) %>%
  mutate.(sub_samp = 'a25', 
          region = 'bs') %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'a50_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>%
               mutate.(sub_samp = 'a50', 
                       region = 'bs'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'a75_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>%
               mutate.(sub_samp = 'a75', 
                       region = 'bs'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'a90_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>%
               mutate.(sub_samp = 'a90', 
                       region = 'bs')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'base_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               mutate.(sub_samp = 'base', 
                       region = 'bs')) -> bs

# compile ai
vroom::vroom(here::here('output', 'ai', 'a25_iss_ag.csv')) %>% 
  rename.('iss_age' = iss) %>%
  mutate.(sub_samp = 'a25', 
          region = 'ai') %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 'a50_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>%
               mutate.(sub_samp = 'a50', 
                       region = 'ai'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 'a75_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>%
               mutate.(sub_samp = 'a75', 
                       region = 'ai'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 'a90_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>%
               mutate.(sub_samp = 'a90', 
                       region = 'ai')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 'base_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               mutate.(sub_samp = 'base', 
                       region = 'ai')) -> ai

# compile goa
vroom::vroom(here::here('output', 'goa', 'a25_iss_ag.csv')) %>% 
  rename.('iss_age' = iss) %>%
  mutate.(sub_samp = 'a25', 
          region = 'goa') %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'a50_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>%
               mutate.(sub_samp = 'a50', 
                       region = 'goa'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'a75_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>%
               mutate.(sub_samp = 'a75', 
                       region = 'goa'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'a90_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>%
               mutate.(sub_samp = 'a90', 
                       region = 'goa')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'base_iss_ag.csv')) %>% 
               rename.('iss_age' = iss) %>% 
               mutate.(sub_samp = 'base', 
                       region = 'goa')) -> goa

# compile all and write results
goa %>% 
  bind_rows.(ai) %>% 
  bind_rows.(bs) %>% 
  vroom::vroom_write(., 
                     here::here('output', 'agesub_iss.csv'), 
                     delim = ',')

# For testing run time of 500 iterations
if(iters < 100){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}
