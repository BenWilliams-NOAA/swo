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
#### compile total-length subsampling results (across regions)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# compile bs
vroom::vroom(here::here('output', 'bs', 't50_iss_ag.csv')) %>% 
  rename.('iss_age' = base) %>% 
  left_join.(vroom::vroom(here::here('output', 'bs', 't50_iss_sz.csv'))) %>% 
  rename.('base_iss_length' = base,
          'sub_iss_length' = sub) %>% 
  mutate.(sub_samp = 't50', 
          region = 'bs') %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 't100_iss_ag.csv')) %>% 
               rename.('iss_age' = base) %>% 
               left_join.(vroom::vroom(here::here('output', 'bs', 't100_iss_sz.csv'))) %>% 
               rename.('base_iss_length' = base,
                       'sub_iss_length' = sub) %>% 
               mutate.(sub_samp = 't100', 
                       region = 'bs'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 't150_iss_ag.csv')) %>% 
               rename.('iss_age' = base) %>% 
               left_join.(vroom::vroom(here::here('output', 'bs', 't150_iss_sz.csv'))) %>% 
               rename.('base_iss_length' = base,
                       'sub_iss_length' = sub) %>% 
               mutate.(sub_samp = 't150', 
                       region = 'bs')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 't200_iss_ag.csv')) %>% 
               rename.('iss_age' = base) %>% 
               left_join.(vroom::vroom(here::here('output', 'bs', 't200_iss_sz.csv'))) %>% 
               rename.('base_iss_length' = base,
                       'sub_iss_length' = sub) %>% 
               mutate.(sub_samp = 't200', 
                       region = 'bs')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 't250_iss_ag.csv')) %>% 
               rename.('iss_age' = base) %>% 
               left_join.(vroom::vroom(here::here('output', 'bs', 't250_iss_sz.csv'))) %>% 
               rename.('base_iss_length' = base,
                       'sub_iss_length' = sub) %>% 
               mutate.(sub_samp = 't250', 
                       region = 'bs')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'base_iss_ag.csv')) %>% 
               rename.('iss_age' = base) %>% 
               left_join.(vroom::vroom(here::here('output', 'bs', 'base_iss_sz.csv'))) %>% 
               rename.('base_iss_length' = base,
                       'sub_iss_length' = sub) %>% 
               mutate.(sub_samp = 'base', 
                       region = 'bs')) -> bs

# compile ai
vroom::vroom(here::here('output', 'ai', 't50_iss_ag.csv')) %>% 
  rename.('iss_age' = base) %>% 
  left_join.(vroom::vroom(here::here('output', 'ai', 't50_iss_sz.csv'))) %>% 
  rename.('base_iss_length' = base,
          'sub_iss_length' = sub) %>% 
  mutate.(sub_samp = 't50', 
          region = 'ai') %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 't100_iss_ag.csv')) %>% 
               rename.('iss_age' = base) %>% 
               left_join.(vroom::vroom(here::here('output', 'ai', 't100_iss_sz.csv'))) %>% 
               rename.('base_iss_length' = base,
                       'sub_iss_length' = sub) %>% 
               mutate.(sub_samp = 't100', 
                       region = 'ai'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 't150_iss_ag.csv')) %>% 
               rename.('iss_age' = base) %>% 
               left_join.(vroom::vroom(here::here('output', 'ai', 't150_iss_sz.csv'))) %>% 
               rename.('base_iss_length' = base,
                       'sub_iss_length' = sub) %>% 
               mutate.(sub_samp = 't150', 
                       region = 'ai')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 't200_iss_ag.csv')) %>% 
               rename.('iss_age' = base) %>% 
               left_join.(vroom::vroom(here::here('output', 'ai', 't200_iss_sz.csv'))) %>% 
               rename.('base_iss_length' = base,
                       'sub_iss_length' = sub) %>% 
               mutate.(sub_samp = 't200', 
                       region = 'ai')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 't250_iss_ag.csv')) %>% 
               rename.('iss_age' = base) %>% 
               left_join.(vroom::vroom(here::here('output', 'ai', 't250_iss_sz.csv'))) %>% 
               rename.('base_iss_length' = base,
                       'sub_iss_length' = sub) %>% 
               mutate.(sub_samp = 't250', 
                       region = 'ai')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 'base_iss_ag.csv')) %>% 
               rename.('iss_age' = base) %>% 
               left_join.(vroom::vroom(here::here('output', 'ai', 'base_iss_sz.csv'))) %>% 
               rename.('base_iss_length' = base,
                       'sub_iss_length' = sub) %>% 
               mutate.(sub_samp = 'base', 
                       region = 'ai')) -> ai

# compile goa
vroom::vroom(here::here('output', 'goa', 't50_iss_ag.csv')) %>% 
  rename.('iss_age' = base) %>% 
  left_join.(vroom::vroom(here::here('output', 'goa', 't50_iss_sz.csv'))) %>% 
  rename.('base_iss_length' = base,
          'sub_iss_length' = sub) %>% 
  mutate.(sub_samp = 't50', 
          region = 'goa') %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 't100_iss_ag.csv')) %>% 
               rename.('iss_age' = base) %>% 
               left_join.(vroom::vroom(here::here('output', 'goa', 't100_iss_sz.csv'))) %>% 
               rename.('base_iss_length' = base,
                       'sub_iss_length' = sub) %>% 
               mutate.(sub_samp = 't100', 
                       region = 'goa'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 't150_iss_ag.csv')) %>% 
               rename.('iss_age' = base) %>% 
               left_join.(vroom::vroom(here::here('output', 'goa', 't150_iss_sz.csv'))) %>% 
               rename.('base_iss_length' = base,
                       'sub_iss_length' = sub) %>% 
               mutate.(sub_samp = 't150', 
                       region = 'goa')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 't200_iss_ag.csv')) %>% 
               rename.('iss_age' = base) %>% 
               left_join.(vroom::vroom(here::here('output', 'goa', 't200_iss_sz.csv'))) %>% 
               rename.('base_iss_length' = base,
                       'sub_iss_length' = sub) %>% 
               mutate.(sub_samp = 't200', 
                       region = 'goa')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 't250_iss_ag.csv')) %>% 
               rename.('iss_age' = base) %>% 
               left_join.(vroom::vroom(here::here('output', 'goa', 't250_iss_sz.csv'))) %>% 
               rename.('base_iss_length' = base,
                       'sub_iss_length' = sub) %>% 
               mutate.(sub_samp = 't250', 
                       region = 'goa')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'base_iss_ag.csv')) %>% 
               rename.('iss_age' = base) %>% 
               left_join.(vroom::vroom(here::here('output', 'goa', 'base_iss_sz.csv'))) %>% 
               rename.('base_iss_length' = base,
                       'sub_iss_length' = sub) %>% 
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
  rename.('base_iss_age' = base,
          'sub_iss_age' = sub) %>%
  mutate.(sub_samp = 'a25', 
          region = 'bs') %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'a50_iss_ag.csv')) %>% 
               rename.('base_iss_age' = base,
                       'sub_iss_age' = sub) %>%
               mutate.(sub_samp = 'a50', 
                       region = 'bs'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'a75_iss_ag.csv')) %>% 
               rename.('base_iss_age' = base,
                       'sub_iss_age' = sub) %>%
               mutate.(sub_samp = 'a75', 
                       region = 'bs'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'a90_iss_ag.csv')) %>% 
               rename.('base_iss_age' = base,
                       'sub_iss_age' = sub) %>%
               mutate.(sub_samp = 'a90', 
                       region = 'bs')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'base_iss_ag.csv')) %>% 
               rename.('base_iss_age' = base,
                       'sub_iss_age' = sub) %>% 
               mutate.(sub_samp = 'base', 
                       region = 'bs')) -> bs

# compile ai
vroom::vroom(here::here('output', 'ai', 'a25_iss_ag.csv')) %>% 
  rename.('base_iss_age' = base,
          'sub_iss_age' = sub) %>%
  mutate.(sub_samp = 'a25', 
          region = 'ai') %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 'a50_iss_ag.csv')) %>% 
               rename.('base_iss_age' = base,
                       'sub_iss_age' = sub) %>%
               mutate.(sub_samp = 'a50', 
                       region = 'ai'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 'a75_iss_ag.csv')) %>% 
               rename.('base_iss_age' = base,
                       'sub_iss_age' = sub) %>%
               mutate.(sub_samp = 'a75', 
                       region = 'ai'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 'a90_iss_ag.csv')) %>% 
               rename.('base_iss_age' = base,
                       'sub_iss_age' = sub) %>%
               mutate.(sub_samp = 'a90', 
                       region = 'ai')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 'base_iss_ag.csv')) %>% 
               rename.('base_iss_age' = base,
                       'sub_iss_age' = sub) %>% 
               mutate.(sub_samp = 'base', 
                       region = 'ai')) -> ai

# compile goa
vroom::vroom(here::here('output', 'goa', 'a25_iss_ag.csv')) %>% 
  rename.('base_iss_age' = base,
          'sub_iss_age' = sub) %>%
  mutate.(sub_samp = 'a25', 
          region = 'goa') %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'a50_iss_ag.csv')) %>% 
               rename.('base_iss_age' = base,
                       'sub_iss_age' = sub) %>%
               mutate.(sub_samp = 'a50', 
                       region = 'goa'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'a75_iss_ag.csv')) %>% 
               rename.('base_iss_age' = base,
                       'sub_iss_age' = sub) %>%
               mutate.(sub_samp = 'a75', 
                       region = 'goa'))  %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'a90_iss_ag.csv')) %>% 
               rename.('base_iss_age' = base,
                       'sub_iss_age' = sub) %>%
               mutate.(sub_samp = 'a90', 
                       region = 'goa')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'base_iss_ag.csv')) %>% 
               rename.('base_iss_age' = base,
                       'sub_iss_age' = sub) %>% 
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
