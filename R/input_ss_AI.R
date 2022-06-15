# example script to run swo code to obtain age/length input sample size

devtools::install_github("BenWilliams-NOAA/swo", force = TRUE)
library(swo)

yrs = 1980
species = c(10110, 10112, 21720, 21740, 21921, 30060, 30420, 30050, 30051, 30052)
region = 'AI'
afsc_user = ''
afsc_pwd = ''

query_data(region, species, yrs, afsc_user, afsc_pwd)

cpue <- vroom::vroom(here::here('data', 'cpue_ai.csv'))
lfreq <- vroom::vroom(here::here('data', 'lfreq_ai.csv'))
strata <- vroom::vroom(here::here('data', 'strata_ai.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_ai.csv'))

iters = 500

st <- Sys.time()

# Run for all species (and subsetting out REBS so we don't have two places with those results)
cpue %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052))) -> .cpue
lfreq %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052))) -> .lfreq
specimen %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052))) -> .specimen

swo_sim(iters = iters, .lfreq, .specimen, .cpue, strata, 
        yrs = yrs, strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, 
        boot_ages = TRUE, reduce_lengths = NULL, length_samples = NULL, sex_samples = NULL, save = 'input_ss', 
        write_comp = FALSE, write_sample = FALSE, region = 'ai', save_orig = TRUE)

# Run for AI REBS stock complex
cpue %>% 
  tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .cpue_rebs
lfreq %>% 
  tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .lfreq_rebs
specimen %>% 
  tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .specimen_rebs

swo_sim_ai_rebs(iters = iters, .lfreq_rebs, .specimen_rebs, .cpue_rebs, strata, 
                yrs = yrs, strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, 
                boot_ages = TRUE, reduce_lengths = NULL, length_samples = NULL, sex_samples = NULL, save = 'input_ss', 
                write_comp = FALSE, write_sample = FALSE, region = 'ai', save_orig = TRUE)

end <- Sys.time()

