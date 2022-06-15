# example script to run swo code to obtain age/length input sample size

devtools::install_github("BenWilliams-NOAA/swo", force = TRUE)
library(swo)


yrs = 1984
species = c(10110, 10130, 10180, 10261, 10262, 10200, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152)
region = 'GOA'
afsc_user = ''
afsc_pwd = ''

query_data(region, species, yrs, afsc_user, afsc_pwd)

cpue <- vroom::vroom(here::here('data', 'cpue_goa.csv'))
lfreq <- vroom::vroom(here::here('data', 'lfreq_goa.csv'))
strata <- vroom::vroom(here::here('data', 'strata_goa.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_goa.csv'))

iters = 500

st <- Sys.time()

# Run for all species (and subsetting out special cases so we don't have multiple places with those results)
cpue %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200))) -> .cpue
lfreq %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200))) -> .lfreq
specimen %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200))) -> .specimen

swo_sim(iters = iters, .lfreq, .specimen, .cpue, strata, 
        yrs = yrs, strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, 
        boot_ages = TRUE, reduce_lengths = NULL, length_samples = NULL, sex_samples = NULL, save = 'input_ss', 
        write_comp = FALSE, write_sample = FALSE, region = 'goa', save_orig = TRUE)

# Run for REBS stock complex
cpue %>% 
  tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .cpue
lfreq %>% 
  tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .lfreq
specimen %>% 
  tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .specimen

swo_sim_goa_rebs(iters = iters, .lfreq, .specimen, .cpue, strata, 
        yrs = yrs, strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, 
        boot_ages = TRUE, reduce_lengths = NULL, length_samples = NULL, sex_samples = NULL, save = 'input_ss', 
        write_comp = FALSE, write_sample = FALSE, region = 'goa', save_orig = TRUE)

# Run for dusky codes
cpue %>% 
  tidytable::filter.(species_code %in% c(30150, 30152)) -> .cpue
lfreq %>% 
  tidytable::filter.(species_code %in% c(30150, 30152)) -> .lfreq
specimen %>% 
  tidytable::filter.(species_code %in% c(30150, 30152)) -> .specimen

swo_sim_goa_rebs(iters = iters, .lfreq, .specimen, .cpue, strata, 
                 yrs = yrs, strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, 
                 boot_ages = TRUE, reduce_lengths = NULL, length_samples = NULL, sex_samples = NULL, save = 'input_ss', 
                 write_comp = FALSE, write_sample = FALSE, region = 'goa', save_orig = TRUE)

# Run W/C & E for rex sole
swo_sim_wc_e(iters = iters, lfreq, specimen, cpue, strata, species = 10200,
                 yrs = yrs, strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, 
                 boot_ages = TRUE, reduce_lengths = NULL, length_samples = NULL, sex_samples = NULL, save = 'input_ss', 
                 write_comp = FALSE, write_sample = FALSE, region = 'goa', save_orig = TRUE)

# Run W, C & E for N and S rock sole
swo_sim_w_c_e(iters = iters, lfreq, specimen, cpue, strata, species = c(10261, 10262),
             yrs = yrs, strata = FALSE, boot_hauls = TRUE, boot_lengths = TRUE, 
             boot_ages = TRUE, reduce_lengths = NULL, length_samples = NULL, sex_samples = NULL, save = 'input_ss', 
             write_comp = FALSE, write_sample = FALSE, region = 'goa', save_orig = TRUE)

end <- Sys.time()

