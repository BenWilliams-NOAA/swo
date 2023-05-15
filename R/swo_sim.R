#' replicate swo function and generate input sample sizes
#'
#' @param iters number of iterations (500 recommended)
#' @param lfreq_data  input dataframe
#' @param specimen_data input dataframe
#' @param cpue_data input dataframe
#' @param strata_data input dataframe
#' @param r_t input dataframe
#' @param yrs any year filter >= (default = NULL)
#' 
#' @param strata switch for regional or by strata (default = FALSE)
#' 
#' @param boot_hauls resample hauls w/replacement (default = FALSE)
#' @param boot_lengths resample lengths w/replacement (default = FALSE)
#' @param boot_ages resample ages w/replacement (default = FALSE)
#' @param al_var include age-length variability (default = FALSE)
#' @param age_err include ageing error (default = FALSE)
#' @param length_samples sample size by length (default = NULL)
#' @param age_samples proportion of sample size by age (default = NULL)
#' @param sexlen_samples  sample size by sex-length (default = NULL)
#' @param region region will create a folder and place results in said folder
#' @param save_interm save the intermediate results: original comps, resampled comps (default = FALSE)
#' @param match_orig match the computed values to gap output (default = FALSE)
#' @param srvy_type only for bering sea survey, denotes whether it's the shelf or slope survey (default = NULL)
#' @param save name to save output
#' 
#' @return effective sample sizes and imput sample sizes
#' @export swo_sim
#'
#' @examples

swo_sim <- function(iters = 1, 
                    lfreq_data, 
                    specimen_data, 
                    cpue_data, 
                    strata_data,
                    r_t,
                    yrs = NULL,
                    strata = FALSE, 
                    boot_hauls = FALSE, 
                    boot_lengths = FALSE, 
                    boot_ages = FALSE,
                    al_var = FALSE, 
                    age_err = FALSE,
                    length_samples = NULL, 
                    age_samples = NULL, 
                    sexlen_samples = NULL, 
                    region = NULL, 
                    save_interm = FALSE, 
                    match_orig = FALSE,
                    srvy_type = NULL, 
                    save){
  
  if(isTRUE(write_interm) & is.null(save) | is.null(save)){
    stop("have to provide a name for the file, save = ...")
  } 
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  
  # restructure data
  lfreq_data <- tidytable::as_tidytable(lfreq_data) 
  specimen_data <- tidytable::as_tidytable(specimen_data) 
  cpue_data <- tidytable::as_tidytable(cpue_data) 
  strata_data <- tidytable::as_tidytable(strata_data) 
  
  # get original values
  og <- swo(lfreq_data = lfreq_data, 
            specimen_data = specimen_data, 
            cpue_data = cpue_data, 
            strata_data = strata_data,
            r_t = r_t,
            yrs = yrs, 
            strata = strata,
            boot_hauls = FALSE, 
            boot_lengths = FALSE, 
            boot_ages = FALSE,
            al_var = FALSE,
            age_err = FALSE,
            length_samples = NULL, 
            age_samples = NULL,
            sexlen_samples = NULL)
  
  oga <- og$age %>% 
    select(-type)
  ogl <- og$length %>% 
    select(-type)
  
  # run iterations
  rr <- purrr::map(1:iters, ~ swo(lfreq_data = lfreq_data, 
                                  specimen_data = specimen_data, 
                                  cpue_data = cpue_data, 
                                  strata_data = strata_data,
                                  r_t = r_t,
                                  yrs = yrs, 
                                  strata = strata,
                                  boot_hauls = boot_hauls, 
                                  boot_lengths = boot_lengths, 
                                  boot_ages = boot_ages,
                                  al_var = al_var,
                                  age_err = age_err,
                                  length_samples = length_samples, 
                                  age_samples = age_samples,
                                  sexlen_samples = sexlen_samples))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # if desired, write out intermediate results
  if(isTRUE(save_interm) & region != 'bs') {
    vroom::vroom_write(oga, file = here::here("output", region, "orig_age.csv"), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, "orig_length.csv"), delim = ",")
    r_length %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_size.csv"), delim = ",")
    r_age %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_age.csv"), delim = ",")
  }
  if(isTRUE(save_interm) & region == 'bs') {
    vroom::vroom_write(oga, file = here::here("output", region, paste0("orig_age_", srvy_type, ".csv")), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, paste0("orig_length_", srvy_type, ".csv")), delim = ",")
    r_length %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0("resampled_size_", srvy_type, ".csv")), delim = ",")
    r_age %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0("resampled_age_", srvy_type, ".csv")), delim = ",")
  }
  
  # compute effective sample size of bootstrapped age/length
  r_age %>%
    tidytable::map.(., ~ess_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::rename(comp_type = ess) %>% 
    tidytable::mutate.(comp_type = tidytable::case_when(comp_type == 'ess_f' ~ 'female',
                                                        comp_type == 'ess_m' ~ 'male',
                                                        comp_type == 'ess_t' ~ 'total')) -> ess_age
  r_length %>%
    tidytable::map.(., ~ess_size(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::rename(comp_type = ess) %>% 
    tidytable::mutate.(comp_type = tidytable::case_when(comp_type == 'ess_f' ~ 'female',
                                                        comp_type == 'ess_m' ~ 'male',
                                                        comp_type == 'ess_t' ~ 'total')) -> ess_size
  
  # compute harmonic mean of iterated effective sample size, which is the input sample size (iss)
  ess_age %>% 
    tidytable::summarise(iss = psych::harmonic.mean(value, na.rm=T),
                         .by = c(year, species_code, comp_type, type)) %>% 
    tidytable::filter.(iss > 0) %>% 
    tidytable::pivot_wider(names_from = type, values_from = iss) -> iss_age
  
  ess_age %>%
    tidytable::pivot_wider(names_from = type, values_from = value) -> ess_age
  
  ess_size %>% 
    tidytable::summarise(iss = psych::harmonic.mean(value, na.rm=T),
                         .by = c(year, species_code, comp_type, type)) %>% 
    tidytable::filter.(iss > 0) %>% 
    tidytable::pivot_wider(names_from = type, values_from = iss) -> iss_size
  
  ess_size %>%
    tidytable::pivot_wider(names_from = type, values_from = value) -> ess_size
  
  # write out ess/iss results
  if(!is.null(save)){
    vroom::vroom_write(ess_age, 
                       here::here("output", region, paste0(save, "_ess_ag.csv")), 
                       delim = ",")
    vroom::vroom_write(ess_size, 
                       here::here("output", region, paste0(save, "_ess_sz.csv")), 
                       delim = ",")
    
    vroom::vroom_write(iss_age, 
                       here::here("output", region, paste0(save, "_iss_ag.csv")), 
                       delim = ",")
    vroom::vroom_write(iss_size, 
                       here::here("output", region, paste0(save, "_iss_sz.csv")), 
                       delim = ",")
    
  }
  
  list(ess_age = ess_age, ess_size = ess_size, iss_age = iss_age, iss_size = iss_size)
}