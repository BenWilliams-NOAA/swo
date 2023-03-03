#' replicate swo function and generate input sample sizes
#'
#' @param iters number of iterations (500 recommended)
#' @param lfreq_data  input dataframe
#' @param specimen_data input dataframe
#' @param cpue_data input dataframe
#' @param strata_data input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param strata switch for regional or by strata (default = FALSE)
#' @param boot_hauls resample hauls w/replacement (default = FALSE)
#' @param boot_lengths resample lengths w/replacement (default = FALSE)
#' @param boot_ages resample ages w/replacement (default = FALSE)
#' @param length_samples sample size by length (default = NULL)
#' @param age_samples proportion of sample size by age (default = NULL)
#' @param sexlen_samples  sample size by sex-length (default = NULL)
#' @param save name to save a file
#' @param write_interm save the intermediate age/length comps, the new "unsexed/unsampled" samples, and original length/age pop'n ests
#' @param region region will create a folder and place results in said folder
#'
#' @return
#' @export swo_sim
#'
#' @examples
swo_sim <- function(iters = 1, lfreq_data, specimen_data, cpue_data, strata_data, yrs = NULL,
                    strata = FALSE, boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE,
                    length_samples = NULL, age_samples = NULL, sexlen_samples = NULL, save = NULL, 
                    write_interm = FALSE, region = NULL){
  
  if(isTRUE(write_interm) & is.null(save) | is.null(save)){
    stop("have to provide a name for the file, save = ...")
  } 
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  
  lfreq_data <- tidytable::as_tidytable(lfreq_data) 
  specimen_data <- tidytable::as_tidytable(specimen_data) 
  cpue_data <- tidytable::as_tidytable(cpue_data) 
  strata_data <- tidytable::as_tidytable(strata_data) 
  
  # get original values
  og <- swo(lfreq_data = lfreq_data, specimen_data = specimen_data, 
            cpue_data = cpue_data, strata_data = strata_data, yrs = yrs, strata = strata,
            boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE,
            length_samples = NULL, age_samples = NULL, sexlen_samples = NULL)
  oga <- og$age %>% 
    select(-type)
  ogl <- og$length %>% 
    select(-type)
  
  # run iterations
  rr <- purrr::map(1:iters, ~ swo(lfreq_data = lfreq_data, 
                                  specimen_data = specimen_data, 
                                  cpue_data = cpue_data, 
                                  strata_data = strata_data, 
                                  yrs = yrs, strata = strata, 
                                  boot_hauls = boot_hauls, 
                                  boot_lengths = boot_lengths, 
                                  boot_ages = boot_ages,
                                  length_samples = length_samples, 
                                  age_samples = age_samples,
                                  sexlen_samples = sexlen_samples))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length 
  
  # write out intermediate results - not saving for now, leaving og as example
  # vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_orig_age.csv")), delim = ",")
  # vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_orig_length.csv")), delim = ",")
  
  # ess of bootstrapped age/length
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
    vroom::vroom_write(iss_age, 
                       here::here("output", region, paste0(save, "_iss_ag.csv")), 
                       delim = ",")
    vroom::vroom_write(ess_size, 
                       here::here("output", region, paste0(save, "_ess_sz.csv")), 
                       delim = ",")
    vroom::vroom_write(iss_size, 
                       here::here("output", region, paste0(save, "_iss_sz.csv")), 
                       delim = ",")
    
  }
  
}