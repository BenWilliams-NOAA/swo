#' replicate swo function for goa rebs stock complex
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
#' @param reduce_lengths reduce the total number of lengths used in the analysis (default = NULL)
#' @param length_samples sample size by length (default = NULL)
#' @param sex_samples  sample size by sex (default = NULL)
#' @param save name to save a file
#' @param write_comp save the intermediate age/length comps
#' @param write_sample save the new "unsexed" default = FALSE
#' @param region region will create a folder and place results in said folder
#' @param save_orig save the original 
#'
#' @return
#' @export swo_sim_goa_rebs
#'
#' @examples
#' 
#'
swo_sim_goa_rebs <- function(iters = 1, lfreq_data, specimen_data, cpue_data, strata_data, 
                              yrs = NULL, strata = FALSE, boot_hauls = FALSE, boot_lengths = FALSE, 
                              boot_ages = FALSE, reduce_lengths = NULL, length_samples = NULL, sex_samples = NULL, save = NULL, 
                              write_comp = FALSE, write_sample = FALSE, region = NULL, save_orig = FALSE){
  
  if(isTRUE(write_comp) & is.null(save) | is.null(save)){
    stop("have to provide a name for the file, save = ...")
  } 
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  
  # get original values
  og <- swo(lfreq_data = lfreq_data, specimen_data = specimen_data, 
            cpue_data = cpue_data, strata_data = strata_data, yrs = yrs, strata = FALSE,
            boot_hauls = FALSE, boot_lengths = FALSE, 
            boot_ages = FALSE, reduce_lengths = NULL, length_samples = NULL, 
            sex_samples = NULL)
  oga <- og$age
  oga %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, age)) %>% 
    tidytable::mutate.(species_code = "REBS") -> oga_c
  ogl <- og$length
  ogl %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, length)) %>% 
    tidytable::mutate.(species_code = "REBS") -> ogl_c
  
  if(isTRUE(save_orig)){
    vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_orig_age_rebs.csv")), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_orig_length_rebs.csv")), delim = ",")
  }
  
  # run iterations
  rr <- purrr::rerun(iters, swo(lfreq_data = lfreq_data, 
                                specimen_data = specimen_data, 
                                cpue_data = cpue_data, 
                                strata_data = strata_data, 
                                yrs = yrs, strata = strata, 
                                boot_hauls = boot_hauls, 
                                boot_lengths = boot_lengths, 
                                boot_ages = boot_ages, 
                                reduce_lengths = reduce_lengths, 
                                length_samples = length_samples, 
                                sex_samples = sex_samples))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_age %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, age)) %>% 
    tidytable::mutate.(species_code = "REBS") %>% 
    split(., .[,'sim']) -> r_age_cmplx
  
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  r_length %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, length)) %>% 
    tidytable::mutate.(species_code = "REBS") %>% 
    split(., .[,'sim']) -> r_length_cmplx
  
  # write out comp data (species-specific)
  if(isTRUE(write_comp)) {
    r_age %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_comp_age_rebs.csv")), delim = ",")
    r_length %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_comp_size_rebs.csv")), delim = ",")
  }
  
  if(isTRUE(write_sample) & !is.null(length_samples)) {
    do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$unsexed %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_removed_length_rebs.csv")), delim = ",")
  }
  
  # ess of bootstrapped age/length (for complex as whole)
  r_age_cmplx %>%
    tidytable::map.(., ~ess_age(sim_data = .x, og_data = oga_c, strata = strata)) %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_age
  
  r_length_cmplx %>%
    tidytable::map.(., ~ess_size(sim_data = .x, og_data = ogl_c, strata = strata)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_size
  
  if(!is.null(save)){
    vroom::vroom_write(ess_age, 
                       here::here("output", region, paste0(save, "_ess_ag_rebs.csv")), 
                       delim = ",")
    vroom::vroom_write(ess_size, 
                       here::here("output", region, paste0(save, "_ess_sz_rebs.csv")), 
                       delim = ",")
  }
  
  list(age = ess_age, size = ess_size)
}