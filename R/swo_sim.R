#' replicate swo function
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
#' @param sex_samples  sample size by sex (default = NULL)
#' @param save name to save a file
#' @param write_comp save the intermediate age/length comps
#' @param region region will create a folder and place results in said folder
#'
#' @return
#' @export swo_sim
#'
#' @examples
swo_sim <- function(iters = 1, lfreq_data, specimen_data, cpue_data, strata_data, 
                    yrs = NULL, strata = FALSE, boot_hauls = FALSE, boot_lengths = FALSE, 
                    boot_ages = FALSE, length_samples = NULL, sex_samples = NULL, save = NULL, 
                    write_comp = FALSE, write_sample = FALSE, region = NULL){
  
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
            cpue_data = cpue_data, strata_data = strata_data, yrs = yrs, strata = strata,
            boot_hauls = FALSE, boot_lengths = FALSE, 
            boot_ages = FALSE, length_samples = NULL, 
            sex_samples = NULL, save = NULL, 
            write_sample = FALSE, region = NULL)
  oga <- og$age
  ogl <- og$length
  
  # run iterations
  rr <- purrr::rerun(iters, swo(lfreq_data = lfreq_data, specimen_data = specimen_data, 
                            cpue_data = cpue_data, strata_data = strata_data, yrs = yrs, strata = strata, 
                            boot_hauls = boot_hauls, boot_lengths = boot_lengths, 
                            boot_ages = boot_ages, length_samples = length_samples, 
                            sex_samples = sex_samples, save = save, 
                            write_sample = write_sample, region = region))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # write out comp data
  if(isTRUE(write_comp)){
    r_age %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    vroom::vroom_write(here::here("output", region, paste0(save, "_comp_age.csv")), delim = ",")
    r_length %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_comp_size.csv")), delim = ",")
  }
  
  
  # ess of bootstrapped age/length
  r_age %>%
    tidytable::map.(., ~ess_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_age
  r_length %>%
    tidytable::map.(., ~ess_size(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_size
  
  if(!is.null(save)){
    vroom::vroom_write(ess_age, 
                       here::here("output", region, paste0(save, "_ess_ag.csv")), 
                       delim = ",")
    vroom::vroom_write(ess_size, 
                       here::here("output", region, paste0(save, "_ess_sz.csv")), 
                       delim = ",")
  }
  
  list(age = ess_age, size = ess_size)
}