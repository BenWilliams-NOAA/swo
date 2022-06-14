#' replicate swo function for spatially-explicit iss for western, central and eastern gulf subregions
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
#' @export swo_sim_goa_w_c_e
#'
#' @examples
#' 
#'
swo_sim_goa_w_c_e <- function(iters = 1, lfreq_data, specimen_data, cpue_data, strata_data, species, 
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
  
  # define data by subregion (swap out 'stratum' column with subregion) 
  lfreq_data %>% 
    tidytable::filter.(species_code %in% species) %>% 
    tidytable::left_join.(strata_data)  %>% 
    tidytable::mutate.(region = ifelse(summary_area %in% c(919), "W",
                                       ifelse(summary_area %in% c(929, 939), "C",
                                              ifelse(summary_area %in% c(949, 959), "E", NA)))) %>% 
    tidytable::select.(-survey, -area, -summary_area) -> .lfreq_data
  
  specimen_data %>% 
    tidytable::filter.(species_code %in% species) %>% 
    tidytable::left_join.(strata_data) %>% 
    tidytable::mutate.(region = ifelse(summary_area %in% c(919), "W",
                                       ifelse(summary_area %in% c(929, 939), "C",
                                              ifelse(summary_area %in% c(949, 959), "E", NA)))) %>%  
    tidytable::select.(-survey, -area, -summary_area) -> .specimen_data
  
  cpue_data %>% 
    tidytable::filter.(species_code %in% species) %>% 
    tidytable::left_join.(strata_data) %>% 
    tidytable::mutate.(region = ifelse(summary_area %in% c(919), "W",
                                       ifelse(summary_area %in% c(929, 939), "C",
                                              ifelse(summary_area %in% c(949, 959), "E", NA)))) %>% 
    tidytable::select.(-survey, -area, -summary_area)  -> .cpue_data

  
  # get original values for western goa
  og_w <- swo(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "W"), 
            specimen_data = subset(.specimen_data, .specimen_data$region == "W"), 
            cpue_data = subset(.cpue_data, .cpue_data$region == "W"),  
            strata_data = strata_data, yrs = yrs, strata = FALSE,
            boot_hauls = FALSE, boot_lengths = FALSE, 
            boot_ages = FALSE, reduce_lengths = NULL, length_samples = NULL, 
            sex_samples = NULL)

  oga <- og_w$age
  oga %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, age, species_code)) -> oga_w
  ogl <- og_w$length
  ogl %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, length, species_code)) -> ogl_w

  # get original values for central goa
  og_c <- swo(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "C"), 
              specimen_data = subset(.specimen_data, .specimen_data$region == "C"), 
              cpue_data = subset(.cpue_data, .cpue_data$region == "C"),  
              strata_data = strata_data, yrs = yrs, strata = FALSE,
              boot_hauls = FALSE, boot_lengths = FALSE, 
              boot_ages = FALSE, reduce_lengths = NULL, length_samples = NULL, 
              sex_samples = NULL)
  
  oga <- og_c$age
  oga %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, age, species_code)) -> oga_c
  ogl <- og_c$length
  ogl %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, length, species_code)) -> ogl_c

  # get original values for eastern goa
  og_e <- swo(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "E"), 
               specimen_data = subset(.specimen_data, .specimen_data$region == "E"), 
               cpue_data = subset(.cpue_data, .cpue_data$region == "E"),  
               strata_data = strata_data, yrs = yrs, strata = FALSE,
               boot_hauls = FALSE, boot_lengths = FALSE, 
               boot_ages = FALSE, reduce_lengths = NULL, length_samples = NULL, 
               sex_samples = NULL)
  
  oga <- og_e$age
  oga %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, age, species_code)) -> oga_e
  ogl <- og_e$length
  ogl %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, length, species_code)) -> ogl_e
  
  # write og results 
  oga_w %>% 
    tidytable::mutate.(region = "W") -> .oga_w
  oga_c %>% 
    tidytable::mutate.(region = "C") -> .oga_c
  oga_e %>% 
    tidytable::mutate.(region = "E") %>% 
    tidytable::bind_rows.(.oga_w, .oga_c) -> oga
  
  ogl_w %>% 
    tidytable::mutate.(region = "W") -> .ogl_w
  ogl_c %>% 
    tidytable::mutate.(region = "C") -> .ogl_c
  ogl_e %>% 
    tidytable::mutate.(region = "E") %>% 
    tidytable::bind_rows.(.ogl_w, .ogl_c) -> ogl
  
  if(isTRUE(save_orig)){
    vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_orig_age_w_c_egoa.csv")), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_orig_length_w_c_egoa.csv")), delim = ",")
  }

  # run iterations for western goa
  rr <- purrr::rerun(iters, swo(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "W"), 
                                specimen_data = subset(.specimen_data, .specimen_data$region == "W"), 
                                cpue_data = subset(.cpue_data, .cpue_data$region == "W"), 
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
                          .by = c(sim, year, age, species_code)) %>% 
    split(., .[,'sim']) -> r_age_w
  
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  r_length %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, length, species_code)) %>% 
    split(., .[,'sim']) -> r_length_w
  
  # run iterations for central goa
  rr <- purrr::rerun(iters, swo(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "C"), 
                                specimen_data = subset(.specimen_data, .specimen_data$region == "C"), 
                                cpue_data = subset(.cpue_data, .cpue_data$region == "C"), 
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
                          .by = c(sim, year, age, species_code)) %>% 
    split(., .[,'sim']) -> r_age_c
  
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  r_length %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, length, species_code)) %>% 
    split(., .[,'sim']) -> r_length_c
  
  # run iterations for eastern goa
  rr <- purrr::rerun(iters, swo(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "E"), 
                                specimen_data = subset(.specimen_data, .specimen_data$region == "E"), 
                                cpue_data = subset(.cpue_data, .cpue_data$region == "E"), 
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
                          .by = c(sim, year, age, species_code)) %>% 
    split(., .[,'sim']) -> r_age_e
  
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  r_length %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, length, species_code)) %>% 
    split(., .[,'sim']) -> r_length_e
  
  # write out comp data
  if(isTRUE(write_comp)) {
    r_age_w %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      tidytable::mutate.(region = "W") -> .r_age_w
    r_age_c %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      tidytable::mutate.(region = "C") -> .r_age_c
    r_age_e %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      tidytable::mutate.(region = "E") %>% 
      tidytable::bind_rows.(.r_age_w, .r_age_c) %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_comp_age_w_c_egoa.csv")), delim = ",")
    r_length_w %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      tidytable::mutate.(region = "W") -> .r_length_w
    r_length_c %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      tidytable::mutate.(region = "C") -> .r_length_c
    r_length_e %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      tidytable::mutate.(region = "E") %>% 
      tidytable::bind_rows.(.r_length_w, .r_length_c) %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_comp_size_w_c_egoa.csv")), delim = ",") 
  }

  # if(isTRUE(write_sample) & !is.null(length_samples)) {
  #   do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$unsexed %>%
  #     tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
  #     vroom::vroom_write(here::here("output", region, paste0(save, "_removed_length_wcgoa.csv")), delim = ",")
  # }
  
  # ess of bootstrapped age/length by subregion
  r_age_w %>%
    tidytable::map.(., ~ess_age(sim_data = .x, og_data = oga_w, strata = FALSE)) %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_age_w
  
  r_length_w %>%
    tidytable::map.(., ~ess_size(sim_data = .x, og_data = ogl_w, strata = FALSE)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_size_w
  
  r_age_c %>%
    tidytable::map.(., ~ess_age(sim_data = .x, og_data = oga_c, strata = FALSE)) %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_age_c
  
  r_length_c %>%
    tidytable::map.(., ~ess_size(sim_data = .x, og_data = ogl_c, strata = FALSE)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_size_c
  
  r_age_e %>%
    tidytable::map.(., ~ess_age(sim_data = .x, og_data = oga_e, strata = FALSE)) %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_age_e
  
  r_length_e %>%
    tidytable::map.(., ~ess_size(sim_data = .x, og_data = ogl_e, strata = FALSE)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_size_e
  
  ess_age_w %>% 
    tidytable::mutate.(region = "W") -> .ess_age_w
  ess_age_c %>% 
    tidytable::mutate.(region = "C") -> .ess_age_c
  ess_age_e %>% 
    tidytable::mutate.(region = "E") %>% 
    tidytable::bind_rows.(.ess_age_w, .ess_age_c) -> ess_age
    
  ess_size_w %>% 
    tidytable::mutate.(region = "WC") -> .ess_size_w
  ess_size_c %>% 
    tidytable::mutate.(region = "WC") -> .ess_size_c
  ess_size_e %>% 
    tidytable::mutate.(region = "E") %>% 
    tidytable::bind_rows.(.ess_size_w, .ess_size_c) -> ess_size
  
  if(!is.null(save)){
    vroom::vroom_write(ess_age, 
                       here::here("output", region, paste0(save, "_ess_ag_w_c_egoa.csv")), 
                       delim = ",")
    vroom::vroom_write(ess_size, 
                       here::here("output", region, paste0(save, "_ess_sz_w_c_egoa.csv")), 
                       delim = ",")
  }
  

  list(age = ess_age, size = ess_size)
}