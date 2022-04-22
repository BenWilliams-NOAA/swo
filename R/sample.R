#' resample length or sex data
#'
#' @param lfreq_un expanded length frequency data
#' @param samples number of samples to take (default = NULL)
#' @param type length or sex samples (default = 'length')
#' @param write_sample switch to save the "new_unsexed" data
#' @param save name to save a file
#' @param region region will create a folder and place results in said folder
#'
#' @return
#' @export
#'
#' @examples
sample <- function(lfreq_un, samples, type = 'length', write_sample, save, region) {
  
  if(type == 'length'){
    lfreq_un %>%
      tidytable::filter.(sex != 3) %>%
      tidytable::mutate.(id = .I) %>%
      tidytable::mutate.(n = .N, 
                         .by = c(year, species_code, stratum, hauljoin)) -> .inter
    
    # sample by sample size
    .inter %>%
      dplyr::group_by(year, species_code, stratum, hauljoin) %>%
      dplyr::sample_n(if(n > samples) samples else n) -> .new_sexed
    
    .inter %>%
      tidytable::anti_join.(.new_sexed, by = "id") %>%
      tidytable::mutate.(sex = 3) -> .new_unsexed
    
    if(isTRUE(write_sample)){
      .new_unsexed %>% 
        dplyr::group_by(year, species_code, stratum, hauljoin) %>%
        dplyr::count() %>% 
        vroom::vroom_write(here::here("output", region, paste0(save, "_removed_length.csv")), delim = ",")
    }
    
    # rejoin to original unsexed
    lfreq_un %>%
      tidytable::filter.(sex == 3) %>%
      tidytable::mutate.(id = .I,
                         n = .N, .by = c(year, species_code, stratum, hauljoin)) %>%
      tidytable::bind_rows.(.new_sexed, .new_unsexed) 
    
  } else {
    
    # this part doesn't do what it should so ignore for now.
    # lfreq_un %>%
    #   tidytable::filter.(sex != 3) %>%
    #   tidytable::mutate.(id = .I) %>%
    #   tidytable::mutate.(n = .N, 
    #                      .by = c(year, species_code, stratum, hauljoin, sex)) -> .inter
    # 
    # # sample by sample size
    # .inter %>%
    #   dplyr::group_by(year, species_code, stratum, hauljoin, sex) %>%
    #   dplyr::sample_n(if(n > samples) samples else n) -> .new_sexed
    # 
    # .inter %>%
    #   tidytable::anti_join.(.new_sexed, by = "id") %>%
    #   tidytable::mutate.(sex = 3) -> .new_unsexed
    # 
    # if(isTRUE(write_sample)){
    #   .new_unsexed %>% 
    #     dplyr::group_by(year, species_code, stratum, hauljoin) %>%
    #     dplyr::count() %>% 
    #     vroom::vroom_write(here::here("output", region, paste0(save, "_removed_sex.csv")), delim = ",")
    }
    
    
    # rejoin to original unsexed
    lfreq_un %>%
      tidytable::filter.(sex == 3) %>%
      tidytable::mutate.(id = .I,
                         n = .N, .by = c(year, species_code, stratum, hauljoin, sex)) %>%
      tidytable::bind_rows.(.new_sexed, .new_unsexed) 
    
  
}