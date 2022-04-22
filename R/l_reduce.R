#' reduce total number of lengths
#'
#' @param data lfreq dataframe
#' @param samples number of lengths to take
#' @param write_sample whether to output intermediate step
#' @param region region being sampled
#' @param save name to save
#'
#' @return
#' @export l_reduce
#'
#'
#' @examples
l_reduce <- function(data, samples, write_sample, region, save){
  data %>%
    tidytable::mutate.(id = .I) %>%
    tidytable::mutate.(n = .N, 
                       .by = c(year, species_code, stratum, hauljoin)) %>%
    dplyr::group_by(year, species_code, stratum, hauljoin) %>%
    dplyr::sample_n(if(n > samples) samples else n) -> .new_sexed
  

  if(isTRUE(write_sample)){
    .new_unsexed %>% 
      dplyr::group_by(year, species_code, stratum, hauljoin) %>%
      dplyr::count() %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_reduced_lengths.csv")), delim = ",")
  }
  
  .new_sexed
}