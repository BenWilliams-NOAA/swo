#' reduce total number of lengths or ages
#'
#' @param data lfreq or afreq dataframe
#' @param samples number of lengths to take
#' @param grp grouping variable(s)
#' @param type either age or length reduction (default = NULL)
#'
#' @return
#' @export reduce_samples
#'
#'
#' @examples
reduce_samples <- function(data, samples, grp = c('year', 'species_code', 'stratum', 'hauljoin'), type = NULL){

  data %>% 
    tidytable::mutate.(id = .I) -> .inter
  
  if(type == 'length'){
    core_samp(.inter, samples, grp = grp, replace = FALSE) -> .new_samp
  } else if (type == 'age'){
    .inter %>% 
      tidytable::slice_sample(prop = samples, .by = c(year, species_code)) -> .new_samp
  }

  .inter %>%
    tidytable::anti_join.(.new_samp, by = "id") -> .redux_samp
  
  .out = list(data = .new_samp, nosamp = .redux_samp)

}