#' resample length data w/replacement
#'
#' @param lfreq_un expanded length frequency data 
#'
boot_length <- function(lfreq_un) {
  
  lfreq_un %>%
    tidytable::summarise(sex_ln = base::sample(sex_ln, .N, replace = TRUE), 
                         .by = c(year, species_code, hauljoin, stratum)) %>% 
    tidytable::separate(sex_ln, c('sex', 'length'), sep = '-', convert = TRUE)   
}