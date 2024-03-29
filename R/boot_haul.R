#' resample hauls w/replacement
#'
#' @param cpue_data cpue dataframe
#'
boot_haul <- function(cpue_data) {
  
  cpue_data %>% 
    tidytable::select(year, species_code, hauljoin) %>% 
    tidytable::distinct() %>% 
    tidytable::mutate(hauljoin = base::sample(hauljoin, .N, replace = TRUE),
                      .by = c(year, species_code))
}
