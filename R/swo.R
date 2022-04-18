#' primary survey workload optimization function
#'
#' @param lfreq_data length frequency data
#' @param specimen_data age-length specimen data
#' @param cpue_data abundance by length data 
#' @param strata_data strata and associated area 
#' @param yrs age filter returns years >= (default = NULL)
#' @param strata switch for analyzing by strata (default = FALSE) - not currently implemented
#' @param boot_hauls switch for resampling hauls (default = FALSE)
#' @param boot_lengths switch for resampling lengths (default = FALSE)
#' @param boot_ages switch for resampling ages (default = FALSE)
#' @param length_samples change sample sizes (default = NULL)
#' @param sex_samples change sample sizes (default = NULL)
#'
#' @return
#' @export swo
#'
#' @examples
#' swo(lfreq, specimen, cpue, strata_data, yrs = 2015, boot_hauls = TRUE,
#'     boot_lengths = TRUE, length_samples = 100)
swo <- function(lfreq_data, specimen_data, cpue_data, strata_data, yrs = NULL, 
                strata = FALSE, boot_hauls = FALSE, boot_lengths = FALSE, 
                boot_ages = FALSE, length_samples = NULL, sex_samples = NULL) {
  # globals ----
  # year switch
  if (is.null(yrs)) yrs <- 0
  
  # prep data ----
  # complete cases by length/sex/strata for all years
  lfreq_data %>%
      dplyr::filter(year >= yrs) %>% 
      dplyr::group_by(species_code) %>%
      dplyr::distinct(length, year, stratum) %>%
      tidyr::expand(length, year, stratum) -> .lngs
  
  # if no strata 
  if(is.null(strata)){
    lfreq_data %>%
      dplyr::filter(year >= yrs) %>% 
      dplyr::group_by(species_code) %>%
      dplyr::distinct(length, year) %>%
      tidyr::expand(length, year) -> .lngs
  }
  
  # first pass of filtering
  data.table::setDT(cpue_data) %>%
    tidytable::filter.(year >= yrs) %>% 
    tidytable::left_join.(strata_data) -> .cpue
  
  data.table::setDT(lfreq_data) %>%
    tidytable::filter.(year >= yrs) %>% 
    tidytable::drop_na.() -> .lfreq
  
  .lfreq %>% 
    tidytable::uncount.(frequency) -> .lfreq_un
  
  data.table::setDT(specimen_data) %>%
    tidytable::filter.(year >= yrs) %>% 
    tidytable::drop_na.() -> .agedat
  
  # randomize hauls ----  
  if(isTRUE(boot_hauls)) {
    boot_haul(.cpue) -> .hls
    
    .hls %>% 
      tidytable::left_join.(.cpue) -> .cpue
    .hls %>% 
      tidytable::left_join.(.lfreq) -> .lfreq
    .hls %>% 
      tidytable::left_join.(.lfreq_un) %>% 
      tidytable::drop_na.() -> .lfreq_un
    .hls %>% 
      tidytable::left_join.(.agedat) %>% 
      tidytable::drop_na.() -> .agedat
    
  } 
  
  
  # randomize lengths ----
  if(isTRUE(boot_lengths)) {
    boot_length(.lfreq_un) -> .lfreq_un
  }
  
  # sample lengths ----
  if(!is.null(length_samples)) {
    sample(.lfreq_un, samples = length_samples) -> .lfreq_un
  } 
  
  
  # sample sex ----  
  if(!is.null(sex_samples)) {
    sample(.lfreq_un, samples = sex_samples, type = 'sex') -> .lfreq_un
  }  
  
  # length comp ----
  lcomp(.lfreq_un) -> .lcomp
  
  # length population ----
  lpop(.lcomp, .cpue, .lngs) -> .lpop
  
  # randomize age ----
  if(isTRUE(boot_age)) {
  boot_age(.agedat) -> .agedat
  }
  
  # age population ----
  apop(.lpop, .agedat) -> .apop
  
  
  
  
  list(age = .apop, length = .lpop)
   
}