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
#' @param age_samples change sample sizes (default = NULL)
#' @param sexlen_samples change sample sizes (default = NULL)
#'
#' @return
#' @export swo
#'
#' @examples
swo <- function(lfreq_data, specimen_data, cpue_data, strata_data, yrs, 
                strata, boot_hauls, boot_lengths, boot_ages, 
                length_samples, age_samples, sexlen_samples) {
  # globals ----
  # year switch
  if (is.null(yrs)) yrs <- 0
  
  # prep data ----
  # complete cases by length/sex/strata for all years
  if(isTRUE(strata)){
    lfreq_data %>%
      tidytable::filter.(year >= yrs) %>% 
      tibble::as_tibble() %>% 
      dplyr::group_by(species_code) %>%
      dplyr::distinct(length, year, stratum) %>%
      tidyr::expand(length, year, stratum) -> .lngs
  } else {
    lfreq_data %>%
      tidytable::filter.(year >= yrs) %>% 
      tibble::as_tibble() %>% 
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
    boot_haul(.cpue) %>% 
      tidytable::mutate.(hauljoin_unq = .I) -> .hls
    
    .hls %>% 
      tidytable::left_join.(.cpue) %>% 
      tidytable::rename(hauljoin_orig = 'hauljoin',
                        hauljoin = 'hauljoin_unq') -> .cpue
    .hls %>% 
      tidytable::left_join.(.lfreq) %>% 
      tidytable::rename(hauljoin_orig = 'hauljoin',
                        hauljoin = 'hauljoin_unq') -> .lfreq
    .hls %>% 
      tidytable::left_join.(.lfreq_un) %>% 
      tidytable::drop_na.() %>% 
      tidytable::rename(hauljoin_orig = 'hauljoin',
                        hauljoin = 'hauljoin_unq') -> .lfreq_un
    .hls %>% 
      tidytable::left_join.(.agedat) %>% 
      tidytable::drop_na.() %>% 
      tidytable::rename(hauljoin_orig = 'hauljoin',
                        hauljoin = 'hauljoin_unq') -> .agedat
    
  } 
  
  # randomize lengths ----
  if(isTRUE(boot_lengths)) {
    boot_length(.lfreq_un) -> .lfreq_un
  }
  
  # reduce sex-specific length freq sample size (and move unselected samples to 'unsexed' category)
  if(!is.null(sexlen_samples)) {
    sample(.lfreq_un, samples = sexlen_samples) -> .out
    .lfreq_un_sub <- .out$data
  }
  
  # reduce overall length freq sample sizes
  if(!is.null(length_samples)) {
    reduce_samples(.lfreq_un, samples = length_samples, type = 'length') -> .out
    .lfreq_un_sub <- .out$data
  }
  
  # length comp ----
  if(!is.null(length_samples) | !is.null(sexlen_samples)) {
    lcomp(.lfreq_un) -> .lcomp
    lcomp(.lfreq_un_sub) -> .lcomp_sub
  } else{
    lcomp(.lfreq_un) -> .lcomp
  }
  
  # length population ----
  if(!is.null(length_samples) | !is.null(sexlen_samples)) {
    lpop(.lcomp, .cpue, .lngs) -> .lpop
    lpop(.lcomp_sub, .cpue, .lngs) -> .lpop_sub
  }else{
    lpop(.lcomp, .cpue, .lngs) -> .lpop
  }
    
  # randomize age ----
  if(isTRUE(boot_ages)) {
    boot_age(.agedat) -> .agedat
  }
  
  # reduce overall age sample sizes
  if(!is.null(age_samples)) {
    reduce_samples(.agedat, samples = age_samples, type = 'age') -> .out
    .agedat_sub <- .out$data
  }
  
  # age population ----
  
  # age pop'n with length subsampling
  if(!is.null(length_samples) | !is.null(sexlen_samples)) {
    apop(.lpop, .agedat, strata = strata) -> .apop
    apop(.lpop_sub, .agedat, strata = strata) -> .apop_sub
  }
  
  # age pop'n with age subsampling
  if(!is.null(age_samples)) {
    apop(.lpop, .agedat, strata = strata) -> .apop
    apop(.lpop, .agedat_sub, strata = strata) -> .apop_sub
  }
  
  # age pop'n with no length or age subsampling
  if(is.null(length_samples) & is.null(sexlen_samples) & is.null(age_samples)){
    apop(.lpop, .agedat, strata = strata) -> .apop
  }
  
  # list results ----
  
  # with length subsampling
  if(!is.null(length_samples) | !is.null(sexlen_samples)) { # with length subsampling
    list(age = .apop, age_sub = .apop_sub, length = .lpop, length_sub = .lpop_sub, nosamp = .out$nosamp)
  } else if(!is.null(age_samples)) { # with age subsampling
    list(age = .apop, age_sub = .apop_sub, length = .lpop, nosamp = .out$nosamp)
  } else if(is.null(length_samples) & is.null(sexlen_samples) & is.null(age_samples)){ # with no subsamplinng
    list(age = .apop, length = .lpop)
  }
  
}