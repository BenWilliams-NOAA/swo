#' @export boot_haul
#' @export boot_length
#' @export boot_age
#' @export sample
#' @export lcomp
#' @export lpop
#' @export apop
boot_haul <- function(cpue_data) {
  
  cpue_data %>%
    dplyr::group_by(year, species_code) %>%
    dplyr::distinct(hauljoin) %>%
    tidytable::as_tidytable(.) %>%
    .[, hauljoin := hauljoin[sample.int(.N, .N, replace = TRUE)],
      by = c('year', 'species_code')]
}

boot_length <- function(lfreq_un) {
  
  lfreq_un %>%
    tidytable::mutate.(sex_ln = paste0(sex, "-", length)) %>%
    .[, sex_ln := sex_ln[sample.int(.N, .N, replace = TRUE)],
      by = c('year', 'species_code', 'hauljoin') ]  %>%
    tidytable::separate.(., sex_ln, c('sex_res', 'len_res'), sep = '-') %>%
    tidytable::mutate.(sex = as.numeric(sex_res), 
                       length = as.numeric(len_res))  %>%
    tidytable::select.(-sex_res, -len_res)
}

boot_age <- function(age_dat) {
  agedat %>%
    tidytable:: mutate.(sex_ln_ag = paste0(sex, "-", length, "-", age)) %>%
    .[, sex_ln_ag := sex_ln_ag[sample.int(.N, .N, replace = TRUE)],
      by = c('year', 'species_code', 'hauljoin') ] %>%
    tidytable::separate.(sex_ln_ag, c('sex_res', 'len_res', "age_res"), sep = '-') %>%
    tidytable::mutate.(sex = as.numeric(sex_res), 
                       length = as.numeric(len_res), 
                       age = as.numeric(age_res)) %>%
    tidytable::select.(-sex_res, -len_res, -age_res)   
}

sample <- function(lfreq_un, samples, type = 'length') {
  
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
    
    # rejoin to original unsexed
    lfreq_un %>%
      tidytable::filter.(sex == 3) %>%
      tidytable::mutate.(id = .I,
                         n = .N, .by = c(year, species_code, stratum, hauljoin)) %>%
      tidytable::bind_rows.(.new_sexed, .new_unsexed) 
    
  } else {
    lfreq_un %>%
      tidytable::filter.(sex != 3) %>%
      tidytable::mutate.(id = .I) %>%
      tidytable::mutate.(n = .N, 
                         .by = c(year, species_code, stratum, hauljoin, sex)) -> .inter
    
    # sample by sample size
    .inter %>%
      dplyr::group_by(year, species_code, stratum, hauljoin, sex) %>%
      dplyr::sample_n(if(n > samples) samples else n) -> .new_sexed
    
    # rejoin to original unsexed
    lfreq_un %>%
      tidytable::filter.(sex == 3) %>%
      tidytable::mutate.(id = .I,
                         n = .N, .by = c(year, species_code, stratum, hauljoin, sex)) %>%
      tidytable::bind_rows.(.new_sexed, .new_unsexed) 
      
  }
}

lcomp <- function(lfreq_un) {
  lfreq_un %>%
    tidytable::summarise.(frequency = .N,
                          .by = c(year, species_code, stratum, hauljoin, sex, length)) %>% 
    tidytable::mutate.(nhauls = data.table::uniqueN(hauljoin),
                       .by = c(year, species_code, stratum)) %>%
    tidytable::mutate.(tot = sum(frequency),
                       .by = c(year, species_code, stratum, hauljoin)) %>%
    tidytable::summarise.(comp = sum(frequency) / mean(tot),
                          nhauls = mean(nhauls),
                          .by = c(year, species_code, stratum, hauljoin, sex, length))
}

lpop <- function(lcomp, cpue, lngs) {
  .lcomp %>%
    tidytable::summarise.(comp = sum(comp) / mean(nhauls), 
                          .by = c(year, species_code, stratum, sex, length)) -> .unk
  
  # id hauls without lengths
  .cpue %>%
    tidytable::filter.(!is.na(catchjoin), 
                       !(hauljoin %in% .lcomp$hauljoin)) -> .no_length
  
  .cpue %>%
    tidytable::mutate.(st_num = mean(numcpue) * area,
                       tot = sum(numcpue), 
                       .by = c(year, species_code, stratum)) %>%
    tidytable::summarise.(abund = mean(numcpue) / tot * st_num,
                          .by = c(year, species_code, stratum, hauljoin)) -> .pop
  
  # if there are any samples w/o lengths rejoin them
  if(nrow(.no_length) == 0){
    .lcomp %>%
      tidytable::left_join.(.pop) %>%
      tidytable::mutate.(sz_pop = round(comp * abund, 0)) -> .temp
  } else {
    .no_length %>%
      tidytable::left_join.(.unk) %>%
      tidytable::select.(year, species_code, stratum, hauljoin, sex, length, comp) %>%
      tidytable::bind_rows.(.lcomp) %>%
      tidytable::left_join.(.pop) %>%
      tidytable::mutate.(sz_pop = round(comp * abund, 0)) -> .temp
  }
  
  .temp %>%
    dplyr::group_by(year, species_code) %>%
    dplyr::count(sex) %>%
    tidytable::filter.(sex == 3) %>%
    tidytable::select.(year, species_code, n) -> .sex_cnt_sz
  
  if(length(.sex_cnt_sz$n)>0){
    .temp %>%
      tidytable::summarise.(abund = sum(sz_pop, na.rm = T), 
                            .by = c(year, species_code, length, sex)) %>%
      tidytable::pivot_wider.(names_from = sex, values_from = abund) %>%
      tidytable::left_join.(lngs, .) %>%
      tidytable::mutate.(across.(.cols = c(`1`, `2`, `3`), ~tidytable::replace_na.(.x, 0))) %>%
      tidytable::select.(year, species_code, length, males = `1`, females = `2`, unsexed = `3`) 
  } else{
    .temp %>%
      tidytable::summarise.(abund = sum(sz_pop, na.rm = T), .by = c(year, species_code, length, sex)) %>%
      tidytable::pivot_wider.(names_from = sex, values_from = abund) %>%
      tidytable::left_join.(lngs, .) %>%
      tidytable::mutate.(across.(.cols = c(`1`, `2`), ~tidytable::replace_na.(.x, 0))) %>%
      tidytable::select.(year, species_code, length, males = `1`, females = `2`) %>%
      tidytable::mutate.(unsexed = 0) 
  }
  
}

apop <- function(lpop, agedat){
  
  # reformat size pop'n data
  lpop %>%
    tidytable::pivot_longer.(cols = c(males, females, unsexed), names_to = "sex") %>%
    tidytable::mutate.(sex = replace(sex, sex == 'males', 1),
                       sex = replace(sex, sex == 'females', 2),
                       sex = replace(sex, sex == 'unsexed', 3)) %>%
    tidytable::mutate.(sex = as.numeric(sex)) %>%
    tidytable::rename.(sizepop = value) -> .lpop_long
  
  # compute resampled age pop'n for females & males
  agedat %>%
    tidytable::summarise.(age_num = length(age),
                          .by = c(year, species_code, sex, length, age))  %>%
    tidytable::mutate.(age_frac = age_num/sum(age_num), 
                       .by = c(year, species_code, sex, length)) %>%
    tidytable::left_join.(.lpop_long) %>%
    tidytable::mutate.(agepop = age_frac * sizepop, 
                       .by = c(year, species_code, sex, length)) %>%
    tidytable::mutate.(agepop = sum(agepop), 
                       .by = c(year, species_code, sex, age)) %>%
    tidytable::select.(year, species_code, sex, age, agepop) %>%
    dplyr::group_by(year, species_code, sex, age, agepop) %>%
    dplyr::distinct(age) %>%
    tidytable::filter.(sex <= 2) -> .agepop_mf
  
  # compute resampled age pop'n for unsexed (og rule is if you have a year with unsexed specimen data you use all the specimen data)
  agedat %>%
    dplyr::group_by(year, species_code) %>%
    dplyr::count(sex) %>%
    tidytable::filter.(sex == 3) %>%
    tidytable::select.(year, species_code, n) -> .sex_cnt_ag
  
  if(length(.sex_cnt_ag$n)>0){
    .lpop_long %>%
      tidytable::filter.(sex == 3) -> .lpop_long_un
    
    agedat %>%
      tidytable::left_join.(.sex_cnt_ag) %>%
      tidytable::filter.(n > 0) %>%
      tidytable::summarise.(age_num = length(age),
                            .by = c(year, species_code, length, age)) %>%
      tidytable::mutate.(age_frac = age_num/sum(age_num), 
                         .by = c(year, species_code, length)) %>%
      tidytable::left_join.(.lpop_long_un) %>%
      tidytable::mutate.(agepop = age_frac * sizepop, 
                         .by = c(year, species_code, length)) %>%
      tidytable::mutate.(agepop = sum(agepop), 
                         .by = c(year, species_code, age)) %>%
      dplyr::group_by(year, species_code, sex, age, agepop) %>%
      dplyr::distinct(age) %>%
      tidytable::bind_rows.(.agepop_mf) %>%
      tidytable::pivot_wider.(names_from = sex, values_from = agepop, values_fill = 0) %>%
      tidytable::rename.(unsexed = '3', males = '1', females = '2') 
  } else{
    .agepop_mf %>%
      tidytable::pivot_wider.(names_from = sex, values_from = agepop, values_fill = 0) %>%
      tidytable::rename.(males = '1', females = '2') %>%
      tidytable::mutate.(unsexed = 0) 
  }
}
