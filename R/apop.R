#' population at age
#'
#' @param lpop length population data
#' @param agedat age dataframe
#'
#' @return
#' @export
#'
#' @examples
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
    tidytable::drop_na.() %>% 
    dplyr::group_by(year, species_code, sex, length, age) %>%
    dplyr::summarise(age_num = length(age))
    tidytable::mutate.(age_frac = age_num/sum(age_num), 
                       .by = c(year, species_code, sex, length)) %>%
    tidytable::left_join.(.lpop_long) %>%
    tidytable::drop_na.() %>% 
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
    tidytable::drop_na.() %>% 
    dplyr::group_by(year, species_code) %>%
    dplyr::count(sex) %>%
    tidytable::filter.(sex == 3) %>%
    tidytable::select.(year, species_code, n) -> .sex_cnt_ag
  
  if(length(.sex_cnt_ag$n)>0){
    .lpop_long %>%
      tidytable::filter.(sex == 3) -> .lpop_long_un
    
    agedat %>%
      tidytable::left_join.(.sex_cnt_ag) %>%
      tidytable::drop_na.() %>% 
      tidytable::filter.(n > 0) %>%
      dplyr::group_by(year, species_code, length, age) %>%
      dplyr::summarise(age_num = length(age)) %>% 
      tidytable::mutate.(age_frac = age_num/sum(age_num), 
                         .by = c(year, species_code, length)) %>%
      tidytable::left_join.(.lpop_long_un) %>%
      tidytable::drop_na.() %>% 
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