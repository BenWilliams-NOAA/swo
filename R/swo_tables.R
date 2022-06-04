#' two basic swo tables
#'
#' @param lfreq length frequency data
#' @param spec species data
#' @param region region of interes
#'
#' @return
#' @export swo_tables
#'
#' @examples
#' \dontrun{ 
#' swo_tables(lfreq, spec, 'goa')
#' }
swo_tables <- function(lfreq, spec, region) { 
  
  # table 1
  # mean, max, n samples
  
  lfreq %>% 
    left_join.(spec) %>% 
    filter.(sex <= 2) %>% 
    summarise.(n = sum(frequency), 
               .by = c(year, species, hauljoin)) %>% 
    summarise.(mean = round(mean(n), 1) ,
               max = max(n),
               n = sum(n),
               .by = c(year, species)) %>% 
    arrange(year, species) %>% 
    vroom::vroom_write(here::here("output", region, paste0("ss_specs.csv")), delim = ",")
  
  # table 2 sexed sample sizes
  lfreq %>% 
    left_join.(spec) %>% 
    filter.(sex <= 2) %>% 
    summarise.(haul_n = sum(frequency),
               .by = c(year, species, hauljoin)) %>% 
    mutate.(s50 = ifelse(haul_n > 50, haul_n - 50, 0), 
            s75 = ifelse(haul_n > 75, haul_n - 75, 0), 
            s100 = ifelse(haul_n > 100, haul_n - 100, 0), 
            s125 = ifelse(haul_n > 125, haul_n - 125, 0), 
            s150 = ifelse(haul_n > 150, haul_n - 150, 0), 
            s175 = ifelse(haul_n > 175, haul_n - 175, 0)) %>% 
    pivot_longer.(cols = c(s50, s75, s100, s125, s150, s175), names_to = 'id') %>% 
    filter.(value >0) %>% 
    summarise.(mean = mean(value), 
               total = sum(value),
               .by = c(year, species, id)) %>% 
    mutate.(id = as.numeric(gsub('s', '', id))) %>% 
    arrange(year, species, id) %>% 
    vroom::vroom_write(here::here("output", region, paste0("ss_saved.csv")), delim = ",")
}