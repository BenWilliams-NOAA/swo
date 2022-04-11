
#' Query racebase
#'
#' @param region specific region for data ('GOA', 'AI')
#' @param species species_codes e.g., c(10110, 21740)
#' @param yrs minimum year to consider (default = NULL)
#' @param afsc_user afsc database username
#' @param afsc_pwd afsc database password
#'
#' @return
#' @export query_data
#'
#' @examples
#' query_date(region = 'AI', 
#'            species = c(30060,21740,10110,30420,21921,21720,10130,30020),
#'            yrs = 2015,
#'            afsc_user = 'your_username',
#'            afsc_pwd = 'your_password')
#'            
query_data <- function(region, species, yrs = NULL, afsc_user, afsc_pwd) {
  
  # create folder
  if (!dir.exists("data")) {dir.create("data")}
  
  # connect to database
  afsc = DBI::dbConnect(odbc::odbc(), "afsc",
                      UID = afsc_user, PWD = afsc_pwd)
  
  # year switch
  if (is.null(yrs)) yrs <- 0

  # length frequency data 
  lfreq = sql_read('length_freq.sql')
  lfreq = sql_filter(x = region, sql_code = lfreq, flag = '-- insert region')
  lfreq = sql_filter(sql_precode = "IN", x = species, sql_code = lfreq, flag = '-- insert species')
  lfreq = sql_filter(sql_precode = ">=", x = yrs, sql_code = lfreq, flag = '-- insert year')
  
  sql_run(afsc, lfreq) %>% 
    dplyr::rename_all(tolower) %>% 
    vroom::vroom_write(., 
                       here::here('data', paste0("lfreq_", tolower(region), ".csv")), 
                       delim = ',')
  
  # specimen data 
  sp = sql_read('specimen.sql')
  sp = sql_filter(x = region, sql_code = sp, flag = '-- insert region')
  sp = sql_filter(sql_precode = "IN", x = species, sql_code = sp, flag = '-- insert species')
  sp = sql_filter(sql_precode = ">=", x = yrs, sql_code = sp, flag = '-- insert year')
  
  sql_run(afsc, sp) %>% 
    dplyr::rename_all(tolower) %>% 
    vroom::vroom_write(., 
                       here::here('data', paste0("specimen_", tolower(region), ".csv")), 
                       delim = ',')
  
  # cpue data 
  cp = sql_read('cpue.sql')
  cp = sql_add(paste0(region, '.cpue'), cp)
  cp = sql_filter(x = region, sql_code = cp, flag = '-- insert region')
  cp = sql_filter(sql_precode = "IN", x = species, sql_code = cp, flag = '-- insert species')
  cp = sql_filter(sql_precode = ">=", x = yrs, sql_code = cp, flag = '-- insert year')
  
  sql_run(afsc, cp) %>% 
    dplyr::rename_all(tolower) %>% 
    vroom::vroom_write(., 
                       here::here('data', paste0("cpue_", tolower(region), ".csv")), 
                       delim = ',')
  
  # strata (GOA and AI)
  st = sql_read('strata.sql')
  st = sql_filter(x = region, sql_code = cp, flag = '-- insert region')
  sql_run(afsc, st) %>% 
    dplyr::rename_all(tolower) %>% 
    vroom::vroom_write(., 
                       here::here('data', paste0("strata_", tolower(region), ".csv")), 
                       delim = ',')
  
  # race pop
  rp = sql_read('race_pop.sql')
  rp = sql_add(paste0(region, '.SIZECOMP_TOTAL'), rp)
  rp = sql_filter(sql_precode = 'IN', sql_code = rp,
                  x = species, 
                  flag = '-- insert species')
  rp = sql_filter(sql_precode = ">=", x = yrs, 
                  sql_code = rp, flag = '-- insert year')
  

  sql_run(afsc, rp) %>% 
    dplyr::rename_all(tolower) %>% 
    vroom::vroom_write(., 
                       here::here('data', paste0("race_pop_", tolower(region), ".csv")), 
                       delim = ',')
  
  # species common name 
  .s = sql_read('species.sql')
  .s = sql_filter(sql_precode = "IN", x = species, sql_code = .s, flag = '-- insert species')
  sql_run(afsc, .s) %>% 
    dplyr::rename_all(tolower) %>% 
    vroom::vroom_write(., 
                       here::here('data', paste0("species_", tolower(region), ".csv")), 
                       delim = ',')

DBI::dbDisconnect(afsc)

}


