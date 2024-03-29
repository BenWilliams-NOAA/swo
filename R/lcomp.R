#' length comp
#'
#' @param lfreq_un expanded length frequency data
#'
lcomp <- function(lfreq_un) {
  lfreq_un %>%
    tidytable::summarise(frequency = .N,
                          .by = c(year, species_code, stratum, hauljoin, sex, length)) %>% 
    tidytable::mutate(nhauls = data.table::uniqueN(hauljoin),
                       .by = c(year, species_code, stratum)) %>%
    tidytable::mutate(tot = sum(frequency),
                       .by = c(year, species_code, stratum, hauljoin)) %>%
    tidytable::summarise(comp = sum(frequency) / mean(tot),
                          nhauls = mean(nhauls),
                          .by = c(year, species_code, stratum, hauljoin, sex, length))
}
