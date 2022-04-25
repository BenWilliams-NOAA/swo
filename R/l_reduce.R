#' reduce total number of lengths
#'
#' @param data lfreq dataframe
#' @param samples number of lengths to take
#' @param write_sample whether to output intermediate step
#' @param region region being sampled
#' @param save name to save
#'
#' @return
#' @export l_reduce
#'
#'
#' @examples
l_reduce <- function(data, samples, grp){
  core_samp(.lfreq_un, samples, c('year', 'species_code', 'stratum', 'hauljoin'), replace = FALSE)
}