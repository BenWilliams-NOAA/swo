#' reduce total number of lengths or ages
#'
#' @param data lfreq or afreq dataframe
#' @param samples number of lengths to take
#' @param grp grouping variable(s)
#'
#' @return
#' @export reduce_samples
#'
#'
#' @examples
reduce_samples <- function(data, samples, grp = c('year', 'species_code', 'stratum', 'hauljoin')){
  core_samp(data, samples, grp = grp, replace = FALSE)
}