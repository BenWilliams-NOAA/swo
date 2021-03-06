% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/swo_sim_goa_rebs.R
\name{swo_sim_goa_rebs}
\alias{swo_sim_goa_rebs}
\title{replicate swo function for goa rebs stock complex}
\usage{
swo_sim_goa_rebs(
  iters = 1,
  lfreq_data,
  specimen_data,
  cpue_data,
  strata_data,
  yrs = NULL,
  strata = FALSE,
  boot_hauls = FALSE,
  boot_lengths = FALSE,
  boot_ages = FALSE,
  reduce_lengths = NULL,
  length_samples = NULL,
  sex_samples = NULL,
  save = NULL,
  write_comp = FALSE,
  write_sample = FALSE,
  region = NULL,
  save_orig = FALSE
)
}
\arguments{
\item{iters}{number of iterations (500 recommended)}

\item{lfreq_data}{input dataframe}

\item{specimen_data}{input dataframe}

\item{cpue_data}{input dataframe}

\item{strata_data}{input dataframe}

\item{yrs}{any year filter >= (default = NULL)}

\item{strata}{switch for regional or by strata (default = FALSE)}

\item{boot_hauls}{resample hauls w/replacement (default = FALSE)}

\item{boot_lengths}{resample lengths w/replacement (default = FALSE)}

\item{boot_ages}{resample ages w/replacement (default = FALSE)}

\item{reduce_lengths}{reduce the total number of lengths used in the analysis (default = NULL)}

\item{length_samples}{sample size by length (default = NULL)}

\item{sex_samples}{sample size by sex (default = NULL)}

\item{save}{name to save a file}

\item{write_comp}{save the intermediate age/length comps}

\item{write_sample}{save the new "unsexed" default = FALSE}

\item{region}{region will create a folder and place results in said folder}

\item{save_orig}{save the original}
}
\description{
replicate swo function for goa rebs stock complex
}
