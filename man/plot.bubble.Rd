\name{plot.bubble}
\alias{plot.bubble}
\title{Compare size comps using bubble plots for Pearson residuals}
\usage{
\method{plot}{bubble}(datao, datap, nlen = 22, sampsize)
}
\arguments{
  \item{datao}{- dataframe of observed size frequencies
  (year in column 1)}

  \item{datap}{- dataframe of predicted size frequencies
  (year in column 1)}

  \item{nlen}{- number of size bins}

  \item{sampsize}{- vector of sample sizes}
}
\description{
Function to compare size comps using bubble plots for
Pearson residuals
}
\details{


Assumes 5mm size bins starting at 27.5 as center of
smallest bin. Filled circles represent pred > obs, empty
circles represent pred. < obs.
}

