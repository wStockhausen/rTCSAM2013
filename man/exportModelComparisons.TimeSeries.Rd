% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportModelComparisons.TimeSeries.R
\name{exportModelComparisons.TimeSeries}
\alias{exportModelComparisons.TimeSeries}
\title{Export time series from a set of model runs to csv for comparison.}
\usage{
exportModelComparisons.TimeSeries(obsyrs = NULL, obs = NULL, obscv = NULL,
  prdyrs = NULL, vartype = NULL, objs = NULL, scaleBy = 1)
}
\arguments{
\item{obsyrs}{- observed years to export}

\item{obs}{- observations}

\item{obscv}{- cv's associated with observations}

\item{prdyrs}{- years covering predictions}

\item{vartype}{- variable type to plot}

\item{objs}{- list of results objects to extract results from}

\item{scaleBy}{- scale predictions by this}
}
\description{
Function to export time series from a set of model runs to csv for comparison.
}
\details{
None.
}
