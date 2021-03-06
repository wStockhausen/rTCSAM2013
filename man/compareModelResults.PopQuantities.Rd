% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compareModelResults.PopQuantities.R
\name{compareModelResults.PopQuantities}
\alias{compareModelResults.PopQuantities}
\title{Compare population time series among several model runs}
\usage{
compareModelResults.PopQuantities(obj, numRecent = 15, showPlot = FALSE,
  pdf = NULL, figno = 1)
}
\arguments{
\item{obj}{- object that can be converted into a list of tcsam2013.resLst objects}

\item{numRecent}{- number of recent years to zoom in on}

\item{showPlot}{- flag (T/F) to show plot immediately}

\item{pdf}{- name for output pdf file}

\item{figno}{- initial figure number for plot captions}
}
\value{
(nested) list of ggplot2 objects, returned invisibly.
}
\description{
Function to compare population time series among 
several model runs.
}
\details{
Plots include mature (mating time) biomass, recruitment, annual abundance by x, xm, and xms, 
initial size distribution, final size distribution, annual NatZ by x.
}
