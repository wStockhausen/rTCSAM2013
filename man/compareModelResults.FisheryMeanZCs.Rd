% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compareModelResults.FisheryMeanZCs.R
\name{compareModelResults.FisheryMeanZCs}
\alias{compareModelResults.FisheryMeanZCs}
\title{Compare estimated/predicted fishery mean size comps and effective N's among several model runs}
\usage{
compareModelResults.FisheryMeanZCs(obj, plot1stObs = TRUE, showPlot = FALSE,
  pdf = NULL)
}
\arguments{
\item{obj}{- object that can be converted into a list of tcsam2013.resLst objects}

\item{plot1stObs}{- flag to plot observations from the first case, only}

\item{showPlot}{- flag (T/F) to show plot}

\item{pdf}{- name for output pdf file}
}
\value{
list of ggplot2 objects
}
\description{
Function to compare estimated/predicted fishery mean size comps and effective N's among 
several model runs.
}
\details{
Uses \code{getMDFR.FisheryQuantities}.
}
