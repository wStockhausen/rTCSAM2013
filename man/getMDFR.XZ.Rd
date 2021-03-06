% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMDFR.XZ.R
\name{getMDFR.XZ}
\alias{getMDFR.XZ}
\title{Get XZ model output from several model runs}
\usage{
getMDFR.XZ(obj, rep.m = NULL, rep.f = NULL, std.m = NULL, std.f = NULL,
  verbose = FALSE)
}
\arguments{
\item{obj}{- object with results for the models to be compared that can be converted to a list of tcsam2013.resLst objects}

\item{rep.m}{- rep object name for males}

\item{rep.f}{- rep object name for females}

\item{verbose}{- flag (T/F) to print debug info}

\item{rep.m}{- std object name for males}

\item{rep.f}{- std object name for females}
}
\value{
dataframe
}
\description{
Function to get XZ from 
several model runs.
}
\details{
Uses \code{reshape2::melt} and \code{reshape2::dcast}. If obj is a tcsam2013.lst
object, then results will be extracted from the tcsam2013.std object for a given case, if it exists; 
otherwise they will be extracted from the tcsam2013.rep object.
}
