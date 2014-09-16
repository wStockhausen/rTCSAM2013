\name{compareModelResults.TimeSeries}
\alias{compareModelResults.TimeSeries}
\title{Compare an estimated/predicted time series among several model runs.}
\usage{
compareModelResults.TimeSeries(objs = NULL, cases = NULL, styr = 1949,
  endyr = 2014, obsyr = 1974, pltyr = 1969, recLag = 5,
  pdf = "ModelResultsComparisonPlots.pdf")
}
\arguments{
  \item{objs}{- list of objects derived from Jack's R files
  for the models to be compared}

  \item{cases}{- vector of labels for model cases (if
  'objs' is not given)}
}
\value{
vector of list objects corresponding to the objects
returned by each model R file.
}
\description{
Function to compare an estimated/predicted time series
among several model runs.
}
\details{


If 'objs' is not given, then the user is prompted to select
Jack's R file output from each model to be compared. If
'cases' is given, the user is prompted to select the file
corresponding to each case. If 'cases' is not given, then
the user may select an arbitrary number of files (one at a
time), ending selection by pressing 'cancel' on the
selection box'.\cr\cr If 'objs' is not given, the working
directory is set two levels above the 1st model case file
selected.
}

