\name{plotTCSAM2013}
\alias{plotTCSAM2013}
\title{Plot TCSAM2013 model results.}
\usage{
plotTCSAM2013(obj.rep = NULL, obj.std = NULL, obj.prs = NULL,
  in.rep = NULL, in.std = NULL, in.prs = NULL, base.dir = "./",
  mdl = "TCSAM2013", styr = 1949, endyr = 2013, obsyr = 1974,
  pltyr = 1969, F35 = 0.73, B35 = 33.54)
}
\arguments{
  \item{obj.rep}{- report file list object}

  \item{obj.std}{- dataframe object with parameter std
  info}

  \item{obj.prs}{- dataframe object w/ parameters info}

  \item{in.rep}{- R-style report filename}

  \item{in.std}{- std filename}

  \item{in.prs}{- parameters info filename (csv)}

  \item{base.dir}{- path to base folder}

  \item{mdl}{- model name (optional if R report file is
  read)}

  \item{styr}{- model start year}

  \item{endyr}{- assessment year}

  \item{obsyr}{- first year for survey observations}

  \item{pltyr}{- first year for some plots}

  \item{F35}{- F35 value for control rule plot}

  \item{B35}{- B35 value for control rule plot}
}
\value{
list with elements: \cr rep - obj.rep std - obj.std prs -
obj.prs
}
\description{
Function to plot model results from TCSAM2013.
}
\details{

}

