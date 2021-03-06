% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getRunCommands.R
\name{getRunCommands}
\alias{getRunCommands}
\title{Generate run commands for a tcsam2013 model run}
\usage{
getRunCommands(os = "osx", model = "tcsam2013alta", path2model = "",
  configFile = "", pin = FALSE, hess = FALSE, mcmc = FALSE,
  jitter = FALSE, seed = NULL, minPhase = NULL, maxPhase = NULL,
  rmEXE = FALSE, cleanup = TRUE)
}
\arguments{
\item{os}{- 'win', 'mac' or 'osx'}

\item{model}{- admb model name}

\item{path2model}{- path to model}

\item{configFile}{- filename (including path) to model configuration file}

\item{pin}{- flag (T/F) to use/not use a default pin file, or an ascii filename to use a non-default one}

\item{hess}{- flag (T/F) to calculate the hessian}

\item{mcmc}{- flag (T/F) to do mcmc calculations}

\item{jitter}{- flag (T/F) to use jitter initial values}

\item{seed}{- value for random number seed to generate jitter}

\item{minPhase}{- min phase to start estimation}

\item{maxPhase}{- max phase for estimation}

\item{rmEXE}{- flag to remove model executable from run folder}

\item{cleanup}{- flag (T/F) to clean up unnecessary files}
}
\description{
Function to generate a script to make a tcsam2013 model run
}
\details{
. If \code{cleanup} is TRUE, then the executable, .bar, .b0*, .p0*, .r0*, variance, 
EchoOut.dat, CheckFile.dat, and fimn.log files are deleted. If not, then only the executable
is deleted.\cr
If the path associated with \code{configFile} is a relative one, it should
be relative to the \code{path} variable.
}
