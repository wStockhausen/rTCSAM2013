\name{checkParams}
\alias{checkParams}
\title{Function to plot parameter values.}
\usage{
checkParams(obj.prs = NULL, obj.std = NULL, in.prs = NULL,
  in.std = NULL, dp = 0.01)
}
\arguments{
  \item{obj.prs}{- dataframe from reading in.prs}

  \item{obj.std}{- object (dataframe) obtained from reading
  the TCSAM2013 .std file}

  \item{in.prs}{- input csv file from which to read
  parameters information}

  \item{in.std}{- input TCSAM2013 .std filename}

  \item{dp}{- % difference between parameter value and
  upper/lower limits used to color plot}
}
\value{
- list
}
\description{
This function plots parameters values, together with their
limits (if any) and the posterior distributions implied by
their estimated standard errors.
}
\details{

}

