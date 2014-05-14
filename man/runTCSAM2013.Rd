\name{runTCSAM2013}
\alias{runTCSAM2013}
\title{Function to run TCSAM2013 multiple times using jittered initial parameter values.}
\usage{
runTCSAM2013(N = 5, model = "tcsam_wtsRKFon",
  model.dir = "/Users/WilliamStockhausen/StockAssessments-Crab/AssessmentModelDevelopment/TCSAM_WTS/dist/Debug/GNU-MacOSX",
  mc = "../TCSAM2013e_ModelConfig.txt", in.csv = "jitterInfo.csv",
  out.csv = "jitterResults.csv")
}
\arguments{
  \item{N}{- number of runs to make}

  \item{model}{- filename of model executable to run}

  \item{model.dir}{- path to model executable}

  \item{mc}{- path/filename for model configuration file}

  \item{in.csv}{- filename for jitter info (seed, obj fun
  value)}

  \item{out.csv}{- filename for jittered results}
}
\value{
- list w/ 2 elements: imx - index of (1st) smallest value
for the objective function dfr - dataframe with iSeed, obj
fun value for each model run
}
\description{
This functions runs a TCSAM2013 model multiple times,
jittering the initial staarting values to assess model
convergence.
}
\details{


For each model run, this function creates a shell script
('./tmp.sh') in the working directory and uses it to run
the ADMB version of the TCSAM2013 model. Initial model
parameters are jittered based on the system clock time as a
seed to the random number generator. The seed and final
objective function value are saved for each model run in a
csv file (the value of out.csv).

When all the models requested have been run, the function
determines the seed associated with the 1st model run that
yielded the smallest value for the objective function and
re-runs the modelusing this seed to re-create the model run
resulting in the minimum objectve function to recreate the
model output files. The final model run is done estimating
the hessian, so standard deviations for estimated model
parameters are available in the .std file.
}

