#'
#'@title Function to run TCSAM2013 multiple times using jittered initial parameter values
#'
#'@description This function runs a TCSAM2013 model multiple times, jittering the
#'initial starting values to assess model convergence.
#'
#'@details
#'For each model run, this function creates a shell script ('./tmp.sh') in the
#'working directory and uses it to run the ADMB version of the TCSAM2013 model.
#'Initial model parameters are jittered based on the system clock time as a seed
#'to the random number generator. The seed and final objective function value are
#'saved for each model run in a csv file (the value of out.csv).
#'
#'When all the models requested have been run,
#'the function determines the seed associated with the 1st model run that yielded
#'the smallest value for the objective function and re-runs the modelusing this seed
#'to re-create the model run resulting in the minimum objectve function to recreate
#'the model output files. The final model run is done estimating the hessian, so
#'standard deviations for estimated model parameters are available in the .std file.
#'
#'Uses \code{wtsUtilities::formatZeros()}.
#'
#'@param os   - 'win' or 'mac' or 'osx'
#'@param path - path for model output
#'@param model      - TCSAM2013 model executable name
#'@param path2model - path to model executable
#'@param configFile - path to model configuration file
#'@param numRuns    - number of jitter runs to make
#'@param minPhase - min phase to start estimation
#'@param maxPhase - max phase for estimation
#'@param onlyEvalJitter - flag (T/F) to only evaluate a (previous) set of jitter runs, not make new runs
#'@param in.csv - filename for jitter info (seed, obj fun value) from ADMB model run
#'@param out.csv - filename for jittered results
#'@param plotResults - T/F to plot final results using \code{plotTCSAM2013I}
#'@param cleanup - flag (T/F) to clean up unnecessary files
#'
#'@return - list w/ 4 elements:
#'  imn  - index of (1st) smallest value for the objective function
#'  seed - seed resulting in the smallest objective function
#'  par  - dataframe with par results from run w/ smallest objective function
#'  objFuns - table of objective function values, max gradients, and seed values from all model runs
#'  parList - list of par dataframes for each model run
#'
#'@export
#'
runJitter<-function(os='osx',
                    path='.',
                    model='tcsam2013alta',
                    path2model='',
                    configFile='',
                    minPhase=NULL,
                    maxPhase=NULL,
                    numRuns=3,
                    onlyEvalJitter=FALSE,
                    in.csv='jitterInfo.csv',
                    out.csv='jitterResults.csv',
                    plotResults=FALSE,
                    cleanup=TRUE){
    #start timing
    stm<-Sys.time();

    #set up output
    out.csv<-file.path(path,out.csv)

    #run models
    if (!onlyEvalJitter){
        rc<-0;
        parList<-list();
        for (r in 1:numRuns){
            cat("\n\n---running ADMB program for",r,"out of",numRuns,"---\n\n");
            fldr<-paste('run',wtsUtilities::formatZeros(r,width=max(2,ceiling(log10(numRuns)))),sep='');
            p2f<-file.path(path,fldr);
            par<-runTCSAM2013(path=p2f,
                              os=os,
                              model=model,
                              path2model=path2model,
                              configFile=configFile,
                              minPhase=minPhase,
                              maxPhase=maxPhase,
                              pin=FALSE,
                              hess=FALSE,
                              mcmc=FALSE,
                              jitter=TRUE,
                              seed=NULL,
                              plotResults=FALSE,
                              cleanup=cleanup);
            if (!is.null(par)){
                rc<-rc+1;
                objFun  <-par$value[par$name=='objective function'];
                seed    <-par$value[par$name=='seed'];
                maxgrad <-par$value[par$name=='max gradient'];
                tbl<-data.frame(idx=r,objFun=objFun,maxGrad=maxgrad,seed=seed);
                if (file.exists(out.csv)) {
                    write.table(tbl,file=out.csv,sep=",",col.names=FALSE,row.names=FALSE,append=TRUE)
                } else {
                    #create out.csv file
                    write.table(tbl,file=out.csv,sep=",",col.names=TRUE,row.names=FALSE,append=FALSE)
                }
            }
            parList[[fldr]]<-par;
        }
    }

    #determine row index associated w/ minimum obj fun value
    #read jitter results from file
    tbl<-read.csv(out.csv);
    ##idx<-which.min(tbl$objFun);
    idx<-order(tbl$objFun,abs(tbl$maxGrad));
    seed<-tbl$seed[idx[1]];
    if (onlyEvalJitter){
        parList<-NULL;
    }

    #re-run case associated with mininum objective function value, save in "best.runxx"
    cat("\n\n---Re-running ADMB program for",idx[1],"out of",numRuns,"as best run---\n");
    fldr<-paste('best.run',wtsUtilities::formatZeros(idx[1],width=max(2,ceiling(log10(numRuns)))),sep='');
    p2f<-file.path(path,fldr);
    cat("---Output folder is '",p2f,"'\n\n",sep='');
    par<-runTCSAM2013(path=p2f,
                      os=os,
                      model=model,
                      path2model=path2model,
                      configFile=configFile,
                      minPhase=minPhase,
                      maxPhase=maxPhase,
                      pin=FALSE,
                      hess=TRUE,
                      mcmc=FALSE,
                      jitter=TRUE,
                      seed=seed,
                      plotResults=plotResults,
                      cleanup=cleanup);

    #print timing-related info
    etm<-Sys.time();
    elt<-etm-stm;
    cat("start time: ")
    print(stm);
    cat("end time: ")
    print(etm);
    cat("elapsed time: ")
    print(elt);

    #return output
    return(list(imn=idx[1],seed=seed,par=par,objFuns=tbl,parList=parList));
}
#res<-jitterTCSAM2013(200);
