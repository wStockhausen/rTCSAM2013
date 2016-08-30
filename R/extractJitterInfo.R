#'
#'@title Function to extract results from a set of previous jittered TCSAM2013 runs
#'
#'@description This function extracts results from a set of previous jittered TCSAM2013 runs
#'and re-runs the best run to create sd info.
#'
#'@details
#'For each model run, this function reads a set of jittered model runs and 
#'determines the seed associated with the 1st model run that yielded
#'the smallest value for the objective function and max gradient. It re-runs the model using this seed
#'to re-create the model run resulting in the minimum objectve function to recreate
#'the model output files. The best model run is done estimating the hessian, so
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
extractJitterInfo<-function(os='osx',
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
    cat("out.csv = '",out.csv,"'\n",sep='');

    parList<-list();
    for (r in 1:numRuns){
        cat("\n\n---extracting jitter run for",r,"out of",numRuns,"\n");
        fldr<-paste('run',wtsUtilities::formatZeros(r,width=max(2,ceiling(log10(numRuns)))),sep='');
        p2f<-file.path(path,fldr);               #path to folder with model run
        p2p<-file.path(p2f,paste0(model,".par"));#path to par file
        cat("par file = '",p2p,"'\n",sep="")
        dfr<-getPar(p2p);
        if (!is.null(dfr)){
            cat("--extracting jitter info\n");
            p2j<-file.path(p2f,'jitterInfo.csv');
            cat("--reading from '",p2j,"'\n",sep='')
            #get jitter info
            if (file.exists(p2j)){
                tbl<-read.csv(p2j,header=TRUE);
                dfr<-rbind(data.frame(name='seed',value=tbl$seed[1]),dfr);
            }
            objFun  <-dfr$value[dfr$name=='objective function'];
            seed    <-dfr$value[dfr$name=='seed'];
            maxgrad <-dfr$value[dfr$name=='max gradient'];
            tbl<-data.frame(idx=r,objFun=objFun,maxGrad=maxgrad,seed=seed);
            print(tbl);
            if (file.exists(out.csv)) {
                cat("--appending to out.csv\n")
                write.table(tbl,file=out.csv,sep=",",col.names=FALSE,row.names=FALSE,append=TRUE)
            } else {
                #create out.csv file
                cat("--creating out.csv\n")
                write.table(tbl,file=out.csv,sep=",",col.names=TRUE,row.names=FALSE,append=FALSE)
            }
        }
        parList[[fldr]]<-par;
    }

    #determine row index associated w/ minimum obj fun value
    #read jitter results from file
    tbl<-read.csv(out.csv);
    ##idx<-which.min(tbl$objFun);
    idx<-order(tbl$objFun,abs(tbl$maxGrad));
    seed<-tbl$seed[idx[1]];

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

    #return output
    return(list(imn=idx[1],seed=seed,par=par,objFuns=tbl,parList=parList));
}