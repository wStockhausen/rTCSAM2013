#'
#'@title Function to run TCSAM2013 multiple times using jittered initial parameter values.
#'
#'@description This functions runs a TCSAM2013 model multiple times, jittering the
#'initial staarting values to assess model convergence.
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
#'@param os   - 'win' or 'mac' or 'osx'
#'@param path - path for model output
#'@param model      - TCSAM2013 model executable name
#'@param path2model - path to model executable
#'@param configFile - path to model configuration file
#'@param numRuns    - number of jitter runs to make
#'@param in.csv - filename for jitter info (seed, obj fun value) from ADMB model run
#'@param out.csv - filename for jittered results
#'
#'@return - list w/ 4 elements:
#'  imx  - index of (1st) smallest value for the objective function
#'  seed - seed resulting in the smallest objective function
#'  par  - dataframe with par results from run w/ smallest objective function
#'  objFuns - vector of objective function values from all model runs
#'  parList - list of par dataframes for each model run
#'
#'@export
#'
runJitter<-function(os='osx',
                    path=ifelse(tolower(os)=='win','.\\','./'),
                    model='tcsam2013alta',
                    path2model='',
                    configFile='',
                    numRuns=3,
                    in.csv='jitterInfo.csv',
                    out.csv='jitterResults.csv'){
    #start timing
    stm<-Sys.time();
    
    #set up output
    out.csv<-file.path(path,out.csv)
    
    #run models
    objFuns<-vector(mode='numeric',length=numRuns);
    parList<-list();
    for (r in 1:numRuns){
        cat("\n\n---running ADMB program for",r,"out of",numRuns,"---\n\n");
        fldr<-paste('run',formatZeros(r,width=max(2,ceiling(log10(numRuns)))),sep='');
        p2f<-file.path(path,fldr);
        par<-runTCSAM2013(path=p2f,
                          os=os,
                          model=model,
                          path2model=path2model,
                          configFile=configFile,
                          pin=FALSE,
                          hess=FALSE,
                          mcmc=FALSE,
                          jitter=TRUE,
                          seed=NULL);
        objFuns[r]<-par$value[3];
        parList[[fldr]]<-par;
        tbl<-data.frame(idx=r,objFun=objFuns[r],seed=par$value[par$name=='seed']);
        if (r==1) write.table(tbl,file=out.csv,sep=",",col.names=TRUE,row.names=FALSE,append=FALSE)
        if (r>1)  write.table(tbl,file=out.csv,sep=",",col.names=FALSE,row.names=FALSE,append=TRUE)
    }
    
#    return(list(objFuns=objFuns,parList=parList))
    
    #determine row index associated w/ minimum obj fun value
    idx<-which.min(objFuns);
    par<-parList[[idx]];
    seed<-par$value[par$name=='seed'];
    
    #re-run case associated with mininum objective function value
    cat("\n\n---Re-running ADMB program for",idx,"out of",numRuns,"as best run---\n\n");
    par<-runTCSAM2013(path=path,
                      os=os,
                      model=model,
                      path2model=path2model,
                      configFile=configFile,
                      pin=FALSE,
                      hess=TRUE,
                      mcmc=FALSE,
                      jitter=TRUE,
                      seed=seed);
    
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
    return(list(imx=idx,seed=seed,par=par,objFuns=objFuns,parList=parList));
}
#res<-jitterTCSAM2013(200);
