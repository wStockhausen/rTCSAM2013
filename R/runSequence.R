#'
#'@title Function to run a sequence of TCSAM2013 models.
#'
#'@description This functions runs a sequence of TCSAM2013 model.
#'
#'@details
#'This function creates a shell script ('./tmp.sh') in the
#'working directory and uses it to run a version of the TCSAM2013 model. Pin files
#'are copied from the previous run's par file. The file 'best.txt' identifies the run
#'with the best objective function value.
#'
#'@param os   - 'win' or 'mac' or 'osx'
#'@param path - path for model output
#'@param model      - TCSAM2013 model executable name
#'@param path2model - path to model executable
#'@param configFile - path to model configuration file
#'@param numRuns    - number of runs in sequence to make
#'
#'@return - par file dataframe
#'
#'@importFrom wtsUtilities formatZeros
#'
#'@export
#'
runSequence<-function(os='osx',
                      path=ifelse(tolower(os)=='win','.\\','./'),
                      model='tcsam2013alta',
                      path2model='',
                      configFile='',
                      numRuns=4){
    #run sequence
    objFuns<-vector(mode='numeric',length=numRuns);
    parList<-list();
    for (r in 1:numRuns){
        pin<-ifelse(r==1,FALSE,TRUE);
        if (pin){par<-readLines(file.path(p2f,paste(model,'.par',sep='')));}
        fldr<-paste('run',formatZeros(r,width=max(2,ceiling(log10(numRuns)))),sep='');
        p2f<-file.path(path,fldr);
        if (!dir.exists(p2f)) dir.create(p2f,recursive=TRUE);
        if (pin) {writeLines(par,file.path(p2f,paste(model,'.pin',sep='')));}
        par<-runTCSAM2013(path=p2f,
                          os=os,
                          model=model,
                          path2model=path2model,
                          configFile=configFile,
                          pin=pin,
                          hess=FALSE,
                          mcmc=FALSE,
                          jitter=FALSE,
                          seed=NULL);
        objFuns[r]<-par[2,'value'];
        parList[[fldr]]<-par;
    }
    
    #find best model
    cat("---objFuns = ",objFuns,"\n")
    idx<-which.min(objFuns);
    bst<-names(parList)[idx];#best folder
    cat("best = ",bst,file=file.path(path,"best.txt"))
    
    #re-run best model
    p2f<-file.path(path,bst);
    par<-runTCSAM2013(path=p2f,
                      os=os,
                      model=model,
                      path2model=path2model,
                      configFile=configFile,
                      pin=TRUE,
                      hess=TRUE,
                      mcmc=FALSE,
                      jitter=FALSE,
                      seed=NULL);
    return(list(idx=idx,best=bst,parList=parList));
}