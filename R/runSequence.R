#'
#'@title Function to run a sequence of TCSAM2013 models.
#'
#'@description This functions runs a sequence of TCSAM2013 model.
#'
#'@details
#'This function creates a shell script ('./tmp.sh') in the
#'working directory and uses it to run a version of the TCSAM2013 model. Pin files
#'are copied from the previous run's par file. The file 'best.txt' identifies the run
#'with the best objective function value. The "best" sub-folder contains results from
#'re-running the best run, this time estimating the hessian and obtaining the std file
#'(if the hessian is invertible).
#'
#'@param os   - 'win' or 'mac' or 'osx'
#'@param path - path for model output
#'@param model      - TCSAM2013 model executable name
#'@param path2model - path to model executable
#'@param configFile - full (absolute) path to model configuration file
#'@param numRuns    - number of runs in sequence to make
#'@param plotResults - T/F to plot final results using \code{plotTCSAM2013I}
#'
#'@return - list indicatng index of best run, the folder with the best run, and a list of results 
#'from the parameter files for each model run.
#'
#'@importFrom wtsUtilities formatZeros
#'
#'@export
#'
runSequence<-function(os='osx',
                      path='.',
                      model='tcsam2013alta',
                      path2model='',
                      configFile='',
                      numRuns=4,
                      plotResults=FALSE){
    #run sequence
    objFuns<-vector(mode='numeric',length=numRuns);
    parList<-list();
    for (r in 1:numRuns){
        pin<-ifelse(r==1,FALSE,TRUE);
        if (pin){par<-readLines(file.path(p2f,paste(model,'.par',sep='')));}
        fldr<-paste('run',formatZeros(r,width=max(2,ceiling(log10(numRuns)))),sep='');
        p2f<-file.path(path,fldr);
        if (!file.exists(p2f)) dir.create(p2f,recursive=TRUE);
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
                          seed=NULL,
                          plotResults=FALSE);
        if (!is.null(par)){
            objFuns[r]<-par[2,'value'];
        }
        parList[[fldr]]<-par;
    }
    
    #find best model
    cat("---objFuns = ",objFuns,"\n")
    idx<-which.min(objFuns);
    bst<-names(parList)[idx];#best folder
    cat("best = ",bst,file=file.path(path,"best.txt"))
    
    #re-run best model
    p2fb<-file.path(path,bst);                          #folder w/ best run
    pinFile<-file.path(p2fb,paste(model,'.pin',sep=''));#pin file used to achieve run
    p2f<-file.path(path,"best");                           #"best" folder to re-run model in
    if (!file.exists(p2f)) dir.create(p2f,recursive=TRUE); #ceate "best" folder, if necessary
    if (file.exists(pinFile)){
        #pin file exists in folder w/ best run, so copy it to "best" folder to use
        pin<-readLines(pinFile);                                  #read pin file from best
        writeLines(pin,file.path(p2f,paste(model,'.pin',sep='')));#write pin file to "best"
        pin<-TRUE;
    } else {
        #pin file doesn't exist (best run was first one)
        pin<-FALSE;
    }
    par<-runTCSAM2013(path=p2f,
                      os=os,
                      model=model,
                      path2model=path2model,
                      configFile=configFile,
                      pin=pin,
                      hess=TRUE,
                      mcmc=FALSE,
                      jitter=FALSE,
                      seed=NULL,
                      plotResults=plotResults);
    return(list(idx=idx,best=bst,parList=parList));
}