#'
#'@title Function to run TCSAM2013.
#'
#'@description This functions runs a TCSAM2013 model once.
#'
#'@details
#'This function creates a shell script ('./tmp.sh') in the
#'working directory and uses it to run a version of the TCSAM2013 model.
#'Initial model parameters can be jittered based on the system clock time as a seed
#'to the random number generator. The seed and final objective function value are
#'saved for each model run in a csv file (the value of out.csv).
#'
#'@param os   - 'win' or 'mac' or 'osx'
#'@param path - path for model output
#'@param model      - TCSAM2013 model executable name
#'@param path2model - path to model executable
#'@param configFile - filename (including path) to model configuration file
#'@param pin  - T/F to use a pin file
#'@param hess - T/F to compute hessian (and .std file)
#'@param mcmc - T/F to run mcmc
#'@param jitter  - T/F to jitter parameters
#'@param seed    - seed for random number generator (or NULL)
#'@param plotResults - T/F to plot results using \code{plotTCSAM2013I}
#'
#'@return - dataframe with 2 columns (name, value) with jitter seed (if jittered) and par file info
#'
#'@details If the path associated with \code{configFile} is a relative one, it should
#'be relative to the \code{path} variable.
#'
#'@export
#'
runTCSAM2013<-function(os='osx',
                       path='.',
                       model='tcsam2013alta',
                       path2model='',
                       configFile='',
                       pin=FALSE,
                       hess=FALSE,
                       mcmc=FALSE,
                       jitter=FALSE,
                       seed=NULL,
                       plotResults=hess){
    #start timing
    stm<-Sys.time();

    #switch to run folder (create if necessary)
    currdir<-getwd();
    on.exit(setwd(currdir));
    if (!file.exists(path)) dir.create(path,recursive=TRUE)
    setwd(path);
    cat("Running tcsam2013 model at '",path,"'.\n");

    #set up copy commands
    fn.par<-file.path(getwd(),"&&model.par");
    fn.par<-gsub('&&model',tolower(model),fn.par)

    run.cmds<-getRunCommands(os=os,
                             model=model,
                             path2model=path2model,
                             configFile=configFile,
                             pin=pin,
                             hess=hess,
                             mcmc=mcmc,
                             jitter=jitter,
                             seed=seed)
    if (tolower(os)=='win'){
        cat(run.cmds,file="tmp.bat")
        Sys.chmod("tmp.bat",mode='7777')
        system("tmp.bat",wait=TRUE);
    } else {
        cat(run.cmds,file="./tmp.sh")
        Sys.chmod("./tmp.sh",mode='7777')
        system("./tmp.sh",wait=TRUE);
    }

    #print timeing-related info
    etm<-Sys.time();
    elt<-etm-stm;
    cat("start time: ")
    print(stm);
    cat("end time: ")
    print(etm);
    cat("elapsed time: ")
    print(elt);

    #parse par file into dataframe
    par<-paste(model,'.par',sep='')
    dfr<-getPar(par);
    
    #get jitter info
    if (jitter) {
        tbl<-read.csv('jitterInfo.csv',header=TRUE);
        dfr<-rbind(data.frame(name='seed',value=tbl$seed[1]),dfr);
    }
    
    if (plotResults){
        obj.rep<-getRep('TCSAM_WTS.oldstyle.R');
        obj.std<-getStd(paste(model,'.std',sep=''));
        obj.prs<-getActiveParams('TCSAM_WTS.final_params.active.csv');
        plotTCSAM2013I(obj.rep,
                       obj.std,
                       obj.prs,
                       mdl=model);

    }

    #return dataframe (and return to original folder as working directory)
    return(dfr);
}
