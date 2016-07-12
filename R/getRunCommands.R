#'
#'@title Generate run commands for a tcsam2013 model run
#'
#'@description Function to generate a script to make a tcsam2013 model run
#'
#'@param os - 'win', 'mac' or 'osx'
#'@param model - admb model name
#'@param path2model - path to model
#'@param configFile - filename (including path) to model configuration file
#'@param pin - flag (T/F) to use/not use a default pin file, or an ascii filename to use a non-default one
#'@param hess - flag (T/F) to calculate the hessian
#'@param mcmc - flag (T/F) to do mcmc calculations
#'@param jitter - flag (T/F) to use jitter initial values
#'@param seed - value for random number seed to generate jitter
#'@param minPhase - min phase to start estimation
#'@param maxPhase - max phase for estimation
#'@param rmEXE - flag to remove model executable from run folder
#'@param cleanup - flag (T/F) to clean up unnecessary files
#'
#'@details. If \code{cleanup} is TRUE, then the executable, .bar, .b0*, .p0*, .r0*, variance, 
#'EchoOut.dat, CheckFile.dat, and fimn.log files are deleted. If not, then only the executable
#'is deleted.\cr
#'If the path associated with \code{configFile} is a relative one, it should
#'be relative to the \code{path} variable.
#'
#'@export
#'
getRunCommands<-function(os='osx',
                         model='tcsam2013alta',
                         path2model='',
                         configFile='',
                         pin=FALSE,
                         hess=FALSE,
                         mcmc=FALSE,
                         jitter=FALSE,
                         seed=NULL,
                         minPhase=NULL,
                         maxPhase=NULL,
                         rmEXE=FALSE,
                         cleanup=TRUE){
    nopath<-FALSE;
    if ((path2model=='.')||(path2model=='./')||(path2model=="")) nopath=TRUE;
    echo.on <-"echo on";
    echo.off<-"echo off";
    if (tolower(os)=='win'){
        model1<-paste(model,'exe',sep='.');
        cpy<-""; 
        if (!nopath) cpy<-"copy &&path2model &&model1";
        rnm<-"&&model -rs -nox  -configFile &&configFile &&mcmc &&nohess &&jitter &&seed &&pin &&minPhase &&maxPhase";
        rmm<-"";
        if (rmEXE) rmm<-"del &&model1";
        cln<-"";
        if (cleanup) {
            cln<-"del &&model.bar
                del &&model.b0*
                del &&model.p0*
                del &&model.r0*
                del variance
                del EchoOut.dat
                del CheckFile.dat
                del fmin.log";
        }
        run.cmds<-paste(echo.on,cpy,rnm,rmm,cln,sep="\n");
        path2model<-gsub("/","\\",file.path(path2model,model1),fixed=TRUE);
    } else if (tolower(os)%in% c('mac','osx')){
        model1<-model;
        cpy<-""; 
        if (!nopath) cpy<-"cp &&path2model ./&&model";
        rnm<-"./&&model -rs -nox  -configFile &&configFile &&mcmc &&nohess &&jitter &&seed &&pin  &&minPhase &&maxPhase";
        rmm<-"";
        if (rmEXE) rmm<-"rm &&model1";
        cln<-"";
        if (cleanup) {
            cln<-"rm &&model.bar
                rm &&model.b0*
                rm &&model.p0*
                rm &&model.r0*
                rm variance
                rm EchoOut.dat
                rm CheckFile.dat
                rm fmin.log";
        }
        cdr<-paste('DIR="$( cd "$( dirname "$0" )" && pwd )"','cd ${DIR}',sep='\n');
        run.cmds<-paste("#!/bin/sh",echo.on,cdr,cpy,rnm,rmm,cln,sep="\n");
        path2model<-file.path(path2model,model1);
    }
    if (!nopath) run.cmds<-gsub("&&path2model",  path2model,  run.cmds,fixed=TRUE);
    run.cmds<-gsub("&&model1",      model1,      run.cmds,fixed=TRUE);
    run.cmds<-gsub("&&model",       model,       run.cmds,fixed=TRUE);
    run.cmds<-gsub("&&configFile",  configFile,  run.cmds,fixed=TRUE);
    if (is.logical(pin)){
        str<-''; if (pin) str<-"-pin";
        run.cmds<-gsub("&&pin",str,run.cmds,fixed=TRUE);
    } else if (is.character(pin)){
        run.cmds<-gsub("&&pin",paste("-ainp",pin),run.cmds,fixed=TRUE);
    }
    str<-''; if (!hess) str<-"-nohess"
    run.cmds<-gsub("&&nohess",str,run.cmds,fixed=TRUE)
    str<-''; if (mcmc) str<-"-mcmc"
    run.cmds<-gsub("&&mcmc",str,run.cmds,fixed=TRUE)
    str<-''; if (jitter) str<-"-jitter"
    run.cmds<-gsub("&&jitter",str,run.cmds,fixed=TRUE)
    str<-''; if (is.numeric(seed)) str<-paste("-iSeed",seed)
    run.cmds<-gsub("&&seed",str,run.cmds,fixed=TRUE)
    str<-''; if (!is.null(minPhase)) str<-paste("-phase",minPhase)
    run.cmds<-gsub("&&minPhase",str,run.cmds,fixed=TRUE)
    str<-''; if (!is.null(maxPhase)) str<-paste("-maxph",maxPhase)
    run.cmds<-gsub("&&maxPhase",str,run.cmds,fixed=TRUE)
    
    cat("Run commands:\n",run.cmds,"\n\n");
    
    return(run.cmds);
}
