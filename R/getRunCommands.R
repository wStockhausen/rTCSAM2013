#'
#'@title Generate run commands for a tcsam2013 model run
#'
#'@description Function to generate a script to make a tcsam2013 model run
#'
#'@param os - 'win', 'mac' or 'osx'
#'@param model - admb model name
#'@param path2model - path to model
#'@param configFile - filename (including path) to model configuration file
#'@param pin - flag (T/F) to use a pin file
#'@param hess - flag (T/F) to calculate the hessian
#'@param mcmc - flag (T/F) to do mcmc calculations
#'@param jitter - flag (T/F) to use jitter initial values
#'@param seed - value for random number seed to generate jitter
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
                         seed=NULL){
    if (tolower(os)=='win'){
        model1<-paste(model,'exe',sep='.');
        run.cmds<-'echo on
                    copy &&path2model &&model1
                    &&model -rs -nox  -configFile &&configFile &&mcmc &&nohess &&jitter &&seed &&pin
                    del &&model1
                    del &&model.bar
                    del &&model.b0*
                    del &&model.p0*
                    del &&model.r0*
                    del variance
                    del EchoOut.dat
                    del CheckFile.dat
                    del fmin.log
                    echo off';
        path2model<-gsub("/","\\",file.path(path2model,model1),fixed=TRUE);
    } else if (tolower(os)%in% c('mac','osx')){
        model1<-model;
        run.cmds<-'#!/bin/sh
                  echo on
                  DIR="$( cd "$( dirname "$0" )" && pwd )"
                  cd ${DIR}
                  cp &&path2model ./&&model
                  ./&&model -rs -nox  -configFile &&configFile &&mcmc &&nohess &&jitter &&seed &&pin
                  rm &&model
                  rm &&model.bar
                  rm &&model.b0*
                  rm &&model.p0*
                  rm &&model.r0*
                  rm variance
                  rm EchoOut.dat
                  rm CheckFile.dat
                  rm fmin.log
                  echo off';
        path2model<-file.path(path2model,model1);
    }
    run.cmds<-gsub("&&path2model",  path2model,  run.cmds,fixed=TRUE);
    run.cmds<-gsub("&&model1",      model1,      run.cmds,fixed=TRUE);
    run.cmds<-gsub("&&model",       model,       run.cmds,fixed=TRUE);
    run.cmds<-gsub("&&configFile",  configFile,  run.cmds,fixed=TRUE)
    str<-''; if (pin) str<-"-pin"
    run.cmds<-gsub("&&pin",str,run.cmds,fixed=TRUE)
    str<-''; if (!hess) str<-"-nohess"
    run.cmds<-gsub("&&nohess",str,run.cmds,fixed=TRUE)
    str<-''; if (mcmc) str<-"-mcmc"
    run.cmds<-gsub("&&mcmc",str,run.cmds,fixed=TRUE)
    str<-''; if (jitter) str<-"-jitter"
    run.cmds<-gsub("&&jitter",str,run.cmds,fixed=TRUE)
    str<-''; if (is.numeric(seed)) str<-paste("-iSeed",seed)
    run.cmds<-gsub("&&seed",str,run.cmds,fixed=TRUE)
    return(run.cmds);
}