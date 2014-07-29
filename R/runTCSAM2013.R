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
#'@param N - number of runs to make
#'@param model - filename of model executable to run
#'@param model.dir - path to model executable
#'@param mc - path/filename for model configuration file
#'@param in.csv - filename for jitter info (seed, obj fun value) from ADMB model run
#'@param out.csv - filename for jittered results
#'
#'@return - list w/ 2 elements:
#'  imx - index of (1st) smallest value for the objective function
#'  dfr - dataframe with iSeed, obj fun value for each model run
#'
#'@export
#'
runTCSAM2013<-function(N=5,
                      model="tcsam_wtsRKFon",
                      model.dir='/Users/WilliamStockhausen/StockAssessments-Crab/AssessmentModelDevelopment/TCSAM2013/dist/Debug/GNU-MacOSX',
                      mc="../TCSAM2013e_ModelConfig.txt",
                      in.csv='jitterInfo.csv',
                      out.csv='jitterResults.csv'){
    #start timing
    stm<-Sys.time();
    
    #set up run command
    path2model<-file.path(model.dir,model)
    run.cmds<-'#!/bin/sh
            echo on
            DIR="$( cd "$( dirname "$0" )" && pwd )"
            cd ${DIR}
           cp &&path2model ./&&model
           ./&&model -rs -nox  -configFile &&mc -nohess -jitter
            echo off';
    run.cmds<-gsub("&&path2model",path2model,run.cmds);
    run.cmds<-gsub("&&model",model,run.cmds);
    run.cmds<-gsub("&&mc",mc,run.cmds)
    cat(run.cmds,file="./tmp.sh")
    Sys.chmod("./tmp.sh",mode='7777')
    
    #set up copy commands
    fn.par<-file.path(getwd(),"&&model.par");
    fn.par<-gsub('&&model',tolower(model),fn.par)
    
    #set up output
    res.dir<-file.path(getwd(),'jitter.runs');
    if (file.exists(res.dir)) {
        fns<-list.files(res.dir);
        if (length(fns)>0) {
            fnsp<-file.path(res.dir,fns);
            file.remove(fnsp);
        }
    } else {
        dir.create(res.dir,recursive=TRUE);
    }
    dfr<-as.data.frame(list(seed=NULL,objfun=NULL));
    
    #run models
    for (i in 1:N){
        cat("\n\n---running ADMB program for",i,"out of",N,"---\n\n");
        system("./tmp.sh",wait=TRUE);        
        tbl<-read.csv(in.csv);
        if (i==1) write.table(tbl,file=out.csv,sep=",",col.names=TRUE,row.names=FALSE,append=FALSE)
        if (i >1) write.table(tbl,file=out.csv,sep=",",col.names=FALSE,row.names=FALSE,append=TRUE)
        dfr<-rbind(dfr,tbl);
        #save par file
        run.str<-formatC(i,width=4,flag='0');
        fnp.par<-file.path(res.dir,gsub('&&model',tolower(model),paste('&&model',run.str,'par',sep='.')));
        file.copy(fn.par,fnp.par);
    }
    
    #determine row index associated w/ minimum obj fun value
    idx<-which.min(dfr$objfun);
    
    #re-run model yielding minimum obj fun value
    run.cmds<-'#!/bin/sh
            echo on
            DIR="$( cd "$( dirname "$0" )" && pwd )"
            cd ${DIR}
           cp &&path2model ./&&model
           ./&&model -rs -nox  -configFile &&mc -jitter -iSeed &&seed
            echo off';
    run.cmds<-gsub("&&path2model",path2model,run.cmds);
    run.cmds<-gsub("&&model",model,run.cmds);
    run.cmds<-gsub("&&mc",mc,run.cmds);
    run.cmds<-gsub("&&seed",dfr$seed[idx],run.cmds);
    cat(run.cmds,file="./tmp.sh");
    Sys.chmod("./tmp.sh",mode='7777');
    system("./tmp.sh",wait=TRUE);
    
    #print timeing-related info
    etm<-Sys.time();
    elt<-etm-stm;
    cat("start time: ")
    print(stm);
    cat("end time: ")
    print(etm);
    cat("elapsed time: ")
    print(elt);
    
    #return output
    return(list(imx=idx,dfr=dfr));
}
#res<-runTCSAM(200);
