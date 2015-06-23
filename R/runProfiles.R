#'
#'@title Function to run profiles on a TCSAM2013 model.
#'
#'@description This functions runs profiles on a TCSAM2013 model.
#'
#'@details
#'This function creates a shell script ('./tmp.sh') in the
#'working directory and uses it to run a version of the TCSAM2013 model.
#'Initial model parameters can be jittered based on the system clock time as a seed
#'to the random number generator. The seed and final objective function value are
#'saved for each model run in a csv file (the value of out.csv).
#'
#'@param os   - 'win' or 'mac' or 'osx'
#'@param path - path to parent folder for model output
#'@param model      - TCSAM2013 model executable name
#'@param path2model - path to model executable
#'@param configFile  - path to model configuration file template
#'@param controlFile - path to model control file template
#'@param profiles    - dataframe with variables to profile on in each row
#'@param runSeqs     - TRUE to run sequences, FALSE to jitter
#'@param numRuns     - number of sequences or jitters to run
#'
#'@return - par file dataframe
#'
#'@export
#'
runProfiles<-function(os='osx',
                      path=ifelse(tolower(os)=='win','.\\','./'),
                      model='tcsam2013alta',
                      path2model='',
                      configFileTemplate=NULL,
                      controlFileTemplate=NULL,
                      profiles=NULL,
                      runSeqs=TRUE,
                      numRuns=4,
                      showPlot=TRUE){
    #read template files
    cfgt<-readLines(con=file.path(path,configFileTemplate));
    ctlt<-readLines(con=file.path(path,controlFileTemplate));
    
    #create string representation of configuration file
    cfgFile<-gsub("&&ControlFile","ModelControl.txt",cfgt,fixed=TRUE)

    #create profiles to run
    nprf<-nrow(profiles);
    prfList<-list();
    for (iprf in 1:nprf){
        #create one profile
        vals<-profiles[iprf,,drop=FALSE];#need drop=FALSE to keep column names for 1-column dataframes
        prfDir<-'prof.';
        prfCtl<-ctlt;
        for (nm in names(vals)){
            prfDir<-paste(prfDir,"_",nm,"=",vals[1,nm],sep='');
            prfCtl<-gsub(paste('&&',nm,sep=''),vals[1,nm],prfCtl,fixed=TRUE)
        }
        prfDir<-file.path(path,prfDir);
        if (!dir.exists(prfDir)) dir.create(prfDir,recursive=TRUE); #create folder for profile, if necessary
        writeLines(cfgFile,file.path(prfDir,'ModelConfig.txt'))
        writeLines(prfCtl,file.path(prfDir,'ModelControl.txt'))
        #run profile
    }
    
#     #run profiles
#     np<-length(prfList);
#     parList<-vector(mode='list',length=np);
#     names(parList)<-names(prfList);
#     for (p in names(prfList)){
#         p2f<-file.path(path,p);#path to output folder
#         writeLines(cfgFile,con=file.path(p2f,'ModelConfiguration.txt'));#write configuration file
#         writeLines(ctlFile,con=file.path(p2f,'ModelControl.txt'));      #write control file
#         if (runSeqs){
#             par<-runSequence(path=p2f,
#                               os=os,
#                               model=model,
#                               path2model=path2model,
#                               configFile='ModelConfiguration.txt',
#                               numRuns=numRuns);
#         } else {
#             par<-runJitter(path=p2f,
#                               os=os,
#                               model=model,
#                               path2model=path2model,
#                               configFile='ModelConfiguration.txt',
#                               numRuns=numRuns);
#         }
#         parList[[p]]<-par;
#     }
#     return(parList);
}