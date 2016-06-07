#'
#'@title Get predicted population processes from several model runs
#'
#'@description Function to get predicted population processes from 
#'several model runs.
#'
#'@param reps - list of objects derived from Jack's R files for the models to be compared
#'@param type - population process to retrieve
#'@param verbose - flag (T/F) to print debug info
#'
#'@details Potential values for 'type' are:
#'\itemize{
#'  \item {'M_yxm' - natural mortality rates}
#'  \item {'R_cz' - size distribution for recruitment}
#'  \item {'prM2M_cxz' - probability of molt-to-maturity}
#'  \item {'mnZAM_cxz' - mean growth increment}
#'  \item {'T_cxzz' - growth transition matrix}
#'}
#'Uses \code{reshape2::melt}.
#'
#'@return dataframe
#'
#'@export
#'
getMDFR.PopProcesses<-function(reps,
                               type=c('M_yxm',"R_cz",'prM2M_cxz','mnZAM_cxz','T_cxzz'),
                               verbose=FALSE){

    cases<-names(reps);
    
    #set up time info
    endyr<-list();
    for (case in cases){
        if (!is.null(reps[[case]]$endyr)) {
            endyr[[case]]<-reps[[case]]$endyr;
        } else {            
            cat("'endyr' missing from rep file for case '",case,"'!\n")
            cat("Must set 'endyr' in rep file.\n","Aborting...\n");
            return(NULL);
        }
    }
    styr<-list();
    for (case in cases){
        if (!is.null(reps[[case]]$styr)) {
            styr[[case]]<-reps[[case]]$styr;
        } else {            
            cat("'styr' missing from rep file for case '",case,"'!\n")
            cat("Must set 'styr' in rep file.\n","Aborting...\n");
            return(NULL);
        }
    }
    obsyr<-list();
    for (case in cases){
        if (!is.null(reps[[case]]$obsyr)) {
            obsyr[[case]]<-reps[[case]]$obsyr;
        } else {            
            cat("'obsyr' missing from rep file for case '",case,"'!\n")
            cat("Must set 'obsyr' in rep file.\n","Aborting...\n");
            return(NULL);
        }
    }
    pltyr<-list();
    for (case in cases){
        if (!is.null(reps[[case]]$pltyr)) {
            pltyr[[case]]<-reps[[case]]$pltyr;
        } else {            
            cat("'pltyr' missing from rep file for case '",case,"'!\n")
            cat("Must set 'pltyr' in rep file.\n","Aborting...\n");
            return(NULL);
        }
    }
    
    #set some constants
    THOUSAND <-1000;

    years    <-list();
    years.m1 <-list();
    obsyears <-list();
    plotyears<-list();
    for (case in cases){
        years[[case]]    <-styr[[case]]:endyr[[case]];
        years.m1[[case]] <-styr[[case]]:(endyr[[case]]-1);
        obsyears[[case]] <-obsyr[[case]]:endyr[[case]];
        plotyears[[case]]<-pltyr[[case]]:endyr[[case]];
    }
    
    #----------------------------------
    #natural mortality rates
    #----------------------------------
    if (type[1]=="M_yxm"){
        dfr<-NULL;
        for (case in cases){
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,
                             y=years.m1[[case]],x="female",m="immature",
                             val=(reps[[case]])[["mod.M.INF"]]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,
                             y=years.m1[[case]],x="female",m="mature",
                             val=(reps[[case]])[["mod.M.MNF"]]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,
                             y=years.m1[[case]],x="male",m="immature",
                             val=(reps[[case]])[["mod.M.INM"]]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,
                             y=years.m1[[case]],x="male",m="mature",
                             val=(reps[[case]])[["mod.M.MNM"]]);
            dfr<-rbind(dfr,dfrp);
        }
        return(dfr);
    }
    #----------------------------------
    #recruitment size distribution
    #----------------------------------
    if (type[1]=="R_cz"){
        dfr<-NULL;
        for (case in cases){
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,
                             pc='1',z=reps[[case]]$zBs,
                             val=(reps[[case]])[["mod.prR_z"]]);
            dfr<-rbind(dfr,dfrp);
        }
        return(dfr);
    }
    #----------------------------------
    #pr(molt-to-maturity|z)
    #----------------------------------
    if (type[1]=="prM2M_cxz"){
        dfr<-NULL;
        for (case in cases){
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,
                             pc=1,x="female",z=reps[[case]]$zBs,
                             val=(reps[[case]])[["mod.prM2M.F"]]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,
                             pc=1,x="male",z=reps[[case]]$zBs,
                             val=(reps[[case]])[["mod.prM2M.M"]]);
            dfr<-rbind(dfr,dfrp);
        }
        return(dfr);
    }
    #----------------------------------
    #mean growth increments
    #----------------------------------
    if (type[1]=="mnZAM_cxz"){
        dfr<-NULL;
        for (case in cases){
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,
                             pc=1,x="female",z=reps[[case]]$zBs,
                             val=(reps[[case]])[["mod.mnPMZ.F"]]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,
                             pc=1,x="male",z=reps[[case]]$zBs,
                             val=(reps[[case]])[["mod.mnPMZ.M"]]);
            dfr<-rbind(dfr,dfrp);
        }
        return(dfr);
    }
    #----------------------------------
    #growth transition matrices
    #----------------------------------
    if (type[1]=="T_cxzz"){
        dfr<-NULL;
        for (case in cases){
            val=(reps[[case]])[["mod.prGr_xzz.F"]];
            dimnames(val)<-list(z =as.character(reps[[case]]$zBs),
                                zp=as.character(reps[[case]]$zBs));
            dfrp<-reshape2::melt(val,value.name='val')
            dfrp<-cbind(modeltype='TCSAM2013',model=case,
                        pc=1,x="female",dfrp);
            dfr<-rbind(dfr,dfrp);
            val=(reps[[case]])[["mod.prGr_xzz.M"]];
            dimnames(val)<-list(z =as.character(reps[[case]]$zBs),
                                zp=as.character(reps[[case]]$zBs));
            dfrp<-reshape2::melt(val,value.name='val')
            dfrp<-cbind(modeltype='TCSAM2013',model=case,
                        pc=1,x="male",dfrp);
            dfr<-rbind(dfr,dfrp);
        }
        return(dfr);
    }
}