#'
#'@title Get predicted population quantities (time series) from several model runs
#'
#'@description Function to get predicted population quantities (time series) from 
#'several model runs.
#'
#'@param reps - list of objects derived from Jack's R files for the models to be compared
#'@param type - population quantity to retrieve
#'@param verbose - flag (T/F) to print debug info
#'
#'@details Potential values for 'type' are:
#'\itemize{
#'  \item {'MB_yx' - mature biomass at mating, by sex (1000's t)}
#'  \item {'R_y' - total recruitment (millions)}
#'  \item {'N_yxmsz' - annual abundance by x,m,s,z (millions)}
#'  \item {'N_yxmz'  - annual abundance by x,m,z (millions)}
#'  \item {'N_yxms'  - annual abundance by x,m,s (millions)}
#'  \item {'N_yxz'   - annual abundance by x,m (millions)}
#'  \item {'N_yx'    - annual abundance by x (millions)}
#'}
#'
#'@return dataframe
#'
#'@export
#'
getMDFR.PopQuants<-function(reps,
                            type=c("MB_xy","R_y","N_yxmsz","N_yxmz","N_yxms","N_yxm","N_yx"),
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
    #Mating Biomass (1000's t)
    #----------------------------------
    if (type[1]=="MB_yx"){
        dfr<-NULL;
        #--males
        for (case in cases){
            va <-(reps[[case]])[["mod.MMB"]];
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,
                             x='male',y=years.m1[[case]],
                             val=val);
            dfr<-rbind(dfr,dfrp);
        }
        #--females
        for (case in cases){
            val <-(reps[[case]])[["mod.MFB"]];
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,
                             x='female',y=years.m1[[case]],
                             val=val);
            dfr<-rbind(dfr,dfrp);
        }
        return(dfr);
    }

    #----------------------------------
    #recruitment (millions)
    #----------------------------------
    if (type[1]=="R_y"){
        dfr<-NULL;
        for (case in cases){
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,
                             y=years[[case]],
                             val=-(reps[[case]])[["mod.R"]]);
            dfr<-rbind(dfr,dfrp);
        }
        return(dfr);
    }

    #----------------------------------
    #population abundance (millions)
    #----------------------------------
    if (substr(type[1])=="N_yx"){
        dfr<-NULL;
        for (case in cases){
            #INF
            val=(reps[[case]])[["mod.NatZ.pop.INF"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character(reps[[case]]$zBs));
            dfrp<-reshape2::melt(val,value.name='val')
            dfrp<-cbind(modeltype='TCSAM2013',model=case,
                        x="female",m="immature",s="new shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('modeltype','model','y','x','m','s','z','val')]);
            #IOF
            val=(reps[[case]])[["mod.NatZ.pop.IOF"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character(reps[[case]]$zBs));
            dfrp<-reshape2::melt(val,value.name='val')
            dfrp<-cbind(modeltype='TCSAM2013',model=case,
                        x="female",m="immature",s="old shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('modeltype','model','y','x','m','s','z','val')]);
            #MNF
            val=(reps[[case]])[["mod.NatZ.pop.MNF"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character(reps[[case]]$zBs));
            dfrp<-reshape2::melt(val,value.name='val')
            dfrp<-cbind(modeltype='TCSAM2013',model=case,
                        x="female",m="mature",s="new shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('modeltype','model','y','x','m','s','z','val')]);
            #MOF
            val=(reps[[case]])[["mod.NatZ.pop.MOF"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character(reps[[case]]$zBs));
            dfrp<-reshape2::melt(val,value.name='val')
            dfrp<-cbind(modeltype='TCSAM2013',model=case,
                        x="female",m="mature",s="old shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('modeltype','model','y','x','m','s','z','val')]);
            #INM
            val=(reps[[case]])[["mod.NatZ.pop.INM"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character(reps[[case]]$zBs));
            dfrp<-reshape2::melt(val,value.name='val')
            dfrp<-cbind(modeltype='TCSAM2013',model=case,
                        x="male",m="immature",s="new shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('modeltype','model','y','x','m','s','z','val')]);
            #IOM
            val=(reps[[case]])[["mod.NatZ.pop.IOM"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character(reps[[case]]$zBs));
            dfrp<-reshape2::melt(val,value.name='val')
            dfrp<-cbind(modeltype='TCSAM2013',model=case,
                        x="male",m="immature",s="old shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('modeltype','model','y','x','m','s','z','val')]);
            #MNM
            val=(reps[[case]])[["mod.NatZ.pop.MNM"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character(reps[[case]]$zBs));
            dfrp<-reshape2::melt(val,value.name='val')
            dfrp<-cbind(modeltype='TCSAM2013',model=case,
                        x="male",m="mature",s="new shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('modeltype','model','y','x','m','s','z','val')]);
            #MOM
            val=(reps[[case]])[["mod.NatZ.pop.MOM"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character(reps[[case]]$zBs));
            dfrp<-reshape2::melt(val,value.name='val')
            dfrp<-cbind(modeltype='TCSAM2013',model=case,
                        x="male",m="mature",s="old shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('modeltype','model','y','x','m','s','z','val')]);
        }
        if (type[1]=="N_yxmsz") return(dfr);
        if (type[1]=="N_yxmz"){
            dfrp<-NULL;
            dfrp<-reshape2::dcast(dfr,"modeltype+model+y+x+m+z~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[7]<-'val';
            return(dfrp);
        }
        if (type[1]=="N_yxms"){
            dfrp<-NULL;
            dfrp<-reshape2::dcast(dfr,"modeltype+model+y+x+m+s~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[7]<-'val';
            return(dfrp);
        }
        if (type[1]=="N_yxm"){
            dfrp<-NULL;
            dfrp<-reshape2::dcast(dfr,"modeltype+model+y+x+m~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[6]<-'val';
            return(dfrp);
        }
        if (type[1]=="N_yx"){
            dfrp<-NULL;
            dfrp<-reshape2::dcast(dfr,"modeltype+model+y+x~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[5]<-'val';
            return(dfrp);
        }
    }
}