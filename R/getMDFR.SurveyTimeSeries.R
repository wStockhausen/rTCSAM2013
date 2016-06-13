#'
#'@title Get estimated/predicted surveys-related time series from several model runs
#'
#'@description Function to get estimated/predicted surveys-related time series fom 
#'several model runs.
#'
#'@param reps - list of objects derived from Jack's R files for the models to be compared
#'@param type - survey-related time series to retrieve
#'@param verbose - flag (T/F) to print debug info
#'
#'@details Potential values for 'type' are:
#'\itemize{
#'  \item {'MB_xy' - mature biomass at survey time, by sex (1000's t)}
#'  \item {'B_xy' - total biomass at survey time, by sex (1000's t)}
#'  \item {'N_yxms'  - annual abundance by x,m,s (millions)}
#'  \item {'N_yxm'   - annual abundance by x,m (millions)}
#'  \item {'N_yx'    - annual abundance by x (millions)}
#'  \item {'lglN_y'  - legal male annual abundance (millions)}
#'  \item {'lglB_y'  - legal male annual abundance (millions)}
#'  \item {'Pr_yxmsz'  - annual proportions-at-size by x,m,s}
#'  \item {'selSrv_cxz' - survey selectivity, by time period and sex}
#'}
#'
#'@return dataframe
#'
#'@export
#'
getMDFR.SurveyTimeSeries<-function(reps,
                              type=c("MB_xy","B_xy","N_yxmsz","N_yxmz","N_yxms","N_yxm","N_yx"),
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
    # observed and predicted mature biomass from survey (1000's t)
    #----------------------------------
    if (type[1]=="MB_yx"){
        dfr<-NULL;
        for (case in cases){
            #observed
            idx<-years[[case]] %in% obsyears[[case]];
            obs <-(reps[[case]])[["obs.bio.srv.MF"]][idx];
            cv  <-(reps[[case]])[["obs.bio.cv.srv.MF"]];
            dfrof<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                              y=obsyears[[case]],x='female',m='mature',s='all',val=obs,cv=cv);
            obs <-(reps[[case]])[["obs.bio.srv.MM"]][idx];
            cv  <-(reps[[case]])[["obs.bio.cv.srv.MM"]];
            dfrom<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                              y=obsyears[[case]],x='male',m='mature',s='all',val=obs,cv=cv);
            #predicted
            mod <-(reps[[case]])[["mod.bio.srv.MF"]][idx];
            dfrmf<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='female',m='mature',s='all',val=mod,cv=NA);
            mod <-(reps[[case]])[["mod.bio.srv.MM"]][idx];
            dfrmm<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='male',m='mature',s='all',val=mod,cv=NA);
            dfr<-rbind(dfr,dfrof,dfrom,dfrmf,dfrmm);
        }
        return(dfr);
    }    

    #----------------------------------
    # observed and predicted total biomass from survey (1000's t)
    #----------------------------------
    if (type[1]=="B_yx"){
        dfr<-NULL;
        for (case in cases){
            #observed
            idx<-years[[case]] %in% obsyears[[case]];
            obs <-(reps[[case]])[["obs.bio.srv.F"]][idx];
            dfrof<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                              y=obsyears[[case]],x='female',m='all',s='all',val=obs,cv=NA);
            obs <-(reps[[case]])[["obs.bio.srv.M"]][idx];
            dfrom<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                              y=obsyears[[case]],x='male',m='all',s='all',val=obs,cv=NA);
            #predicted
            mod <-(reps[[case]])[["mod.bio.srv.F"]];
            dfrmf<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]],x='female',m='all',s='all',val=mod,cv=NA);
            mod <-(reps[[case]])[["mod.bio.srv.M"]];
            dfrmm<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]],x='male',m='all',s='all',val=mod,cv=NA);
            dfr<-rbind(dfr,dfrof,dfrom,dfrmf,dfrmm);
        }
        return(dfr);
    }    

    #----------------------------------
    # observed and predicted abundance from survey (millions)
    #----------------------------------
    if (substr(type[1],1,4)=="N_yx"){
        dfr<-NULL;
        for (case in cases){
            #observed
            idx<-years[[case]] %in% obsyears[[case]];
            #--females
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='female',m='immature',s='new shell',
                             val=(reps[[case]])[["obs.num.srv.INF"]][idx],cv=NA);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='female',m='immature',s='old shell',
                             val=(reps[[case]])[["obs.num.srv.IOF"]][idx],cv=NA);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='female',m='mature',s='new shell',
                             val=(reps[[case]])[["obs.num.srv.MNF"]][idx],cv=NA);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='female',m='mature',s='old shell',
                             val=(reps[[case]])[["obs.num.srv.MOF"]][idx],cv=NA);
            dfr<-rbind(dfr,dfrp);
            #--males
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='male',m='immature',s='new shell',
                             val=(reps[[case]])[["obs.num.srv.INM"]][idx],cv=NA);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='male',m='immature',s='old shell',
                             val=(reps[[case]])[["obs.num.srv.IOM"]][idx],cv=NA);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='male',m='mature',s='new shell',
                             val=(reps[[case]])[["obs.num.srv.MNM"]][idx],cv=NA);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='male',m='mature',s='old shell',
                             val=(reps[[case]])[["obs.num.srv.MOM"]][idx],cv=NA);
            dfr<-rbind(dfr,dfrp);
            #predicted
            #--females
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='female',m='immature',s='new shell',
                             val=(reps[[case]])[["mod.num.srv.INF"]][idx],cv=NA);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='female',m='immature',s='old shell',
                             val=(reps[[case]])[["mod.num.srv.IOF"]][idx],cv=NA);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='female',m='mature',s='new shell',
                             val=(reps[[case]])[["mod.num.srv.MNF"]][idx],cv=NA);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='female',m='mature',s='old shell',
                             val=(reps[[case]])[["mod.num.srv.MOF"]][idx],cv=NA);
            dfr<-rbind(dfr,dfrp);
            #--males
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='male',m='immature',s='new shell',
                             val=(reps[[case]])[["mod.num.srv.INM"]][idx],cv=NA);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='male',m='immature',s='old shell',
                             val=(reps[[case]])[["mod.num.srv.IOM"]][idx],cv=NA);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='male',m='mature',s='new shell',
                             val=(reps[[case]])[["mod.num.srv.MNM"]][idx],cv=NA);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='male',m='mature',s='old shell',
                             val=(reps[[case]])[["mod.num.srv.MOM"]][idx],cv=NA);
            dfr<-rbind(dfr,dfrp);
        }
        ##TODO: complete
        return(dfr);
    }    

    #----------------------------------
    #legal male abundance at survey time (millions)
    #----------------------------------
    if (type[1]=="lglN_y"){
        dfr<-NULL;
        for (case in cases){
            #observed
            idx<-years[[case]] %in% obsyears[[case]];
            dfro<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='male',
                             val=(reps[[case]])[["obs.num.srv.legalmales"]],cv=NA);
            #predicted
            dfrm<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='male',
                             val=(reps[[case]])[["mod.num.srv.legalmales"]][idx],cv=NA);
            dfr<-rbind(dfr,dfro,dfrm);
        }
        return(dfr);
    }

    #----------------------------------
    #legal male biomass at survey time (1000's t)
    #----------------------------------
    if (type[1]=="lglB_y"){
        dfr<-NULL;
        for (case in cases){
            #observed
            idx<-years[[case]] %in% obsyears[[case]];
            dfro<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='male',
                             val=(reps[[case]])[["obs.bio.srv.legalmales"]],cv=NA);
            #predicted
            dfrm<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='male',
                             val=(reps[[case]])[["mod.bio.srv.legalmales"]][idx],cv=NA);
            dfr<-rbind(dfr,dfro,dfrm);
        }
        return(dfr);
    }
    
    #----------------------------------
    #survey selectivities
    #----------------------------------
    if (type[1]=="selSrv_cxz"){
        dfr<-NULL;
        for (case in cases){
            #females
            sel_cz<-(reps[[case]])[["mod.sel.srv.F"]];
            dimnames(sel_cz)<-list(pc=c('1','2','3'),
                                   z=as.character(reps[[case]]$zBs));
            dfrp<-reshape2::melt(sel_cz,value.name='val');
            dfrp<-cbind(modeltype='TCSAM2013',model=case,
                             x='female',dfrp);
            dfr<-rbind(dfr,dfrp[,c("modeltype","model","pc","x","z","val")]);
            #males
            sel_cz<-(reps[[case]])[["mod.sel.srv.M"]];
            dimnames(sel_cz)<-list(pc=c('1','2','3'),
                                   z=as.character(reps[[case]]$zBs));
            dfrp<-reshape2::melt(sel_cz,value.name='val');
            dfrp<-cbind(modeltype='TCSAM2013',model=case,
                             x='male',dfrp);
            dfr<-rbind(dfr,dfrp[,c("modeltype","model","pc","x","z","val")]);
        }
        return(dfr);
    }
}