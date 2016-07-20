#'
#'@title Get estimated/predicted surveys-related quantities from several model runs
#'
#'@description Function to get estimated/predicted surveys-related quantities fom 
#'several model runs.
#'
#'@param reps - list of objects derived from Jack's R files for the models to be compared
#'@param type - survey-related time series to retrieve
#'@param verbose - flag (T/F) to print debug info
#'
#'@details Potential values for 'type' are:
#'\itemize{
#'  \item {'MB_yx' - mature biomass at survey time, by sex (1000's t)}
#'  \item {'B_yx' - total biomass at survey time, by sex (1000's t)}
#'  \item {'N_yxms'  - annual abundance by x,m,s (millions)}
#'  \item {'N_yxm'   - annual abundance by x,m (millions)}
#'  \item {'N_yx'    - annual abundance by x (millions)}
#'  \item {'lglN_y'  - legal male annual abundance (millions)}
#'  \item {'lglB_y'  - legal male annual abundance (millions)}
#'  \item {'NatZ_yxmsz' - annual abundance-at-size by x,m,s (millions)}
#'  \item {'NatZ_yxmz'  - annual abundance-at-size by x,m (millions)}
#'  \item {'NatZ_yxz'   - annual abundance-at-size by x (millions)}
#'  \item {'prNatZ_yxmsz' - annual proportions-at-size by x,m,s}
#'  \item {'prNatZ_yxmz'  - annual proportions-at-size by x,m}
#'  \item {'prNatZ_yxz'   - annual proportions-at-size by x}
#'  \item {'PRs_yxz'      - pearsons residuals for annual proportions-at-size by x}
#'  \item {'mnPrNatZ_xz'  - mean proportions-at-size by x}
#'  \item {'selSrv_cxz' - survey selectivity, by time period and sex}
#'  \item {'zscrs_yx'  - annual z-scores for fit to mature survey biomass, by sex}
#'  \item {'effSS_y' - effective (and input) sample sizes for multinomial fits}
#'}
#'Requires sqldf package.
#'
#'@return dataframe
#'
#'@export
#'
getMDFR.SurveyQuantities<-function(reps,
                                   type=c("MB_yx","B_yx",
                                          "lglN_y","lblB_y",
                                          "N_yxms","N_yxm","N_yx",
                                          "N_yxmsz","N_yxmz","N_yxz",
                                          "prNatZ_yxmsz","prNatZ_yxmz",
                                          "prNatZ_yxz","PRs_yxz","mnPrNatZ_xz",
                                          "zscrs_yx","effSS_y"),
                                   verbose=FALSE){

    if (inherits(reps,"tcsam2013.rep")) reps<-list(tcsam2013=reps);
    cases<-names(reps);
    
    #set up time info
    endyr<-list();
    for (case in cases){
        if (!is.null(reps[[case]]$mod.endyr)) {
            endyr[[case]]<-reps[[case]]$mod.endyr;
        } else {            
            cat("'endyr' missing from rep file for case '",case,"'!\n")
            cat("Must set 'endyr' in rep file.\n","Aborting...\n");
            return(NULL);
        }
    }
    styr<-list();
    for (case in cases){
        if (!is.null(reps[[case]]$mod.styr)) {
            styr[[case]]<-reps[[case]]$mod.styr;
        } else {            
            cat("'styr' missing from rep file for case '",case,"'!\n")
            cat("Must set 'styr' in rep file.\n","Aborting...\n");
            return(NULL);
        }
    }
    obsyr<-list();
    for (case in cases){
        if (!is.null(reps[[case]]$mod.obsyr)) {
            obsyr[[case]]<-reps[[case]]$mod.obsyr;
        } else {            
            cat("'obsyr' missing from rep file for case '",case,"'!\n")
            cat("Must set 'obsyr' in rep file.\n","Aborting...\n");
            return(NULL);
        }
    }
    
    #set some constants
#    THOUSAND <-1000;
    years    <-list();
    years.m1 <-list();
    obsyears <-list();
    for (case in cases){
        years[[case]]    <-styr[[case]]:endyr[[case]];
        years.m1[[case]] <-styr[[case]]:(endyr[[case]]-1);
        obsyears[[case]] <-obsyr[[case]]:endyr[[case]];
    }
    
    #----------------------------------
    # observed and predicted mature biomass from survey (1000's t)
    #----------------------------------
    if (type[1]=="MB_yx"){
        dfr<-NULL;
        for (case in cases){
            #observed
            idx<-years[[case]] %in% obsyears[[case]];
            obs <-(reps[[case]])[["srv.obs.bio.MF"]][idx];
            cv  <-(reps[[case]])[["srv.obs.bio.cv.MF"]];
            dfrof<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                              y=obsyears[[case]],x='female',m='mature',s='all',val=obs,cv=cv);
            obs <-(reps[[case]])[["srv.obs.bio.MM"]][idx];
            cv  <-(reps[[case]])[["srv.obs.bio.cv.MM"]];
            dfrom<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                              y=obsyears[[case]],x='male',m='mature',s='all',val=obs,cv=cv);
            #predicted
            mod <-(reps[[case]])[["srv.mod.bio.MF"]][idx];
            dfrmf<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='female',m='mature',s='all',val=mod,cv=NA);
            mod <-(reps[[case]])[["srv.mod.bio.MM"]][idx];
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
            obs <-(reps[[case]])[["srv.obs.bio.F"]][idx];
            dfrof<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                              y=obsyears[[case]],x='female',m='all',s='all',val=obs,cv=NA);
            obs <-(reps[[case]])[["srv.obs.bio.M"]][idx];
            dfrom<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                              y=obsyears[[case]],x='male',m='all',s='all',val=obs,cv=NA);
            #predicted
            mod <-(reps[[case]])[["srv.mod.bio.F"]];
            dfrmf<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]],x='female',m='all',s='all',val=mod,cv=NA);
            mod <-(reps[[case]])[["srv.mod.bio.M"]];
            dfrmm<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]],x='male',m='all',s='all',val=mod,cv=NA);
            dfr<-rbind(dfr,dfrof,dfrom,dfrmf,dfrmm);
        }
        return(dfr);
    }    

    #----------------------------------
    # observed and predicted abundance from survey (millions)
    #----------------------------------
    if (substr(type[1],1,2)=="N_"){
        dfr<-NULL;
        for (case in cases){
            #observed
            idx<-years[[case]] %in% obsyears[[case]];
            #--females
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='female',m='immature',s='new shell',
                             val=(reps[[case]])[["srv.obs.num.INF"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='female',m='immature',s='old shell',
                             val=(reps[[case]])[["srv.obs.num.IOF"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='female',m='mature',s='new shell',
                             val=(reps[[case]])[["srv.obs.num.MNF"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='female',m='mature',s='old shell',
                             val=(reps[[case]])[["srv.obs.num.MOF"]][idx]);
            dfr<-rbind(dfr,dfrp);
            #--males
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='male',m='immature',s='new shell',
                             val=(reps[[case]])[["srv.obs.num.INM"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='male',m='immature',s='old shell',
                             val=(reps[[case]])[["srv.obs.num.IOM"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='male',m='mature',s='new shell',
                             val=(reps[[case]])[["srv.obs.num.MNM"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='male',m='mature',s='old shell',
                             val=(reps[[case]])[["srv.obs.num.MOM"]][idx]);
            dfr<-rbind(dfr,dfrp);
            #predicted
            #--females
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='female',m='immature',s='new shell',
                             val=(reps[[case]])[["srv.mod.num.INF"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='female',m='immature',s='old shell',
                             val=(reps[[case]])[["srv.mod.num.IOF"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='female',m='mature',s='new shell',
                             val=(reps[[case]])[["srv.mod.num.MNF"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='female',m='mature',s='old shell',
                             val=(reps[[case]])[["srv.mod.num.MOF"]][idx]);
            dfr<-rbind(dfr,dfrp);
            #--males
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='male',m='immature',s='new shell',
                             val=(reps[[case]])[["srv.mod.num.INM"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='male',m='immature',s='old shell',
                             val=(reps[[case]])[["srv.mod.num.IOM"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='male',m='mature',s='new shell',
                             val=(reps[[case]])[["srv.mod.num.MNM"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='male',m='mature',s='old shell',
                             val=(reps[[case]])[["srv.mod.num.MOM"]][idx]);
            dfr<-rbind(dfr,dfrp);
        }##-cases
        if (type[1]=="N_yxms") return(dfr);
        if (type[1]=="N_yxm") {
            dfrp<-reshape2::dcast(dfr,formula="modeltype+model+category+y+x+m~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[7]<-'val';
            dfrp$s<-'all';
            dfrp<-dfrp[,c(1:6,8,7)];
            return(dfrp);
        }
        if (type[1]=="N_yx") {
            dfrp<-reshape2::dcast(dfr,formula="modeltype+model+category+y+x~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[6]<-'val';
            dfrp$m<-'all';
            dfrp$s<-'all';
            dfrp<-dfrp[,c(1:5,7,8,6)];
            return(dfrp);
        }
    }    

    #----------------------------------
    #legal male abundance at survey time (millions)
    #----------------------------------
    if (type[1]=="lglN_y"){
        dfr<-NULL;
        for (case in cases){
            #observed
            dfro<-data.frame(modeltype='TCSAM2013',model=case,category='observed',
                             y=obsyears[[case]],x='male',
                             val=(reps[[case]])[["srv.obs.num.LMs"]]);
            #predicted
            idx<-years[[case]] %in% obsyears[[case]];
            dfrm<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='male',
                             val=(reps[[case]])[["srv.mod.num.LMs"]][idx]);
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
                             val=(reps[[case]])[["srv.obs.bio.LMs"]]);
            #predicted
            dfrm<-data.frame(modeltype='TCSAM2013',model=case,category='predicted',
                             y=years[[case]][idx],x='male',
                             val=(reps[[case]])[["srv.mod.bio.LMs"]][idx]);
            dfr<-rbind(dfr,dfro,dfrm);
        }
        return(dfr);
    }
    
    #----------------------------------
    # observed and predicted abundance-at-size from survey (millions)
    #----------------------------------
    if (substr(type[1],1,2)=="NatZ_"){
        rws<-rbind(data.frame(x='female',m='immature',s='new shell',var="srv.obs.NatZ.INF",stringsAsFactors=FALSE),
                   list(x='female',m='immature',s='old shell',var="srv.obs.NatZ.IOF"),
                   list(x='female',m=  'mature',s='new shell',var="srv.obs.NatZ.MNF"),
                   list(x='female',m=  'mature',s='old shell',var="srv.obs.NatZ.MOF"),
                   list(x=  'male',m='immature',s='new shell',var="srv.obs.NatZ.INM"),
                   list(x=  'male',m='immature',s='old shell',var="srv.obs.NatZ.IOM"),
                   list(x=  'male',m=  'mature',s='new shell',var="srv.obs.NatZ.MNM"),
                   list(x=  'male',m=  'mature',s='old shell',var="srv.obs.NatZ.MOM"));
        dfr<-NULL;
        for (case in cases){
            #observed
            for (r in 1:nrow(rws)){
                vals_yz<-(reps[[case]])[[rws$var[r]]];
                dimnames(vals_yz)<-list(y=as.character(obsyears[[case]]),
                                        z=as.character(reps[[case]]$mod.zBs));
                dfrp<-reshape2::melt(vals_yz,value.name='val');
                dfrp<-cbind(modeltype='TCSAM2013',model=case,category='observed',
                            x=rws$x[r],m=rws$m[r],s=rws$s[r],dfrp);
                dfr<-rbind(dfr,dfrp[,c("modeltype","model","category","y","x","m","s","z","val")]);
            }
            #predicted
            idx<-years[[case]] %in% obsyears[[case]];
            rws$var<-gsub(".obs.",".mod.",rws$var,fixed=TRUE);
            for (r in 1:nrow(rws)){
                vals_yz<-(reps[[case]])[[rws$var[r]]];
                vals_yz<-vals_yz[idx,];
                dimnames(vals_yz)<-list(y=as.character(obsyears[[case]]),
                                        z=as.character(reps[[case]]$mod.zBs));
                dfrp<-reshape2::melt(vals_yz,value.name='val');
                dfrp<-cbind(modeltype='TCSAM2013',model=case,category='predicted',
                            x=rws$x[r],m=rws$m[r],s=rws$s[r],dfrp);
                dfr<-rbind(dfr,dfrp[,c("modeltype","model","category","y","x","m","s","z","val")]);
            }
        }##-cases
        if (type[1]=="NatZ_yxmsz") return(dfr);
        if (type[1]=="NatZ_yxmz") {
            dfrp<-reshape2::dcast(dfr,formula="modeltype+model+category+y+x+m+z~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[8]<-'val';
            dfrp$s<-'all';
            dfrp<-dfrp[,c(1:7,9,8)];
            return(dfrp);
        }
        if (type[1]=="NatZ_yxz") {
            dfrp<-reshape2::dcast(dfr,formula="modeltype+model+category+y+x+z~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[7]<-'val';
            dfrp$m<-'all';
            dfrp$s<-'all';
            dfrp<-dfrp[,c(1:5,8,9,7)];
            return(dfrp);
        }
    }    

    #----------------------------------
    # observed and predicted size comps from survey
    #----------------------------------
    if (substr(type[1],1,7)=="prNatZ_"){
        rws<-rbind(data.frame(x='female',m='immature',s='new shell',var="srv.obs.PrNatZ.INF",stringsAsFactors=FALSE),
                   list(x='female',m='immature',s='old shell',var="srv.obs.PrNatZ.IOF"),
                   list(x='female',m=  'mature',s='new shell',var="srv.obs.PrNatZ.MNF"),
                   list(x='female',m=  'mature',s='old shell',var="srv.obs.PrNatZ.MOF"),
                   list(x=  'male',m='immature',s='new shell',var="srv.obs.PrNatZ.INM"),
                   list(x=  'male',m='immature',s='old shell',var="srv.obs.PrNatZ.IOM"),
                   list(x=  'male',m=  'mature',s='new shell',var="srv.obs.PrNatZ.MNM"),
                   list(x=  'male',m=  'mature',s='old shell',var="srv.obs.PrNatZ.MOM"));
        dfr<-NULL;
        for (case in cases){
            #observed
            for (r in 1:nrow(rws)){
                vals_yz<-(reps[[case]])[[rws$var[r]]];
                dimnames(vals_yz)<-list(y=as.character(obsyears[[case]]),
                                        z=as.character(reps[[case]]$mod.zBs));
                dfrp<-reshape2::melt(vals_yz,value.name='val');
                dfrp<-cbind(modeltype='TCSAM2013',model=case,category='observed',
                            x=rws$x[r],m=rws$m[r],s=rws$s[r],dfrp);
                dfr<-rbind(dfr,dfrp[,c("modeltype","model","category","y","x","m","s","z","val")]);
            }
            #predicted
            #idx<-years[[case]] %in% obsyears[[case]];
            rws$var<-gsub(".obs.",".mod.",rws$var,fixed=TRUE);
            for (r in 1:nrow(rws)){
                vals_yz<-(reps[[case]])[[rws$var[r]]];
                #vals_yz<-vals_yz[idx,];
                dimnames(vals_yz)<-list(y=as.character(obsyears[[case]]),
                                        z=as.character(reps[[case]]$mod.zBs));
                dfrp<-reshape2::melt(vals_yz,value.name='val');
                dfrp<-cbind(modeltype='TCSAM2013',model=case,category='predicted',
                            x=rws$x[r],m=rws$m[r],s=rws$s[r],dfrp);
                dfr<-rbind(dfr,dfrp[,c("modeltype","model","category","y","x","m","s","z","val")]);
            }
        }##-cases
        if (type[1]=="prNatZ_yxmsz") return(dfr);
        if (type[1]=="prNatZ_yxmz") {
            dfrp<-reshape2::dcast(dfr,formula="modeltype+model+category+y+x+m+z~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[8]<-'val';
            dfrp$s<-'all';
            dfrp<-dfrp[,c(1:7,9,8)];
            return(dfrp);
        }
        if (type[1]=="prNatZ_yxz") {
            dfrp<-reshape2::dcast(dfr,formula="modeltype+model+category+y+x+z~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[7]<-'val';
            dfrp$m<-'all';
            dfrp$s<-'all';
            dfrp<-dfrp[,c(1:6,8,9,7)];
            return(dfrp);
        }
    }    

    #----------------------------------
    # mean size comps from survey
    #----------------------------------
    if (type[1]=="mnPrNatZ_xz"){
        dfrp<-getMDFR.SurveyQuantities(reps,type="prNatZ_yxmz")
        dfrp1<-reshape2::dcast(dfrp,formula="modeltype+model+category+x+z~.",fun.aggregate=mean,value.var='val');
        names(dfrp1)[6]<-'val';
        dfrp2<-reshape2::dcast(dfrp,formula="modeltype+model+category+x+z~.",fun.aggregate=sd,value.var='val');
        names(dfrp2)[6]<-'stdv';
        dfrp3<-reshape2::dcast(dfrp,formula="modeltype+model+category+x+z~.",fun.aggregate=length,value.var='val');
        names(dfrp3)[6]<-'N';
        dfrp<-cbind(dfrp1,stdv=dfrp2$stdv/sqrt(dfrp3$N))
        return(dfrp);
    }

    #----------------------------------
    # Pearson's residuals for size comps from survey
    #----------------------------------
    if (type[1]=="PRs_yxmz"){
        dfrp1<-getMDFR.SurveyQuantities(reps,type="prNatZ_yxmz")
        dfrp1<-reshape2::dcast(dfrp1,formula="modeltype+model+y+x+m+s+z~category",fun.aggregate=sum,value.var='val')
        dfrp1$val<-(dfrp1$observed-dfrp1$predicted)/sqrt(dfrp1$predicted*(1-dfrp1$predicted));
        dfrp2<-getMDFR.SurveyQuantities(reps,type="effSS_y");
        qry<-'select
                p.modeltype,p.model,"Pearsons Residuals" as category,p.y,p.x,p.m,p.s,p.z,sqrt(s.val)*p.val as val
              from
                dfrp2 s left join dfrp1 p
              on 
                p.modeltype=p.modeltype and
                p.model=p.model and
                s.y=p.y and
                s.category="inpSS";'
        dfrp<-sqldf::sqldf(qry);
        return(dfrp);
    }
    if (type[1]=="PRs_yxz"){
        dfrp1<-getMDFR.SurveyQuantities(reps,type="prNatZ_yxz")
        dfrp1<-reshape2::dcast(dfrp1,formula="modeltype+model+y+x+m+s+z~category",fun.aggregate=sum,value.var='val')
        dfrp1$val<-(dfrp1$observed-dfrp1$predicted)/sqrt(dfrp1$predicted*(1-dfrp1$predicted));
        dfrp2<-getMDFR.SurveyQuantities(reps,type="effSS_y");
        qry<-'select
                p.modeltype,p.model,"Pearsons Residuals" as category,p.y,p.x,p.m,p.s,p.z,sqrt(s.val)*p.val as val
              from
                dfrp2 s, dfrp1 p
              where 
                p.modeltype=p.modeltype and
                p.model=p.model and
                s.y=p.y and
                s.category="input";'
        dfrp<-sqldf::sqldf(qry);
        return(dfrp);
    }
    
    #----------------------------------
    #survey selectivities
    #----------------------------------
    if (type[1]=="selSrv_cxz"){
        dfr<-NULL;
        for (case in cases){
            #females
            sel_cz<-(reps[[case]])[["srv.mod.sel.F"]];
            dimnames(sel_cz)<-list(pc=c('1','2','3'),
                                   z=as.character(reps[[case]]$mod.zBs));
            dfrp<-reshape2::melt(sel_cz,value.name='val');
            dfrp<-cbind(modeltype='TCSAM2013',model=case,
                             x='female',dfrp);
            dfr<-rbind(dfr,dfrp[,c("modeltype","model","pc","x","z","val")]);
            #males
            sel_cz<-(reps[[case]])[["srv.mod.sel.M"]];
            dimnames(sel_cz)<-list(pc=c('1','2','3'),
                                   z=as.character(reps[[case]]$mod.zBs));
            dfrp<-reshape2::melt(sel_cz,value.name='val');
            dfrp<-cbind(modeltype='TCSAM2013',model=case,
                             x='male',dfrp);
            dfr<-rbind(dfr,dfrp[,c("modeltype","model","pc","x","z","val")]);
        }
        return(dfr);
    }
    
    #----------------------------------
    # z-scores for mature survey biomass
    #----------------------------------
    if (type[1]=="zscrs_yx"){
        dfr<-NULL;
        for (case in cases){
            idx<-years[[case]] %in% obsyears[[case]];#select only years with observations
            #females
            val <-(reps[[case]])[["srv.bio.zscr.F"]][idx];
            dfrf<-data.frame(modeltype='TCSAM2013',model=case,category='z-score',
                              y=obsyears[[case]],x='female',m='all',s='all',val=val);
            #males
            val <-(reps[[case]])[["srv.bio.zscr.M"]][idx];
            dfrm<-data.frame(modeltype='TCSAM2013',model=case,category='z-score',
                              y=obsyears[[case]],x='male',m='all',s='all',val=val);
            dfr<-rbind(dfr,dfrf,dfrm);
        }
        return(dfr);
    }    

    #----------------------------------
    # sample sizes
    #----------------------------------
    if (type[1]=="effSS_y"){
        dfr<-NULL;
        for (case in cases){
            #input sample sizes 
            val <-(reps[[case]])[["srv.inpSS"]];
            dfri<-data.frame(modeltype='TCSAM2013',model=case,category='input',
                              y=obsyears[[case]],x='all',m='all',s='all',val=val);
            #effective sample sizes (McAllister-Ianelli)
            idx<-years[[case]] %in% obsyears[[case]];#select only years with observations
            val <-(reps[[case]])[["srv.effSS.McI"]][idx];
            dfre<-data.frame(modeltype='TCSAM2013',model=case,category='McAllister-Ianelli',
                              y=obsyears[[case]],x='all',m='all',s='all',val=val);
            dfr<-rbind(dfr,dfri,dfre);
        }
        return(dfr);
    }    

    cat("Requested type '",type,"' not found!\n",sep="");
}