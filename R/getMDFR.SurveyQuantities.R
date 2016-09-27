#'
#'@title Get estimated/predicted survey-related quantities from several model runs
#'
#'@description Function to get estimated/predicted survey-related quantities from 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param type - survey-related quantity to retrieve
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
#'  \item {'NatZ_yxz'   - annual abundance-at-size by x (millions)}
#'  \item {'prNatZ_yxmsz' - annual proportions-at-size by x,m,s}
#'  \item {'prNatZ_yxmz'  - annual proportions-at-size by x,m}
#'  \item {'prNatZ_yxz'   - annual proportions-at-size by x}
#'  \item {'PRs_yxmsz'    - pearsons residuals for annual proportions-at-size by x,m,s}
#'  \item {'PRs_yxmz'     - pearsons residuals for annual proportions-at-size by x,m}
#'  \item {'PRs_yxz'      - pearsons residuals for annual proportions-at-size by x}
#'  \item {'mnPrNatZ_xmz' - mean proportions-at-size by x,m}
#'  \item {'mnPrNatZ_xz'  - mean proportions-at-size by x}
#'  \item {'selSrv_cxz' - survey selectivity, by time period and sex}
#'  \item {'zscrs_yx'  - annual z-scores for fit to mature survey biomass, by sex}
#'  \item {'effSS_y' - effective (and input) sample sizes for multinomial fits}
#'}
#'Requires sqldf package.
#'
#'@return dataframe with columns in canonical format
#'
#'@export
#'
getMDFR.SurveyQuantities<-function(obj,
                                   type=c("MB_yx","B_yx",
                                          "lglN_y","lglB_y",
                                          "N_yxms","N_yxm","N_yx",
                                          "NatZ_yxz",
                                          "prNatZ_yxmsz","prNatZ_yxmz","prNatZ_yxz",
                                          "PRs_yxmsz","PRs_yxmz","PRs_yxz",
                                          "mnPrNatZ_xmz","mnPrNatZ_xz",
                                          "zscrs_yx","effSS_y"),
                                   pdfType=c('lognormal','normal'),
                                   ci=0.80,
                                   verbose=FALSE){

    lst<-convertToListOfResults(obj);
    cases<-names(lst);

    #set up time info
    tinfo<-getTimeInfo(obj);
    years    <-tinfo$years;
    years.m1 <-tinfo$years.m1;
    obsyears <-tinfo$obsyears;

    fleet<-'NMFS';
    
    #----------------------------------
    # observed and predicted mature biomass from survey (1000's t)
    #----------------------------------
    if (type[1]=="MB_yx"){
        dfr<-NULL;
        for (case in cases){
            idx<-years[[case]] %in% obsyears[[case]]
            #observed
            obs <-(lst[[case]]$rep)[["srv.obs.bio.MF"]][idx];
            cv  <-(lst[[case]]$rep)[["srv.obs.bio.cv.MF"]];
            cis<-calcCIs(obs,cvs=cv,pdfType=pdfType[1],ci=ci);
            dfrof<-data.frame(case=case,category='observed',
                              y=years[[case]][idx],x='female',m='mature',s='all',
                              val=obs,lci=cis$lci,uci=cis$uci);
            obs <-(lst[[case]]$rep)[["srv.obs.bio.MM"]][idx];
            cv  <-(lst[[case]]$rep)[["srv.obs.bio.cv.MM"]];
            cis<-calcCIs(obs,cvs=cv,pdfType=pdfType[1],ci=ci);
            dfrom<-data.frame(case=case,category='observed',
                              y=years[[case]][idx],x='male',m='mature',s='all',
                              val=obs,lci=cis$lci,uci=cis$uci);
            #predicted
            mod <-(lst[[case]]$rep)[["srv.mod.bio.MF"]][idx];
            dfrmf<-data.frame(case=case,category='predicted',
                              y=years[[case]][idx],x='female',m='mature',s='all',val=mod,lci=NA,uci=NA);
            mod <-(lst[[case]]$rep)[["srv.mod.bio.MM"]][idx];
            dfrmm<-data.frame(case=case,category='predicted',
                              y=years[[case]][idx],x='male',m='mature',s='all',val=mod,lci=NA,uci=NA);
            dfr<-rbind(dfr,dfrof,dfrom,dfrmf,dfrmm);
        }
        dfr<-getMDFR.CanonicalFormat(dfr);
        dfr$fleet<-fleet;
        return(dfr);
    }    

    #----------------------------------
    # observed and predicted total biomass from survey (1000's t)
    #----------------------------------
    if (type[1]=="B_yx"){
        dfr<-NULL;
        for (case in cases){
            idx<-years[[case]] %in% obsyears[[case]];
            #observed
            obs <-(lst[[case]]$rep)[["srv.obs.bio.F"]][idx];
            dfrof<-data.frame(case=case,category='observed',
                              y=years[[case]][idx],x='female',m='all',s='all',val=obs);
            obs <-(lst[[case]]$rep)[["srv.obs.bio.M"]][idx];
            dfrom<-data.frame(case=case,category='observed',
                              y=years[[case]][idx],x='male',m='all',s='all',val=obs);
            #predicted
            mod <-(lst[[case]]$rep)[["srv.mod.bio.F"]][idx];
            dfrmf<-data.frame(case=case,category='predicted',
                             y=years[[case]][idx],x='female',m='all',s='all',val=mod);
            mod <-(lst[[case]]$rep)[["srv.mod.bio.M"]][idx];
            dfrmm<-data.frame(case=case,category='predicted',
                             y=years[[case]][idx],x='male',m='all',s='all',val=mod);
            dfr<-rbind(dfr,dfrof,dfrom,dfrmf,dfrmm);
        }
        dfr<-getMDFR.CanonicalFormat(dfr);
        dfr$fleet<-fleet;
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
            dfrp<-data.frame(case=case,category='observed',
                             y=years[[case]][idx],x='female',m='immature',s='new shell',
                             val=(lst[[case]]$rep)[["srv.obs.num.INF"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(case=case,category='observed',
                             y=years[[case]][idx],x='female',m='immature',s='old shell',
                             val=(lst[[case]]$rep)[["srv.obs.num.IOF"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(case=case,category='observed',
                             y=years[[case]][idx],x='female',m='mature',s='new shell',
                             val=(lst[[case]]$rep)[["srv.obs.num.MNF"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(case=case,category='observed',
                             y=years[[case]][idx],x='female',m='mature',s='old shell',
                             val=(lst[[case]]$rep)[["srv.obs.num.MOF"]][idx]);
            dfr<-rbind(dfr,dfrp);
            #--males
            dfrp<-data.frame(case=case,category='observed',
                             y=years[[case]][idx],x='male',m='immature',s='new shell',
                             val=(lst[[case]]$rep)[["srv.obs.num.INM"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(case=case,category='observed',
                             y=years[[case]][idx],x='male',m='immature',s='old shell',
                             val=(lst[[case]]$rep)[["srv.obs.num.IOM"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(case=case,category='observed',
                             y=years[[case]][idx],x='male',m='mature',s='new shell',
                             val=(lst[[case]]$rep)[["srv.obs.num.MNM"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(case=case,category='observed',
                             y=years[[case]][idx],x='male',m='mature',s='old shell',
                             val=(lst[[case]]$rep)[["srv.obs.num.MOM"]][idx]);
            dfr<-rbind(dfr,dfrp);
            #predicted
            #--females
            dfrp<-data.frame(case=case,category='predicted',
                             y=years[[case]][idx],x='female',m='immature',s='new shell',
                             val=(lst[[case]]$rep)[["srv.mod.num.INF"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(case=case,category='predicted',
                             y=years[[case]][idx],x='female',m='immature',s='old shell',
                             val=(lst[[case]]$rep)[["srv.mod.num.IOF"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(case=case,category='predicted',
                             y=years[[case]][idx],x='female',m='mature',s='new shell',
                             val=(lst[[case]]$rep)[["srv.mod.num.MNF"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(case=case,category='predicted',
                             y=years[[case]][idx],x='female',m='mature',s='old shell',
                             val=(lst[[case]]$rep)[["srv.mod.num.MOF"]][idx]);
            dfr<-rbind(dfr,dfrp);
            #--males
            dfrp<-data.frame(case=case,category='predicted',
                             y=years[[case]][idx],x='male',m='immature',s='new shell',
                             val=(lst[[case]]$rep)[["srv.mod.num.INM"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(case=case,category='predicted',
                             y=years[[case]][idx],x='male',m='immature',s='old shell',
                             val=(lst[[case]]$rep)[["srv.mod.num.IOM"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(case=case,category='predicted',
                             y=years[[case]][idx],x='male',m='mature',s='new shell',
                             val=(lst[[case]]$rep)[["srv.mod.num.MNM"]][idx]);
            dfr<-rbind(dfr,dfrp);
            dfrp<-data.frame(case=case,category='predicted',
                             y=years[[case]][idx],x='male',m='mature',s='old shell',
                             val=(lst[[case]]$rep)[["srv.mod.num.MOM"]][idx]);
            dfr<-rbind(dfr,dfrp);
        }##-cases
        if (type[1]=="N_yxms") dfrp<-dfr;
        if (type[1]=="N_yxm") {
            dfrp<-reshape2::dcast(dfr,formula="case+category+y+x+m~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[6]<-'val';
        }
        if (type[1]=="N_yx") {
            dfrp<-reshape2::dcast(dfr,formula="case+category+y+x~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[5]<-'val';
        }
        dfrp<-getMDFR.CanonicalFormat(dfrp);
        dfrp$fleet<-fleet;
        return(dfrp);
    }    

    #----------------------------------
    #legal male abundance at survey time (millions)
    #----------------------------------
    if (type[1]=="lglN_y"){
        dfr<-NULL;
        for (case in cases){
            idx<-years[[case]] %in% obsyears[[case]];
            #observed
            dfro<-data.frame(case=case,category='observed',
                             y=obsyears[[case]],x='male',
                             val=(lst[[case]]$rep)[["srv.obs.num.LMs"]]);
            #predicted
            dfrm<-data.frame(case=case,category='predicted',
                             y=years[[case]][idx],x='male',
                             val=(lst[[case]]$rep)[["srv.mod.num.LMs"]][idx]);
            dfr<-rbind(dfr,dfro,dfrm);
        }
        dfrp<-getMDFR.CanonicalFormat(dfr);
        dfrp$fleet<-fleet;
        return(dfrp);
    }

    #----------------------------------
    #legal male biomass at survey time (1000's t)
    #----------------------------------
    if (type[1]=="lglB_y"){
        dfr<-NULL;
        for (case in cases){
            idx<-years[[case]] %in% obsyears[[case]];
            #observed
            dfro<-data.frame(case=case,category='observed',
                             y=obsyears[[case]],x='male',
                             val=(lst[[case]]$rep)[["srv.obs.bio.LMs"]]);
            #predicted
            dfrm<-data.frame(case=case,category='predicted',
                             y=years[[case]][idx],x='male',
                             val=(lst[[case]]$rep)[["srv.mod.bio.LMs"]][idx]);
            dfr<-rbind(dfr,dfro,dfrm);
        }
        dfrp<-getMDFR.CanonicalFormat(dfr);
        dfrp$fleet<-fleet;
        return(dfrp);
    }
    
    #----------------------------------
    # observed and predicted abundance-at-size from survey (millions)
    #----------------------------------
    if (substr(type[1],1,5)=="NatZ_"){
        dfr<-NULL;
        #observed
        rws<-list();
        rws[["srv.obs.NatZ.F"]]<-list(x='female',m='all',s='all')
        rws[["srv.obs.NatZ.M"]]<-list(x=  'male',m='all',s='all')
        for (case in cases){
            for (nm in names(rws)){
                rw<-rws[[nm]];
                vals_yz<-(lst[[case]]$rep)[[nm]];
                dimnames(vals_yz)<-list(y=as.character(obsyears[[case]]),
                                        z=as.character(lst[[case]]$rep$mod.zBs));
                dfrp<-reshape2::melt(vals_yz,value.name='val');
                dfrp<-cbind(case=case,category='observed',
                            x=rw$x,m=rw$m,s=rw$s,dfrp);
                dfr<-rbind(dfr,dfrp[,c("case","category","y","x","m","s","z","val")]);
            }
        }
        #predicted
        rws<-list();
        rws[["srv.mod.NatZ.F"]]<-list(x='female',m='all',s='all')
        rws[["srv.mod.NatZ.M"]]<-list(x=  'male',m='all',s='all')
        for (case in cases){
            idx<-years[[case]] %in% obsyears[[case]];
            for (nm in names(rws)){
                rw<-rws[[nm]];
                vals_yz<-(lst[[case]]$rep)[[nm]];
                dimnames(vals_yz)<-list(y=as.character(obsyears[[case]]),
                                        z=as.character(lst[[case]]$rep$mod.zBs));
                dfrp<-reshape2::melt(vals_yz,value.name='val');
                dfrp<-cbind(case=case,category='predicted',
                            x=rw$x,m=rw$m,s=rw$s,dfrp);
                dfr<-rbind(dfr,dfrp[,c("case","category","y","x","m","s","z","val")]);
            }
        }##-cases
        dfrp<-getMDFR.CanonicalFormat(dfr);
        dfrp$fleet<-fleet;
        return(dfrp);
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
                vals_yz<-(lst[[case]]$rep)[[rws$var[r]]];
                dimnames(vals_yz)<-list(y=as.character(obsyears[[case]]),
                                        z=as.character(lst[[case]]$rep$mod.zBs));
                dfrp<-reshape2::melt(vals_yz,value.name='val');
                dfrp<-cbind(case=case,category='observed',
                            x=rws$x[r],m=rws$m[r],s=rws$s[r],dfrp);
                dfr<-rbind(dfr,dfrp[,c("case","category","y","x","m","s","z","val")]);
            }
            #predicted
            #idx<-years[[case]] %in% obsyears[[case]];
            rws$var<-gsub(".obs.",".mod.",rws$var,fixed=TRUE);#switch to ".mod."
            for (r in 1:nrow(rws)){
                vals_yz<-(lst[[case]]$rep)[[rws$var[r]]];
                #vals_yz<-vals_yz[idx,];
                dimnames(vals_yz)<-list(y=as.character(obsyears[[case]]),
                                        z=as.character(lst[[case]]$rep$mod.zBs));
                dfrp<-reshape2::melt(vals_yz,value.name='val');
                dfrp<-cbind(case=case,category='predicted',
                            x=rws$x[r],m=rws$m[r],s=rws$s[r],dfrp);
                dfr<-rbind(dfr,dfrp[,c("case","category","y","x","m","s","z","val")]);
            }
            rws$var<-gsub(".mod.",".obs.",rws$var,fixed=TRUE);#switch back to ".obs."
        }##-cases
        if (type[1]=="prNatZ_yxmsz") dfrp<-dfr;
        if (type[1]=="prNatZ_yxmz") {
            dfrp<-reshape2::dcast(dfr,formula="case+category+y+x+m+z~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[7]<-'val';
        }
        if (type[1]=="prNatZ_yxz") {
            dfrp<-reshape2::dcast(dfr,formula="case+category+y+x+z~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[6]<-'val';
        }
        dfrp<-getMDFR.CanonicalFormat(dfrp);
        dfrp$fleet<-fleet;
        return(dfrp);
    }    

    #----------------------------------
    # mean size comps from survey
    #----------------------------------
    if (substr(type[1],1,8)=="mnPrNatZ"){
        if (type[1]=="mnPrNatZ_xmz"){
            dfrp<-getMDFR.SurveyQuantities(obj,type="prNatZ_yxmz")
            dfrp1<-reshape2::dcast(dfrp,formula="case+category+x+m+z~.",fun.aggregate=mean,value.var='val');
            names(dfrp1)[6]<-'val';
            dfrp2<-reshape2::dcast(dfrp,formula="case+category+x+m+z~.",fun.aggregate=sd,value.var='val');
            names(dfrp2)[6]<-'stdv';
            dfrp3<-reshape2::dcast(dfrp,formula="case+category+x+m+z~.",fun.aggregate=length,value.var='val');
            names(dfrp3)[6]<-'N';
        }
        if (type[1]=="mnPrNatZ_xz"){
            dfrp<-getMDFR.SurveyQuantities(obj,type="prNatZ_yxmz")
            dfrp1<-reshape2::dcast(dfrp,formula="case+category+x+z~.",fun.aggregate=mean,value.var='val');
            names(dfrp1)[5]<-'val';
            dfrp2<-reshape2::dcast(dfrp,formula="case+category+x+z~.",fun.aggregate=sd,value.var='val');
            names(dfrp2)[5]<-'stdv';
            dfrp3<-reshape2::dcast(dfrp,formula="case+category+x+z~.",fun.aggregate=length,value.var='val');
            names(dfrp3)[5]<-'N';
        }
        cis<-calcCIs(dfrp1$val,sdvs=dfrp2$stdv/sqrt(dfrp3$N),pdfType='normal',ci=0.80)
        dfrp<-cbind(dfrp1,lci=cis$lci,uci=cis$uci)
        dfrp<-getMDFR.CanonicalFormat(dfrp);
        dfrp$fleet<-fleet;
        return(dfrp);
    }

    #----------------------------------
    # Pearson's residuals for size comps from survey
    #----------------------------------
    if (substr(type[1],1,4)=="PRs_"){
        if (type[1]=="PRs_yxmsz"){
            dfrp1<-getMDFR.SurveyQuantities(obj,type="prNatZ_yxmz")
            dfrp1<-reshape2::dcast(dfrp1,formula="case+y+x+m+s+z~category",fun.aggregate=sum,value.var='val')
            dfrp1$val<-(dfrp1$observed-dfrp1$predicted)/sqrt((dfrp1$predicted+1.0e-5)*(1-dfrp1$predicted));
            dfrp2<-getMDFR.SurveyQuantities(obj,type="effSS_y");
            qry<-'select
                    p."case","Pearsons Residuals" as category,p.y,p.x,p.m,p.s,p.z,sqrt(s.val)*p.val as val
                  from
                    dfrp2 s, dfrp1 p
                  where 
                    s."case"=p."case" and
                    s.y=p.y and
                    s.category="input";'
            dfrp<-sqldf::sqldf(qry);
        }
        if (type[1]=="PRs_yxmz"){
            dfrp1<-getMDFR.SurveyQuantities(obj,type="prNatZ_yxmz")
            dfrp1<-reshape2::dcast(dfrp1,formula="case+y+x+m+z~category",fun.aggregate=sum,value.var='val')
            dfrp1$val<-(dfrp1$observed-dfrp1$predicted)/sqrt((dfrp1$predicted+1.0e-5)*(1-dfrp1$predicted));
            dfrp2<-getMDFR.SurveyQuantities(obj,type="effSS_y");
            qry<-'select
                    p."case","Pearsons Residuals" as category,p.y,p.x,p.m,p.z,sqrt(s.val)*p.val as val
                  from
                    dfrp2 s, dfrp1 p
                  where 
                    s."case"=p."case" and
                    s.y=p.y and
                    s.category="input";'
            dfrp<-sqldf::sqldf(qry);
        }
        if (type[1]=="PRs_yxz"){
            dfrp1<-getMDFR.SurveyQuantities(obj,type="prNatZ_yxz")
            dfrp1<-reshape2::dcast(dfrp1,formula="case+y+x+z~category",fun.aggregate=sum,value.var='val')
            dfrp1$val<-(dfrp1$observed-dfrp1$predicted)/sqrt((dfrp1$predicted+1.0e-5)*(1-dfrp1$predicted));
            dfrp2<-getMDFR.SurveyQuantities(obj,type="effSS_y");
            qry<-'select
                    p."case","Pearsons Residuals" as category,p.y,p.x,p.z,sqrt(s.val)*p.val as val
                  from
                    dfrp2 s, dfrp1 p
                  where 
                    s."case"=p."case" and
                    s.y=p.y and
                    s.category="input";'
            dfrp<-sqldf::sqldf(qry);
        }
        dfrp<-getMDFR.CanonicalFormat(dfrp);
        dfrp$fleet<-fleet;
        return(dfrp);
    }
    
    #----------------------------------
    #survey selectivities
    #----------------------------------
    if (type[1]=="selSrv_cxz"){
        dfr<-NULL;
        for (case in cases){
            #females
            sel_cz<-(lst[[case]]$rep)[["srv.mod.sel.F"]];
            dimnames(sel_cz)<-list(pc=c('1','2','3'),
                                   z=as.character(lst[[case]]$rep$mod.zBs));
            dfrp<-reshape2::melt(sel_cz,value.name='val');
            dfrp<-cbind(case=case,
                             x='female',dfrp);
            dfr<-rbind(dfr,dfrp[,c("case","pc","x","z","val")]);
            #males
            sel_cz<-(lst[[case]]$rep)[["srv.mod.sel.M"]];
            dimnames(sel_cz)<-list(pc=c('1','2','3'),
                                   z=as.character(lst[[case]]$rep$mod.zBs));
            dfrp<-reshape2::melt(sel_cz,value.name='val');
            dfrp<-cbind(case=case,
                             x='male',dfrp);
            dfr<-rbind(dfr,dfrp[,c("case","pc","x","z","val")]);
        }
        dfrp<-getMDFR.CanonicalFormat(dfr);
        dfrp$category<-'selectivity';
        dfrp$fleet<-fleet;
        return(dfrp);
    }
    
    #----------------------------------
    # z-scores for mature survey biomass
    #----------------------------------
    if (type[1]=="zscrs_yx"){
        dfr<-NULL;
        for (case in cases){
            idx<-years[[case]] %in% obsyears[[case]];#select only years with observations
            #females
            val <-(lst[[case]]$rep)[["srv.bio.zscr.F"]][idx];
            dfrf<-data.frame(case=case,category='z-score',
                              y=obsyears[[case]],x='female',m='all',s='all',val=val);
            #males
            val <-(lst[[case]]$rep)[["srv.bio.zscr.M"]][idx];
            dfrm<-data.frame(case=case,category='z-score',
                              y=obsyears[[case]],x='male',m='all',s='all',val=val);
            dfr<-rbind(dfr,dfrf,dfrm);
        }
        dfrp<-getMDFR.CanonicalFormat(dfr);
        dfrp$fleet<-fleet;
        return(dfrp);
    }    

    #----------------------------------
    # sample sizes
    #----------------------------------
    if (type[1]=="effSS_y"){
        dfr<-NULL;
        for (case in cases){
            #input sample sizes 
            val <-(lst[[case]]$rep)[["srv.inpSS"]];
            dfri<-data.frame(case=case,category='input',
                              y=obsyears[[case]],x='all',m='all',s='all',val=val);
            #effective sample sizes (McAllister-Ianelli)
            idx<-years[[case]] %in% obsyears[[case]];#select only years with observations
            val <-(lst[[case]]$rep)[["srv.effSS.McI"]][idx];
            dfre<-data.frame(case=case,category='McAllister-Ianelli',
                              y=obsyears[[case]],x='all',m='all',s='all',val=val);
            dfr<-rbind(dfr,dfri,dfre);
        }
        dfrp<-getMDFR.CanonicalFormat(dfr);
        dfrp$fleet<-fleet;
        return(dfrp);
    }    

    cat("Requested type '",type,"' not found!\n",sep="");
}