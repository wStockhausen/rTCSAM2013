#'
#'@title Get estimated/predicted fishery-related quantities from several model runs
#'
#'@description Function to get estimated/predicted fishery-related quantities from 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param type - fishery-related quantity to retrieve
#'@param verbose - flag (T/F) to print debug info
#'
#'@details Potential values for 'type' are:
#'\itemize{
#'  \item {'bio.retm' - retained catch mortality (1000's t)}
#'  \item {'bio.dscm' - discard catch mortality (1000's t) for males in directed fishery}
#'  \item {'bio.totm' - total catch mortality (1000's t)}
#'  \item {'prNatZ.ret' - annual retained catch proportions-at-size}
#'  \item {'prNatZ.tot' - annual total catch proportions-at-size}
#'  \item {'PRs.ret'      - pearsons residuals for retained catch proportions-at-size}
#'  \item {'PRs.tot'      - pearsons residuals for total catch proportions-at-size}
#'  \item {'mnPrNatZ_ret' - mean proportions-at-size for retained catch data}
#'  \item {'mnPrNatZ.tot' - mean proportions-at-size for total catch data}
#'  \item {'selfcns' - fishery selectivity and retention functions, by time period and sex}
#'  \item {'zscores' - annual z-scores for fits to catch biomass}
#'  \item {'effSS.tot' - effective (and input) sample sizes for total catch size comps}
#'  \item {'effSS.ret' - effective (and input) sample sizes for retained catch size comps}
#'  \item {'max rates' - max fishing mortality, retained mortality, and capture rates}
#'  \item {'mean rates' - mean fishing mortality, retained mortality, and capture rates}
#'}
#'Requires sqldf package.
#'
#'@return dataframe
#'
#'@export
#'
getMDFR.FisheryQuantities<-function(obj,
                                    type=c("bio.retm","bio.dscm","bio.totm",
                                           "prNatZ.ret","prNatZ.tot",
                                           "PRs.ret","PRs.tot",
                                           "mnPrNatZ.ret","mnPrNatZ.tot",
                                           "selfcns","zscores","effSS.ret","effSS.tot",
                                           "max rates","mean rates"),
                                    verbose=FALSE){

    lst<-convertToListOfResults(obj);
    cases<-names(lst);

    #set up time info
    tinfo<-getTimeInfo(obj);
    years    <-tinfo$years;
    years.m1 <-tinfo$years.m1;

    #----------------------------------
    # observed and predicted retained catch mortality from fisheries (1000's t)
    #----------------------------------
    if (type[1]=="bio.retm"){
        dfr<-NULL;
        for (fsh in c('TCF')){
            nmo<-gsub("&&fsh",fsh,"fsh.obs.ret.bio.&&fsh",fixed=TRUE);
            nmp<-gsub("&&fsh",fsh,"fsh.mod.ret.bio.&&fsh",fixed=TRUE);
            nmy<-gsub("&&fsh",fsh,"fsh.obs.ret.bio.yrs.&&fsh",fixed=TRUE);
            for (case in cases){
                for (x in c('male')){
                    #observed
                    idx <- years.m1[[case]] %in% (lst[[case]]$rep)[[nmy]];
                    val <-(lst[[case]]$rep)[[paste0(nmo,".M")]];
                    dfrp<-data.frame(case=case,category='observed',fishery=fsh,
                                      y=(years.m1[[case]])[idx],x='male',m='all',s='all',val=val[idx]);
                    dfr<-rbind(dfr,dfrp);
                    #predicted
                    idx <- years.m1[[case]] %in% (lst[[case]]$rep)[[nmy]];
                    val <-(lst[[case]]$rep)[[paste0(nmp,".M")]];
                    dfrp<-data.frame(case=case,category='predicted',fishery=fsh,
                                      y=(years.m1[[case]])[idx],x='male',m='all',s='all',val=val[idx]);
                    dfr<-rbind(dfr,dfrp);
                }#--x
            }#--case
        }#--fsh
        return(dfr);
    }    

    #----------------------------------
    # observed and predicted total catch mortality from fisheries (1000's t)
    #----------------------------------
    if (type[1]=="bio.totm"){
        dfr<-NULL;
        for (fsh in c('TCF','SCF','RKF','GTF')){
            nmo<-gsub("&&fsh",fsh,"fsh.obs.totm.bio.&&fsh",fixed=TRUE);
            nmp<-gsub("&&fsh",fsh,"fsh.mod.totm.bio.&&fsh",fixed=TRUE);
            nmy<-gsub("&&fsh",fsh,"fsh.obs.totm.bio.yrs.&&fsh",fixed=TRUE);
            for (case in cases){
                if (fsh!='GTF'){
                    for (x in c('female','male')){
                        #observed females
                        val <-(lst[[case]]$rep)[[paste0(nmo,".F")]];
                        dfrp<-data.frame(case=case,category='observed',fishery=fsh,
                                          y=(lst[[case]]$rep)[[nmy]],x='female',m='all',s='all',val=val);
                        dfr<-rbind(dfr,dfrp);
                        #observed males
                        val <-(lst[[case]]$rep)[[paste0(nmo,".M")]];
                        dfrp<-data.frame(case=case,category='observed',fishery=fsh,
                                          y=(lst[[case]]$rep)[[nmy]],x='male',m='all',s='all',val=val);
                        dfr<-rbind(dfr,dfrp);
                        #predicted females
                        idx <- years.m1[[case]] %in% (lst[[case]]$rep)[[nmy]];
                        val <-(lst[[case]]$rep)[[paste0(nmp,".F")]];
                        dfrp<-data.frame(case=case,category='predicted',fishery=fsh,
                                          y=(years.m1[[case]])[idx],x='female',m='all',s='all',val=val[idx]);
                        dfr<-rbind(dfr,dfrp);
                        #predicted males
                        val <-(lst[[case]]$rep)[[paste0(nmp,".M")]];
                        dfrp<-data.frame(case=case,category='predicted',fishery=fsh,
                                          y=(years.m1[[case]])[idx],x='male',m='all',s='all',val=val[idx]);
                        dfr<-rbind(dfr,dfrp);
                    }#--x
                } else {
                    #observed for GTF is males+females
                    val <-(lst[[case]]$rep)[[paste0(nmo,"")]];
                    dfrp<-data.frame(case=case,category='observed',fishery=fsh,
                                      y=(lst[[case]]$rep)[[nmy]],x='all',m='all',s='all',val=val);
                    dfr<-rbind(dfr,dfrp);
                    #predicted
                    idx <- years.m1[[case]] %in% (lst[[case]]$rep)[[nmy]];
                    val <-(lst[[case]]$rep)[[paste0(nmp,"")]];
                    dfrp<-data.frame(case=case,category='predicted',fishery=fsh,
                                      y=(years.m1[[case]])[idx],x='all',m='all',s='all',val=val[idx]);
                    dfr<-rbind(dfr,dfrp);
                }
            }#--case
        }#--fsh
        return(dfr);
    }    

    #----------------------------------
    # observed and predicted discard catch mortality from fisheries (1000's t)
    #----------------------------------
    if (type[1]=="bio.dscm"){
        dfr<-NULL;
        for (fsh in c('TCF')){
            nmo<-gsub("&&fsh",fsh,"fsh.obs.dscm.bio.&&fsh",fixed=TRUE);
            nmp<-gsub("&&fsh",fsh,"fsh.mod.dscm.bio.&&fsh",fixed=TRUE);
            nmy<-gsub("&&fsh",fsh,"fsh.obs.totm.bio.yrs.&&fsh",fixed=TRUE);
            for (case in cases){
                for (x in c('male')){
                    #observed
                    val <-(lst[[case]]$rep)[[paste0(nmo,".M")]];
                    dfrp<-data.frame(case=case,category='observed',fishery=fsh,
                                      y=(lst[[case]]$rep)[[nmy]],x='male',m='all',s='all',val=val);
                    dfr<-rbind(dfr,dfrp);
                    #predicted
                    idx <- years.m1[[case]] %in% (lst[[case]]$rep)[[nmy]];
                    val <-(lst[[case]]$rep)[[paste0(nmp,".M")]];
                    dfrp<-data.frame(case=case,category='predicted',fishery=fsh,
                                      y=(years.m1[[case]])[idx],x='male',m='all',s='all',val=val[idx]);
                    dfr<-rbind(dfr,dfrp);
                }#--x
            }#--case
        }#--fsh
        return(dfr);
    }    

    #----------------------------------
    # observed and predicted fishery total catch size comps
    #----------------------------------
    if (type[1]=="prNatZ.tot"){
        dfr<-NULL;
        for (fsh in c('TCF','SCF','RKF','GTF')){
            nmo<-gsub("&&fsh",fsh,"fsh.obs.tot.PrNatZ.&&fsh",fixed=TRUE);
            nmp<-gsub("&&fsh",fsh,"fsh.mod.tot.PrNatZ.&&fsh",fixed=TRUE);
            nmy<-gsub("&&fsh",fsh,"fsh.obs.tot.PrNatZ.yrs.&&fsh",fixed=TRUE);
            for (case in cases){
                for (x in c('female','male')){
                    #observed
                    vals_yz<-(lst[[case]]$rep)[[paste0(nmo,".",toupper(substr(x,1,1)))]];
                    dimnames(vals_yz)<-list(y=as.character((lst[[case]]$rep)[[nmy]]),
                                            z=as.character(lst[[case]]$rep$mod.zBs));
                    dfrp<-reshape2::melt(vals_yz,value.name='val');
                    dfrp<-cbind(case=case,category='observed',fishery=fsh,
                                x=x,m="all",s="all",dfrp);
                    dfr<-rbind(dfr,dfrp[,c("case","category","fishery","y","x","m","s","z","val")]);
                    #predicted
                    idx<-years.m1[[case]] %in% (lst[[case]]$rep)[[nmy]];
                    vals_yz<-(lst[[case]]$rep)[[paste0(nmp,".",toupper(substr(x,1,1)))]];
                    dimnames(vals_yz)<-list(y=as.character(years.m1[[case]]),
                                            z=as.character(lst[[case]]$rep$mod.zBs));
                    dfrp<-reshape2::melt(vals_yz,value.name='val');
                    dfrp<-cbind(case=case,category='predicted',fishery=fsh,
                                x=x,m="all",s="all",dfrp[idx,]);
                    dfr<-rbind(dfr,dfrp[,c("case","category","fishery","y","x","m","s","z","val")]);
                }#--x
            }#--cases
        }#--fsh
        return(dfr);
    }    

    #----------------------------------
    # observed and predicted fishery retained catch size comps
    #----------------------------------
    if (type[1]=="prNatZ.ret"){
        dfr<-NULL;
        for (fsh in c('TCF')){
            nmo<-gsub("&&fsh",fsh,"fsh.obs.ret.PrNatZ.&&fsh",fixed=TRUE);
            nmp<-gsub("&&fsh",fsh,"fsh.mod.ret.PrNatZ.&&fsh",fixed=TRUE);
            nmy<-gsub("&&fsh",fsh,"fsh.obs.ret.PrNatZ.yrs.&&fsh",fixed=TRUE);
            for (case in cases){
                for (x in c('male')){
                    #observed
                    vals_yz<-(lst[[case]]$rep)[[paste0(nmo,".",toupper(substr(x,1,1)))]];
                    dimnames(vals_yz)<-list(y=as.character((lst[[case]]$rep)[[nmy]]),
                                            z=as.character(lst[[case]]$rep$mod.zBs));
                    dfrp<-reshape2::melt(vals_yz,value.name='val');
                    dfrp<-cbind(case=case,category='observed',fishery=fsh,
                                x=x,m="all",s="all",dfrp);
                    dfr<-rbind(dfr,dfrp[,c("case","category","fishery","y","x","m","s","z","val")]);
                    #predicted
                    idx<-years.m1[[case]] %in% (lst[[case]]$rep)[[nmy]];
                    vals_yz<-(lst[[case]]$rep)[[paste0(nmp,".",toupper(substr(x,1,1)))]];
                    dimnames(vals_yz)<-list(y=as.character(years.m1[[case]]),
                                            z=as.character(lst[[case]]$rep$mod.zBs));
                    dfrp<-reshape2::melt(vals_yz,value.name='val');
                    dfrp<-cbind(case=case,category='predicted',fishery=fsh,
                                x=x,m="all",s="all",dfrp[idx,]);
                    dfr<-rbind(dfr,dfrp[,c("case","category","fishery","y","x","m","s","z","val")]);
                }#--x
            }#--cases
        }#--fsh
        return(dfr);
    }    

    #----------------------------------
    # mean total catch size comps from fisheries
    #----------------------------------
    if (type[1]=="mnPrNatZ.tot"){
        dfrp<-getMDFR.FisheryQuantities(obj,type="prNatZ.tot")
        dfrp1<-reshape2::dcast(dfrp,formula="case+category+fishery+x+m+z~.",fun.aggregate=mean,value.var='val');
        names(dfrp1)[7]<-'val';
        dfrp2<-reshape2::dcast(dfrp,formula="case+category+fishery+x+m+z~.",fun.aggregate=sd,value.var='val');
        names(dfrp2)[7]<-'stdv';
        dfrp3<-reshape2::dcast(dfrp,formula="case+category+fishery+x+m+z~.",fun.aggregate=length,value.var='val');
        names(dfrp3)[7]<-'N';
        dfrp<-cbind(dfrp1,stdv=dfrp2$stdv/sqrt(dfrp3$N))
        return(dfrp);
    }

    #----------------------------------
    # mean retained catch size comps from fisheries
    #----------------------------------
    if (type[1]=="mnPrNatZ.ret"){
        dfrp<-getMDFR.FisheryQuantities(obj,type="prNatZ.ret")
        dfrp1<-reshape2::dcast(dfrp,formula="case+category+fishery+x+m+z~.",fun.aggregate=mean,value.var='val');
        names(dfrp1)[7]<-'val';
        dfrp2<-reshape2::dcast(dfrp,formula="case+category+fishery+x+m+z~.",fun.aggregate=sd,value.var='val');
        names(dfrp2)[7]<-'stdv';
        dfrp3<-reshape2::dcast(dfrp,formula="case+category+fishery+x+m+z~.",fun.aggregate=length,value.var='val');
        names(dfrp3)[7]<-'N';
        dfrp<-cbind(dfrp1,stdv=dfrp2$stdv/sqrt(dfrp3$N))
        return(dfrp);
    }

    #----------------------------------
    # Pearson's residuals for total catch size comps from fisheries
    #----------------------------------
    if (type[1]=="PRs.tot"){
        dfrp1<-getMDFR.FisheryQuantities(obj,type="prNatZ.tot")
        dfrp1<-reshape2::dcast(dfrp1,formula="case+fishery+y+x+m+s+z~category",fun.aggregate=sum,value.var='val')
        dfrp1$val<-(dfrp1$observed-dfrp1$predicted)/sqrt((dfrp1$predicted+1.0e-5)*(1-dfrp1$predicted));
        dfrp2<-getMDFR.FisheryQuantities(obj,type="effSS.tot");
        #--TCF, SCF, RKF fit proportions by sex
        fsh<-c('TCF','SCF','RKF');
        dfrpp1<-dfrp1[dfrp1$fishery %in% fsh,];
        dfrpp2<-dfrp2[dfrp2$fishery %in% fsh,];
        qry<-'select
                p."case", "Pearsons Residuals" as category, p.fishery,
                p.y, p.x, p.m, p.s, p.z,
                1*s.val as SS, p.observed, p.predicted, sqrt(s.val)*p.val as val
              from
                dfrpp2 s, dfrpp1 p
              where 
                s."case"=p."case" and
                s.fishery=p.fishery and
                s.y=p.y and
                s.x=p.x and
                s.category="input";';
        dfrppp1<-sqldf::sqldf(qry);
        #--GTF fits proportions extended over sex
        fsh<-c('GTF');
        dfrpp1<-dfrp1[dfrp1$fishery %in% fsh,];
        dfrpp2<-dfrp2[dfrp2$fishery %in% fsh,];
        qry<-'select
                p."case", "Pearsons Residuals" as category, p.fishery,
                p.y, p.x, p.m, p.s, p.z,
                s.val as SS, p.observed, p.predicted, sqrt(s.val)*p.val as val
              from
                dfrpp2 s, dfrpp1 p
              where 
                s."case"=p."case" and
                s.fishery=p.fishery and
                s.y=p.y and
                s.category="input";'
        dfrppp2<-sqldf::sqldf(qry);
        dfrp<-rbind(dfrppp1,dfrppp2);
        return(dfrp);
    }

    #----------------------------------
    # Pearson's residuals for retained catch size comps from fisheries
    #----------------------------------
    if (type[1]=="PRs.ret"){
        dfrp1<-getMDFR.FisheryQuantities(obj,type="prNatZ.ret")
        dfrp1<-reshape2::dcast(dfrp1,formula="case+fishery+y+x+m+s+z~category",fun.aggregate=sum,value.var='val')
        dfrp1$val<-(dfrp1$observed-dfrp1$predicted)/sqrt((dfrp1$predicted+1.0e-5)*(1-dfrp1$predicted));
        dfrp2<-getMDFR.FisheryQuantities(obj,type="effSS.ret");
        qry<-'select
                p."case","Pearsons Residuals" as category,p.fishery,
                p.y, p.x, p.m, p.s, p.z, 
                s.val as SS, p.observed, p.predicted, sqrt(s.val)*p.val as val
              from
                dfrp2 s, dfrp1 p
              where 
                s."case"=p."case" and
                s.fishery=p.fishery and
                s.y=p.y and
                s.x=p.x and
                s.category="input";'
        dfrp<-sqldf::sqldf(qry);
        return(dfrp);
    }

    #----------------------------------
    #selectivity functions
    #----------------------------------
    if (type[1]=="selfcns"){
        dfr<-NULL;
        cols.out<-c("case","category","fishery","pc","x","m","s","z","val");
        for (fsh in c('TCF','SCF','RKF','GTF')){
            nm<-gsub("&&fsh",fsh,"fsh.mod.sel.&&fsh",fixed=TRUE);
            for (case in cases){
                if (fsh!='TCF'){
                    #females
                    sel_cz<-(lst[[case]]$rep)[[paste0(nm,".F")]];
                    dimnames(sel_cz)<-list(pc=c('1','2','3'),
                                           z=as.character(lst[[case]]$rep$mod.zBs));
                    dfrp<-reshape2::melt(sel_cz,value.name='val');
                    dfrp<-cbind(case=case,category='selectivity',fishery=fsh,
                                     x='female',m='all',s='all',dfrp);
                    dfr<-rbind(dfr,dfrp[,cols.out]);
                    #males
                    sel_cz<-(lst[[case]]$rep)[[paste0(nm,".M")]];
                    dimnames(sel_cz)<-list(pc=c('1','2','3'),
                                           z=as.character(lst[[case]]$rep$mod.zBs));
                    dfrp<-reshape2::melt(sel_cz,value.name='val');
                    dfrp<-cbind(case=case,category='selectivity',fishery=fsh,
                                     x='male',m='all',s='all',dfrp);
                    dfr<-rbind(dfr,dfrp[,cols.out]);
                } else { 
                    #TCF
                    #--female selectivity
                    sel_cz<-(lst[[case]]$rep)[[paste0(nm,".F")]];
                    sel_cz<-array(sel_cz,dim=c(1,length(sel_cz)));
                    dimnames(sel_cz)<-list(pc=c('1'),
                                           z=as.character(lst[[case]]$rep$mod.zBs));
                    dfrp<-reshape2::melt(sel_cz,value.name='val');
                    dfrp<-cbind(case=case,category='selectivity',fishery=fsh,
                                     x='female',m='all',s='all',dfrp);
                    dfr<-rbind(dfr,dfrp[,cols.out]);
                    #--male selectivity
                    sel_cz<-(lst[[case]]$rep)[[paste0(nm,".M")]];
                    dimnames(sel_cz)<-list(pc=as.character(years.m1[[case]]),
                                           z=as.character(lst[[case]]$rep$mod.zBs));
                    dfrp<-reshape2::melt(sel_cz,value.name='val');
                    dfrp<-cbind(case=case,category='selectivity',fishery=fsh,
                                     x='male',m='all',s='all',dfrp);
                    dfr<-rbind(dfr,dfrp[,cols.out]);
                    #--retained selectivity (male only)
                    sel_cz<-(lst[[case]]$rep)[["fsh.mod.selr.TCF.M"]];
                    dimnames(sel_cz)<-list(pc=as.character(years.m1[[case]]),
                                           z=as.character(lst[[case]]$rep$mod.zBs));
                    dfrp<-reshape2::melt(sel_cz,value.name='val');
                    dfrp<-cbind(case=case,category='ret-sel',fishery=fsh,
                                     x='male',m='all',s='all',dfrp);
                    dfr<-rbind(dfr,dfrp[,cols.out]);
                    #--retention functions
                        ret_cz<-(lst[[case]]$rep)[["fsh.mod.ret.TCF.M"]];
                        dimnames(ret_cz)<-list(pc=as.character(years.m1[[case]]),
                                               z=as.character(lst[[case]]$rep$mod.zBs));
                        dfrp<-reshape2::melt(ret_cz,value.name='val');
                        dfrp<-cbind(case=case,category='retention',fishery='TCF',
                                         x='male',m='all',s='all',dfrp);
                        dfr<-rbind(dfr,dfrp[,cols.out]);
                }
            }#--case
        }#--fsh
        return(dfr);
    }
    
    #----------------------------------
    # z-scores for total catch biomass
    #----------------------------------
    if (type[1]=="zscores"){
        dfr<-NULL;
        for (fsh in c('TCF','SCF','RKF','GTF')){
            nmv<-gsub("&&fsh",fsh,"fsh.bio.zscr.&&fsh",fixed=TRUE);
            nmy<-gsub("&&fsh",fsh,"fsh.obs.totm.bio.yrs.&&fsh",fixed=TRUE);
            for (case in cases){
                if (fsh!='GTF'){
                    idx<-years[[case]] %in% (lst[[case]]$rep)[[nmy]];#select only years with observations
                    #females
                    val <-(lst[[case]]$rep)[[paste0(nmv,".F")]][idx];
                    dfrf<-data.frame(case=case,category='total catch',fishery=fsh,
                                      y=years[[case]][idx],x='female',m='all',s='all',val=val);
                    #males
                    val <-(lst[[case]]$rep)[[paste0(nmv,".M")]][idx];
                    dfrm<-data.frame(case=case,category='catch',fishery=fsh,
                                      y=years[[case]][idx],x='male',m='all',s='all',val=val);
                    dfr<-rbind(dfr,dfrf,dfrm);
                } else {
                    idx<-years[[case]] %in% (lst[[case]]$rep)[[nmy]];#select only years with observations
                    val <-(lst[[case]]$rep)[[nmv]][idx];
                    dfrp<-data.frame(case=case,category='catch',fishery=fsh,
                                      y=years[[case]][idx],x='all',m='all',s='all',val=val);
                    dfr<-rbind(dfr,dfrp);
                }
                if (fsh=='TCF'){
                    idx<-years[[case]] %in% as.numeric((lst[[case]]$rep)[["fsh.obs.ret.bio.yrs.TCF"]]);#select only years with observations
                    val <-(lst[[case]]$rep)[["fsh.ret.zscr.TCF"]][idx];
                    dfrp<-data.frame(case=case,category='retained catch',fishery=fsh,
                                      y=years[[case]][idx],x='male',m='all',s='all',val=val);
                    dfr<-rbind(dfr,dfrp);
                }
            }#--case
        }#--fsh
        return(dfr);
    }    

    #----------------------------------
    # total catch sample sizes
    #----------------------------------
    if (type[1]=="effSS.tot"){
        dfr<-NULL;
        for (fsh in c('TCF','SCF','RKF','GTF')){
            nmi<-gsub("&&fsh",fsh,"fsh.inpSS.tot.&&fsh",fixed=TRUE);
            nme<-gsub("&&fsh",fsh,"fsh.effSS.McI.tot.&&fsh",fixed=TRUE);
            nmy<-gsub("&&fsh",fsh,"fsh.obs.tot.PrNatZ.yrs.&&fsh",fixed=TRUE);
            for (case in cases){
                if (fsh!='GTF'){
                    #input sample sizes 
                    #--females
                    val <-(lst[[case]]$rep)[[paste0(nmi,".F")]];
                    dfrp<-data.frame(case=case,category='input',fishery=fsh,
                                      y=(lst[[case]]$rep)[[nmy]],x='female',m='all',s='all',val=val);
                    dfr<-rbind(dfr,dfrp);
                    #--males
                    val <-(lst[[case]]$rep)[[paste0(nmi,".M")]];
                    dfrp<-data.frame(case=case,category='input',fishery=fsh,
                                      y=(lst[[case]]$rep)[[nmy]],x='male',m='all',s='all',val=val);
                    dfr<-rbind(dfr,dfrp);
                    #effective sample sizes (McAllister-Ianelli)
                    idx<-years[[case]] %in% (lst[[case]]$rep)[[nmy]];#select only years with observations
                    #--females
                    val <-(lst[[case]]$rep)[[paste0(nme,".F")]][idx];
                    dfrp<-data.frame(case=case,category='McAllister-Ianelli',fishery=fsh,
                                      y=(lst[[case]]$rep)[[nmy]],x='female',m='all',s='all',val=val);
                    dfr<-rbind(dfr,dfrp);
                    #--males
                    val <-(lst[[case]]$rep)[[paste0(nme,".M")]][idx];
                    dfrp<-data.frame(case=case,category='McAllister-Ianelli',fishery=fsh,
                                      y=(lst[[case]]$rep)[[nmy]],x='male',m='all',s='all',val=val);
                    dfr<-rbind(dfr,dfrp);
                } else {
                    #input sample sizes 
                    val <-(lst[[case]]$rep)[[nmi]];
                    dfrp<-data.frame(case=case,category='input',fishery=fsh,
                                      y=(lst[[case]]$rep)[[nmy]],x='all',m='all',s='all',val=val);
                    dfr<-rbind(dfr,dfrp);
                    #effective sample sizes (McAllister-Ianelli)
                    idx<-years[[case]] %in% (lst[[case]]$rep)[[nmy]];#select only years with observations
                    val <-(lst[[case]]$rep)[[nme]][idx];
                    dfrp<-data.frame(case=case,category='McAllister-Ianelli',fishery=fsh,
                                      y=(lst[[case]]$rep)[[nmy]],x='all',m='all',s='all',val=val);
                    dfr<-rbind(dfr,dfrp);
                }
            }#--case
        }#--fsh
        return(dfr);
    }    

    #----------------------------------
    # retained catch sample sizes
    #----------------------------------
    if (type[1]=="effSS.ret"){
        dfr<-NULL;
        for (fsh in c('TCF')){
            nmi<-gsub("&&fsh",fsh,"fsh.inpSS.ret.&&fsh",fixed=TRUE);
            nme<-gsub("&&fsh",fsh,"fsh.effSS.McI.ret.&&fsh",fixed=TRUE);
            nmy<-gsub("&&fsh",fsh,"fsh.obs.ret.PrNatZ.yrs.&&fsh",fixed=TRUE);
            for (case in cases){
                #input sample sizes 
                val <-(lst[[case]]$rep)[[nmi]];
                dfrp<-data.frame(case=case,category='input',fishery=fsh,
                                  y=(lst[[case]]$rep)[[nmy]],x='male',m='all',s='all',val=val);
                dfr<-rbind(dfr,dfrp);
                #effective sample sizes (McAllister-Ianelli)
                idx<-years[[case]] %in% (lst[[case]]$rep)[[nmy]];#select only years with observations
                val <-(lst[[case]]$rep)[[nme]][idx];
                dfrp<-data.frame(case=case,category='McAllister-Ianelli',fishery=fsh,
                                  y=(lst[[case]]$rep)[[nmy]],x='male',m='all',s='all',val=val);
                dfr<-rbind(dfr,dfrp);
            }#--case
        }#--fsh
        return(dfr);
    }    

    #----------------------------------
    # max fishing mortality rates
    #----------------------------------
    if (type[1]=="max rates"){
        dfr<-NULL;
        for (fsh in c('TCF','SCF','RKF','GTF')){
            nmmr<-gsub("&&fsh",fsh,"fsh.fmr.max.&&fsh",fixed=TRUE);
            nmcr<-gsub("&&fsh",fsh,"fsh.fcr.max.&&fsh",fixed=TRUE);
            for (case in cases){
                for (x in c('male','female')){
                    #fishing mortality rates
                    vals<-(lst[[case]]$rep)[[paste0(nmmr,".",toupper(substr(x,1,1)))]];
                    if (!is.null(vals)){
                        dfrp<-data.frame(case=case,category="total mortality",fishery=fsh,
                                         y=years.m1[[case]],x=x,m="all",s="all",val=vals);
                        dfr<-rbind(dfr,dfrp);
                    }
                    #fishery capture rates
                    vals<-(lst[[case]]$rep)[[paste0(nmcr,".",toupper(substr(x,1,1)))]];
                    if (!is.null(vals)){
                        dfrp<-data.frame(case=case,category="capture",fishery=fsh,
                                         y=years.m1[[case]],x=x,m="all",s="all",val=vals);
                        dfr<-rbind(dfr,dfrp);
                    }
                }#--x
            }#--case
        }#--fsh
        #retained mortality
        for (case in cases){
            for (x in c('male')){
                #retained mortality rates in TCF
                vals<-(lst[[case]]$rep)[["fsh.rmr.max"]];
                dfrp<-data.frame(case=case,category='retained mortality',fishery="TCF",
                                 y=years.m1[[case]],x=x,m="all",s="all",val=vals);
                dfr<-rbind(dfr,dfrp);
            }#--x
        }#--case
        return(dfr)
    }
    
    #----------------------------------
    # mean fishing mortality rates
    #----------------------------------
    if (type[1]=="mean rates"){
        dfr<-NULL;
        for (fsh in c('TCF','SCF','RKF','GTF')){
            nmmr<-gsub("&&fsh",fsh,"fsh.fmr.mean.&&fsh",fixed=TRUE);
            nmcr<-gsub("&&fsh",fsh,"fsh.fcr.mean.&&fsh",fixed=TRUE);
            for (case in cases){
                for (x in c('male','female')){
                    #fishing mortality rates
                    vals<-(lst[[case]]$rep)[[paste0(nmmr,".",toupper(substr(x,1,1)))]];
                    if (!is.null(vals)){
                        dfrp<-data.frame(case=case,category="total mortality",fishery=fsh,
                                         y=years.m1[[case]],x=x,m="all",s="all",val=vals);
                        dfr<-rbind(dfr,dfrp);
                    }
                    #fishery capture rates
                    vals<-(lst[[case]]$rep)[[paste0(nmcr,".",toupper(substr(x,1,1)))]];
                    if (!is.null(vals)){
                        dfrp<-data.frame(case=case,category="capture",fishery=fsh,
                                         y=years.m1[[case]],x=x,m="all",s="all",val=vals);
                        dfr<-rbind(dfr,dfrp);
                    }
                }#--x
            }#--case
        }#--fsh
        #retained mortality
        for (case in cases){
            for (x in c('male')){
                #retained mortality rates in TCF
                vals<-(lst[[case]]$rep)[["fsh.rmr.mean"]];
                dfrp<-data.frame(case=case,category='retained mortality',fishery="TCF",
                                 y=years.m1[[case]],x=x,m="all",s="all",val=vals);
                dfr<-rbind(dfr,dfrp);
            }#--x
        }#--case
        return(dfr)
    }
    
    cat("Requested type '",type,"' not found!\n",sep="");
}

