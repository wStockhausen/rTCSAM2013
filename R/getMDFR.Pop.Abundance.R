#'
#'@title Get population abundance time series from model results from TCSAM2013 model runs as a dataframe
#'
#'@description Function to get population abundance time series from model results from TCSAM2013 model runs as a dataframe.
#'
#'@param obj - single tcsam2013.rep object, tcsam2013.resLst object, or named list of the latter
#'@param cast - casting formula (e.g., "x+m") for excluding x,m,s,z factor levels from a sum of N_yxmsz across the unspecified factors
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format
#'
#'@details Extracts the estimated population abundance time series.
#'
#'@export
#'
getMDFR.Pop.Abundance<-function(obj,cast="x",verbose=FALSE){

    if (verbose) cat("--starting rTCSAM2013::getMDFR.Pop.Abundance().\n");
    options(stringsAsFactors=FALSE);

    lst<-convertToListOfResults(obj);
    cases<-names(lst);

    #set up time info
    tinfo<-getTimeInfo(lst);
    years    <-tinfo$years;
    years.m1 <-tinfo$years.m1;
    obsyears <-tinfo$obsyears;

    #----------------------------------
    # predicted abundance from survey
    #----------------------------------
    rws<-rbind(data.frame(x='female',m='immature',s='new shell',var="pop.mod.NatZ.INF",stringsAsFactors=FALSE),
                     list(x='female',m='immature',s='old shell',var="pop.mod.NatZ.IOF"),
                     list(x='female',m=  'mature',s='new shell',var="pop.mod.NatZ.MNF"),
                     list(x='female',m=  'mature',s='old shell',var="pop.mod.NatZ.MOF"),
                     list(x=  'male',m='immature',s='new shell',var="pop.mod.NatZ.INM"),
                     list(x=  'male',m='immature',s='old shell',var="pop.mod.NatZ.IOM"),
                     list(x=  'male',m=  'mature',s='new shell',var="pop.mod.NatZ.MNM"),
                     list(x=  'male',m=  'mature',s='old shell',var="pop.mod.NatZ.MOM"));
    dfr<-NULL;
    for (case in cases){
        if (verbose) {
            cat("Processing",case,"\n");
            cat("yrs:",years[[case]],"\n");
            cat("zBs:",(lst[[case]]$rep)[["mod.zBs"]],"\n");
        }
        for (r in 1:nrow(rws)){
            vals_yz<-(lst[[case]]$rep)[[rws$var[r]]];
            if (!is.null(vals_yz)){
                #vals_yz<-vals_yz[idx,];
                dimnames(vals_yz)<-list(y=as.character(years[[case]]),
                                        z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
                dfrp<-reshape2::melt(vals_yz,value.name='val');
                dfrp<-cbind(case=case,
                            x=rws$x[r],m=rws$m[r],s=rws$s[r],dfrp);
                dfr<-rbind(dfr,dfrp[,c("case","y","x","m","s","z","val")]);
            }
        }
    }##-cases
    mdfr<-rCompTCMs::getMDFR.CanonicalFormat(dfr);
    mdfr$process<-'population';
    mdfr$type<-'predicted';

    mdfr<-removeImmOS(mdfr);

    castform<-"case+process+fleet+category+type+pc+y";
    if (!is.null(cast)|(cast!='')) castform<-paste0(castform,"+",cast);
    castform<-paste0(castform,"~.");
    ddfr<-reshape2::dcast(mdfr,castform,fun.aggregate=sum,na.rm=TRUE,value.var='val',drop=TRUE)
    ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
    ddfr<-ddfr[!is.na(ddfr[['.']]),];#remove NA's

    mdfr<-rCompTCMs::getMDFR.CanonicalFormat(ddfr);

    if (verbose) cat("--finished rTCSAM2013::getMDFR.Pop.Abundance().\n");
    return(mdfr);
}
