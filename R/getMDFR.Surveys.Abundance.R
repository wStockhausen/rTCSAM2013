#'
#'@title Get survey abundance time series from model results from TCSAM2013 model runs as a dataframe
#'
#'@description Function to get survey abundance time series from model results from TCSAM2013 model runs as a dataframe.
#'
#'@param obj - single tcsam2013.rep object, tcsam2013.resLst object, or named list of the latter
#'@param category - 'index' is only choice
#'@param cast - casting formula for excluding y,x,m,s,z factor levels from an average-at-size across unspecified factors
#'@param fleets - name(s) of surveys
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format
#'
#'@details Extracts the estimated survey abundance time series.
#'
#'@export
#'
getMDFR.Surveys.Abundance<-function(obj,category='index',cast="y+x",
                                    fleets='NMFS trawl survey',verbose=FALSE){

    if (verbose) cat("--rTCSAM2013::Getting survey abundance time series.\n");

    lst<-convertToListOfResults(obj);
    cases<-names(lst);

    #set up time info
    tinfo<-getTimeInfo(lst);
    years    <-tinfo$years;
    years.m1 <-tinfo$years.m1;
    obsyears <-tinfo$obsyears;

    category='index';

    #----------------------------------
    # predicted abundance from survey
    #----------------------------------
    rws<-rbind(data.frame(x='female',m='immature',s='new shell',var="srv.mod.NatZ.INF",stringsAsFactors=FALSE),
                     list(x='female',m='immature',s='old shell',var="srv.mod.NatZ.IOF"),
                     list(x='female',m=  'mature',s='new shell',var="srv.mod.NatZ.MNF"),
                     list(x='female',m=  'mature',s='old shell',var="srv.mod.NatZ.MOF"),
                     list(x=  'male',m='immature',s='new shell',var="srv.mod.NatZ.INM"),
                     list(x=  'male',m='immature',s='old shell',var="srv.mod.NatZ.IOM"),
                     list(x=  'male',m=  'mature',s='new shell',var="srv.mod.NatZ.MNM"),
                     list(x=  'male',m=  'mature',s='old shell',var="srv.mod.NatZ.MOM"));
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
                dfr<-rbind(dfr,dfrp[,c("case","category","pc","y","x","m","s","z","val")]);
            }
        }
    }##-cases
    mdfr<-rCompTCMs::getMDFR.CanonicalFormat(dfr);
    mdfr$fleet<-fleets[1];
    mdfr$process<-'surveys';
    mdfr$category<-category;
    mdfr$type<-'predicted';

    mdfr$fleet<-gsub("_"," ",mdfr$fleet,fixed=TRUE);#replace '_'s in survey names with spaces
    mdfr<-removeImmOS(mdfr);

    castform<-"case+process+fleet+category+type+pc&&cast~.";
    castform<-gsub("&&cast",paste0("+",cast),castform,fixed=TRUE);
    ddfr<-reshape2::dcast(mdfr,castform,fun.aggregate=mean,na.rm=TRUE,value.var='val',drop=TRUE)
    ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
    ddfr<-ddfr[!is.na(ddfr[['.']]),];#remove NA's

    mdfr<-rCompTCMs::getMDFR.CanonicalFormat(ddfr);

    if (verbose) cat("--Done. \n");
    return(mdfr);
}
