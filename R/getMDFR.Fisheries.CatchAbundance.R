#'
#'@title Get fishery catch abundance time series from model results from TCSAM2013 model runs as a dataframe
#'
#'@description Function to get fishery catch abundance time series from model results from TCSAM2013 model runs as a dataframe.
#'
#'@param obj - single tcsam2013.rep object, tcsam2013.resLst object, or named list of the latter
#'@param category - 'captured','discarded','retained', 'discard mortality', 'total mortality', or 'index'
#'@param cast - casting formula (e.g., "x+m") for excluding x,m,s,z factor levels from a sum across the unspecified factors
#'@param fleets - name(s) of fisheriess
#'@param verbose - flag (T/F) to print debug info
#'
#'@return dataframe in canonical format
#'
#'@details Extracts the estimated fishery abundance time series.
#'
#'@export
#'
getMDFR.Fisheries.CatchAbundance<-function(obj,
                                           category=c('captured','discarded','retained','discard mortality','total mortality','index'),
                                           cast="y+x",
                                           fleets=c('TCF','SCF','RKF','GTF'),
                                           verbose=FALSE){
    if (verbose) cat("--starting rTCSAM2013::getMDFR.Fisheries.CatchAbundance().\n");
    options(stringsAsFactors=FALSE);

    lst<-convertToListOfResults(obj);
    cases<-names(lst);
    if (verbose) cat("cases: ",paste0("'",cases,"'",collapse=", "),"\n")

    #set up time info
    tinfo<-getTimeInfo(lst);
    years    <-tinfo$years;
    years.m1 <-tinfo$years.m1;
    obsyears <-tinfo$obsyears;
    
    category<-category[1];

    rws<-rbind(data.frame(x='female',m='immature',s='new shell',var="fsh.mod.&&cat.NatZ.&&flt.INF"),
                     list(x='female',m='immature',s='old shell',var="fsh.mod.&&cat.NatZ.&&flt.IOF"),
                     list(x='female',m=  'mature',s='new shell',var="fsh.mod.&&cat.NatZ.&&flt.MNF"),
                     list(x='female',m=  'mature',s='old shell',var="fsh.mod.&&cat.NatZ.&&flt.MOF"),
                     list(x=  'male',m='immature',s='new shell',var="fsh.mod.&&cat.NatZ.&&flt.INM"),
                     list(x=  'male',m='immature',s='old shell',var="fsh.mod.&&cat.NatZ.&&flt.IOM"),
                     list(x=  'male',m=  'mature',s='new shell',var="fsh.mod.&&cat.NatZ.&&flt.MNM"),
                     list(x=  'male',m=  'mature',s='old shell',var="fsh.mod.&&cat.NatZ.&&flt.MOM"));
    
    if (category=='captured'){
        rws$var<-gsub("&&cat","cap",rws$var,fixed=TRUE);
    } else if (category=='discarded'){
        rws$var<-gsub("&&cat","dsc",rws$var,fixed=TRUE);
    } else if (category=='retained'){
        rws$var<-gsub("&&cat","rm",rws$var,fixed=TRUE);
    } else if (category=='discard mortality'){
        rws$var<-gsub("&&cat","dm",rws$var,fixed=TRUE);
    } else if (category=='total mortality'){
        rws$var<-gsub("&&cat","tm",rws$var,fixed=TRUE);
    } else {
        cat("Category '",category,"' not recognized!\nReturning NULL...\n");
        return(NULL);
    }
    
    dfr<-NULL;
    for (flt in fleets){
        if (verbose) cat("Processing",flt,"\n");
        rwsp<-rws;
        rwsp$var<-gsub("&&flt",flt,rwsp$var,fixed=TRUE)
        for (case in cases){
            if (verbose) {
                cat("Processing",case,"\n");
                cat("yrs:",years.m1[[case]],"\n");
                cat("zBs:",(lst[[case]]$rep)[["mod.zBs"]],"\n");
            }
            for (r in 1:nrow(rwsp)){
                vals_yz<-(lst[[case]]$rep)[[rwsp$var[r]]];
                if (!is.null(vals_yz)){
                    if (verbose) cat(rwsp$var[r],": dim(vals_yz) = ",dim(vals_yz),"\n");
                    dimnames(vals_yz)<-list(y=as.character(years.m1[[case]]),
                                            z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
                    dfrp<-reshape2::melt(vals_yz,value.name='val');
                    dfrp<-cbind(case=case,fleet=flt,
                                x=rwsp$x[r],m=rwsp$m[r],s=rwsp$s[r],dfrp);
                    dfr<-rbind(dfr,dfrp[,c("case","fleet","y","x","m","s","z","val")]);
                }
            }
        }##-cases
    }#--flt
    
    mdfr<-rCompTCMs::getMDFR.CanonicalFormat(dfr);
    mdfr$process<-'fishery';
    mdfr$category<-category;
    mdfr$type<-"predicted";
    mdfr<-removeImmOS(mdfr);

    castform<-"case+process+fleet+category+type+pc+y";
    if (!is.null(cast)|(cast!='')) castform<-paste0(castform,"+",cast);
    castform<-paste0(castform,"~.");
    ddfr<-reshape2::dcast(mdfr,castform,fun.aggregate=sum,na.rm=TRUE,value.var='val',drop=TRUE)
    ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
    ddfr<-ddfr[!is.na(ddfr[['.']]),];#remove NA's

    mdfr<-rCompTCMs::getMDFR.CanonicalFormat(ddfr);

    if (verbose) cat("--finished getMDFR.Fisheries.CatchAbundance(). \n");
    return(mdfr);
}
