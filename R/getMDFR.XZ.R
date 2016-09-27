#'
#'@title Get XZ model output from several model runs
#'
#'@description Function to get XZ from 
#'several model runs.
#'
#'@param obj - object with results for the models to be compared that can be converted to a list of tcsam2013.resLst objects
#'@param rep.m - rep object name for males
#'@param rep.f - rep object name for females
#'@param rep.m - std object name for males
#'@param rep.f - std object name for females
#'@param verbose - flag (T/F) to print debug info
#'
#'@details Uses \code{reshape2::melt} and \code{reshape2::dcast}. If obj is a tcsam2013.lst
#'object, then results will be extracted from the tcsam2013.std object for a given case, if it exists; 
#'otherwise they will be extracted from the tcsam2013.rep object.
#'
#'@return dataframe
#'
#'@export
#'
getMDFR.XZ<-function(obj,rep.m=NULL,rep.f=NULL,std.m=NULL,std.f=NULL,verbose=FALSE){

    lst<-convertToListOfResults(obj);
    cases<-names(lst);
    
    dfr<-NULL;
    for (case in cases){
        if (is.null(lst[[case]]$std)) {
            #males
            val <-(lst[[case]]$rep)[[rep.m]];
            lci <- NA;
            uci <- NA;
            dfrp<-data.frame(case=case,
                             x='male',z=(lst[[case]]$rep)$mod.zBs,
                             val=val,lci=lci,uci=uci);
            dfr<-rbind(dfr,dfrp);
            #females
            val <-(lst[[case]]$rep)[[rep.f]];
            lci <- NA;
            uci <- NA;
            dfrp<-data.frame(case=case,
                             x='female',z=(lst[[case]]$rep)$mod.zBs,
                             val=val,lci=lci,uci=uci);
            dfr<-rbind(dfr,dfrp);
        } else {
            #males
            name<-std.m;
            idx<-lst[[case]]$std$name==name;
            val <-(lst[[case]]$std$value)[idx];
            lci <- val-(lst[[case]]$std$stdv)[idx];
            uci <- val+(lst[[case]]$std$stdv)[idx];
            dfrp<-data.frame(case=case,
                             x='male',z=(lst[[case]]$rep)$mod.zBs,
                             val=val,lci=lci,uci=uci);
            dfr<-rbind(dfr,dfrp);
            #females
            name<-std.f;
            idx<-lst[[case]]$std$name==name;
            val <-(lst[[case]]$std$value)[idx];
            lci <- val-(lst[[case]]$std$stdv)[idx];
            uci <- val+(lst[[case]]$std$stdv)[idx];
            dfrp<-data.frame(case=case,
                             x='female',z=(lst[[case]]$rep)$mod.zBs,
                             val=val,lci=lci,uci=uci);
            dfr<-rbind(dfr,dfrp);
        }
    }#--case
    return(dfr);
}
