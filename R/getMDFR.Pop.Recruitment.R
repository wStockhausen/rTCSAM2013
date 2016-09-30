#'
#'@title Get predicted recruitment (time series) from several model runs
#'
#'@description Function to get predicted recruitment (time series) from 
#'several model runs.
#'
#'@param obj - object with results for the models to be compared that can be converted to a list of tcsam2013.resLst objects
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
getMDFR.Pop.Recruitment<-function(obj,
                                verbose=FALSE){

    lst<-convertToListOfResults(obj);
    cases<-names(lst);
    
    tinfo<-getTimeInfo(lst);
    styr<-tinfo$styr;
    endyr<-tinfo$endyr;
    years<-tinfo$years;
    years.m1<-tinfo$years.m1;
    
    
    #----------------------------------
    #Recruitment (millions)
    #----------------------------------
    dfr<-NULL;
    for (case in cases){
        if (is.null(lst[[case]]$std)) {
            val <-(lst[[case]]$rep)[["pop.R"]];
            lci <- NA;
            uci <- NA;
            dfrp<-data.frame(case=case,
                             y=years[[case]],x='all',
                             val=val,lci=lci,uci=uci);
            dfr<-rbind(dfr,dfrp);
        } else {
            name<-"sdrLnRec";
            idx<-lst[[case]]$std$name==name;
            val <-exp((lst[[case]]$std$value)[idx]);
            lci <- val/exp((lst[[case]]$std$stdv)[idx]);
            uci <- val*exp((lst[[case]]$std$stdv)[idx]);
            dfrp<-data.frame(case=case,
                             y=years[[case]],x='all',
                             val=val,lci=lci,uci=uci);
            dfr<-rbind(dfr,dfrp);
        }
    }#--case
    dfrp<-getMDFR.CanonicalFormat(dfr);
    dfrp$type<-'population';
    dfrp$m<-'immature';
    dfrp$s<-'new shell';
    return(dfrp);
}
