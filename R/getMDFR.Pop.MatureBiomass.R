#'
#'@title Get predicted mature biomass (time series) from several model runs
#'
#'@description Function to get predicted mature biomass (time series) from 
#'several model runs.
#'
#'@param obj - object with results for the models to be compared that can be converted to a list of tcsam2013.resLst objects
#'@param verbose - flag (T/F) to print debug info
#'
#'@details Uses \code{reshape2::melt} and \code{reshape2::dcast}. If obj is a tcsam2013.lst
#'object, then results will be extracted from the tcsam2013.std object for a given case, if it exists; 
#'otherwise they will be extracted from the tcsam2013.rep object.
#'
#'@return dataframe in canonical format
#'
#'@export
#'
getMDFR.Pop.MatureBiomass<-function(obj,
                                verbose=FALSE){
    options(stringsAsFactors=FALSE);

    lst<-convertToListOfResults(obj);
    cases<-names(lst);
    
    tinfo<-getTimeInfo(lst);
    styr<-tinfo$styr;
    endyr<-tinfo$endyr;
    years<-tinfo$years;
    years.m1<-tinfo$years.m1;
    
    
    #----------------------------------
    #Mating Biomass (1000's t)
    #----------------------------------
    dfr<-NULL;
    for (case in cases){
        if (is.null(lst[[case]]$std)) {
            #--males
            val <-(lst[[case]]$rep)[["pop.MMB"]];
            lci <- NA;
            uci <- NA;
            dfrp<-data.frame(case=case,
                             y=years.m1[[case]],x='male',m='mature',
                             val=val,lci=lci,uci=uci);
            dfr<-rbind(dfr,dfrp);
            #--females
            val <-(lst[[case]]$rep)[["pop.MFB"]];
            lci <- NA;
            uci <- NA;
            dfrp<-data.frame(case=case,
                             y=years.m1[[case]],x='female',m='mature',
                             val=val,lci=lci,uci=uci);
            dfr<-rbind(dfr,dfrp);
        } else {
            #--males
            name<-"sdrMMB";
            idx<-lst[[case]]$std$name==name;
            val <-(lst[[case]]$std$value)[idx];
            lci <- val - (lst[[case]]$std$stdv)[idx];
            uci <- val + (lst[[case]]$std$stdv)[idx];
            dfrp<-data.frame(case=case,
                             y=years.m1[[case]],x='male',m='mature',
                             val=val,lci=lci,uci=uci);
            dfr<-rbind(dfr,dfrp);
            #--females
            name<-"sdrMFB";
            idx<-lst[[case]]$std$name==name;
            val <-(lst[[case]]$std$value)[idx];
            lci <- val - (lst[[case]]$std$stdv)[idx];
            uci <- val + (lst[[case]]$std$stdv)[idx];
            dfrp<-data.frame(case=case,
                             y=years.m1[[case]],x='female',m='mature',
                             val=val,lci=lci,uci=uci);
            dfr<-rbind(dfr,dfrp);
        }
    }#--case
    dfr<-rCompTCMs::getMDFR.CanonicalFormat(dfr);
    dfr$process<-"population";
    return(dfr);
}
