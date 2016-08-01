#'
#'@title Get predicted natural mortality (time series) from several model runs
#'
#'@description Function to get predicted natural mortality (time series) from 
#'several model runs.
#'
#'@param obj - object with results for the models to be compared that can be converted to a list of tcsam2013.resLst objects
#'@param verbose - flag (T/F) to print debug info
#'
#'@details Uses \code{reshape2::melt} and \code{reshape2::dcast}. If obj is a list of tcsam2013.resLst objects,
#'then results will be extracted from the tcsam2013.std object for a given case, if it exists; 
#'otherwise they will be extracted from the tcsam2013.rep object.
#'
#'@return dataframe
#'
#'@export
#'
getMDFR.NaturalMortality<-function(obj,
                                   verbose=FALSE){

    lst<-convertToListOfResults(obj);
    cases<-names(lst);
    
    tinfo<-getTimeInfo(lst);
    years<-tinfo$years;

    
    #----------------------------------
    #Natural Mortality
    #----------------------------------
    rws<-list();
    rws$INF<-list(m='immature',s='new shell',x='female');
    rws$MNF<-list(m=  'mature',s='new shell',x='female');
    rws$MOF<-list(m=  'mature',s='old shell',x='female');
    rws$INM<-list(m='immature',s='new shell',x=  'male');
    rws$MNM<-list(m=  'mature',s='new shell',x=  'male');
    rws$MOM<-list(m=  'mature',s='old shell',x=  'male');
    dfr<-NULL;
    for (case in cases){
        if (is.null(lst[[case]]$std)) {
            for (nm in names(rws)){
                rw<-rws[[nm]];
                val <-(lst[[case]]$rep)[[paste0("pop.M.",nm)]];
                lci <- NA;
                uci <- NA;
                dfrp<-data.frame(case=case,y=years[[case]],
                                 x=rw$x,m=rw$m,s=rw$s,
                                 val=val,lci=lci,uci=uci);
                dfr<-rbind(dfr,dfrp);
            }#--nm
        } else {
            for (nm in names(rws)){
                name<-paste0("sdrNatMort_",nm);
                rw<-rws[[nm]];
                idx<-lst[[case]]$std$name==name;
                val <-(lst[[case]]$std$value)[idx];
                lci <- val - (lst[[case]]$std$stdv)[idx];
                uci <- val + (lst[[case]]$std$stdv)[idx];
                dfrp<-data.frame(case=case,y=years[[case]],
                                 x=rw$x,m=rw$m,s=rw$s,
                                 val=val,lci=lci,uci=uci);
                dfr<-rbind(dfr,dfrp);
            }
        }
    }#--case
    return(dfr);
}
