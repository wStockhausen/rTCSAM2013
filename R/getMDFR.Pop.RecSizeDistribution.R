#'
#'@title Get recruitment size distributions from several model runs
#'
#'@description Function to get recruitment size distributions from 
#'several model runs.
#'
#'@param obj - object with results for the models to be compared that can be converted to a list of tcsam2013.resLst objects
#'@param verbose - flag (T/F) to print debug info
#'
#'@details Uses \code{reshape2} package.
#'
#'@return dataframe in canonical format.
#'
#'@export
#'
getMDFR.Pop.RecSizeDistribution<-function(obj,
                                          verbose=FALSE){

    if (verbose) cat("rTCSAM2013::getMDFR.Pop.RecSizeDistribution: starting!\n");
    
    lst<-convertToListOfResults(obj);
    cases<-names(lst);
    
    tinfo<-getTimeInfo(lst);
    styr<-tinfo$styr;
    endyr<-tinfo$endyr;
    years<-tinfo$years;
    years.m1<-tinfo$years.m1;

    #----------------------------------
    #recruitment size distribution
    #----------------------------------
    dfr<-NULL;
    for (case in cases){
        pc<-paste0(styr[[case]],"-",endyr[[case]]);
        dfrp<-data.frame(case=case,pc=pc,y=pc,x='all',m='immature',s='new shell',
                         z=lst[[case]]$rep$mod.zBs,
                         val=(lst[[case]]$rep)[["pop.prR_z"]]);
        dfr<-rbind(dfr,dfrp);
    }
    mdfr<-getMDFR.CanonicalFormat(dfr);
    mdfr$type<-"population";
    
    
    if (verbose) cat("rTCSAM2013::getMDFR.Pop.RecSizeDistribution: Done!\n");
    return(mdfr);
}

