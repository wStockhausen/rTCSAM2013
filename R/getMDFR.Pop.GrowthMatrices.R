#'
#'@title Get predicted growth matrices from several model runs
#'
#'@description Function to get predicted growth matrices from 
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
getMDFR.Pop.GrowthMatrices<-function(obj,
                                     verbose=FALSE){

    lst<-convertToListOfResults(obj);
    cases<-names(lst);
    
    tinfo<-getTimeInfo(lst);
    styr<-tinfo$styr;
    endyr<-tinfo$endyr;
    years<-tinfo$years;
    years.m1<-tinfo$years.m1;

    #----------------------------------
    #growth transition matrices
    #----------------------------------
    rws<-list();
    rws[["M"]]<-list(x=  'male');
    rws[["F"]]<-list(x='female');
    dfr<-NULL;
    for (case in cases){
        pc<-paste0(styr[[case]],"-",endyr[[case]]);
        for (nm in names(rws)) {
            rw<-rws[[nm]];
            val=(lst[[case]]$rep)[[paste0("pop.grw.prGr_xzz.",nm)]];
            dimnames(val)<-list(z =as.character(lst[[case]]$rep$mod.zBs),
                                zp=as.character(lst[[case]]$rep$mod.zBs));
            dfrp<-reshape2::melt(val,value.name='val')
            dfrp<-cbind(case=case,
                        pc=pc,y=pc,x=rw$x,m='immature',s='all',dfrp);
            dfr<-rbind(dfr,dfrp);
        }
    }
    dfr<-getMDFR.CanonicalFormat(dfr);
    dfr$type<-"population";
    return(dfr);
}

