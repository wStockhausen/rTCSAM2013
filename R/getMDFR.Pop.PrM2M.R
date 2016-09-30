#'
#'@title Get predicted pr(M2M) from several model runs
#'
#'@description Function to get predicted pr(M2M) from 
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
getMDFR.Pop.PrM2M<-function(obj,
                       verbose=FALSE){

    dfr<-getMDFR.XZ(obj,
                    rep.m="pop.prM2M.M",
                    rep.f="pop.prM2M.F",
                    std.m="sdrPrM2M_M",
                    std.f="sdrPrM2M_F",
                    verbose=verbose);
    dfrp<-getMDFR.CanonicalFormat(dfr);
    dfrp$type<-'population';
    dfrp$m<-'immature';
    return(dfrp);
}
