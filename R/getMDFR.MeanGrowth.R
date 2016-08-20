#'
#'@title Get predicted mean growth from several model runs
#'
#'@description Function to get predicted mean growth from 
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
getMDFR.meanGrowth<-function(obj,
                             verbose=FALSE){

    dfr<-getMDFR.XZ(obj,
                    rep.m="pop.grw.mnPMZ.M",
                    rep.f="pop.grw.mnPMZ.F",
                    std.m="sdrMnGrw_M",
                    std.f="sdrMnGrw_F",
                    verbose=verbose);
    return(dfr);
}
