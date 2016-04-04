#'
#' @title Plot TCSAM2013 model results.
#' 
#' @description Function to plot model results from TCSAM2013.
#' 
#' @param obj.rep - report file list object or filename for R-style report
#' @param obj.std - dataframe object with parameter std info or filename for std file
#' @param obj.prs - dataframe object w/ parameters info or csv file with parameters info
#' @param obj.wts - TCSAM_WTS-type list object
#' @param base.dir - path to base folder
#' @param mdl   - model name (optional if R report file is read)
#' @param styr  - model start year
#' @param endyr - assessment year
#' @param obsyr - first year for survey observations
#' @param pltyr - first year for some plots
#' @param F35 - F35 value for control rule plot
#' @param B35 - B35 value for control rule plot
#' 
#' @return list with elements: \cr
#' \itemize{
#'  \item rep - obj.rep
#'  \item std - obj.std
#'  \item prs - obj.prs
#'  \item wts - obj.wts
#' }
#' 
#' @details Convenience wrapper for \code{plotTCSAM2013I()}. User is prompted
#' for any obj.??? inputs that are NULL.
#' 
#'@export
#' 
#----------------------------------
# Set model variables for plots
plotTCSAM2013<-function(obj.rep=NULL,
                        obj.std=NULL,
                        obj.prs=NULL,
                        obj.wts=NULL,
                        base.dir='.',
                        mdl='tcsam2013alta',#executable model name
                        endyr=NULL,    #assessment year
                        styr=NULL,     #model start year
                        obsyr=NULL,    #first year of survey observations
                        pltyr=1965,    #first year for plots
                        F35=0.73,      #F35 value
                        B35=33.54){    #B35 value

    #----------------------------------
    # Load files, if necessary
    if(!is.list(obj.rep)){
        obj.rep<-getRep(obj.rep);
    }
    if (!is.data.frame(obj.std)){
        obj.std<-getStd(obj.std);
    }
    if (is.character(obj.prs)){
        obj.prs<-getPrs(type=obj.prs);
    }
    if(!is.list(obj.wts)){
        obj.wts<-getRep(obj.wta);
    }
    
    #actually plot the results
    plotTCSAM2013I(obj.rep,
                   obj.std,
                   obj.prs,
                   obj.wts,
                   base.dir=base.dir,
                   mdl=mdl,        #executable model name
                   endyr=endyr,    #assessment year
                   styr=styr,      #model start year
                   obsyr=obsyr,    #first year of survey observations
                   pltyr=pltyr,    #first year for plots
                   F35=F35,        #F35 value
                   B35=B35);       #B35 value
    
    #return the objects
    return(invisible(list(rep=obj.rep,std=obj.std,prs=obj.prs,wts=obj.wts)));
}