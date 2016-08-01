#'
#'@title Get time information for a number of model runs
#'
#'@description Function to get time information for a number of model runs.
#'
#'@param obj - object to convert (see details)
#'
#'@details 'obj' can be one of the following:
#'\itemize{
#'  \item{a list of tcsam2013.resLst objects}
#'  \item{a single tcsam2013.resLst object}
#'  \item{a list of tcsam2013.rep objects}
#'  \item{a single tcsam2013.rep object}
#'}
#'
#'@return List with model time information
#'
#'@export
#'
getTimeInfo<-function(obj){
    lst<-convertToListOfResults(obj);
    cases<-names(lst);
    
    #set up time info
    endyr<-list();
    for (case in cases){
        if (!is.null(lst[[case]]$rep$mod.endyr)) {
            endyr[[case]]<-lst[[case]]$rep$mod.endyr;
        } else {            
            cat("'endyr' missing from rep file for case '",case,"'!\n",sep='')
            cat("Must set 'endyr' in rep file.\n","Aborting...\n",sep='');
            return(NULL);
        }
    }
    styr<-list();
    for (case in cases){
        if (!is.null(lst[[case]]$rep$mod.styr)) {
            styr[[case]]<-lst[[case]]$rep$mod.styr;
        } else {            
            cat("'styr' missing from rep file for case '",case,"'!\n",sep='')
            cat("Must set 'styr' in rep file.\n","Aborting...\n",sep='');
            return(NULL);
        }
    }
    obsyr<-list();
    for (case in cases){
        if (!is.null(lst[[case]]$rep$mod.obsyr)) {
            obsyr[[case]]<-lst[[case]]$rep$mod.obsyr;
        } else {            
            cat("'obsyr' missing from rep file for case '",case,"'!\n")
            cat("Must set 'obsyr' in rep file.\n","Aborting...\n");
            return(NULL);
        }
    }
    pltyr<-list();
    for (case in cases){
        if (!is.null(lst[[case]]$rep$mod.pltyr)) {
            pltyr[[case]]<-lst[[case]]$rep$mod.pltyr;
        } else {            
            cat("'pltyr' missing from rep file for case '",case,"'!\n")
            cat("Must set 'pltyr' in rep file.\n","Aborting...\n");
            return(NULL);
        }
    }

    years    <-list();
    years.m1 <-list();
    obsyears <-list();
    plotyears<-list();
    for (case in cases){
        years[[case]]    <-styr[[case]]:endyr[[case]];
        years.m1[[case]] <-styr[[case]]:(endyr[[case]]-1);
        obsyears[[case]] <-obsyr[[case]]:endyr[[case]];
        plotyears[[case]]<-pltyr[[case]]:endyr[[case]];
    }
    
    return(invisible(list(endyr=endyr,
                          styr=styr,
                          years=years,
                          years.m1=years.m1,
                          plotyears=plotyears,
                          obsyears=obsyears)));
}
