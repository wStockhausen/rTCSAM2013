#'
#'@title Plot TCSAM_WTS...R output from a TCSAM2013 model run
#'
#'@description Function plot TCSAM_WTS...R output from a TCSAM2013 model run.
#'
#'@param res - TCSAM_WTS-type list
#'@param zscores.facet.scales - flag to control zscore scales for bycatch fisheries ('fixed','free_y')
#'@param showPlot - flag (T/F) to show plots immediately
#'
#'@return list of lists with ggplot2 objects as elements:
#'\itemize{
#'  \item zscores - srv, fsh plots
#'}
#'
#'@details None.
#'
#'@export
#'
plotTCSAM_WTS<-function(res=NULL,
                        zscores.facet.scales='fixed',
                        showPlot=FALSE){
    #----------------------------------
    # Load files, if necessary
    if(!is.list(res)){
        res<-getWTS(res);
    }
    
    ##plot z-scores
    pZscores<-plotTCSAM_WTS.ZScores(res,
                                   facet.scales=zscores.facet.scales,
                                   showPlot=showPlot);
    
    return(list(zscores=pZscores));
}

