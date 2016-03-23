#'
#'@title Plot TCSAM_WTS...R output from a TCSAM2013 model run
#'
#'@description Function plot TCSAM_WTS...R output from a TCSAM2013 model run.
#'
#'@param res - TCSAM_WTS-type list
#'@param zscores.facet.scales - flag to control zscore scales for bycatch fisheries ('fixed','free_y')
#'@param showPlot - flag (T/F) to show plots immediately
#'
#'@return list of listsnwith ggplot2 objects as elements.
#'
#'@export
#'
plotTCSAM_WTS<-function(res=NULL,
                        zscores.facet.scales='fixed',
                        showPlot=FALSE){
    #----------------------------------
    # Load files, if necessary
    if(!is.list(res)){
        if (!is.character(res)){
            inp<-wtsUtilities::selectFile(ext="R",caption="Select TCSAM_WTS...R output file");
            base.dir=dirname(inp);
            if (is.null(mdl)) {mdl<-strsplit(basename(inp),".",fixed=TRUE)[[1]][1];}
        } else {
            inp<-res;
        }
        source(file=inp,local=TRUE,echo=FALSE);##create 'res' list object
    }
    
    ##plot z-scores
    zscores<-plotTCSAM_WTS.ZScores(res,
                                   facet.scales=zscores.facet.scales,
                                   showPlot=showPlot);
    
    return(list(zscores=zscores));
}

