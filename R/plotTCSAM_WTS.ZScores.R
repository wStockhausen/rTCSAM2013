#'
#'@title Plot z-scores from a TCSAM2013 model run
#'
#'@description Function to plot z-scores from a TCSAM2013 model run.
#'
#'@param res - TCSAM_WTS-type list
#'@param facet.scales - flag to control scales for bycatch fisheries ('fixed','free_y')
#'@param showPlot - flag (T/F) to show plots immediately
#'
#'@details Uses \code{reshape2::melt()}.
#'
#'@return list with ggplot2 objects as elements:
#'\itemize{
#'  \item srv - plot for survey
#'  \item fsh - plot for fisheries
#'}
#'
#'@import ggplot2
#'
#'@export
#'
plotTCSAM_WTS.ZScores<-function(res=NULL,
                                facet.scales='fixed',
                                showPlot=FALSE){
    #----------------------------------
    # Load files, if necessary
    if(!is.list(res)){
        res<-getWTS(res);
    }
    
    ##plot z-scores for survey mature biomass
    tmp<-res$srv$fits$zscr;
    zscrs<-reshape2::melt(tmp,value.name="zscore");
    xmax<-max(zscrs$y,na.rm=TRUE);
    pSrv<-plotZScores(zscrs,color='x',shape='x',legend='sex',
                      facets=NULL,facet.scales='fixed',position='dodge',
                      ylab='z-score',title='fits to mature survey biomass',
                      xlims=c(1975,xmax+1),
                      showPlot=showPlot);
    
    ##plot z-scores for TCF catches
    zscrs<-NULL;
    ##TCF retained
    tmp<-res$fsh$TCFR$fits$zscr;
    dfr<-reshape2::melt(tmp,value.name="zscore");
    dfr$class<-'male retained'; dfr$fishery<-'TCF'; dfr$type<-'retained';dfr$x<-'male retained';
    dfr<-dfr[,c('fishery','type','class','x','y','zscore')];
    zscrs<-rbind(zscrs,dfr); 
    ##TCF male total (bycatch + retained)
    tmp<-res$fsh$TCFM$fits$zscr;
    dfr<-reshape2::melt(tmp,value.name="zscore");
    dfr$class<-'male total'; dfr$fishery<-'TCF'; dfr$type<-'total';dfr$x<-'male total';
    dfr<-dfr[,c('fishery','type','class','x','y','zscore')];
    zscrs<-rbind(zscrs,dfr); 
    ##TCF female bycatch
    tmp<-res$fsh$TCFF$fits$zscr;
    dfr<-reshape2::melt(tmp,value.name="zscore");
    dfr$class<-'female bycatch'; dfr$fishery<-'TCF'; dfr$type<-'bycatch';dfr$x<-'female bycatch';
    dfr<-dfr[,c('fishery','type','class','x','y','zscore')];
    zscrs<-rbind(zscrs,dfr); 
    # pTCF<-plotZScores(
    #         zscrs,color='class',shape='class',legend='type',
    #         facets=NULL,facet.scales='fixed',position='dodge',
    #         ylab='z-score',title='fits to TCF catch mortality',
    #         showPlot=showPlot);
    # 
    # ##plot z-scores for other fishery catches
    # zscrs<-NULL;
    ##SCF
    tmp<-res$fsh$SCF$fits$zscr;
    dfr<-reshape2::melt(tmp,value.name="zscore");
    dfr$class<-'SCF'; dfr$fishery<-'SCF'; dfr$type<-'bycatch'; dfr$x<-tolower(paste(dfr$x,'bycatch'));
    dfr<-dfr[,c('fishery','type','class','x','y','zscore')];
    zscrs<-rbind(zscrs,dfr); 
    ##RKF
    tmp<-res$fsh$RKF$fits$zscr;
    dfr<-reshape2::melt(tmp,value.name="zscore");
    dfr$class<-'RKF'; dfr$fishery<-'RKF'; dfr$type<-'bycatch'; dfr$x<-tolower(paste(dfr$x,'bycatch'));
    dfr<-dfr[,c('fishery','type','class','x','y','zscore')];
    zscrs<-rbind(zscrs,dfr); 
    ##GTF
    tmp<-res$fsh$GTF$fits$zscr;
    dfr<-reshape2::melt(tmp,value.name="zscore");
    dfr$class<-'GTF'; dfr$fishery<-'GTF'; dfr$type<-'bycatch'; dfr$x<-'combined-sex bycatch';
    dfr<-dfr[,c('fishery','type','class','x','y','zscore')];
    zscrs<-rbind(zscrs,dfr); 
    
    xmax<-max(zscrs$y,na.rm=TRUE);
    pFsh<-plotZScores(zscrs,color='x',shape='x',legend='sex',
                      facets='fishery~.',facet.scales=facet.scales,position='identity',
                      ylab='z-score',title='fits to fishery catch/mortality',
                      xlims=c(1965,xmax+1),
                      showPlot=showPlot);
    
    return(list(srv=pSrv,fsh=pFsh));
}

