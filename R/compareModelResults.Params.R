#'
#'@title Function to compare parameter values from different models
#'
#'@description This function extracts and plots parameters values from several models, 
#'together with their limits(if any) and their estimated standard errors (if any).
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param dp - percent difference between parameter value and upper/lower limits used to flag outliers
#'@param showPlot - flag to show plots
#'@param pdf - file name for printing plots to a pdf file (or NULL to print to screen)
#'
#'@return - list of lists with ggplot2 objects as elements
#'
#'@details Uses \code{getMDFR.ParamsPlusStdDevs()}. Returned list has top level elements:
#'\itemize{
#'  \item{population}
#'  \item{surveys}
#'  \item{fisheries}
#'}
#'
#'@import ggplot2
#'
#'@export
#'
compareModelResults.Params<-function(obj,
                                     dp=0.01,
                                     showPlot=FALSE,
                                     pdf=NULL){
    
    #----------------------------------
    # 
    #----------------------------------
    obj<-convertToListOfResults(obj);
    cases<-names(obj);
    
    #extract dataframe with parameter estimates and info
    cat('Extracting params info\n')
    dfr<-getMDFR.ParamsPlusStdDevs(obj,dp=dp);
    dfr$case<-factor(dfr$case,levels=cases);

    dfr$label <- gsub("\\n","\n",dfr$label,fixed=TRUE);
    
    #plot parameter estimates
    plots<-list();
    figno<-1;
    dodge<-position_dodge(width=1/length(cases));
    for (ctg in c('population','surveys','fisheries')){
        dfrp<-dfr[dfr$category==ctg,];
        for (prc in as.character(unique(dfrp$process))){
            dfrpp<-dfrp[dfrp$process==prc,];
            dfrpp$uci<-dfrpp$value+dfrpp$stdv;
            dfrpp$lci<-dfrpp$value-dfrpp$stdv;
            np<-length(unique(dfrpp$label));
            nc<-floor(sqrt(np)); 
            p <- ggplot(dfrpp);
            p <- p + geom_rect(mapping=aes_string(ymin='min',ymax='max'),xmin=I(-1),xmax=I(1),alpha=0.5,fill='grey');
            p <- p + geom_hline(aes_string(yintercept='min', colour='case'),linetype=2,size=1,alpha=0.7,show.legend=FALSE)
            p <- p + geom_hline(aes_string(yintercept='max', colour='case'),linetype=2,size=1,alpha=0.7,show.legend=FALSE)
            p <- p + geom_errorbar(aes_string(x=0,ymin='lci',ymax='uci',colour='case'),position=dodge,width=0.2,show.legend=FALSE)
            p <- p + geom_point(aes_string(x=0,y='init', colour='case'),fill=NA,position=dodge,shape=25,size=4,show.legend=FALSE);
            p <- p + geom_point(aes_string(x=0,y='value',colour='case',fill='case'),position=dodge,shape=21,size=3);
            p <- p + guides(colour=guide_legend());
            p <- p + scale_x_continuous(breaks=NULL);
            p <- p + labs(y='parameter value',x='',title=prc);
            p <- p + facet_wrap(~label,ncol=nc,drop=FALSE,scales="free_y");
            cap<-paste0("Figure &&fno. Estimated ",ctg," parameters for ",prc,".\n");
            if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
            plots[[cap]]<-p; p<-NULL;
        }
    } #ctg's

    return(invisible(plots));
}
