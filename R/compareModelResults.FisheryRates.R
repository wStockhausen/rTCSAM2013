#'
#'@title Compare estimated/predicted fishery capture/mortality rates among several model runs
#'
#'@description Function to compare estimated/predicted fishery capture/mortality rates among 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param numRecent - number of recent years to plot
#'@param plot1stObs - flag to plot observations from the first case, only
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - name for output pdf file
#'
#'@details Uses \code{getMDFR.FisheryQuantities}.
#'
#'@return list of ggplot2 objects
#'
#'@importFrom wtsUtilities printGGList
#'@import ggplot2
#'
#'@export
#'
compareModelResults.FisheryRates<-function(obj,
                                           numRecent=15,
                                           plot1stObs=TRUE,
                                           showPlot=FALSE,
                                           pdf=NULL){
    
    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }
    plots<-list();#output list
    
    
    #----------------------------------
    # convert to list of ResLst objects
    #----------------------------------
    obj<-convertToListOfResults(obj);
    
    #----------------------------------
    # pull out generic info
    #----------------------------------
    cases<-names(obj);
    tinfo<-getTimeInfo(obj);
    endyr<-tinfo$endyr;
    
    #----------------------------------
    # define output list of plots
    #----------------------------------
    plots<-list();
    figno<-1;
    
    #----------------------------------
    # plot max fishing mortality/capture rates
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,'max rates');
    dfrp$case<-factor(dfrp$case,levels=cases);
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx<-(dfrp$fishery==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='val',colour="case",shape="category",linetype="category"));
        p <- p + geom_line();
        p <- p + geom_point();
        p <- p + labs(y="max rate",x="year");
        p <- p + guides(colour=guide_legend("case"),
                        shape=guide_legend("category"),
                        linetype=guide_legend("category"));
        p <- p + facet_grid(x~fishery);
        cap<-paste0("  \n  \nFigure &&fno. Estimated max fishery rates in ",fsh,".  \n  \n");
        if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
        #zoomed to recent
        dfrpp<-dfrp[idx&(dfrp$y>=(max(dfrp$y)-15)-numRecent),];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='val',colour="case",shape="category",linetype="category"));
        p <- p + geom_line();
        p <- p + geom_point();
        p <- p + labs(y="max rate",x="year");
        p <- p + guides(colour=guide_legend("case"),
                        shape=guide_legend("category"),
                        linetype=guide_legend("category"));
        p <- p + facet_grid(x~fishery);
        cap<-paste0("  \n  \nFigure &&fno. Estimated max fishery rates in ",fsh," (zoomed to recent years).  \n  \n");
        if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot mean fishing mortality/capture rates
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,'mean rates');
    dfrp$case<-factor(dfrp$case,levels=cases);
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx<-(dfrp$fishery==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='val',colour="case",shape="category",linetype="category"));
        p <- p + geom_line();
        p <- p + geom_point();
        p <- p + labs(y="mean rate",x="year");
        p <- p + guides(colour=guide_legend("case"),
                        shape=guide_legend("category"),
                        linetype=guide_legend("category"));
        p <- p + facet_grid(x~fishery);
        cap<-paste0("  \n  \nFigure &&fno. Estimated mean fishery rates in ",fsh,".  \n  \n");
        if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
        #zoom to recent
        dfrpp<-dfrp[idx&(dfrp$y>=(max(dfrp$y)-15)-numRecent),];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='val',colour="case",shape="category",linetype="category"));
        p <- p + geom_line();
        p <- p + geom_point();
        p <- p + labs(y="mean rate",x="year");
        p <- p + guides(colour=guide_legend("case"),
                        shape=guide_legend("category"),
                        linetype=guide_legend("category"));
        p <- p + facet_grid(x~fishery);
        cap<-paste0("  \n  \nFigure &&fno. Estimated mean fishery rates in ",fsh," (zoomed to recent years).  \n  \n");
        if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    return(invisible(plots));
}