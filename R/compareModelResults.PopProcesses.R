#'
#'@title Compare predicted population processes among several model runs
#'
#'@description Function to compare predicted population processes among 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - name for output pdf file
#'
#'@details None
#'
#'@return list of ggplot2 objects, returned invisibly.
#'
#'@importFrom wtsUtilities printGGList
#'@import ggplot2
#'
#'@export
#'
compareModelResults.PopProcesses<-function(obj,
                                           showPlot=FALSE,
                                           pdf=NULL){

    cases<-names(obj);
    
    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }
    
    plots<-list();#output list
    figno<-1;
    
    #-------------------------------------------#
    #plot natural mortality
    #-------------------------------------------#
    p<-compareModelResults.NatMort(obj,showPlot=FALSE);
    cap<-"  \n  \nFigure &&fno.Estimated natural mortality rates.  \n  \n";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;

    #-------------------------------------------#
    #plot prM2M
    #-------------------------------------------#
    p<-compareModelResults.PrM2M(obj,showPlot=FALSE);
    cap<-"  \n  \nFigure &&fno. Estimated probabilities of molt-to-maturity at size (mm CW).  \n  \n";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;

    #-------------------------------------------#
    #plot mean growth
    #-------------------------------------------#
    p<-compareModelResults.MeanGrowth(obj,showPlot=FALSE);
    if (showPlot) print(p);
    cap<-"  \n  \nFigure &&fno. Estimated mean growth patterns.  \n  \n";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;

    #-------------------------------------------#
    #plot growth transition matrices
    #-------------------------------------------#
    p<-compareModelResults.GrowthMatrices(obj);
    cap<-paste0("  \n  \nFigure &&fno. Estimated growth transition matrix.  \n  \n");
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;

    #-------------------------------------------#
    #plot recruitment size distribution
    #-------------------------------------------#
    dfr<-getMDFR.Pop.Processes(obj,type="R_cz");
    dfr$case<-factor(dfr$case,levels=cases);
    p <- ggplot(dfr,aes_string(x='z',y='val',colour='case'));
    p <- p + geom_line();
    p <- p + geom_point();
    p <- p + ylim(c(0,NA));
    p <- p + labs(x="size (mm CW)",y="recruitment size distribution");
    cap<-"  \n  \nFigure &&fno. Estimated/assumed size distribution at recruitment to the model.  \n  \n";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    return(invisible(plots));
}
