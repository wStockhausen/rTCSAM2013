#'
#'@title Compare estimated/predicted TCF male selectivity functions among several model runs
#'
#'@description Function to compare estimated/predicted TCF male selectivity functions among 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - name for output pdf file
#'
#'@details Uses \code{getMDFR.FisheryQuantities}.
#'
#'@importFrom wtsUtilities printGGList
#'
#'@return list of ggplot2 objects
#'
#'@importFrom wtsUtilities printGGList
#'@import ggplot2
#'
#'@export
#'
compareModelResults.FisheryTCFMaleSelFcns<-function(obj,
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
    # plot selectivity functions
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,'selfcns');
    dfrp$case<-factor(dfrp$case,levels=cases);
    dfrp$pc<-as.character(dfrp$pc);
    idc<-(dfrp$category=="selectivity");
    for (fsh in c('TCF')){
        idx<-(dfrp$fishery==fsh);
        idt<-(dfrp$pc %in% as.character(1991:2030))
        dfrpp<-dfrp[idx&idc&idt,];
        dfrppp<-dfrp[idx&idc&(dfrp$pc=='1990'),c("z","val","case")];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='z',y='val',colour="case"));
        #p <- p + annotate("line",x=dfrppp$z,y=dfrppp$val,colour=dfrppp$case,linetype=2);
        p <- p + geom_line();
        p <- p + labs(y="selectivity",x="size (mm CW)");
        p <- p + guides(colour=guide_legend("case"));
        p <- p + facet_wrap(~pc,ncol=3);
        cap<-paste0("  \n  \nFigure &&fno. Estimated selectivity functions for total catch in",fsh,". Recent time period.  \n  \n");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    return(invisible(plots));
}