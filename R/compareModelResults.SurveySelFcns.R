#'
#'@title Compare estimated/predicted survey selectivity functions among several model runs
#'
#'@description Function to compare estimated/predicted survey selectivity functions among 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - name for output pdf file
#'
#'@details Uses \code{getMDFR.SurveyQuantities}.
#'
#'@return non-nested list of ggplot2 objects, with captions as names
#'
#'@export
#'
compareModelResults.SurveySelFcns<-function(obj=NULL,
                                            showPlot=FALSE,
                                            pdf=NULL){
    
    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }
    
    #----------------------------------
    # 
    #----------------------------------
    obj<-convertToListOfResults(obj);
    cases<-names(obj);
    
    #----------------------------------
    # define output list of plots
    #----------------------------------
    plots<-list();
    figno<-1;
    
    #----------------------------------
    # plot survey selectivity functions
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,'selSrv_cxz');
    dfrp$case<-factor(dfrp$case,levels=cases);
    dfrp$pc<-as.character(dfrp$pc);
    p <- ggplot(data=dfrp,mapping=aes_string(x='z',y='val',colour="case",linetype='pc'));
    p <- p + geom_line();
    p <- p + labs(y="selectivity",x="size (mm CW)");
    p <- p + guides(linetype=guide_legend("time period"));
    p <- p + facet_grid(x~.);
    cap<-"Figure &&fno. Estimated selectivity functions for the survey.";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    return(invisible(plots));
}