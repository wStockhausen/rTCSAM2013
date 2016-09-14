#'
#'@title Compare Pearson's residuals from survey size comps among several model runs
#'
#'@description Function to compare Pearson's residuals from  survey size comps among 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param numRecent - number of recent years to plot
#'@param plot1stObs - flag to plot observations from the first case, only
#'@param ncol - number of columns/page for size comp plots
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - name for output pdf file
#'
#'@details Uses \code{getMDFR.SurveyQuantities}.
#'
#'@return non-nested list of ggplot2 objects, with captions as names
#'
#'@importFrom wtsUtilities printGGList
#'@import ggplot2
#'
#'@export
#'
compareModelResults.SurveyPRs<-function(obj=NULL,
                                        numRecent=15,
                                        plot1stObs=TRUE,
                                        ncol=5,
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
    # plot size comp residuals from the survey 
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,type="PRs_yxz");
    dfrp$case<-factor(dfrp$case,levels=cases);
    dfrp$sign<-ifelse(test=dfrp$val>0,yes=">0",no="<0");
    dfrp$val <- abs(dfrp$val);

    p <- ggplot(data=dfrp,mapping=aes_string(x='y',y='z',size='val',fill='sign'));
    p <- p + scale_size_area(max_size=10);
    p <- p + geom_point(alpha=0.8,shape=21,color='black');
    p <- p + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
    p <- p + labs(y="size (mm CW)",x="year") + ggtitle("Pearson's residuals");
    p <- p + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                      size=guide_legend(order=1));
    if (length(cases)==1){
        p <- p + facet_grid(x~.);
    } else {
        p <- p + facet_grid(case~x);
    }
    p <- p + theme(legend.box='horizontal')
    cap<-"  \n  \nFigure &&fno. Pearson's residuals for proportions-at-size from the survey.  \n  \n";
    if (showPlot) figno<-(printGGList(ps,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    return(invisible(plots));
}