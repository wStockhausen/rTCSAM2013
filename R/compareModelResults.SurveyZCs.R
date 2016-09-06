#'
#'@title Compare estimated/predicted survey size comps among several model runs
#'
#'@description Function to compare estimated/predicted survey size comps among 
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
#'@export
#'
compareModelResults.SurveyZCs<-function(obj=NULL,
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
    # plot observed size comps from the survey as bubble plots
    #----------------------------------
    if (length(cases)==1){
        dfrp<-getMDFR.SurveyQuantities(obj,type="prNatZ_yxz");
        idxo<-dfrp$category=="observed";
        if (plot1stObs) idxo<-idxo & (dfrp$case==cases[1]);
    
        p <- ggplot(data=dfrp[idxo,],
                    mapping=aes_string(x='y',y='z',size='val',fill='category'));
        p <- p + scale_size_area(max_size=10);
        p <- p + geom_point(alpha=0.8,shape=21,color='black');
        p <- p + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
        p <- p + labs(y="size (mm CW)",x="year") + ggtitle("Observed proportions");
        p <- p + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                          size=guide_legend(order=1));
        p <- p + facet_grid(x~.)
        p <- p + theme(legend.box='horizontal')
        cap<-"Figure &&fno. Observed proportions-at-size from the survey by sex.";
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }
    
    #----------------------------------
    # plot observed and predicted size comps from the survey as line plots
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,type="prNatZ_yxz");
    dfrp$case<-factor(dfrp$case,levels=cases);
    idxm<-dfrp$x=="male";
    idxf<-dfrp$x=="female";
    idxp<-dfrp$category=="predicted";
    idxo<-dfrp$category=="observed";
    if (plot1stObs) idxo<-idxo&(dfrp$case==cases[1]);

    p1 <- ggplot(mapping=aes_string(x='z',y='val'));
    p1 <- p1 + geom_bar(data=dfrp[idxo&idxm,],stat='identity',position='identity',mapping=aes_string(fill='category'),colour=NA);
    p1 <- p1 + geom_line(data=dfrp[idxp&idxm,],mapping=aes_string(colour='case'));
    p1 <- p1 + facet_wrap(~y,ncol=ncol);
    p1 <- p1 + labs(x="size (mm CW)",y="proportion") + ggtitle("males");
    cap1<-"Figure &&fno. Observed and predicted proportions-at-size for males from the survey.";
    if (showPlot) figno<-(printGGList(p1,figno=figno,cap=cap1))$figno;
    plots[[cap1]]<-p1; p1<-NULL;
    
    p2 <- ggplot(data=dfrp,mapping=aes_string(x='z',y='val'));
    p2 <- p2 + geom_bar(data=dfrp[idxo&idxf,],stat='identity',position='identity',mapping=aes_string(fill='category'),colour=NA);
    p2 <- p2 + geom_line(data=dfrp[idxp&idxf,],mapping=aes_string(colour='case'));
    p2 <- p2 + facet_wrap(~y,ncol=ncol);
    p2 <- p2 + labs(x="size (mm CW)",y="proportion") + ggtitle("females");
    cap2<-"Figure &&fno. Observed and predicted proportions-at-size for females from the survey.";
    if (showPlot) figno<-(printGGList(p2,figno=figno,cap=cap2))$figno;
    plots[[cap2]]<-p2; p2<-NULL;
    
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
    cap<-"Figure &&fno. Pearson's residuals for proportions-at-size from the survey.";
    if (showPlot) figno<-(printGGList(ps,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    return(invisible(plots));
}