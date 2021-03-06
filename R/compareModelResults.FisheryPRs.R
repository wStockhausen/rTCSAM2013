#'
#'@title Compare Pearson's residuals from fishery size comps among several model runs
#'
#'@description Function to Pearson's residuals from fishery size comps among 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
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
compareModelResults.FisheryPRs<-function(obj,
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
    # plot retained catch size comp residuals 
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,type="PRs.ret");
    dfrp$case<-factor(dfrp$case,levels=cases);
    dfrp$sign<-ifelse(test=dfrp$val>0,yes=">0",no="<0");
    dfrp$val <- abs(dfrp$val);
    for (fsh in c('TCF')){
        idx<-(dfrp$fleet==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='z',size='val',fill='sign',colour='case',linetype='case'));
        p <- p + scale_size_area(max_size=10);
        p <- p + geom_point(alpha=0.8,shape=21,color='black');
        p <- p + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
        p <- p + labs(y="size (mm CW)",x="year") + ggtitle(paste0(fsh,": retained catch Pearson's residuals"));
        p <- p + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                          size=guide_legend(order=1));
        if (length(cases)==1){
            p <- p + facet_grid(x~.);
        } else {
            p <- p + facet_grid(case~x);
        }
        p <- p + theme(legend.box='horizontal');
        cap<-paste0("  \n  \nFigure &&fno. Pearson's residuals for retained catch proportions-at-size in ",fsh,".  \n  \n");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot total catch size comp residuals 
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,type="PRs.tot");
    dfrp$case<-factor(dfrp$case,levels=cases);
    dfrp$sign<-ifelse(test=dfrp$val>0,yes=">0",no="<0");
    dfrp$val <- abs(dfrp$val);
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx<-(dfrp$fleet==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='z',size='val',fill='sign'));
        p <- p + scale_size_area(max_size=10);
        p <- p + geom_point(alpha=0.8,shape=21,color='black');
        p <- p + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
        p <- p + labs(y="size (mm CW)",x="year") + ggtitle(paste0(fsh,": total catch Pearson's residuals"));
        p <- p + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                          size=guide_legend(order=1));
        if (length(cases)==1){
            p <- p + facet_grid(x~.);
        } else {
            p <- p + facet_grid(case~x);
        }
        p <- p + theme(legend.box='horizontal');
        cap<-paste0("  \n  \nFigure &&fno. Pearson's residuals for total catch proportions-at-size in ",fsh,".  \n  \n");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    return(invisible(plots));
}