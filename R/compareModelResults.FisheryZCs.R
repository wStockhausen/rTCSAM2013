#'
#'@title Compare estimated/predicted fishery size comps among several model runs
#'
#'@description Function to compare estimated/predicted fishery size comps among 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param numRecent - number of recent years to plot
#'@param plot1stObs - flag to plot observations from the first case, only
#'@param ncol - number of columns/page for size comp plots
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - name for output pdf file
#'
#'@details Uses \code{getMDFR.FisheryQuantities}.
#'
#'@return list of ggplot2 objects
#'
#'@export
#'
compareModelResults.FisheryZCs<-function(obj,
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
    # plot observed retained catch size comps as bubble plots
    #----------------------------------
    if (length(cases)==1){
        dfrp<-getMDFR.FisheryQuantities(obj,type="prNatZ.ret");
        for (fsh in c('TCF')){
            idxo<-(dfrp$fishery==fsh)&(dfrp$category=="observed")
            if (plot1stObs) idxo<-idxo & (dfrp$case==cases[1]);
            dfrpp<-dfrp[idxo,]
            p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='z',size='val',fill='category'));
            p <- p + scale_size_area(max_size=10);
            p <- p + geom_point(alpha=0.8,shape=21,color='black');
            p <- p + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
            p <- p + labs(y="size (mm CW)",x="year") + ggtitle(paste0(fsh," retained catch: observed proportions"));
            p <- p + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                              size=guide_legend(order=1));
            p <- p + facet_grid(x~.)
            p <- p + theme(legend.box='horizontal')
            if (showPlot) print(p);
            cap<-paste0("Figure &&fno. Observed proportions-at-size for retained catch in ",fsh,".");
            if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
            plots[[cap]]<-p; p<-NULL;
        }
    }

    #----------------------------------
    # plot observed total catch size comps as bubble plots
    #----------------------------------
    if (length(cases)==1){
        dfrp<-getMDFR.FisheryQuantities(obj,type="prNatZ.tot");
        for (fsh in c('TCF','SCF','RKF','GTF')){
            idxo<-(dfrp$fishery==fsh)&(dfrp$category=="observed")
            if (plot1stObs) idxo<-idxo & (dfrp$case==cases[1]);
            dfrpp<-dfrp[idxo,]
            p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='z',size='val',fill='category'));
            p <- p + scale_size_area(max_size=10);
            p <- p + geom_point(alpha=0.8,shape=21,color='black');
            p <- p + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
            p <- p + labs(y="size (mm CW)",x="year") + ggtitle(paste0(fsh," total catch: observed proportions"));
            p <- p + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                              size=guide_legend(order=1));
            p <- p + facet_grid(x~.)
            p <- p + theme(legend.box='horizontal')
            if (showPlot) print(p);
            cap<-paste0("Figure &&fno. Observed proportions-at-size for total catch in ",fsh,".");
            if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
            plots[[cap]]<-p; p<-NULL;
        }
    }

    #----------------------------------
    # plot observed and predicted retained catch size comps as line plots
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,type="prNatZ.ret");
    dfrp$case<-factor(dfrp$case,levels=cases);
    for (fsh in c('TCF')){
        idx<-(dfrp$fishery==fsh)
        dfrpp<-dfrp[idx,];
        
        idxo<-dfrpp$category=="observed";
        if (plot1stObs) idxo<-idxo & (dfrpp$case==cases[1]);
        idxp<-dfrpp$category=="predicted";
        
        p <- ggplot(data=dfrpp,mapping=aes_string(x='z',y='val'));
        p <- p + geom_bar(data=dfrpp[idxo,],stat='identity',position='identity',mapping=aes_string(fill='category'),colour=NA);
        p <- p + geom_line(data=dfrpp[idxp,],mapping=aes_string(colour='case'));
        p <- p + facet_wrap(~y,ncol=5);
        p <- p + labs(x="size (mm CW)",y="proportion") + ggtitle(paste0("Retained catch in ",fsh));
        cap<-paste0("Figure &&fno. Observed proportions-at-size for retained catch in ",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot observed and predicted total catch size comps as line plots
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,type="prNatZ.tot");
    dfrp$case<-factor(dfrp$case,levels=cases);
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx<-(dfrp$fishery==fsh)
        dfrpp<-dfrp[idx,];
        
        idxm<-dfrpp$x=="male";
        idxf<-dfrpp$x=="female";
        idxo<-dfrpp$category=="observed";
        if (plot1stObs) idxo<-idxo & (dfrpp$case==cases[1]);
        idxp<-dfrpp$category=="predicted";
        
        p <- ggplot(data=dfrpp,mapping=aes_string(x='z',y='val'));
        p <- p + geom_bar(data=dfrpp[idxo&idxm,],stat='identity',position='identity',mapping=aes_string(fill='category'),colour=NA);
        p <- p + geom_line(data=dfrpp[idxp&idxm,],mapping=aes_string(colour='case'));
        p <- p + facet_wrap(~y,ncol=5);
        p <- p + labs(x="size (mm CW)",y="proportion") + ggtitle(paste0("Male total catch in ",fsh));
        cap<-paste0("Figure &&fno. Observed and predicted proportions-at-size for male total catch in ",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
        
        p <- ggplot(data=dfrpp,mapping=aes_string(x='z',y='val'));
        p <- p + geom_bar(data=dfrpp[idxo&idxf,],stat='identity',position='identity',mapping=aes_string(fill='category'),colour=NA);
        p <- p + geom_line(data=dfrpp[idxp&idxf,],mapping=aes_string(colour='case'));
        p <- p + facet_wrap(~y,ncol=5);
        p <- p + labs(x="size (mm CW)",y="proportion") + ggtitle(paste0("Female total catch in ",fsh));
        cap<-paste0("Figure &&fno. Observed and predicted proportions-at-size for female total catch in ",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot retained catch size comp residuals 
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,type="PRs.ret");
    dfrp$case<-factor(dfrp$case,levels=cases);
    dfrp$sign<-ifelse(test=dfrp$val>0,yes=">0",no="<0");
    dfrp$val <- abs(dfrp$val);
    for (fsh in c('TCF')){
        idx<-(dfrp$fishery==fsh);
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
        cap<-paste0("Figure &&fno. Pearson's residuals for retained catch proportions-at-size in ",fsh,".");
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
        idx<-(dfrp$fishery==fsh);
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
        cap<-paste0("Figure &&fno. Pearson's residuals for total catch proportions-at-size in ",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    return(invisible(plots));
}