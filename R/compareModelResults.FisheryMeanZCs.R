#'
#'@title Compare estimated/predicted fishery mean size comps and effective N's among several model runs
#'
#'@description Function to compare estimated/predicted fishery mean size comps and effective N's among 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
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
compareModelResults.FisheryMeanZCs<-function(obj,
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
    # plot observed and predicted mean retained catch size comps
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"mnPrNatZ.ret");
    dfrp$case<-factor(dfrp$case,levels=cases);
    dfrp$category<-factor(dfrp$category,c("predicted","observed"))
    if (plot1stObs) {
        idxo<-(dfrp$category=="observed")&(dfrp$case==cases[1]);
        dfrp<-rbind(dfrp[idxo,],dfrp[dfrp$category=="predicted",])
    }
    for (fsh in c('TCF')){
        idx<-(dfrp$fleet==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(dfrpp,aes_string(x='z',y='val',colour="case",linetype="category"));
        p <- p + geom_line();
        p <- p + geom_errorbar(mapping=aes_string(ymin='lci',ymax='uci'))
        p <- p + facet_grid("x~fleet");
        p <- p + labs(y="proportion",x="size (mm CW)")
        cap<-paste0("  \n  \nFigure &&fno. Observed and predicted mean proportions-at-size for retained catch in ",fsh,".  \n  \n");
        if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot observed and predicted mean total catch size comps
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"mnPrNatZ.tot");
    dfrp$case<-factor(dfrp$case,levels=cases);
    if (plot1stObs) {
        idxo<-(dfrp$category=="observed")&(dfrp$case==cases[1]);
        dfrp<-rbind(dfrp[idxo,],dfrp[dfrp$category=="predicted",])
    }
    dfrp$category<-factor(dfrp$category,c("predicted","observed"))
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx<-(dfrp$fleet==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(dfrpp,aes_string(x='z',y='val',colour="case",linetype="category"));
        p <- p + geom_line();
        p <- p + geom_errorbar(mapping=aes_string(ymin='lci',ymax='uci'))
        p <- p + facet_grid("x~fleet");
        p <- p + labs(y="proportion",x="size (mm CW)")
        cap<-paste0("  \n  \nFigure &&fno. Observed and predicted mean proportions-at-size for total catch in ",fsh,".  \n  \n");
        if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot input and effective sample sizes for retained catch size comps
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"effSS.ret");
    dfrp$case<-factor(dfrp$case,levels=cases);
    dfrp$category<-factor(dfrp$category,c("McAllister-Ianelli","input"))
    for (fsh in c('TCF')){
        idx<-(dfrp$fleet==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='val',colour="case",linetype="category"));
        p <- p + geom_point();
        p <- p + geom_line();
        p <- p + facet_grid(x~fleet)
        p <- p + labs(y="sample size",x="year")
        cap<-paste0("  \n  \nFigure &&fno. Input and effective sample sizes for retained catch proportions-at-size in",fsh,".  \n  \n");
        if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot input and effective sample sizes for total catch size comps
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"effSS.tot");
    dfrp$case<-factor(dfrp$case,levels=cases);
    dfrp$category<-factor(dfrp$category,c("McAllister-Ianelli","input"))
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx<-(dfrp$fleet==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='val',colour="case",linetype="category"));
        p <- p + geom_point();
        p <- p + geom_line();
        p <- p + facet_grid(x~fleet)
        p <- p + labs(y="sample size",x="year")
        cap<-paste0("  \n  \nFigure &&fno. Input and effective sample sizes for total catch proportions-at-size in",fsh,".  \n  \n");
        if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    return(invisible(plots));
}