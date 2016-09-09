#'
#'@title Compare estimated/predicted mean survey size comps and effective N's among several model runs
#'
#'@description Function to compare estimated/predicted mean survey size comps and effective N's among 
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
compareModelResults.SurveyMeanZCs<-function(obj=NULL,
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
    # plot observed and predicted mean size comps from the survey
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,"mnPrNatZ_xmz");
    dfrp$case<-factor(dfrp$case,levels=cases);
    if (plot1stObs) {
        idxo<-(dfrp$category=="observed")&(dfrp$case==cases[1]);
        dfrp<-rbind(dfrp[idxo,],dfrp[dfrp$category=="predicted",])
    }
    dfrp$category<-factor(dfrp$category,c("predicted","observed"))
    dfrp$lci<-dfrp$val-dfrp$stdv;
    dfrp$uci<-dfrp$val+dfrp$stdv;
    p <- ggplot(dfrp,aes_string(x='z',y='val',colour="case",linetype='category'));
    p <- p + geom_line();
    p <- p + geom_errorbar(mapping=aes_string(ymin='lci',ymax='uci'))
    p <- p + facet_grid("x~m");
    p <- p + labs(y="proportion",x="size (mm CW)")
    cap<-"  \n  \nFigure &&fno. Observed and predicted proportions-at-size from the survey by sex and maturity.  \n  \n";
    if (showPlot) figno<-(printGGList(ps,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    dfrp<-getMDFR.SurveyQuantities(obj,"mnPrNatZ_xz");
    dfrp$case<-factor(dfrp$case,levels=cases);
    if (plot1stObs) {
        idxo<-(dfrp$category=="observed")&(dfrp$case==cases[1]);
        dfrp<-rbind(dfrp[idxo,],dfrp[dfrp$category=="predicted",])
    }
    dfrp$category<-factor(dfrp$category,c("predicted","observed"))
    dfrp$lci<-dfrp$val-dfrp$stdv;
    dfrp$uci<-dfrp$val+dfrp$stdv;
    p <- ggplot(dfrp,aes_string(x='z',y='val',colour="case",linetype='category'));
    p <- p + geom_line();
    p <- p + geom_errorbar(mapping=aes_string(ymin='lci',ymax='uci'))
    p <- p + facet_grid("x~.");
    p <- p + labs(y="proportion",x="size (mm CW)")
    cap<-"  \n  \nFigure &&fno. Observed and predicted proportions-at-size from the survey by sex.  \n  \n";
    if (showPlot) figno<-(printGGList(ps,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    #----------------------------------
    # plot input and effective sample sizes for size comps from the survey
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,"effSS_y");
    dfrp$case<-factor(dfrp$case,levels=cases);
    dfrp$category<-factor(dfrp$category,c("McAllister-Ianelli","input"))
    p <- ggplot(data=dfrp,mapping=aes_string(x='y',y='val',colour="case",linetype="category"));
    p <- p + geom_line();
    p <- p + labs(y="sample size",x="year")
    cap<-"  \n  \nFigure &&fno. Input and effective sample sizes for proportions-at-size from the survey.  \n  \n";
    if (showPlot) figno<-(printGGList(ps,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    return(invisible(plots));
}