#'
#'@title Compare population time series among several model runs
#'
#'@description Function to compare population time series among 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param numRecent - number of recent years to zoom in on
#'@param showPlot - flag (T/F) to show plot immediately
#'@param pdf - name for output pdf file
#'@param figno - initial figure number for plot captions
#'
#'@details Plots include mature (mating time) biomass, recruitment, annual abundance by x, xm, and xms, 
#'initial size distribution, final size distribution, annual NatZ by x.
#'
#'@return (nested) list of ggplot2 objects, returned invisibly.
#'
#'@importFrom wtsUtilities printGGList
#'@import ggplot2
#'
#'@export
#'
compareModelResults.PopQuantities<-function(obj,
                                            numRecent=15,
                                            showPlot=FALSE,
                                            pdf=NULL,
                                            figno=1){
    
    lst<-convertToListOfResults(obj);
    cases<-names(lst);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        oldpar<-par(oma=c(0.5,1,1,0.5),mar=c(2,5,1,1)+0.2,mfrow=c(3,1));
        on.exit(dev.off());
        showPlot<-TRUE;
    } else {
        oldpar<-par(oma=c(0.5,1,1,0.5),mar=c(2,5,1,1)+0.2);
    }
    on.exit(par(oldpar),add=TRUE);
    
    plots<-list();#output list

    #-------------------------------------------#
    #plot mature biomass
    #-------------------------------------------#
    dfr<-getMDFR.Pop.MatureBiomass(lst);
    dfr$case<-factor(dfr$case,levels=cases);
    p <- ggplot(dfr,aes_string(x='y',y='val',colour='case'));
    p <- p + geom_line();
    p <- p + geom_point();
    if (any(!is.na(dfr$lci))) p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'));
    p <- p + labs(x='year',y="Mature Biomass (1000's t)");
    p <- p + ggtitle("Mature Biomass");
    p <- p + facet_grid(x~.);
    cap<-"  \n  \nFigure &&fno. Estimated mature biomass at mating time.  \n  \n";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; 
    
    dfrp<-dfr[dfr$y>=max(dfr$y)-numRecent,];
    p <- p %+% dfrp;
    cap<-"  \n  \nFigure &&fno. Estimated mature biomass at mating time (recent years only).  \n  \n";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    #-------------------------------------------#
    #plot recruitment
    #-------------------------------------------#
    dfr<-getMDFR.Pop.Recruitment(lst);
    dfr$case<-factor(dfr$case,levels=cases);
    p <- ggplot(dfr,aes_string(x='y',y='val',colour='case'));
    p <- p + geom_line();
    p <- p + geom_point();
    if (any(!is.na(dfr$lci))) p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'));
    p <- p + labs(x='year',y="Recruitment (millions)");
    p <- p + ggtitle("Recruitment");
    p <- p + facet_grid(x~.);
    cap<-"  \n  \nFigure &&fno. Estimated annual recruitment.  \n  \n";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; 
    
    dfrp<-dfr[dfr$y>=max(dfr$y)-numRecent,];
    p <- p %+% dfrp;
    cap<-"  \n  \nFigure &&fno. Estimated annual recruitment (recent years only).  \n  \n";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    #-------------------------------------------#
    #plot annual abundance by x
    #-------------------------------------------#
    dfr<-getMDFR.Pop.Quantities(lst,type="N_yx");
    dfr$case<-factor(dfr$case,levels=cases);
    p <- ggplot(dfr,aes_string(x='y',y='val',colour='case'));
    p <- p + geom_line();
    p <- p + labs(x='year',y="Abundance (millions)");
    p <- p + ggtitle("Population Abundance");
    p <- p + facet_grid(x~.);
    cap<-"  \n  \nFigure &&fno. Estimated annual abundance.  \n  \n";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; 
    
    dfrp<-dfr[dfr$y>=max(dfr$y)-numRecent,];
    p <- p %+% dfrp;
    cap<-"  \n  \nFigure &&fno. Estimated annual abundance (recent years only).  \n  \n";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    #-------------------------------------------#
    #plot annual abundance by xm
    #-------------------------------------------#
    dfr<-getMDFR.Pop.Quantities(lst,type="N_yxm");
    dfr$case<-factor(dfr$case,levels=cases);
    p <- ggplot(dfr,aes_string(x='y',y='val',colour='case'));
    p <- p + geom_line();
    p <- p + labs(x='year',y="Abundance (millions)");
    p <- p + ggtitle("Population Abundance");
    p <- p + facet_grid(m~x);
    cap<-"\n  \nFigure &&fno. Estimated annual abundance, by sex and maturity.  \n  \n";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; 
    
    dfrp<-dfr[dfr$y>=max(dfr$y)-numRecent,];
    p <- p %+% dfrp;
    cap<-"  \n  \nFigure &&fno. Estimated annual abundance, by sex and maturity (recent years only).  \n  \n";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; 
    
    #-------------------------------------------#
    #plot annual abundance by xms
    #-------------------------------------------#
    dfr<-getMDFR.Pop.Quantities(lst,type="N_yxms");
    dfr$case<-factor(dfr$case,levels=cases);
    p <- ggplot(dfr,aes_string(x='y',y='val',colour='case'));
    p <- p + geom_line();
    p <- p + labs(x='year',y="Abundance (millions)");
    p <- p + ggtitle("Population Abundance");
    p <- p + facet_grid(m+s~x);
    cap<-"  \n  \nFigure &&fno. Estimated annual abundance, by sex, maturity, and shell condition.  \n  \n";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; 
    
    dfrp<-dfr[dfr$y>=max(dfr$y)-numRecent,];
    p <- p %+% dfrp;
    cap<-"  \n  \nFigure &&fno. Estimated annual abundance, by sex, maturity, and shell condition (recent yeaers only).  \n  \n";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; 
    
    #-------------------------------------------#
    #plot final abundance by xmz
    #-------------------------------------------#
    dfr<-getMDFR.Pop.Quantities(lst,type="fN_xmsz");
    dfr<-reshape2::dcast(dfr,"case+x+m+z~.",fun.aggregate=sum,value.var='val');
    dfr$case<-factor(dfr$case,levels=cases);
    p <- ggplot(dfr,aes_string(x='z',y='.',colour='case'));
    p <- p + geom_line();
    p <- p + labs(x='size (mm CW)',y="Abundance (millions)");
    p <- p + ggtitle("Final Population Abundance");
    p <- p + facet_grid(m~x);
    cap<-"  \n  \nFigure &&fno. Estimated final abundance, by sex, maturity, shell condition and size.  \n  \n";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; 

    return(invisible(plots));
}