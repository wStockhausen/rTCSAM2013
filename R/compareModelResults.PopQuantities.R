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
#'
#'@details Plots include mature (mating time) biomass, recruitment, annual abundance by x, xm, and xms, 
#'initial size distribution, final size distribution, annual NatZ by x.
#'
#'@return (nested) list of ggplot2 objects, returned invisibly.
#'
#'@import ggplot2
#'
#'@export
#'
compareModelResults.PopQuantities<-function(obj,
                                            numRecent=15,
                                            showPlot=FALSE,
                                            pdf=NULL){
    
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
    dfr<-getMDFR.MatureBiomass(lst);
    p <- ggplot(dfr,aes_string(x='y',y='val',colour='case'));
    p <- p + geom_line();
    p <- p + geom_point();
    if (any(!is.na(dfr$lci))) p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'));
    p <- p + labs(x='year',y="Mature Biomass (1000's t)");
    p <- p + ggtitle("Mature Biomass");
    p <- p + facet_grid(x~.);
    if (showPlot) print(p);
    plots[["MB"]]<-p;
    dfrp<-dfr[dfr$y>=max(dfr$y)-numRecent,];
    p <- p %+% dfrp;
    if (showPlot) print(p);
    plots[["MB.zoom"]]<-p;
    
    #-------------------------------------------#
    #plot recruitment
    #-------------------------------------------#
    dfr<-getMDFR.Recruitment(lst);
    p <- ggplot(dfr,aes_string(x='y',y='val',colour='case'));
    p <- p + geom_line();
    p <- p + geom_point();
    if (any(!is.na(dfr$lci))) p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'));
    p <- p + labs(x='year',y="Recruitment (millions)");
    p <- p + ggtitle("Recruitment");
    p <- p + facet_grid(x~.);
    if (showPlot) print(p);
    plots[["rec"]]<-p;
    dfrp<-dfr[dfr$y>=max(dfr$y)-numRecent,];
    p <- p %+% dfrp;
    if (showPlot) print(p);
    plots[["rec.zoom"]]<-p;
    
    #-------------------------------------------#
    #plot annual abundance by x
    #-------------------------------------------#
    dfr<-getMDFR.PopQuantities(lst,type="N_yx");
    p <- ggplot(dfr,aes_string(x='y',y='val',colour='case'));
    p <- p + geom_line();
    p <- p + labs(x='year',y="Abundance (millions)");
    p <- p + ggtitle("Population Abundance");
    p <- p + facet_grid(x~.);
    if (showPlot) print(p);
    plots[["N_yx"]]<-p;
    dfrp<-dfr[dfr$y>=max(dfr$y)-numRecent,];
    p <- p %+% dfrp;
    if (showPlot) print(p);
    plots[["N_yx.zoom"]]<-p;
    
    #-------------------------------------------#
    #plot annual abundance by xm
    #-------------------------------------------#
    dfr<-getMDFR.PopQuantities(lst,type="N_yxm");
    p <- ggplot(dfr,aes_string(x='y',y='val',colour='case'));
    p <- p + geom_line();
    p <- p + labs(x='year',y="Abundance (millions)");
    p <- p + ggtitle("Population Abundance");
    p <- p + facet_grid(m~x);
    if (showPlot) print(p);
    plots[["N_yxm"]]<-p;
    dfrp<-dfr[dfr$y>=max(dfr$y)-numRecent,];
    p <- p %+% dfrp;
    if (showPlot) print(p);
    plots[["N_yxm.zoom"]]<-p;
    
    #-------------------------------------------#
    #plot annual abundance by xms
    #-------------------------------------------#
    dfr<-getMDFR.PopQuantities(lst,type="N_yxms");
    p <- ggplot(dfr,aes_string(x='y',y='val',colour='case'));
    p <- p + geom_line();
    p <- p + labs(x='year',y="Abundance (millions)");
    p <- p + ggtitle("Population Abundance");
    p <- p + facet_grid(m+s~x);
    if (showPlot) print(p);
    plots[["N_yxms"]]<-p;
    dfrp<-dfr[dfr$y>=max(dfr$y)-numRecent,];
    p <- p %+% dfrp;
    if (showPlot) print(p);
    plots[["N_yxms.zoom"]]<-p;
    
    #-------------------------------------------#
    #plot final abundance by xmz
    #-------------------------------------------#
    dfr<-getMDFR.PopQuantities(lst,type="fN_xmsz");
    dfr<-reshape2::dcast(dfr,"case+x+m+z~.",fun.aggregate=sum,value.var='val');
    p <- ggplot(dfr,aes_string(x='z',y='.',colour='case'));
    p <- p + geom_line();
    p <- p + labs(x='year',y="Abundance (millions)");
    p <- p + ggtitle("Population Abundance");
    p <- p + facet_grid(m~x);
    if (showPlot) print(p);
    plots[["fN_xmsz"]]<-p;

    return(invisible(plots));
}