#'
#'@title Plot comparisons of time series from a set of model runs
#'
#'@description Function to plot a comparison of time series from a set of model runs.
#'
#'@param dfr - dataframe
#'@param plot1stObs - flag to plot observations from first case only
#'@param x - column name with x axis values
#'@param y - column name with y axis values
#'@param lci - column name with y axis values
#'@param uci - column name with y axis values
#'@param case - column name with case names 
#'@param category - column name with category values (i.e., "observed","predicted")
#'@param facets - string giving faceting formula
#'@param position - indicates ggplot2 position_ to use ('dodge','jitter','identity',)
#'@param scales - ggplot2 scales option for facet_grid
#'@param plotObs - plot observations
#'@param plotMod - plot case fits/predictions
#'@param ci - confidence interval for error bars
#'@param pdfType - assumed error distribution for confidence intervals
#'@param xlab - 
#'@param ylab - 
#'@param title - 
#'@param xlims - 
#'@param ylims - 
#'@param showPlot - 
#'
#'@return ggplot object
#'
#'@details None.
#'
#'@import ggplot2
#'
#'@export
#'
plotModelComparisonsGG.TimeSeries<-function(dfr,
                                            plot1stObs=TRUE,
                                            x="y",
                                            y="val",
                                            lci="lci",
                                            uci="uci",
                                            case="case",
                                            category="category",
                                            facets=NULL,
                                            position='dodge',
                                            scales='fixed',
                                            plotObs=TRUE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=NULL,
                                            title=NULL,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE){
    if ((plotObs)&(!is.null(dfr$cv))&any(!is.na(dfr$cv))){
        cis<-calcCIs(dfr$val,dfr$cv,pdfType=pdfType,ci=ci);
        dfr<-cbind(dfr,cis);#adds columns lci and uci
        lci<-'lci';
        uci<-'uci';
    }
    idx<-dfr[[category]]!='observed';
    dfrp<-dfr[idx,];#predicted values
    if (plot1stObs){
        #remove observations from all but first case
        cases<-as.character(unique(dfr$case));
        dfro<-dfr[(dfr$case==cases[1])&(!idx),];
        dfro$case<-'observed';
        dfr<-rbind(dfro,dfrp);
    } else {
        dfro<-dfr[!idx,];
    }
    p <- ggplot(dfr,aes_string(x=x,y=y,color=case));
    if (plotObs){
        p <- p + geom_point(aes_string(shape=case),data=dfro,size=4,alpha=0.5,position=position);
        if (!is.null(dfro$lci)){
            p <- p + geom_errorbar(aes_string(ymin=lci,ymax=uci),data=dfro,position=position);
        }
    }
    if (plotMod) p <- p + geom_line(data=dfrp);
    p <- p + coord_cartesian(xlim=xlims,ylim=ylims)
    p <- p + labs(x=xlab,y=ylab);
    p <- p + ggtitle(title);
    if (!is.null(facets)) p <- p + facet_grid(facets,scales=scales);
    if (showPlot) print(p);
    
    return(p);
}

