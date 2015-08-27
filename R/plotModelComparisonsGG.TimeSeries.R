#'
#'@title Plot comparisons of time series from a set of model runs
#'
#'@description Function to plot a comparison of time series from a set of model runs.
#'
#'@param dfr - dataframe
#'@param position - indicates ggplot2 position_ to use ('dodge','jitter','identity',)
#'@param facets - string giving faceting formula
#'@param scales - ggplot2 scales option for facet_grid
#'@param plotObs - plot observations
#'@param plotMod - plot model fits/predictions
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
#'@import ggplot2
#'
#'@export
#'
plotModelComparisonsGG.TimeSeries<-function(dfr,
                                            position='dodge',
                                            facets=NULL,
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
    }
    idx<-dfr$category=='observed';
    dfro<-dfr[idx,]
    dfrp<-dfr[!idx,]
    p <- ggplot(dfr,aes_string(x='year',y='val',color='case'));
    if (plotObs){
        p <- p + geom_point(aes_string(shape='case'),data=dfro,size=4,alpha=0.5,position=position);
        if (!is.null(dfro$lci)){
            p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),data=dfro,position=position);
        }
    }
    if (plotMod) p <- p + geom_line(data=dfrp);
    p <- p + coord_cartesian(xlim=xlims,ylim=ylims)
    p <- p + labs(x=xlab,y=ylab);
    p <- p + ggtitle(title);
    if (!is.null(facets)) p <- p + facet_grid(facets);
    if (showPlot) print(p);
    
    return(p);
}