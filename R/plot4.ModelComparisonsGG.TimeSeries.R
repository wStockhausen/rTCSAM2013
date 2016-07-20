#'
#'@title Plot comparisons of time series from a set of model runs, with a "zoom" for recent years
#'
#'@description Function to plot a comparison of time series from a set of model runs, with a "zoom" for recent years.
#'
#'@param dfr - dataframe
#'@param numRecent - number of "recent" years to plot
#'@param x - column name with x axis values
#'@param y - column name with y axis values
#'@param lci - column name with y axis values
#'@param uci - column name with y axis values
#'@param model - column name with model names 
#'@param category - column name with category values (i.e., "observed","predicted")
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
#'@details numRecent provides the "zoom" for a second set of faceted plots including
#'only the most recent years.
#'
#'@return list with requested ggplot objects
#'
#'@export
#'
plot4.ModelComparisonsGG.TimeSeries<-function(dfr,
                                              numRecent=15,
                                              x="y",
                                              y="val",
                                              lci="lci",
                                              uci="uci",
                                              model="model",
                                              category="category",
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
    plots<-list();
    
    #plot with observations & model results
    if (plotObs&&plotMod){
        p1<-plotModelComparisonsGG.TimeSeries(dfr,
                                              x=x,
                                              y=y,
                                              lci=lci,
                                              uci=uci,
                                              model=model,
                                              category=category,
                                              facets=facets,
                                              plotObs=TRUE,
                                              plotMod=TRUE,
                                              ci=ci,
                                              pdfType=pdfType,
                                              xlab=xlab,
                                              ylab=ylab,
                                              title=title,
                                              xlims=xlims,
                                              ylims=ylims,
                                              showPlot=showPlot);
        plots$p1<-p1;

        #plot in recent years only
        xmx<-max(dfr[[x]],na.rm=TRUE);
        xplims<-c(xmx-numRecent,xmx+1);
        if (!is.null(xlims)){
            xplims[1]<-max(xlims[1],xplims[1],na.rm=TRUE);#max of mins
            xplims[2]<-min(xlims[2],xplims[2],na.rm=TRUE);#min of maxes
        }
        # idx<-!(dfr$category=='observed');
        # idy<-dfr$year %in% xplims[1]:xplims[2];
        # yplims<-range(dfr$val[idx&idy],na.rm=TRUE,finite=TRUE);
        idy<-dfr[[x]] %in% xplims[1]:xplims[2];
        yplims<-range(dfr[[y]][idy],na.rm=TRUE,finite=TRUE);
        if (!is.null(ylims)){
            yplims[1]<-max(ylims[1],yplims[1],na.rm=TRUE);#max of mins
            yplims[2]<-min(ylims[2],yplims[2],na.rm=TRUE);#min of maxes
        }
        p2<-plotModelComparisonsGG.TimeSeries(dfr,
                                              x=x,
                                              y=y,
                                              lci=lci,
                                              uci=uci,
                                              model=model,
                                              category=category,
                                              facets=facets,
                                              plotObs=TRUE,
                                              plotMod=TRUE,
                                              ci=ci,
                                              pdfType=pdfType,
                                              xlab=xlab,
                                              ylab=ylab,
                                              title=title,
                                              xlims=xplims,
                                              ylims=yplims,
                                              showPlot=showPlot);
        plots$p2<-p2;
    }
    
    #plot with observations only
    if (plotObs&&(!plotMod)){
        p1<-plotModelComparisonsGG.TimeSeries(dfr,
                                              x=x,
                                              y=y,
                                              lci=lci,
                                              uci=uci,
                                              model=model,
                                              category=category,
                                              facets=facets,
                                              plotObs=TRUE,
                                              plotMod=FALSE,
                                              ci=ci,
                                              pdfType=pdfType,
                                              xlab=xlab,
                                              ylab=ylab,
                                              title=title,
                                              xlims=xlims,
                                              ylims=ylims,
                                              showPlot=showPlot);
        plots$p1<-p1;
    }
    
    #plot with model results only
    if (plotMod&&(!plotObs)){
        #plot full time series
        p1<-plotModelComparisonsGG.TimeSeries(dfr,
                                              x=x,
                                              y=y,
                                              lci=lci,
                                              uci=uci,
                                              model=model,
                                              category=category,
                                              facets=facets,
                                              plotObs=FALSE,
                                              plotMod=TRUE,
                                              ci=ci,
                                              pdfType=pdfType,
                                              xlab=xlab,
                                              ylab=ylab,
                                              title=title,
                                              xlims=xlims,
                                              ylims=ylims,
                                              showPlot=showPlot);
        plots$p1<-p1;
        #plot in recent years only
        xmx<-max(dfr[[x]],na.rm=TRUE);
        xplims<-c(xmx-numRecent,xmx+1);
        if (!is.null(xlims)){
            xplims[1]<-max(xlims[1],xplims[1],na.rm=TRUE);#max of mins
            xplims[2]<-min(xlims[2],xplims[2],na.rm=TRUE);#min of maxes
        }
        idy<-dfr[[x]] %in% xplims[1]:xplims[2];
        yplims<-range(dfr[[y]][idy],na.rm=TRUE,finite=TRUE);
        if (!is.null(ylims)){
            yplims[1]<-max(ylims[1],yplims[1],na.rm=TRUE);#max of mins
            yplims[2]<-min(ylims[2],yplims[2],na.rm=TRUE);#min of maxes
        }
        p2<-plotModelComparisonsGG.TimeSeries(dfr,
                                              x=x,
                                              y=y,
                                              lci=lci,
                                              uci=uci,
                                              model=model,
                                              category=category,
                                              facets=facets,
                                              plotObs=FALSE,
                                              plotMod=TRUE,
                                              ci=ci,
                                              pdfType=pdfType,
                                              xlab=xlab,
                                              ylab=ylab,
                                              title=title,
                                              xlims=xplims,
                                              ylims=yplims,
                                              showPlot=showPlot);
        plots$p2<-p2;
    }
    return(plots);
}