#'
#'@title Compare estimated/predicted fishery quantities among several model runs
#'
#'@description Function to compare estimated/predicted fishery quantities among 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param numRecent - number of recent years to plot
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
compareModelResults.FisheryQuantities<-function(obj,
                                               numRecent=15,
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
    # observed and predicted retained catch mortality from fisheries (1000's t)
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"bio.retm");
    dfrp$case<-factor(dfrp$case,levels=cases);
    #make 4-plot from observations & case results
    ps<-plot2.ModelComparisonsGG.TimeSeries(dfrp,
                                            numRecent=numRecent,
                                            plot1stObs=plot1stObs,
                                            facets='x~fleet',
                                            plotObs=TRUE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab="Retained catch mortality (1000's t)",
                                            title=NULL,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=showPlot);
    cap1<-"  \n  \nFigure &&fno. Comparison of observed and predicted retained catch mortality.  \n  \n";
    cap2<-"  \n  \nFigure &&fno. Comparison of observed and predicted retained catch mortality. Recent time period.  \n  \n";
    names(ps)<-c(cap1,cap2);
    if (showPlot) figno<-(wtsUtilities::printGGList(ps,figno=figno))$figno;
    plots[[cap1]]<-ps[[1]];
    plots[[cap2]]<-ps[[2]];
    ps<-NULL;

    #----------------------------------
    # observed and predicted discard catch mortality from fisheries (1000's t)
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"bio.dscm");
    dfrp$case<-factor(dfrp$case,levels=cases);
    #make 4-plot from observations & case results
    ps<-plot2.ModelComparisonsGG.TimeSeries(dfrp,
                                            numRecent=numRecent,
                                            plot1stObs=plot1stObs,
                                            facets='x~fleet',
                                            plotObs=TRUE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab="Discard catch mortality (1000's t)",
                                            title=NULL,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=showPlot);
    cap1<-"  \n  \nFigure &&fno. Comparison of observed and predicted discard catch mortality.  \n  \n";
    cap2<-"  \n  \nFigure &&fno. Comparison of observed and predicted discard catch mortality. Recent time period.  \n  \n";
    names(ps)<-c(cap1,cap2);
    if (showPlot) figno<-(wtsUtilities::printGGList(ps,figno=figno))$figno;
    plots[[cap1]]<-ps[[1]];
    plots[[cap2]]<-ps[[2]];
    ps<-NULL;

    #----------------------------------
    # observed and predicted total catch mortality from fisheries (1000's t)
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"bio.totm");
    dfrp$case<-factor(dfrp$case,levels=cases);
    #make 4-plot from observations & case results
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx <- dfrp$fleet==fsh;
        ps<-plot2.ModelComparisonsGG.TimeSeries(dfrp[idx,],
                                                numRecent=numRecent,
                                                plot1stObs=plot1stObs,
                                                facets='x~fleet',
                                                plotObs=TRUE,
                                                plotMod=TRUE,
                                                ci=0.95,
                                                pdfType='lognormal',
                                                xlab='year',
                                                ylab="Total catch mortality (1000's t)",
                                                title=NULL,
                                                scales="free_y",
                                                xlims=NULL,
                                                ylims=NULL,
                                                showPlot=showPlot);
        cap1<-paste0("  \n  \nFigure &&fno. Comparison of observed and predicted total catch biomass for ",fsh,".  \n  \n");
        cap2<-paste0("  \n  \nFigure &&fno. Comparison of observed and predicted total catch biomass for ",fsh,". Recent time period.  \n  \n");
        names(ps)<-c(cap1,cap2);
        if (showPlot) figno<-(wtsUtilities::printGGList(ps,figno=figno))$figno;
        plots[[cap1]]<-ps[[1]];
        plots[[cap2]]<-ps[[2]];
        ps<-NULL;
    }

    #----------------------------------
    # plot z-scores for observed and predicted mature (spawning) biomass from the survey
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"zscores");
    dfrp$case<-factor(dfrp$case,levels=cases);
    #--retained catch
    for (fsh in c('TCF')){
        idx   <- (dfrp$fleet==fsh)&(dfrp$category=="retained catch");
        dfrpp <- dfrp[idx,];
        xmax  <- max(dfrpp$y,na.rm=TRUE);
        p<-plotZScores(dfrpp,x='y',y='val',
                       color='case',shape='case',legend='case',
                       facets="x~fleet",facet.scales='free_y',position='dodge',
                       ylab='z-score (retained catch)',title=NULL,
                       showPlot=showPlot);
        cap<-"  \n  \nFigure &&fno. Z-scores for retained catch.  \n  \n";
        if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }
    #--total catch
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx   <- (dfrp$fleet==fsh)&(dfrp$category=="catch");
        dfrpp <- dfrp[idx,];
        xmax  <- max(dfrpp$y,na.rm=TRUE);
        p<-plotZScores(dfrpp,x='y',y='val',
                       color='case',shape='case',legend='case',
                       facets="x~fleet",facet.scales='free_y',position='dodge',
                       ylab='z-score (total catch)',title=NULL,
                       showPlot=showPlot);
        cap<-paste0("  \n  \nFigure &&fno. Z-scores for total catch in ",fsh,".  \n  \n");
        if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    return(invisible(plots));
}