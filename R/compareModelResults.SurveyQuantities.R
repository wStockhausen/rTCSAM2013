#'
#'@title Compare estimated/predicted survey quantities among several model runs
#'
#'@description Function to compare estimated/predicted survey quantities among 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param numRecent - number of recent years to plot
#'@param plot1stObs - flag to plot observations from the first case, only
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
compareModelResults.SurveyQuantities<-function(obj=NULL,
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
    # plot observed and predicted mature (spawning) biomass from the survey
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,"MB_yx");
    dfrp$case<-factor(dfrp$case,levels=cases);
    #make 4-plot from observations & model results
    ps<-plot2.ModelComparisonsGG.TimeSeries(dfrp,
                                            numRecent=numRecent,
                                            plot1stObs=plot1stObs,
                                            facets='x~.',
                                            plotObs=TRUE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab="Biomass (1000's t)",
                                            title="Mature survey biomass",
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=FALSE);
    cap1<-"  \n  \nFigure &&fno. Comparison of observed and predicted mature biomass from the survey.  \n  \n";
    cap2<-"  \n  \nFigure &&fno. Comparison of observed and predicted mature biomass from the survey (zoomed to recent).  \n  \n";
    names(ps)<-c(cap1,cap2);
    if (showPlot) figno<-(printGGList(ps,figno=figno))$figno;
    plots[[cap1]]<-ps[[1]];
    plots[[cap2]]<-ps[[2]];
    ps<-NULL;

    #----------------------------------
    # plot z-scores for observed and predicted mature (spawning) biomass from the survey
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,"zscrs_yx");
    dfrp$case<-factor(dfrp$case,levels=cases);
    xmax<-max(dfrp$y,na.rm=TRUE);
    p<-plotZScores(dfrp,x='y',y='val',
                   color='case',shape='case',legend='case',
                   facets="x~.",facet.scales='fixed',position='dodge',
                   ylab='z-score',title='fits to mature survey biomass',
                   xlims=c(1974,xmax+1),
                   showPlot=FALSE);
    cap<-"Figure &&fno. Z-scores for mature biomass from the survey.  \n  \n";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    #----------------------------------
    # plot observed and predicted legal male abundance from the survey
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,"lglN_y")
    dfrp$case<-factor(dfrp$case,levels=cases);
    #make 4-plot from observations & model results
    ps<-plot2.ModelComparisonsGG.TimeSeries(dfrp,
                                            numRecent=numRecent,
                                            plot1stObs=plot1stObs,
                                            facets='x~.',
                                            plotObs=TRUE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab="Abundance (millions)",
                                            title="Legal male abundance",
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=FALSE);
    cap1<-"  \n  \nFigure &&fno. Comparison of observed and predicted legal male abundance from the survey.  \n  \n";
    cap2<-"  \n  \nFigure &&fno. Comparison of observed and predicted legal male abundance from the survey (zoomed to recent).  \n  \n";
    names(ps)<-c(cap1,cap2);
    if (showPlot) figno<-(printGGList(ps,figno=figno))$figno;
    plots[[cap1]]<-ps[[1]];
    plots[[cap2]]<-ps[[2]];
    ps<-NULL;

    #----------------------------------
    # plot observed and predicted legal male biomass from the survey
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,"lglB_y")
    dfrp$case<-factor(dfrp$case,levels=cases);
    #make 4-plot from observations & model results
    ps<-plot2.ModelComparisonsGG.TimeSeries(dfrp,
                                            numRecent=numRecent,
                                            plot1stObs=plot1stObs,
                                            facets='x~.',
                                            plotObs=TRUE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab="Biomass (1000's t)",
                                            title="Legal male biomass",
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=FALSE);
    cap1<-"  \n  \nFigure &&fno. Comparison of observed and predicted legal male biomass from the survey.  \n  \n";
    cap2<-"  \n  \nFigure &&fno. Comparison of observed and predicted legal male biomass from the survey (zoomed to recent).  \n  \n";
    names(ps)<-c(cap1,cap2);
    if (showPlot) figno<-(printGGList(ps,figno=figno))$figno;
    plots[[cap1]]<-ps[[1]];
    plots[[cap2]]<-ps[[2]];
    ps<-NULL;
    
    return(invisible(plots));
}