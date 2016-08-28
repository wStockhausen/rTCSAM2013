#'
#'@title Compare estimated/predicted survey quatities among several model runs
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
    cap1<-"Figure &&fno. Comparison of observed and predicted mature biomass from the survey.";
    cap2<-"Figure &&fno. Comparison of observed and predicted mature biomass from the survey (zoomed to recent).";
    names(ps)<-c(cap1,cap2);
    if (showPlot) figno<-(printGGList(ps,figno=figno))$figno;
    plots[[cap1]]<-ps[[1]];
    plots[[cap2]]<-ps[[2]];
    ps<-NULL;

    #----------------------------------
    # plot z-scores for observed and predicted mature (spawning) biomass from the survey
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,"zscrs_yx");
    xmax<-max(dfrp$y,na.rm=TRUE);
    p<-plotZScores(dfrp,x='y',y='val',
                   color='case',shape='case',legend='case',
                   facets="x~.",facet.scales='fixed',position='dodge',
                   ylab='z-score',title='fits to mature survey biomass',
                   xlims=c(1974,xmax+1),
                   showPlot=FALSE);
    cap<-"Figure &&fno. Z-scores for mature biomass from the survey.";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    #----------------------------------
    # plot observed size comps from the survey as bubble plots
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,type="prNatZ_yxz");
    idxo<-dfrp$category=="observed";
    if (plot1stObs) idxo<-idxo & (dfrp$case==cases[1]);

    p <- ggplot(data=dfrp[idxo,],
                mapping=aes_string(x='y',y='z',size='val',fill='category'));
    p <- p + scale_size_area(max_size=10);
    p <- p + geom_point(alpha=0.8,shape=21,color='black');
    p <- p + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
    p <- p + labs(y="size (mm CW)",x="year") + ggtitle("Observed proportions");
    p <- p + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                      size=guide_legend(order=1));
    p <- p + facet_grid(x~.)
    p <- p + theme(legend.box='horizontal')
    cap<-"Figure &&fno. Observed proportions-at-size from the survey by sex.";
    if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    #----------------------------------
    # plot observed and predicted size comps from the survey as line plots
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,type="prNatZ_yxz");
    idxm<-dfrp$x=="male";
    idxf<-dfrp$x=="female";
    idxp<-dfrp$category=="predicted";
    idxo<-dfrp$category=="observed";
    if (plot1stObs) idxo<-idxo&(dfrp$case==cases[1]);

    p1 <- ggplot(data=dfrp,mapping=aes_string(x='z',y='val',fill='category',colour='category',linetype='case'));
    p1 <- p1 + geom_bar(data=dfrp[idxo&idxm,],stat='identity');
    p1 <- p1 + geom_line(data=dfrp[idxp&idxm,]);
    p1 <- p1 + facet_wrap(~y,ncol=5);
    p1 <- p1 + labs(x="size (mm CW)",y="proportion") + ggtitle("males");
    cap1<-"Figure &&fno. Observed and predicted proportions-at-size for males from the survey.";
    if (showPlot) figno<-(printGGList(p1,figno=figno,cap=cap1))$figno;
    plots[[cap1]]<-p1; p1<-NULL;
    
    p2 <- ggplot(data=dfrp,mapping=aes_string(x='z',y='val',fill='category',colour='category',linetype='case'));
    p2 <- p2 + geom_bar(data=dfrp[idxo&idxf,],stat='identity');
    p2 <- p2 + geom_line(data=dfrp[idxp&idxf,]);
    p2 <- p2 + facet_wrap(~y,ncol=5);
    p2 <- p2 + labs(x="size (mm CW)",y="proportion") + ggtitle("females");
    cap2<-"Figure &&fno. Observed and predicted proportions-at-size for females from the survey.";
    if (showPlot) figno<-(printGGList(p2,figno=figno,cap=cap2))$figno;
    plots[[cap2]]<-p2; p2<-NULL;
    
    #----------------------------------
    # plot size comp residuals from the survey 
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,type="PRs_yxz");
    dfrp$sign<-ifelse(test=dfrp$val>0,yes=">0",no="<0");
    dfrp$val <- abs(dfrp$val);

    p <- ggplot(data=dfrp,mapping=aes_string(x='y',y='z',size='val',fill='sign',colour='case',linetype='case'));
    p <- p + scale_size_area(max_size=10);
    p <- p + geom_point(alpha=0.8,shape=21,color='black');
    p <- p + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
    p <- p + labs(y="size (mm CW)",x="year") + ggtitle("Pearson's residuals");
    p <- p + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                      size=guide_legend(order=1));
    p <- p + facet_grid(x~.)
    p <- p + theme(legend.box='horizontal')
    cap<-"Figure &&fno. Pearson's residuals for proportions-at-size from the survey.";
    if (showPlot) figno<-(printGGList(ps,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    #----------------------------------
    # plot observed and predicted mean size comps from the survey
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,"mnPrNatZ_xmz");
    if (plot1stObs) {
        idxo<-(dfrp$category=="observed")&(dfrp$case==cases[1]);
        dfrp<-rbind(dfrp[idxo,],dfrp[dfrp$category=="predicted",])
    }
    dfrp$lci<-dfrp$val-dfrp$stdv;
    dfrp$uci<-dfrp$val+dfrp$stdv;
    p <- ggplot(dfrp,aes_string(x='z',y='val',colour="category",linetype='case'));
    p <- p + geom_line();
    p <- p + geom_errorbar(mapping=aes_string(ymin='lci',ymax='uci'))
    p <- p + facet_grid("x~m");
    p <- p + labs(y="proportion",x="size (mm CW)")
    cap<-"Figure &&fno. Observed and predicted proportions-at-size from the survey by sex and maturity.";
    if (showPlot) figno<-(printGGList(ps,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    dfrp<-getMDFR.SurveyQuantities(obj,"mnPrNatZ_xz");
    if (plot1stObs) {
        idxo<-(dfrp$category=="observed")&(dfrp$case==cases[1]);
        dfrp<-rbind(dfrp[idxo,],dfrp[dfrp$category=="predicted",])
    }
    dfrp$lci<-dfrp$val-dfrp$stdv;
    dfrp$uci<-dfrp$val+dfrp$stdv;
    p <- ggplot(dfrp,aes_string(x='z',y='val',colour="category",linetype='case'));
    p <- p + geom_line();
    p <- p + geom_errorbar(mapping=aes_string(ymin='lci',ymax='uci'))
    p <- p + facet_grid("x~.");
    p <- p + labs(y="proportion",x="size (mm CW)")
    cap<-"Figure &&fno. Observed and predicted proportions-at-size from the survey by sex.";
    if (showPlot) figno<-(printGGList(ps,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    #----------------------------------
    # plot input and effective sample sizes for size comps from the survey
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,"effSS_y");
    p <- ggplot(data=dfrp,mapping=aes_string(x='y',y='val',colour="category",linetype='case'));
    p <- p + geom_line();
    p <- p + labs(y="sample size",x="year")
    cap<-"Figure &&fno. Input and effective sample sizes for proportions-at-size from the survey.";
    if (showPlot) figno<-(printGGList(ps,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    #----------------------------------
    # plot survey selectivity functions
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,'selSrv_cxz');
    dfrp$pc<-as.character(dfrp$pc);
    p <- ggplot(data=dfrp,mapping=aes_string(x='z',y='val',colour="pc",linetype='case'));
    p <- p + geom_line();
    p <- p + labs(y="selectivity",x="size (mm CW)");
    p <- p + guides(colour=guide_legend("time period"));
    p <- p + facet_grid(x~.);
    cap<-"Figure &&fno. Estimated selectivity functions for the survey.";
    if (showPlot) figno<-(printGGList(ps,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    #----------------------------------
    # plot observed and predicted legal male abundance from the survey
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,"lglN_y")
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
    cap1<-"Figure &&fno. Comparison of observed and predicted legal male abundance from the survey.";
    cap2<-"Figure &&fno. Comparison of observed and predicted legal male abundance from the survey (zoomed to recent).";
    names(ps)<-c(cap1,cap2);
    if (showPlot) figno<-(printGGList(ps,figno=figno))$figno;
    plots[[cap1]]<-ps[[1]];
    plots[[cap2]]<-ps[[2]];
    ps<-NULL;

    #----------------------------------
    # plot observed and predicted legal male biomass from the survey
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(obj,"lglB_y")
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
    cap1<-"Figure &&fno. Comparison of observed and predicted legal male biomass from the survey.";
    cap2<-"Figure &&fno. Comparison of observed and predicted legal male biomass from the survey (zoomed to recent).";
    names(ps)<-c(cap1,cap2);
    if (showPlot) figno<-(printGGList(ps,figno=figno))$figno;
    plots[[cap1]]<-ps[[1]];
    plots[[cap2]]<-ps[[2]];
    ps<-NULL;
    
    return(invisible(plots));
}