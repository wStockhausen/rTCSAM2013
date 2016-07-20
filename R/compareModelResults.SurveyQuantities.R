#'
#'@title Compare estimated/predicted survey quatities among several model runs
#'
#'@description Function to compare estimated/predicted survey quantities among 
#'several model runs.
#'
#'@param reps - list of objects derived from Jack's R files for the models to be compared
#'@param numRecent - number of recent years to plot
#'@param plot1stObs - flag to plot observations from the first case, only
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - name for output pdf file
#'
#'@details Uses \code{getMDFR.SurveyQuantities}.
#'
#'@return list of ggplot2 objects
#'
#'@export
#'
compareModelResults.SurveyQuantities<-function(reps=NULL,
                                               numRecent=15,
                                               plot1stObs=TRUE,
                                               showPlot=FALSE,
                                               pdf="ModelComparisons.SurveyTimeSeries.pdf"){
    
    #make sure reps is a list of tcsam2013.rep objects
    if (inherits(reps,"tcsam2013.rep")) reps<-list(tcsam2013=reps);
    cases<-names(reps);
    

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }
    
    #----------------------------------
    # define output list of plots
    #----------------------------------
    plots<-list();
    
    #----------------------------------
    # plot observed and predicted mature (spawning) biomass from the survey
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(reps,"MB_yx");
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfrp,
                                            numRecent=numRecent,
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
                                            showPlot=showPlot);
    cap1<-"Figure &&fno. Comparison of observed and predicted mature biomass from the survey.";
    cap2<-"Figure &&fno. Comparison of observed and predicted mature biomass from the survey (zoomed to recent).";
    plots[["MB_yx"]]<-list(norm=list(cap=cap1,plot=ps[[1]]),
                           zoom=list(cap=cap2,plot=ps[[2]]));

    #----------------------------------
    # plot z-scores for observed and predicted mature (spawning) biomass from the survey
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(reps,"zscrs_yx");
    xmax<-max(dfrp$y,na.rm=TRUE);
    p<-plotZScores(dfrp,x='y',y='val',
                   color='model',shape='model',legend='model',
                   facets="x~.",facet.scales='fixed',position='dodge',
                   ylab='z-score',title='fits to mature survey biomass',
                   xlims=c(1975,xmax+1),
                   showPlot=showPlot);
    cap<-"Figure &&fno. Z-scores for mature biomass from the survey.";
    plots[["zscrs_yx"]]<-list(cap=cap,plot=p);
    
    #----------------------------------
    # plot observed size comps from the survey as bubble plots
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(reps,type="prNatZ_yxz");

    p1 <- ggplot(data=dfrp[dfrp$category=="observed",],mapping=aes_string(x='y',y='z',size='val',fill='category'));
    p1 <- p1 + scale_size_area(max_size=10);
    p1 <- p1 + geom_point(alpha=0.8,shape=21,color='black');
    p1 <- p1 + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
    p1 <- p1 + labs(y="size (mm CW)",x="year") + ggtitle("Observed proportions");
    p1 <- p1 + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                      size=guide_legend(order=1));
    p1 <- p1 + facet_grid(x~.)
    p1 <- p1 + theme(legend.box='horizontal')
    if (showPlot) print(p1);
    cap1<-"Figure &&fno. Observed proportions-at-size from the survey.";
    plots[["ZCs.obs"]]<-list(cap=cap1,plot=p1);
    
    #----------------------------------
    # plot observed and predicted size comps from the survey as line plots
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(reps,type="prNatZ_yxz");
    idxm<-dfrp$x=="male";
    idxf<-dfrp$x=="female";
    idxo<-dfrp$category=="observed";
    idxp<-dfrp$category=="predicted";
    
    p1 <- ggplot(data=dfrp,mapping=aes_string(x='z',y='val',fill='category',colour='category',linetype='model'));
    p1 <- p1 + geom_bar(data=dfrp[idxo&idxm,],stat='identity');
    p1 <- p1 + geom_line(data=dfrp[idxp&idxm,]);
    p1 <- p1 + facet_wrap(~y,ncol=5);
    p1 <- p1 + labs(x="size (mm CW)",y="proportion") + ggtitle("males");
    if (showPlot) print(p1);
    cap1<-"Figure &&fno. Observed and predicted proportions-at-size for males from the survey.";
    p2 <- ggplot(data=dfrp,mapping=aes_string(x='z',y='val',fill='category',colour='category',linetype='model'));
    p2 <- p2 + geom_bar(data=dfrp[idxo&idxf,],stat='identity');
    p2 <- p2 + geom_line(data=dfrp[idxp&idxf,]);
    p2 <- p2 + facet_wrap(~y,ncol=5);
    p2 <- p2 + labs(x="size (mm CW)",y="proportion") + ggtitle("females");
    if (showPlot) print(p2);
    cap2<-"Figure &&fno. Observed and predicted proportions-at-size for females from the survey.";
    plots[["ZCs.comp"]]<-list(males=list(cap=cap1,plot=p1),females=list(cap=cap2,plot=p2));
    
    #----------------------------------
    # plot size comp residuals from the survey 
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(reps,type="PRs_yxz");
    dfrp$sign<-ifelse(test=dfrp$val>0,yes=">0",no="<0");
    dfrp$val <- abs(dfrp$val);

    p1 <- ggplot(data=dfrp,mapping=aes_string(x='y',y='z',size='val',fill='sign',colour='model',linetype='model'));
    p1 <- p1 + scale_size_area(max_size=10);
    p1 <- p1 + geom_point(alpha=0.8,shape=21,color='black');
    p1 <- p1 + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
    p1 <- p1 + labs(y="size (mm CW)",x="year") + ggtitle("Pearson's residuals");
    p1 <- p1 + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                      size=guide_legend(order=1));
    p1 <- p1 + facet_grid(x~.)
    p1 <- p1 + theme(legend.box='horizontal')
    if (showPlot) print(p1);
    cap1<-"Figure &&fno. Pearson's residuals for proportions-at-size from the survey.";
    plots[["ZCs.PRs"]]<-list(cap=cap1,plot=p1);
    
    #----------------------------------
    # plot observed and predicted mean size comps from the survey
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(reps,"mnPrNatZ_xz");
    dfrp$lci<-dfrp$val-dfrp$stdv;
    dfrp$uci<-dfrp$val+dfrp$stdv;
    p <- ggplot(dfrp,aes_string(x='z',y='val',colour="category",linetype="model"));
    p <- p + geom_line();
    p <- p + geom_errorbar(mapping=aes_string(ymin='lci',ymax='uci'))
    p <- p + facet_grid("x~.");
    p <- p + labs(y="proportion",x="size (mm CW)")
    if (showPlot) print(p);
    cap<-"Figure &&fno. Observed and predicted proportions-at-size from the survey.";
    plots[["ZCs.means"]]<-list(cap=cap,plot=p);
    
    #----------------------------------
    # plot input and effective sample sizes for size comps from the survey
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(reps,"effSS_y");
    p <- ggplot(data=dfrp,mapping=aes_string(x='y',y='val',colour="category",linetype="model"));
    p <- p + geom_line();
    p <- p + labs(y="sample size",x="year")
    if (showPlot) print(p);
    cap<-"Figure &&fno. Input and effective sample sizes for proportions-at-size from the survey.";
    plots[["ZCs.effSSs"]]<-list(cap=cap,plot=p);
    
    #----------------------------------
    # plot observed and predicted legal male abundance from the survey
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(reps,"lglN_y")
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfrp,
                                            numRecent=numRecent,
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
                                            showPlot=showPlot);
    cap1<-"Figure &&fno. Comparison of observed and predicted legal male abundance from the survey.";
    cap2<-"Figure &&fno. Comparison of observed and predicted legal male abundance from the survey (zoomed to recent).";
    plots[["lglN_y"]]<-list(norm=list(cap=cap1,plot=ps[[1]]),
                            zoom=list(cap=cap2,plot=ps[[2]]));

    #----------------------------------
    # plot observed and predicted legal male biomass from the survey
    #----------------------------------
    dfrp<-getMDFR.SurveyQuantities(reps,"lglB_y")
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfrp,
                                            numRecent=numRecent,
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
                                            showPlot=showPlot);
    cap1<-"Figure &&fno. Comparison of observed and predicted legal male biomass from the survey.";
    cap2<-"Figure &&fno. Comparison of observed and predicted legal male biomass from the survey (zoomed to recent).";
    plots[["lglB_y"]]<-list(norm=list(cap=cap1,plot=ps[[1]]),
                            zoom=list(cap=cap2,plot=ps[[2]]));
    
    return(invisible(plots));
}