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
#'@details Uses \code{getMDFR.FisheryQuantities}.
#'
#'@return list of ggplot2 objects
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
    #make 4-plot from observations & case results
    ps<-plot2.ModelComparisonsGG.TimeSeries(dfrp,
                                            numRecent=numRecent,
                                            plot1stObs=plot1stObs,
                                            facets='x~fishery',
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
    cap1<-"Figure &&fno. Comparison of observed and predicted retained catch mortality.";
    cap2<-"Figure &&fno. Comparison of observed and predicted retained catch mortality. Recent time period.";
    names(ps)<-c(cap1,cap2);
    if (showPlot) figno<-(printGGList(ps,figno=figno))$figno;
    plots[[cap1]]<-ps[[1]];
    plots[[cap2]]<-ps[[2]];
    ps<-NULL;

    #----------------------------------
    # observed and predicted discard catch mortality from fisheries (1000's t)
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"bio.dscm");
    #make 4-plot from observations & case results
    ps<-plot2.ModelComparisonsGG.TimeSeries(dfrp,
                                            numRecent=numRecent,
                                            plot1stObs=plot1stObs,
                                            facets='x~fishery',
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
    cap1<-"Figure &&fno. Comparison of observed and predicted discard catch mortality.";
    cap2<-"Figure &&fno. Comparison of observed and predicted discard catch mortality. Recent time period.";
    names(ps)<-c(cap1,cap2);
    if (showPlot) figno<-(printGGList(ps,figno=figno))$figno;
    plots[[cap1]]<-ps[[1]];
    plots[[cap2]]<-ps[[2]];
    ps<-NULL;

    #----------------------------------
    # observed and predicted total catch mortality from fisheries (1000's t)
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"bio.totm");
    #make 4-plot from observations & case results
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx <- dfrp$fishery==fsh;
        ps<-plot2.ModelComparisonsGG.TimeSeries(dfrp[idx,],
                                                numRecent=numRecent,
                                                plot1stObs=plot1stObs,
                                                facets='x~fishery',
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
        cap1<-paste0("Figure &&fno. Comparison of observed and predicted total catch biomass for ",fsh,".");
        cap2<-paste0("Figure &&fno. Comparison of observed and predicted total catch biomass for ",fsh,". Recent time period.");
        names(ps)<-c(cap1,cap2);
        if (showPlot) figno<-(printGGList(ps,figno=figno))$figno;
        plots[[cap1]]<-ps[[1]];
        plots[[cap2]]<-ps[[2]];
        ps<-NULL;
    }

    #----------------------------------
    # plot z-scores for observed and predicted mature (spawning) biomass from the survey
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"zscores");
    #--retained catch
    for (fsh in c('TCF')){
        idx   <- (dfrp$fishery==fsh)&(dfrp$category=="retained catch");
        dfrpp <- dfrp[idx,];
        xmax  <- max(dfrpp$y,na.rm=TRUE);
        p<-plotZScores(dfrpp,x='y',y='val',
                       color='case',shape='case',legend='case',
                       facets="x~fishery",facet.scales='free_y',position='dodge',
                       ylab='z-score (retained catch)',title=NULL,
                       showPlot=showPlot);
        cap<-"Figure &&fno. Z-scores for retained catch.";
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }
    #--total catch
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx   <- (dfrp$fishery==fsh)&(dfrp$category=="catch");
        dfrpp <- dfrp[idx,];
        xmax  <- max(dfrpp$y,na.rm=TRUE);
        p<-plotZScores(dfrpp,x='y',y='val',
                       color='case',shape='case',legend='case',
                       facets="x~fishery",facet.scales='free_y',position='dodge',
                       ylab='z-score (total catch)',title=NULL,
                       showPlot=showPlot);
        cap<-paste0("Figure &&fno. Z-scores for total catch in ",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot observed retained catch size comps as bubble plots
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,type="prNatZ.ret");
    for (fsh in c('TCF')){
        idxo<-(dfrp$fishery==fsh)&(dfrp$category=="observed")
        if (plot1stObs) idxo<-idxo & (dfrp$case==cases[1]);
        dfrpp<-dfrp[idxo,]
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='z',size='val',fill='category'));
        p <- p + scale_size_area(max_size=10);
        p <- p + geom_point(alpha=0.8,shape=21,color='black');
        p <- p + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
        p <- p + labs(y="size (mm CW)",x="year") + ggtitle(paste0(fsh," retained catch: observed proportions"));
        p <- p + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                          size=guide_legend(order=1));
        p <- p + facet_grid(x~.)
        p <- p + theme(legend.box='horizontal')
        if (showPlot) print(p);
        cap<-paste0("Figure &&fno. Observed proportions-at-size for retained catch in ",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot observed total catch size comps as bubble plots
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,type="prNatZ.tot");
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idxo<-(dfrp$fishery==fsh)&(dfrp$category=="observed")
        if (plot1stObs) idxo<-idxo & (dfrp$case==cases[1]);
        dfrpp<-dfrp[idxo,]
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='z',size='val',fill='category'));
        p <- p + scale_size_area(max_size=10);
        p <- p + geom_point(alpha=0.8,shape=21,color='black');
        p <- p + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
        p <- p + labs(y="size (mm CW)",x="year") + ggtitle(paste0(fsh," total catch: observed proportions"));
        p <- p + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                          size=guide_legend(order=1));
        p <- p + facet_grid(x~.)
        p <- p + theme(legend.box='horizontal')
        if (showPlot) print(p);
        cap<-paste0("Figure &&fno. Observed proportions-at-size for total catch in ",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot observed and predicted retained catch size comps as line plots
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,type="prNatZ.ret");
    for (fsh in c('TCF')){
        idx<-(dfrp$fishery==fsh)
        dfrpp<-dfrp[idx,];
        
        idxo<-dfrpp$category=="observed";
        if (plot1stObs) idxo<-idxo & (dfrpp$case==cases[1]);
        idxp<-dfrpp$category=="predicted";
        
        p <- ggplot(data=dfrpp,mapping=aes_string(x='z',y='val',fill='category',colour='category',linetype='case'));
        p <- p + geom_bar(data=dfrpp[idxo,],stat='identity');
        p <- p + geom_line(data=dfrpp[idxp,]);
        p <- p + facet_wrap(~y,ncol=5);
        p <- p + labs(x="size (mm CW)",y="proportion") + ggtitle(paste0("Retained catch in ",fsh));
        cap<-paste0("Figure &&fno. Observed proportions-at-size for retained catch in ",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot observed and predicted total catch size comps as line plots
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,type="prNatZ.tot");
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx<-(dfrp$fishery==fsh)
        dfrpp<-dfrp[idx,];
        
        idxm<-dfrpp$x=="male";
        idxf<-dfrpp$x=="female";
        idxo<-dfrpp$category=="observed";
        if (plot1stObs) idxo<-idxo & (dfrpp$case==cases[1]);
        idxp<-dfrpp$category=="predicted";
        
        p <- ggplot(data=dfrpp,mapping=aes_string(x='z',y='val',fill='category',colour='category',linetype='case'));
        p <- p + geom_bar(data=dfrpp[idxo&idxm,],stat='identity');
        p <- p + geom_line(data=dfrpp[idxp&idxm,]);
        p <- p + facet_wrap(~y,ncol=5);
        p <- p + labs(x="size (mm CW)",y="proportion") + ggtitle(paste0("Male total catch in ",fsh));
        cap<-paste0("Figure &&fno. Observed and predicted proportions-at-size for male total catch in ",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
        
        p <- ggplot(data=dfrpp,mapping=aes_string(x='z',y='val',fill='category',colour='category',linetype='case'));
        p <- p + geom_bar(data=dfrpp[idxo&idxf,],stat='identity');
        p <- p + geom_line(data=dfrpp[idxp&idxf,]);
        p <- p + facet_wrap(~y,ncol=5);
        p <- p + labs(x="size (mm CW)",y="proportion") + ggtitle(paste0("Female total catch in ",fsh));
        cap<-paste0("Figure &&fno. Observed and predicted proportions-at-size for female total catch in ",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot retained catch size comp residuals 
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,type="PRs.ret");
    dfrp$sign<-ifelse(test=dfrp$val>0,yes=">0",no="<0");
    dfrp$val <- abs(dfrp$val);
    for (fsh in c('TCF')){
        idx<-(dfrp$fishery==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='z',size='val',fill='sign',colour='case',linetype='case'));
        p <- p + scale_size_area(max_size=10);
        p <- p + geom_point(alpha=0.8,shape=21,color='black');
        p <- p + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
        p <- p + labs(y="size (mm CW)",x="year") + ggtitle(paste0(fsh,": retained catch Pearson's residuals"));
        p <- p + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                          size=guide_legend(order=1));
        p <- p + facet_grid(x~fishery);
        p <- p + theme(legend.box='horizontal');
        cap<-paste0("Figure &&fno. Pearson's residuals for retained catch proportions-at-size in ",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot total catch size comp residuals 
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,type="PRs.tot");
    dfrp$sign<-ifelse(test=dfrp$val>0,yes=">0",no="<0");
    dfrp$val <- abs(dfrp$val);
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx<-(dfrp$fishery==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='z',size='val',fill='sign',colour='case',linetype='case'));
        p <- p + scale_size_area(max_size=10);
        p <- p + geom_point(alpha=0.8,shape=21,color='black');
        p <- p + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
        p <- p + labs(y="size (mm CW)",x="year") + ggtitle(paste0(fsh,": total catch Pearson's residuals"));
        p <- p + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                          size=guide_legend(order=1));
        p <- p + facet_grid(x~fishery);
        p <- p + theme(legend.box='horizontal');
        cap<-paste0("Figure &&fno. Pearson's residuals for total catch proportions-at-size in ",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot observed and predicted mean retained catch size comps
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"mnPrNatZ.ret");
    if (plot1stObs) {
        idxo<-(dfrp$category=="observed")&(dfrp$case==cases[1]);
        dfrp<-rbind(dfrp[idxo,],dfrp[dfrp$category=="predicted",])
    }
    dfrp$lci<-dfrp$val-dfrp$stdv;
    dfrp$uci<-dfrp$val+dfrp$stdv;
    for (fsh in c('TCF')){
        idx<-(dfrp$fishery==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(dfrpp,aes_string(x='z',y='val',colour="category",linetype="case"));
        p <- p + geom_line();
        p <- p + geom_errorbar(mapping=aes_string(ymin='lci',ymax='uci'))
        p <- p + facet_grid("x~fishery");
        p <- p + labs(y="proportion",x="size (mm CW)")
        cap<-paste0("Figure &&fno. Observed and predicted mean proportions-at-size for retained catch in ",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot observed and predicted mean total catch size comps
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"mnPrNatZ.tot");
    if (plot1stObs) {
        idxo<-(dfrp$category=="observed")&(dfrp$case==cases[1]);
        dfrp<-rbind(dfrp[idxo,],dfrp[dfrp$category=="predicted",])
    }
    dfrp$lci<-dfrp$val-dfrp$stdv;
    dfrp$uci<-dfrp$val+dfrp$stdv;
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx<-(dfrp$fishery==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(dfrpp,aes_string(x='z',y='val',colour="category",linetype="case"));
        p <- p + geom_line();
        p <- p + geom_errorbar(mapping=aes_string(ymin='lci',ymax='uci'))
        p <- p + facet_grid("x~fishery");
        p <- p + labs(y="proportion",x="size (mm CW)")
        cap<-paste0("Figure &&fno. Observed and predicted mean proportions-at-size for total catch in ",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot input and effective sample sizes for retained catch size comps
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"effSS.ret");
    for (fsh in c('TCF')){
        idx<-(dfrp$fishery==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='val',colour="category",linetype="case"));
        p <- p + geom_point();
        p <- p + geom_line();
        p <- p + facet_grid(x~fishery)
        p <- p + labs(y="sample size",x="year")
        cap<-paste0("Figure &&fno. Input and effective sample sizes for retained catch proportions-at-size in",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot input and effective sample sizes for total catch size comps
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"effSS.tot");
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx<-(dfrp$fishery==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='val',colour="category",linetype="case"));
        p <- p + geom_point();
        p <- p + geom_line();
        p <- p + facet_grid(x~fishery)
        p <- p + labs(y="sample size",x="year")
        cap<-paste0("Figure &&fno. Input and effective sample sizes for total catch proportions-at-size in",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot selectivity functions
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,'selfcns');
    dfrp$pc<-as.character(dfrp$pc);
    idc<-(dfrp$category=="selectivity");
    for (fsh in c('SCF','RKF','GTF')){
        idx<-(dfrp$fishery==fsh);
        dfrpp<-dfrp[idx&idc,];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='z',y='val',colour="case",shape="pc",linetype="pc"));
        p <- p + geom_line();
        p <- p + geom_point();
        p <- p + labs(y="selectivity",x="size (mm CW)");
        p <- p + guides(colour=guide_legend("case"),
                        shape=guide_legend("time period"),
                        linetype=guide_legend("time period"));
        p <- p + facet_grid(x~fishery);
        cap<-paste0("Figure &&fno. Estimated selectivity functions for total catch in",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }
    for (fsh in c('TCF')){
        idx<-(dfrp$fishery==fsh);
        idt<-(dfrp$pc %in% c('1','1949'))
        dfrpp<-dfrp[idx&idc&idt,];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='z',y='val',colour="case"));
        p <- p + geom_line();
        p <- p + geom_point();
        p <- p + labs(y="selectivity",x="size (mm CW)");
        p <- p + guides(colour=guide_legend("case"));
        p <- p + facet_grid(x~fishery);
        cap<-paste0("Figure &&fno. Estimated selectivity functions for total catch in",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }
    for (fsh in c('TCF')){
        idx<-(dfrp$fishery==fsh);
        idt<-(dfrp$pc %in% as.character(1991:2030))
        dfrpp<-dfrp[idx&idc&idt,];
        dfrppp<-dfrp[idx&idc&(dfrp$pc=='1990'),c("z","val","case")];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='z',y='val',colour="case"));
        #p <- p + annotate("line",x=dfrppp$z,y=dfrppp$val,colour=dfrppp$case,linetype=2);
        p <- p + geom_line();
        p <- p + labs(y="selectivity",x="size (mm CW)");
        p <- p + guides(colour=guide_legend("case"));
        p <- p + facet_wrap(~pc,ncol=3);
        cap<-paste0("Figure &&fno. Estimated selectivity functions for total catch in",fsh,". Recent time period");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }
    idc<-(dfrp$category=="retention");
    for (fsh in c('TCF')){
        idx<-(dfrp$fishery==fsh);
        idt<-(dfrp$pc %in% c('1990', '1991'))
        dfrpp<-dfrp[idx&idc&idt,];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='z',y='val',colour="case",shape="pc",linetype="pc"));
        p <- p + geom_line();
        p <- p + geom_point();
        p <- p + labs(y="retention",x="size (mm CW)");
        p <- p + guides(colour=guide_legend("case"),
                        shape=guide_legend("time period"),
                        linetype=guide_legend("time period"));
        p <- p + facet_grid(x~fishery);
        cap<-paste0("Figure &&fno. Estimated retention functions for total catch in",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot max fishing mortality/capture rates
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,'max rates');
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx<-(dfrp$fishery==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='val',colour="case",shape="category",linetype="category"));
        p <- p + geom_line();
        p <- p + geom_point();
        p <- p + labs(y="max rate",x="year");
        p <- p + guides(colour=guide_legend("case"),
                        shape=guide_legend("category"),
                        linetype=guide_legend("category"));
        p <- p + facet_grid(x~fishery);
        cap<-paste0("Figure &&fno. Estimated max fishery rates in ",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
        #zoomed to recent
        dfrpp<-dfrp[idx&(dfrp$y>=(max(dfrp$y)-15)-numRecent),];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='val',colour="case",shape="category",linetype="category"));
        p <- p + geom_line();
        p <- p + geom_point();
        p <- p + labs(y="max rate",x="year");
        p <- p + guides(colour=guide_legend("case"),
                        shape=guide_legend("category"),
                        linetype=guide_legend("category"));
        p <- p + facet_grid(x~fishery);
        cap<-paste0("Figure &&fno. Estimated max fishery rates in ",fsh," (zoomed to recent years).");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    #----------------------------------
    # plot mean fishing mortality/capture rates
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,'mean rates');
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx<-(dfrp$fishery==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='val',colour="case",shape="category",linetype="category"));
        p <- p + geom_line();
        p <- p + geom_point();
        p <- p + labs(y="mean rate",x="year");
        p <- p + guides(colour=guide_legend("case"),
                        shape=guide_legend("category"),
                        linetype=guide_legend("category"));
        p <- p + facet_grid(x~fishery);
        cap<-paste0("Figure &&fno. Estimated mean fishery rates in ",fsh,".");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
        #zoom to recent
        dfrpp<-dfrp[idx&(dfrp$y>=(max(dfrp$y)-15)-numRecent),];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='val',colour="case",shape="category",linetype="category"));
        p <- p + geom_line();
        p <- p + geom_point();
        p <- p + labs(y="mean rate",x="year");
        p <- p + guides(colour=guide_legend("case"),
                        shape=guide_legend("category"),
                        linetype=guide_legend("category"));
        p <- p + facet_grid(x~fishery);
        cap<-paste0("Figure &&fno. Estimated mean fishery rates in ",fsh," (zoomed to recent years).");
        if (showPlot) figno<-(printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    return(invisible(plots));
}