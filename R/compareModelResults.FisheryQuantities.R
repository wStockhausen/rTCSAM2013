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
    # define output list of plots
    #----------------------------------
    plots<-list();
    
    #----------------------------------
    # observed and predicted retained catch mortality from fisheries (1000's t)
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"bio.retm");
    #make 4-plot from observations & case results
    ps<-plot2.ModelComparisonsGG.TimeSeries(dfrp,
                                            numRecent=numRecent,
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
    cap2<-"Figure &&fno. Comparison of observed and predicted retained catch mortality.";
    plots[["catchmort.ret"]]<-list(norm=list(cap=cap1,plot=ps[[1]]),
                                  zoom=list(cap=cap2,plot=ps[[2]]));

    #----------------------------------
    # observed and predicted discard catch mortality from fisheries (1000's t)
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"bio.dscm");
    #make 4-plot from observations & case results
    ps<-plot2.ModelComparisonsGG.TimeSeries(dfrp,
                                            numRecent=numRecent,
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
    cap2<-"Figure &&fno. Comparison of observed and predicted discard catch mortality.";
    plots[["catchmort.dsc"]]<-list(norm=list(cap=cap1,plot=ps[[1]]),
                                  zoom=list(cap=cap2,plot=ps[[2]]));

    #----------------------------------
    # observed and predicted total catch mortality from fisheries (1000's t)
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"bio.totm");
    #make 4-plot from observations & case results
    plots1<-list();
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx <- dfrp$fishery==fsh;
        ps<-plot2.ModelComparisonsGG.TimeSeries(dfrp[idx,],
                                                numRecent=numRecent,
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
        cap2<-paste0("Figure &&fno. Comparison of observed and predicted total catch biomass for ",fsh,".");
        plots1[[fsh]]<-list(norm=list(cap=cap1,plot=ps[[1]]),
                            zoom=list(cap=cap2,plot=ps[[2]]));
    }
    plots[["catchmort.totm"]]<-plots1; plots1<-NULL;

    #----------------------------------
    # plot z-scores for observed and predicted mature (spawning) biomass from the survey
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"zscores");
    #--retained catch
    plots1<-list();
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
        plots1[[fsh]] <- list(cap=cap,plot=p);
    }
    plots[["zscores.retm"]]<-plots1; plots1<-NULL;
    #--total catch
    plots1<-list();
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
        plots1[[fsh]] <- list(cap=cap,plot=p);
    }
    plots[["zscores.totm"]]<-plots1; plots1<-NULL;
    
    #----------------------------------
    # plot observed retained catch size comps as bubble plots
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,type="prNatZ.ret");
    plots1<-list();
    for (fsh in c('TCF')){
        idx<-(dfrp$fishery==fsh)&(dfrp$category=="observed")
        dfrpp<-dfrp[idx,]
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
        plots1[[fsh]]<-list(cap=cap,plot=p);
    }
    plots[["ZCs.ret.obs"]]<-plots1; plots1<-NULL;
    
    #----------------------------------
    # plot observed total catch size comps as bubble plots
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,type="prNatZ.tot");
    plots1<-list();
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx<-(dfrp$fishery==fsh)&(dfrp$category=="observed")
        dfrpp<-dfrp[idx,]
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
        plots1[[fsh]]<-list(cap=cap,plot=p);
    }
    plots[["ZCs.tot.obs"]]<-plots1; plots1<-NULL;
    
    #----------------------------------
    # plot observed and predicted retained catch size comps as line plots
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,type="prNatZ.ret");
    plots1<-list();
    for (fsh in c('TCF')){
        idx<-(dfrp$fishery==fsh)
        dfrpp<-dfrp[idx,];
        
        idxo<-dfrpp$category=="observed";
        idxp<-dfrpp$category=="predicted";
        
        p1 <- ggplot(data=dfrpp,mapping=aes_string(x='z',y='val',fill='category',colour='category',linetype='case'));
        p1 <- p1 + geom_bar(data=dfrpp[idxo,],stat='identity');
        p1 <- p1 + geom_line(data=dfrpp[idxp,]);
        p1 <- p1 + facet_wrap(~y,ncol=5);
        p1 <- p1 + labs(x="size (mm CW)",y="proportion") + ggtitle(paste0("Retained catch in ",fsh));
        if (showPlot) print(p1);
        plots1[[fsh]]<-list(males=list(cap=cap1,plot=p1),females=NULL);
    }
    plots[["ZCs.ret.comp"]]<-plots1; plots1<-NULL;
    
    #----------------------------------
    # plot observed and predicted total catch size comps as line plots
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,type="prNatZ.tot");
    plots1<-list();
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx<-(dfrp$fishery==fsh)
        dfrpp<-dfrp[idx,];
        
        idxm<-dfrpp$x=="male";
        idxf<-dfrpp$x=="female";
        idxo<-dfrpp$category=="observed";
        idxp<-dfrpp$category=="predicted";
        
        p1 <- ggplot(data=dfrpp,mapping=aes_string(x='z',y='val',fill='category',colour='category',linetype='case'));
        p1 <- p1 + geom_bar(data=dfrpp[idxo&idxm,],stat='identity');
        p1 <- p1 + geom_line(data=dfrpp[idxp&idxm,]);
        p1 <- p1 + facet_wrap(~y,ncol=5);
        p1 <- p1 + labs(x="size (mm CW)",y="proportion") + ggtitle(paste0("Male total catch in ",fsh));
        if (showPlot) print(p1);
        cap1<-paste0("Figure &&fno. Observed and predicted proportions-at-size for male total catch in ",fsh,".");
        p2 <- ggplot(data=dfrpp,mapping=aes_string(x='z',y='val',fill='category',colour='category',linetype='case'));
        p2 <- p2 + geom_bar(data=dfrpp[idxo&idxf,],stat='identity');
        p2 <- p2 + geom_line(data=dfrpp[idxp&idxf,]);
        p2 <- p2 + facet_wrap(~y,ncol=5);
        p2 <- p2 + labs(x="size (mm CW)",y="proportion") + ggtitle(paste0("Female total catch in ",fsh));
        if (showPlot) print(p2);
        cap2<-paste0("Figure &&fno. Observed and predicted proportions-at-size for female total catch in ",fsh,".");
        plots1[[fsh]]<-list(males=list(cap=cap1,plot=p1),females=list(cap=cap2,plot=p2));
    }
    plots[["ZCs.tot.comp"]]<-plots1; plots1<-NULL;
    
    #----------------------------------
    # plot retained catch size comp residuals 
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,type="PRs.ret");
    dfrp$sign<-ifelse(test=dfrp$val>0,yes=">0",no="<0");
    dfrp$val <- abs(dfrp$val);
    plots1<-list();
    for (fsh in c('TCF')){
        idx<-(dfrp$fishery==fsh);
        dfrpp<-dfrp[idx,];
        p1 <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='z',size='val',fill='sign',colour='case',linetype='case'));
        p1 <- p1 + scale_size_area(max_size=10);
        p1 <- p1 + geom_point(alpha=0.8,shape=21,color='black');
        p1 <- p1 + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
        p1 <- p1 + labs(y="size (mm CW)",x="year") + ggtitle(paste0(fsh,": retained catch Pearson's residuals"));
        p1 <- p1 + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                          size=guide_legend(order=1));
        p1 <- p1 + facet_grid(x~fishery);
        p1 <- p1 + theme(legend.box='horizontal');
        if (showPlot) print(p1);
        cap1<-paste0("Figure &&fno. Pearson's residuals for retained catch proportions-at-size in ",fsh,".");
        plots1[[fsh]]<-list(cap=cap1,plot=p1);
    }
    plots[["ZCs.tot.PRs"]]<-plots1; plots1<-NULL;
    
    #----------------------------------
    # plot total catch size comp residuals 
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,type="PRs.tot");
    dfrp$sign<-ifelse(test=dfrp$val>0,yes=">0",no="<0");
    dfrp$val <- abs(dfrp$val);
    plots1<-list();
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx<-(dfrp$fishery==fsh);
        dfrpp<-dfrp[idx,];
        p1 <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='z',size='val',fill='sign',colour='case',linetype='case'));
        p1 <- p1 + scale_size_area(max_size=10);
        p1 <- p1 + geom_point(alpha=0.8,shape=21,color='black');
        p1 <- p1 + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
        p1 <- p1 + labs(y="size (mm CW)",x="year") + ggtitle(paste0(fsh,": total catch Pearson's residuals"));
        p1 <- p1 + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                          size=guide_legend(order=1));
        p1 <- p1 + facet_grid(x~fishery);
        p1 <- p1 + theme(legend.box='horizontal');
        if (showPlot) print(p1);
        cap1<-paste0("Figure &&fno. Pearson's residuals for proportions-at-size in ",fsh,".");
        plots1[[fsh]]<-list(cap=cap1,plot=p1);
    }
    plots[["ZCs.tot.PRs"]]<-plots1; plots1<-NULL;
    
    #----------------------------------
    # plot observed and predicted mean retained catch size comps
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"mnPrNatZ.ret");
    dfrp$lci<-dfrp$val-dfrp$stdv;
    dfrp$uci<-dfrp$val+dfrp$stdv;
    plots1<-list();
    for (fsh in c('TCF')){
        idx<-(dfrp$fishery==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(dfrpp,aes_string(x='z',y='val',colour="category",linetype="case"));
        p <- p + geom_line();
        p <- p + geom_errorbar(mapping=aes_string(ymin='lci',ymax='uci'))
        p <- p + facet_grid("x~fishery");
        p <- p + labs(y="proportion",x="size (mm CW)")
        if (showPlot) print(p);
        cap<-paste0("Figure &&fno. Observed and predicted mean proportions-at-size for retained catch in ",fsh,".");
        plots1[[fsh]]<-list(cap=cap,plot=p);
    }
    plots[["ZCs.means.ret"]]<-plots1; plots1<-NULL;

    #----------------------------------
    # plot observed and predicted mean total catch size comps
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"mnPrNatZ.tot");
    dfrp$lci<-dfrp$val-dfrp$stdv;
    dfrp$uci<-dfrp$val+dfrp$stdv;
    plots1<-list();
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx<-(dfrp$fishery==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(dfrpp,aes_string(x='z',y='val',colour="category",linetype="case"));
        p <- p + geom_line();
        p <- p + geom_errorbar(mapping=aes_string(ymin='lci',ymax='uci'))
        p <- p + facet_grid("x~fishery");
        p <- p + labs(y="proportion",x="size (mm CW)")
        if (showPlot) print(p);
        cap<-paste0("Figure &&fno. Observed and predicted mean proportions-at-size for total catch in ",fsh,".");
        plots1[[fsh]]<-list(cap=cap,plot=p);
    }
    plots[["ZCs.means.tot"]]<-plots1; plots1<-NULL;

    #----------------------------------
    # plot input and effective sample sizes for retained catch size comps
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"effSS.ret");
    plots1<-list();
    for (fsh in c('TCF')){
        idx<-(dfrp$fishery==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='val',colour="category",linetype="case"));
        p <- p + geom_point();
        p <- p + geom_line();
        p <- p + facet_grid(x~fishery)
        p <- p + labs(y="sample size",x="year")
        if (showPlot) print(p);
        cap<-paste0("Figure &&fno. Input and effective sample sizes for retained catch proportions-at-size in",fsh,".");
        plots1[[fsh]]<-list(cap=cap,plot=p);
    }
    plots[["ZCs.effSSs.ret"]]<-plots1;
    
    #----------------------------------
    # plot input and effective sample sizes for total catch size comps
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,"effSS.tot");
    plots1<-list();
    for (fsh in c('TCF','SCF','RKF','GTF')){
        idx<-(dfrp$fishery==fsh);
        dfrpp<-dfrp[idx,];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='y',y='val',colour="category",linetype="case"));
        p <- p + geom_point();
        p <- p + geom_line();
        p <- p + facet_grid(x~fishery)
        p <- p + labs(y="sample size",x="year")
        if (showPlot) print(p);
        cap<-paste0("Figure &&fno. Input and effective sample sizes for total catch proportions-at-size in",fsh,".");
        plots1[[fsh]]<-list(cap=cap,plot=p);
    }
    plots[["ZCs.effSSs.tot"]]<-plots1;
    
    #----------------------------------
    # plot selectivity functions
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,'selfcns');
    dfrp$pc<-as.character(dfrp$pc);
    plots1<-list();
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
        if (showPlot) print(p);
        cap<-paste0("Figure &&fno. Estimated selectivity functions for total catch in",fsh,".");
        plots1[[fsh]]<-list(cap=cap,plot=p);
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
        if (showPlot) print(p);
        cap<-paste0("Figure &&fno. Estimated selectivity functions for total catch in",fsh,".");
        plots1[[fsh]]<-list(cap=cap,plot=p);
    }
    plots[["selfcns"]]<-plots1;
    plots2<-list();
    for (fsh in c('TCF')){
        idx<-(dfrp$fishery==fsh);
        idt<-(dfrp$pc %in% as.character(1991:2014))
        dfrpp<-dfrp[idx&idc&idt,];
        dfrppp<-dfrp[idx&idc&(dfrp$pc=='1990'),c("z","val","case")];
        p <- ggplot(data=dfrpp,mapping=aes_string(x='z',y='val',colour="case"));
        #p <- p + annotate("line",x=dfrppp$z,y=dfrppp$val,colour=dfrppp$case,linetype=2);
        p <- p + geom_line();
        p <- p + labs(y="selectivity",x="size (mm CW)");
        p <- p + guides(colour=guide_legend("case"));
        p <- p + facet_wrap(~pc,ncol=3);
        if (showPlot) print(p);
        cap<-paste0("Figure &&fno. Estimated selectivity functions for total catch in",fsh,".");
        plots2[[fsh]]<-list(cap=cap,plot=p);
    }
    plots[["selfcns.TCF"]]<-plots2;
    plots3<-list();
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
        if (showPlot) print(p);
        cap<-paste0("Figure &&fno. Estimated retention functions for total catch in",fsh,".");
        plots3[[fsh]]<-list(cap=cap,plot=p);
    }
    plots[["retfcns"]]<-plots3;
    
    return(invisible(plots));
}