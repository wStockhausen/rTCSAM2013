#'
#'@title Compare estimated/predicted fishery selectivity functions among several model runs
#'
#'@description Function to compare estimated/predicted fishery selectivity functions among 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - name for output pdf file
#'
#'@details Uses \code{getMDFR.FisheryQuantities}.
#'
#'@return list of ggplot2 objects
#'
#'@export
#'
compareModelResults.FisherySelFcns<-function(obj,
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
    # plot selectivity functions
    #----------------------------------
    dfrp<-getMDFR.FisheryQuantities(obj,'selfcns');
    dfrp$case<-factor(dfrp$case,levels=cases);
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

    return(invisible(plots));
}