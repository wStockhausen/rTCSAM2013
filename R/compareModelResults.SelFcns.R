#'
#'@title Compare selectivity functions among several model runs.
#'
#'@description Function to compare selectivity functions among
#'several model runs.
#'
#'@param objs  - list of objects derived from Jack's R files for the models to be compared
#'@param cases - vector of labels for model cases (if 'objs' is not given)
#'@param dodge_width - horizontal amount to "dodge" overlapping curves
#'@param dodge_height - doesn't work--no vertical dodging
#'@param pdf   - name for output pdf file
#'
#'@details If 'objs' is not given, then the user is prompted to select Jack's R file output from each
#'model to be compared. If 'cases' is given, the user is prompted to select the file
#'corresponding to each case. If 'cases' is not given, then the user may select an
#'arbitrary number of files (one at a time), ending selection by pressing 'cancel' on the selection box'.\cr\cr
#'If 'objs' is not given, the working directory is set two levels above the 1st model case file selected.\cr\cr
#'Uses \code{PBSmodelling::readList}, \code{reshape2::melt} and \code{wtsUtilities::selectFile}.
#'
#'@return vector of list objects corresponding to the objects returned by each model R file.
#'
#'@import ggplot2
#'
#'@export
#'
compareModelResults.SelFcns<-function(objs=NULL,
                                      cases=NULL,
                                      dodge_width=NULL,
                                      dodge_height=NULL,
                                      pdf="ModelComparisons.SelFcns"){
    if (is.null(objs)){
        in.obj<-0;
        in.objs<-vector(mode="character",length=0)
        nc<-0;
        nca<-Inf;
        if (!is.null(cases)) nca<-length(cases);
        cap<-"Select Jack's R model results file (or cancel to end)";
        while (!is.null(in.obj)&&(nc<nca)){
            nc<-nc+1;
            if (!is.null(cases)) {
                cap<-paste("Select Jack's R model results file for '",cases[nc],"'",sep='');
            }
            in.obj<-wtsUtilities::selectFile(ext="R",caption=cap);
            if(is.character(in.obj)) in.objs<-c(in.objs,in.obj);
        }
        setwd(dirname(in.objs[1]));
        setwd('../..'); #set working dir to location two folder levels above 1st file

        nc<-length(in.objs);
        objs<-vector(mode='list',length=nc);
        if (is.null(cases)) cases<-in.objs;
        names(objs)<-cases;
        for (ic in 1:nc){
            objs[[ic]]<-PBSmodelling::readList(in.objs[ic]);
        }
    }

    if (is.null(cases)){
        cases<-names(objs);
    }

    if(!is.null(pdf)){
        pdf(file=paste(pdf,'TCFMales','pdf',sep='.'),width=8,height=11,onefile=TRUE);
        on.exit(dev.off());
    }

    #set some constants
    THOUSAND<-1000;

    nms<-c("selectivity.fishery.total.new.males",
            "selectivity.fishery.total.old.males",
            "selectivity.fishery.ret.new.males",
            "selectivity.fishery.ret.old.males",
            "retention.curve.males.new",
            "retention.curve.males.old",
            "selectivity.discard.females",
            "selectivity.trawl.females",
            "selectivity.trawl.males",
            "selectivity.snow.females",
            "selectivity.snow.males",
            "selectivity.redk.females",
            "selectivity.redk.males",
            "selectivity.survey.females.1969.1973",
            "selectivity.survey.males.1969.1973",
            "selectivity.survey.females.1974.to.1981",
            "selectivity.survey.males.1974.to.1981",
            "selectivity.survey.females.1982.to.1987",
            "selectivity.survey.males.1982.to.1987",
            "selectivity.survey.females.1988.to.endyr",
            "selectivity.survey.males.1988.to.endyr");

    #----------------------------------------------
    # TCF total selectivity for males
    #----------------------------------------------
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        sel<-obj$"selectivity.fishery.total.new.males";
        rownames(sel)<-obj$"styr":(obj$"endyr"-1);
        colnames(sel)<-obj$"length.bins";
        yrs.tcf<-obj$"years.obs.total.catch.directed.fishery";
        yrs.tcf<-as.character(c(1990,yrs.tcf));
        mdfrp<-reshape2::melt(sel,value.name='val');
        colnames(mdfrp)<-c('y','z','val');
        mdfrp<-mdfrp[mdfrp$y %in% yrs.tcf,];
        mdfrp$z<-as.numeric(mdfrp$z);
        idx<-mdfrp$y=='1990'
        mdfrp$y[idx]<-'y <= 1990'
        mdfrp$case<-case;
        mdfr<-rbind(mdfr,mdfrp);
    }
    mdfr<-reshape2::dcast(mdfr,case+y+z~.,value.var='val')
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1)
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="TCF: male (total) selectivity")
    p <- p + facet_wrap(~y,ncol=3)
    print(p);
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1,position=position_dodge(width=dodge_width))
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="TCF: male (total) selectivity (dodged)")
    p <- p + facet_wrap(~y,ncol=3)
    print(p);

    #----------------------------------------------
    # TCF retained selectivity for males
    #----------------------------------------------
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        sel<-obj$"selectivity.fishery.ret.new.males";
        rownames(sel)<-obj$"styr":(obj$"endyr"-1);
        colnames(sel)<-obj$"length.bins";
        yrs.tcf<-obj$"years.obs.total.catch.directed.fishery";
        yrs.tcf<-as.character(c(1990,yrs.tcf));
        mdfrp<-reshape2::melt(sel,value.name='val');
        colnames(mdfrp)<-c('y','z','val');
        mdfrp<-mdfrp[mdfrp$y %in% yrs.tcf,];
        mdfrp$z<-as.numeric(mdfrp$z);
        idx<-mdfrp$y=='1990'
        mdfrp$y[idx]<-'y <= 1990'
        mdfrp$case<-case;
        mdfr<-rbind(mdfr,mdfrp);
    }
    mdfr<-reshape2::dcast(mdfr,case+y+z~.,value.var='val')
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1)
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="TCF: male retained selectivity")
    p <- p + facet_wrap(~y,ncol=3)
    print(p);
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1,position=position_dodge(width=dodge_width))
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="TCF: male retained selectivity (dodged)")
    p <- p + facet_wrap(~y,ncol=3)
    print(p);

    if (!is.null(pdf)) dev.off();
    if(!is.null(pdf)){
        pdf(file=paste(pdf,'TheRest','pdf',sep='.'),width=6,height=8,onefile=TRUE);
        on.exit(dev.off());
    }


    #----------------------------------------------
    # TCF retention for males
    #----------------------------------------------
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        sel<-obj$"retention.curve.males.new";
        rownames(sel)<-obj$"styr":(obj$"endyr"-1);
        colnames(sel)<-obj$"length.bins";
        yrs.tcf<-as.character(c(1990,1991));
        mdfrp<-reshape2::melt(sel,value.name='val');
        colnames(mdfrp)<-c('y','z','val');
        mdfrp<-mdfrp[mdfrp$y %in% yrs.tcf,];
        mdfrp$z<-as.numeric(mdfrp$z);
        idx<-mdfrp$y=='1990'
        mdfrp$y[idx]<-'y <= 1990'
        idx<-mdfrp$y=='1991'
        mdfrp$y[idx]<-'y >= 1991'
        mdfrp$case<-case;
        mdfr<-rbind(mdfr,mdfrp);
    }
    mdfr<-reshape2::dcast(mdfr,case+y+z~.,value.var='val')
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1)
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="TCF: male retention functions")
    p <- p + facet_wrap(~y,ncol=1)
    print(p);
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1,position=position_dodge(width=dodge_width))
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="TCF: male retention functions (dodged)")
    p <- p + facet_wrap(~y,nrow=3)
    print(p);

    #----------------------------------------------
    # TCF total selectivity for females
    #----------------------------------------------
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        sel<-obj$"selectivity.discard.females";
        names(sel)<-obj$"length.bins";
        mdfrp<-reshape2::melt(sel,value.name='val');
        mdfrp$z<-as.numeric(rownames(mdfrp));
        mdfrp$y<-'all years';
        mdfrp$case<-case;
        mdfr<-rbind(mdfr,mdfrp);
    }
    mdfr<-reshape2::dcast(mdfr,case+y+z~.,value.var='val')
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1)
#    p <- p + geom_point(size=3,alpha=0.85)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="TCF: female selectivity")
    p <- p + facet_wrap(~y,nrow=3)
    print(p)
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1,position=position_dodge(width=dodge_width))
#    p <- p + geom_point(size=3,alpha=0.85)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="TCF: female selectivity (dodged)")
    p <- p + facet_wrap(~y,nrow=3)
    print(p)

    #----------------------------------------------
    # SCF selectivity for males
    #----------------------------------------------
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        sel<-obj$"selectivity.snow.males";
        rownames(sel)<-c("y <= 1996","1997 <= y <= 2004","2005 <= y");
        colnames(sel)<-obj$"length.bins";
        mdfrp<-reshape2::melt(sel,value.name='val');
        colnames(mdfrp)<-c('y','z','val');
        mdfrp$z<-as.numeric(mdfrp$z);
        mdfrp$case<-case;
        mdfr<-rbind(mdfr,mdfrp);
    }
    mdfr<-reshape2::dcast(mdfr,case+y+z~.,value.var='val')
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1)
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="SCF: male selectivity")
    p <- p + facet_wrap(~y,ncol=1,nrow=3)
    print(p)
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1,position=position_dodge(width=dodge_width))
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="SCF: male selectivity (dodged)")
    p <- p + facet_wrap(~y,ncol=1,nrow=3)
    print(p)

    #----------------------------------------------
    # SCF selectivity for females
    #----------------------------------------------
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        sel<-obj$"selectivity.snow.females";
        rownames(sel)<-c("y <= 1996","1997 <= y <= 2004","2005 <= y");
        colnames(sel)<-obj$"length.bins";
        mdfrp<-reshape2::melt(sel,value.name='val');
        colnames(mdfrp)<-c('y','z','val');
        mdfrp$z<-as.numeric(mdfrp$z);
        mdfrp$case<-case;
        mdfr<-rbind(mdfr,mdfrp);
    }
    mdfr<-reshape2::dcast(mdfr,case+y+z~.,value.var='val')
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1)
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="SCF: female selectivity")
    p <- p + facet_wrap(~y,ncol=1,nrow=3);
    print(p)
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1,position=position_dodge(width=dodge_width))
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="SCF: female selectivity (dodged)")
    p <- p + facet_wrap(~y,ncol=1,nrow=3);
    print(p)

    #----------------------------------------------
    # RKF selectivity for males
    #----------------------------------------------
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        sel<-obj$"selectivity.redk.males";
        rownames(sel)<-c("y <= 1996","1997 <= y <= 2004","2005 <= y");
        colnames(sel)<-obj$"length.bins";
        mdfrp<-reshape2::melt(sel,value.name='val');
        colnames(mdfrp)<-c('y','z','val');
        mdfrp$z<-as.numeric(mdfrp$z);
        mdfrp$case<-case;
        mdfr<-rbind(mdfr,mdfrp);
    }
    p <- ggplot(data=mdfr,aes_string('z','val',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1)
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="RKF: male selectivity")
    p <- p + facet_wrap(~y,ncol=1,nrow=3)
    print(p)
    p <- ggplot(data=mdfr,aes_string('z','val',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1,position=position_dodge(width=dodge_width))
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="RKF: male selectivity (dodged)")
    p <- p + facet_wrap(~y,ncol=1,nrow=3)
    print(p)

    #----------------------------------------------
    # RKF selectivity for females
    #----------------------------------------------
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        sel<-obj$"selectivity.redk.females";
        rownames(sel)<-c("y <= 1996","1997 <= y <= 2004","2005 <= y");
        colnames(sel)<-obj$"length.bins";
        mdfrp<-reshape2::melt(sel,value.name='val');
        colnames(mdfrp)<-c('y','z','val');
        mdfrp$z<-as.numeric(mdfrp$z);
        mdfrp$case<-case;
        mdfr<-rbind(mdfr,mdfrp);
    }
    mdfr<-reshape2::dcast(mdfr,case+y+z~.,value.var='val')
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1)
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="RKF: female selectivity")
    p <- p + facet_wrap(~y,ncol=1,nrow=3)
    print(p)
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1,position=position_dodge(width=dodge_width))
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="RKF: female selectivity (dodged)")
    p <- p + facet_wrap(~y,ncol=1,nrow=3)
    print(p)

    #----------------------------------------------
    # GTF selectivity for males
    #----------------------------------------------
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        sel<-obj$"selectivity.trawl.males";
        rownames(sel)<-c("y <= 1986","1977 <= y <= 1996","1997 <= y");
        colnames(sel)<-obj$"length.bins";
        mdfrp<-reshape2::melt(sel,value.name='val');
        colnames(mdfrp)<-c('y','z','val');
        mdfrp$z<-as.numeric(mdfrp$z);
        mdfrp$case<-case;
        mdfr<-rbind(mdfr,mdfrp);
    }
    mdfr<-reshape2::dcast(mdfr,case+y+z~.,value.var='val')
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1)
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="GTF: male selectivity")
    p <- p + facet_wrap(~y,ncol=1,nrow=3)
    print(p)
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1,position=position_dodge(width=dodge_width))
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="GTF: male selectivity (dodged)")
    p <- p + facet_wrap(~y,ncol=1,nrow=3)
    print(p)

    #----------------------------------------------
    # GTF selectivity for females
    #----------------------------------------------
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        sel<-obj$"selectivity.trawl.females";
        rownames(sel)<-c("y <= 1986","1977 <= y <= 1996","1997 <= y");
        colnames(sel)<-obj$"length.bins";
        mdfrp<-reshape2::melt(sel,value.name='val');
        colnames(mdfrp)<-c('y','z','val');
        mdfrp$z<-as.numeric(mdfrp$z);
        mdfrp$case<-case;
        mdfr<-rbind(mdfr,mdfrp);
    }
    mdfr<-reshape2::dcast(mdfr,case+y+z~.,value.var='val')
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1)
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="GTF: female selectivity")
    p <- p + facet_wrap(~y,ncol=1,nrow=32)
    print(p)
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1,position=position_dodge(width=dodge_width))
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="GTF: female selectivity (dodged)")
    p <- p + facet_wrap(~y,ncol=1,nrow=3)
    print(p)

    #----------------------------------------------
    # survey selectivity for males
    #----------------------------------------------
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        sel<-obj$"selectivity.survey.males.1974.to.1981";
        names(sel)<-obj$"length.bins";
        mdfrp<-reshape2::melt(sel,value.name='val');
        mdfrp$z<-as.numeric(rownames(mdfrp));
        mdfrp$y<-'1974 <= y <= 1981';
        mdfrp$case<-case;
        mdfr<-rbind(mdfr,mdfrp);
        sel<-obj$"selectivity.survey.males.1982.to.1987";
        names(sel)<-obj$"length.bins";
        mdfrp<-reshape2::melt(sel,value.name='val');
        mdfrp$z<-as.numeric(rownames(mdfrp));
        mdfrp$y<-'1982 <= y <= 1987';
        mdfrp$case<-case;
        mdfr<-rbind(mdfr,mdfrp);
        sel<-obj$"selectivity.survey.males.1988.to.endyr";
        names(sel)<-obj$"length.bins";
        mdfrp<-reshape2::melt(sel,value.name='val');
        mdfrp$z<-as.numeric(rownames(mdfrp));
        mdfrp$y<-'1988 <= y';
        mdfrp$case<-case;
        mdfr<-rbind(mdfr,mdfrp);
    }
    mdfr<-reshape2::dcast(mdfr,case+y+z~.,value.var='val')
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1)
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="NMFS trawl survey: male selectivity")
    p <- p + facet_wrap(~y,ncol=1,nrow=3)
    print(p)
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1,position=position_dodge(width=dodge_width))
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="NMFS trawl survey: male selectivity (dodged)")
    p <- p + facet_wrap(~y,ncol=1,nrow=3)
    print(p)

    #----------------------------------------------
    # survey selectivity for females
    #----------------------------------------------
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        sel<-obj$"selectivity.survey.females.1974.to.1981";
        names(sel)<-obj$"length.bins";
        mdfrp<-reshape2::melt(sel,value.name='val');
        mdfrp$z<-as.numeric(rownames(mdfrp));
        mdfrp$y<-'1974 <= y <= 1981';
        mdfrp$case<-case;
        mdfr<-rbind(mdfr,mdfrp);
        sel<-obj$"selectivity.survey.females.1982.to.1987";
        names(sel)<-obj$"length.bins";
        mdfrp<-reshape2::melt(sel,value.name='val');
        mdfrp$z<-as.numeric(rownames(mdfrp));
        mdfrp$y<-'1982 <= y <= 1987';
        mdfrp$case<-case;
        mdfr<-rbind(mdfr,mdfrp);
        sel<-obj$"selectivity.survey.females.1988.to.endyr";
        names(sel)<-obj$"length.bins";
        mdfrp<-reshape2::melt(sel,value.name='val');
        mdfrp$z<-as.numeric(rownames(mdfrp));
        mdfrp$y<-'1988 <= y';
        mdfrp$case<-case;
        mdfr<-rbind(mdfr,mdfrp);
    }
    mdfr<-reshape2::dcast(mdfr,case+y+z~.,value.var='val')
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1)
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="NMFS trawl survey: female selectivity")
    p <- p + facet_wrap(~y,ncol=1,nrow=3)
    print(p)
    p <- ggplot(data=mdfr,aes_string('z','.',colour='case',shape='case'))
    p <- p + geom_line(size=0.5,alpha=1,position=position_dodge(width=dodge_width))
#    p <- p + geom_point(size=3,alpha=0.8)
    p <- p + labs(x="size (mm CW)",y="selectivity",title="NMFS trawl survey: female selectivity (dodged)")
    p <- p + facet_wrap(~y,ncol=1,nrow=3)
    print(p)

}
