#'
#' @title Plot TCSAM2013 model results.
#' 
#' @description Function to plot model results from TCSAM2013.
#' 
#' @param endyr - assessment year [required]
#' @param obj.rep - report file list object or filename for R-style report
#' @param obj.std - dataframe object with parameter std info or filename for std file
#' @param obj.prs - dataframe object w/ parameters info or csv file with parameters info
#' @param base.dir - path to base folder
#' @param mdl   - model name (optional if R report file is read)
#' @param isFRev - flag (T/F) indicating whether TCSAM2103 is FRev (TRUE) or not (FALSE)
#' @param styr  - model start year
#' @param obsyr - first year for survey observations
#' @param pltyr - first year for some plots
#' @param F35 - F35 value for control rule plot
#' @param B35 - B35 value for control rule plot
#' 
#' @return list with elements: \cr
#' rep - obj.rep
#' std - obj.std
#' prs - obj.prs
#' 
#' @import graphics
#' @import stats
#' @import PBSmodelling
#' @import tcltk
#' @importFrom wtsUtilities selectFile
#' @importFrom wtsPlots plotErrorBars.V
#'
#'@export
#' 
#----------------------------------
# Set model variables for plots
plotTCSAM2013<-function(endyr=NULL,    #assessment year
                        obj.rep=NULL,
                        obj.std=NULL,
                        obj.prs=NULL,
                        base.dir='./',
                        mdl=NULL,      #executable model name
                        isFRev=FALSE,  #flag indicating model is FRev type
                        styr=1949,     #model start year
                        obsyr=1974,    #first year of survey observations
                        pltyr=1969,    #first year for plots
                        F35=0.73,      #F35 value
                        B35=33.54){    #B35 value

    if (is.null(endyr)){
        cat("Must set 'endyr' to assessment year.\n",
            "Aborting...\n");
        return(NULL);
    }
    #set some constants
    THOUSAND<-1000;
    length.bins<- seq(27,182,length=32);
    years<-seq(styr,endyr);
    obsyears=seq(obsyr,endyr);
    plotyears=seq(pltyr,endyr);
    years.m1<-years[1:(length(years)-1)]; 

    #----------------------------------
    # Load files
    if(!is.list(obj.rep)){
        if (!is.character(obj.rep)){
            in.rep<-wtsUtilities::selectFile(ext="R",caption="Select Jack's R output file");
            base.dir=dirname(in.rep);
            if (is.null(mdl)) {mdl<-strsplit(basename(in.rep),".",fixed=TRUE)[[1]][1];}
        } else {
            in.rep<-obj.rep;
        }
        obj.rep = readList(in.rep);
    }
    if (!is.data.frame(obj.std)){
        if (!is.character(obj.std)){
            in.std<-wtsUtilities::selectFile(ext='std',caption="Select std file");
        } else {
            in.std<-obj.std;
        }
        obj.std<-NULL;
        if (!is.null(in.std)) obj.std = read.table(in.std,as.is=T,header=F,skip=1);
    }
    if (!is.data.frame(obj.prs)){
        if (!is.character(obj.prs)){
            in.prs<-wtsUtilities::selectFile(ext='csv',caption="Select active parameters info csv file");
        } else {
            in.prs<-obj.prs;
        }
        obj.prs<-NULL;
        if (!is.null(in.prs)){
            obj.prs<-read.csv(in.prs,stringsAsFactors=FALSE);
        } else {
            cat('Must specify an active parameters csv file.\n',
                'Aborting...\n');
            return(NULL);
        }
    }
    
    #----------------------------------
    # Set filename for pdf output
    filen=paste(mdl,"oldstyle.pdf",sep='.');
    pdf(file=file.path(base.dir,filen),width=8.5,height=11);
    #----------------------------------

    #----------------------------------
    # Plot parameter values w/ limits and std's
    checkParams(obj.prs,obj.std);
    #----------------------------------
    
    #----------------------------------
    # Pull out data
    #----------------------------------
    recruitsd<-NULL;
    if (!is.null(obj.std)) recruitsd=rbind(obj.std[obj.std[,2]=="rec_early_sd",],obj.std[obj.std[,2]=="recm_sd",])
    tanner.cv= cbind(obsyears,obj.rep$Observed.survey.female.spawning.biomass.cv,obj.rep$Observed.survey.male.spawning.biomass.cv)
    #----------------------------------

    #----------------------------------
    # plot observed total and mature (spawning) biomass from survey
    #----------------------------------
    #observed spawning biomass
    spB.m.obs <-obj.rep$"Observed.survey.male.spawning.biomass"
    spB.f.obs <-obj.rep$"Observed.survey.female.spawning.biomass"
    spB.t.obs <-obj.rep$"Observed.survey.biomass"
    spB.tm.obs<-spB.m.obs+spB.f.obs;#total observed survey MATURE biomass
    #cv's    
    idx<-(1:length(years))[years==min(obsyears)]:length(years)  
    cv.f<-tanner.cv[,2];
    cv.m<-tanner.cv[,3];
    cv.t=sqrt((spB.m.obs[idx]*cv.m)^2 + (spB.f.obs[idx]*cv.f)^2)/(spB.tm.obs[idx]);


    par(oma=c(0.5,1,1,0.5),mar=c(2,5,2,1)+0.2,mfrow=c(2,1))
    #total survey biomass
    plot(c(0,1),c(0,1),type="l",lty=2,
         ylim=c(0,400),xlim=c(min(obsyears),max(obsyears)),ylab="Biomass (1000's t)",xlab='');
    #CV's assumed same as for total SPAWNING biomass in survey
    lower= (spB.t.obs)*(exp(-1.96*sqrt(log(1+cv.t^2)))-1);#lower error bar
    upper= (spB.t.obs)*(exp( 1.96*sqrt(log(1+cv.t^2)))-1);#upper error bar
    plotErrorBars.V(obsyears,spB.t.obs,upper=upper,lower=lower);
    points(obsyears,spB.t.obs,pch=21);
    mtext("Observed total survey biomass", side=3, adj=0.05)

    #total mature survey biomass
    plot(c(0,1),c(0,1),type="l",lty=2,
         ylim=c(0,400),xlim=c(min(obsyears),max(obsyears)),ylab="Biomass (1000's t)",xlab='');
    lower= (spB.tm.obs)[idx]*(exp(-1.96*sqrt(log(1+cv.t^2)))-1);#lower error bar
    upper= (spB.tm.obs)[idx]*(exp( 1.96*sqrt(log(1+cv.t^2)))-1);#upper error bar
    plotErrorBars.V(obsyears,spB.tm.obs[idx],upper=upper,lower=lower);
    points(obsyears,spB.tm.obs[idx],pch=21);
    mtext("Observed total mature survey biomass", side=3, adj=0.05)

    par(oma=c(0.5,1,1,0.5),mar=c(2,5,2,1)+0.2,mfrow=c(2,1))
    #mature survey biomass by sex
    plot(c(0,1),c(0,1),type="l",lty=2,
         ylim=c(0,400),xlim=c(min(obsyears),max(obsyears)),ylab="Biomass (1000's t)",xlab='');
    lower= (spB.f.obs)[idx]*(exp(-1.96*sqrt(log(1+cv.f^2)))-1);#lower error bar
    upper= (spB.f.obs)[idx]*(exp( 1.96*sqrt(log(1+cv.f^2)))-1);#upper error bar
    plotErrorBars.V(obsyears,spB.f.obs[idx],upper=upper,lower=lower,col='green');
    points(obsyears,spB.f.obs[idx],pch=21,col='green');
    lower= (spB.m.obs)[idx]*(exp(-1.96*sqrt(log(1+cv.m^2)))-1);#lower error bar
    upper= (spB.m.obs)[idx]*(exp( 1.96*sqrt(log(1+cv.m^2)))-1);#upper error bar
    plotErrorBars.V(obsyears+0.2,spB.m.obs[idx],upper=upper,lower=lower,col='blue');
    points(obsyears+0.2,spB.m.obs[idx],pch=23,col='blue');
    mtext("Observed mature survey biomass", side=3, adj=0.05)
    legend("topright",
           c("females", "males"),
           pch=c(21,23),col=c('green','blue')) 
    #----------------------------------

    #----------------------------------
    # plot natural mortality estimates
    #----------------------------------
    if (!is.null(obj.std)){
        par(oma=c(0.5,1,1,0.5),mar=c(4,5,2,1)+0.2,mfrow=c(2,1))
        plotNatMort(obj.std,min.yr=1970);
    }
    #----------------------------------

    #----------------------------------
    # plot observed and predicted mature biomass by sex
    #----------------------------------
    #observed spawning biomass
    spB.f.obs <-obj.rep$"Observed.survey.female.spawning.biomass"
    spB.m.obs <-obj.rep$"Observed.survey.male.spawning.biomass"
    cv.f<-tanner.cv[,2];
    cv.m<-tanner.cv[,3];
    spB.f.prd <-obj.rep$"Predicted.Female.survey.mature.Biomass"
    spB.m.prd <-obj.rep$"Predicted.Male.survey.mature.Biomass"

    par(oma=c(0.5,1,1,0.5),mar=c(2,5,1,1)+0.2,mfrow=c(3,1))
    #mature survey biomass by sex
    #--females
    lower= (spB.f.obs)[idx]*(exp(-1.96*sqrt(log(1+cv.f^2)))-1);#lower error bar
    upper= (spB.f.obs)[idx]*(exp( 1.96*sqrt(log(1+cv.f^2)))-1);#upper error bar
    plot(c(0,1),c(0,1),type="n",
         ylim=c(0,300),xlim=c(min(obsyears),max(obsyears)),ylab="Biomass (1000's t)",xlab='');
    lines(years,spB.f.prd,lty=1,lwd=2,col='black')
    plotErrorBars.V(obsyears,spB.f.obs[idx],upper=upper,lower=lower,col='green');
    points(obsyears,spB.f.obs[idx],pch=21,col='green',bg='green');
    mtext("Mature female survey biomass", side=3, adj=0.05,cex=0.9)
    legend("topright",
           c("observed", "predicted"),
           pch=c(21,NA),col=c('green','black'),pt.bg=c('green',NA),
           lty=c(NA,1)) 
    #--males
    lower= (spB.m.obs)[idx]*(exp(-1.96*sqrt(log(1+cv.m^2)))-1);#lower error bar
    upper= (spB.m.obs)[idx]*(exp( 1.96*sqrt(log(1+cv.m^2)))-1);#upper error bar
    plot(c(0,1),c(0,1),type="n",
         ylim=c(0,300),xlim=c(min(obsyears),max(obsyears)),ylab="Biomass (1000's t)",xlab='');
    lines(years,spB.m.prd,lty=1,lwd=2,col='black')
    plotErrorBars.V(obsyears,spB.m.obs[idx],upper=upper,lower=lower,col='blue');
    points(obsyears,spB.m.obs[idx],pch=23,col='blue',bg='blue');
    mtext("Mature male survey biomass", side=3, adj=0.05,cex=0.9)
    legend("topright",
           c("observed", "predicted"),
           pch=c(21,NA),col=c('blue','black'),pt.bg=c('blue',NA),
           lty=c(NA,1)) 

    # Standardize survey biomass residuals and plot them
    idx<-(obsyears[1]-years[1]+1):(length(years))
    st.devs.f<-(log(spB.f.obs[idx])-log(spB.f.prd[idx]))/sqrt(log(1+cv.f^2));
    st.devs.m<-(log(spB.m.obs[idx])-log(spB.m.prd[idx]))/sqrt(log(1+cv.m^2));
    #--plot of standardized devs
    ymx<-max(abs(st.devs.f),abs(st.devs.m))
    plot(c(0,1),c(0,1),type="n",xlim=range(obsyears),ylim=ymx*c(-1.05,1.05),
         ylab="Ln-scale Standardized Residuals",xlab='');
    abline(h=0,lty=2)
    points(obsyears,st.devs.f,pch=21,col='green',bg='green')
    points(obsyears,st.devs.m,pch=23,col='blue',bg='blue')
    mtext("Fits to mature survey biomass", side=3, adj=0.05)
    legend("topright",
           c("females", "males"),
           pch=c(21,23),col=c('green','blue'),pt.bg=c('green','blue')) 
    #----------------------------------

    #----------------------------------
    # total spawning biomass
    #----------------------------------
    spB.t.prd<-obj.rep$"Total.Spawning.Biomass";
    plot(years,spB.t.prd,type="l",lty=2,lwd=1,
         ylim=c(0,500),xlim=c(min(obsyears),max(obsyears)),
         ylab="Mature Biomass (1000 t)",xlab="Year");

    plotErrorBars.V(obsyears,spB.tm.obs[idx],upper=upper,lower=lower);
    points(obsyears,spB.tm.obs[idx],pch=21);
    
    spB.m.prd<-obj.rep$"Predicted.Male.survey.mature.Biomass"
    spB.f.prd<-obj.rep$"Predicted.Female.survey.mature.Biomass"
    spB.t.prd<-spB.m.prd+spB.f.prd;
    lines(years,spB.t.prd,type="l",lty=1,lwd=1)
    mtext("Total mature biomass", side=3, adj=0.05)
    legend("topright",
           c("predicted in population","predicted in survey", "observed in survey"),
           lty=c(2,1,NA),pch=c(NA,NA,21))
    #----------------------------------

    #----------------------------------
    # spawning biomass by sex
    #----------------------------------
    par(oma=c(0.5,1,1,0.5),mar=c(4,5,2,1)+0.2,mfrow=c(2,1))
    prd.MMB<-obj.rep$"Mating.time.Male.Spawning.Biomass";
    prd.MFB<-obj.rep$"Mating.time.Female.Spawning.Biomass";
    plot(years.m1,prd.MMB+prd.MFB,type="l",lty=2,lwd=1,
         ylab="Mature Biomass (1000 t)",xlab="Year");
    lines(years.m1,prd.MMB,lty=1,lwd=2,col='blue')
    lines(years.m1,prd.MFB,lty=1,lwd=2,col='green')
    mtext("Spawning biomass at mating time", side=3, adj=0.05)
    legend("topright",
           c("males (MMB)","females", "total"),
           lty=c(1,1,2),col=c('blue','green','black'))
    #----------------------------------

    #--------------------------------------------
    #Legal males.
    #--------------------------------------------
    par(oma=c(0.5,1,1,0.5),mar=c(4,5,2,1)+0.2,mfrow=c(2,1))
    plot(obsyears, obj.rep$"observed.number.of.males.greater.than.101.mm"/THOUSAND,type="p",
        xlab="Year",ylab="Number (millions)",ylim=c(0,200))
    lines(obsyears,obj.rep$"estimated.survey.numbers.of.males.101"[(1:length(years))[years==min(obsyears)]:length(years)]/THOUSAND,
          lty=1,lwd=1)
    lines(obsyears,obj.rep$"pop.estimate.numbers.of.males.101"[(1:length(years))[years==min(obsyears)]:length(years)]/THOUSAND,
          lty=2)
    legend("topright",
           c("predicted total numbers","predicted survey numbers", "observed survey numbers"),
           lty=c(2,1,NA),pch=c(NA,NA,21))
    mtext("Legal males (>= 138 mm CW)", side=3, adj=0.05)
    #----------------------------------

    #----------------------------------
    # Plot survey numbers
    #----------------------------------
    par(oma=c(0.5,1,1,0.5),mar=c(4,5,2,1)+0.2,mfrow=c(2,1))
    plot(years,obj.rep$"predicted.survey.numbers.female"/THOUSAND,type="l",
         ylim=c(0,1000),xlim=range(obsyears),ylab="Survey Numbers (millions)",xlab="Year");
    points(years, obj.rep$"Observed.survey.numbers.female"/THOUSAND,pch=21);
    mtext("Females",side=3,adj=0.1,cex=1)
    legend("topright",
           c("predicted","observed"),
           lty=c(1,NA),pch=c(NA,21))
    
    plot(years,obj.rep$"predicted.survey.numbers.male"/THOUSAND,type="l",
      ylim=c(0,1000),xlim=range(obsyears),ylab="Survey Numbers (millions)",xlab="Year");
    points(years, obj.rep$"Observed.survey.numbers.male"/THOUSAND);
    mtext("Males",side=3,adj=0.1,cex=1)
    #----------------------------------

    #----------------------------------
    # Plot mature new/old shell survey numbers (female)
    #----------------------------------
    par(oma=c(0.5,1,1,0.5),mar=c(4,5,2,1)+0.2,mfrow=c(3,1))
    
    plot(years,obj.rep$"Predicted.Female.survey.new.mature.numbers"/THOUSAND,type="l",
         ylim=c(0,500),xlim=range(obsyears),ylab="Survey Numbers (millions)",xlab="Year");
    points(years,obj.rep$"Observed.survey.female.new.spawning.numbers"/THOUSAND);
    mtext("Mature new shell female",side=3,adj=0.1)
    legend("topright",
           c("predicted","observed"),
           lty=c(1,NA),pch=c(NA,21))
    
    plot(years,obj.rep$"Predicted.Female.survey.old.mature.numbers"/THOUSAND,type="l",
      ylim=c(0,500),xlim=range(obsyears),ylab="Survey Numbers (millions)",xlab="Year");
    points(years,obj.rep$"Observed.survey.female.old.spawning.numbers"/THOUSAND);
    mtext("Mature old shell female",side=3,adj=0.1)
    
    tot.p.f<-obj.rep$"Predicted.Female.survey.new.mature.numbers"+
             obj.rep$"Predicted.Female.survey.old.mature.numbers";
    tot.o.f<-obj.rep$"Observed.survey.female.old.spawning.numbers"+
             obj.rep$"Observed.survey.female.new.spawning.numbers";
    plot(years,tot.p.f/THOUSAND,type="l",
      ylim=c(0,1000),xlim=range(obsyears),ylab="Survey Numbers (millions)",xlab="Year");
    points(years,tot.o.f/THOUSAND);
    mtext("All mature female",side=3,adj=0.1)
    #----------------------------------

    #----------------------------------
    # Plot mature new/old shell survey numbers (male)
    #----------------------------------
    par(oma=c(0.5,1,1,0.5),mar=c(4,5,2,1)+0.2,mfrow=c(3,1))
    
    plot(years,obj.rep$"Predicted.Male.survey.new.mature.numbers"/THOUSAND,type="l",
      ylim=c(0,500),xlim=range(obsyears),ylab="Survey Numbers (millions)",xlab="Year");
    points(years,obj.rep$"Observed.survey.male.new.spawning.numbers"/THOUSAND);
    mtext("Mature new shell male",side=3,adj=0.1)
    legend("topright",
           c("predicted","observed"),
           lty=c(1,NA),pch=c(NA,21))
    
    plot(years,obj.rep$"Predicted.Male.survey.old.mature.numbers"/THOUSAND,type="l",
      ylim=c(0,500),xlim=range(obsyears),ylab="Survey Numbers (millions)",xlab="Year");
    points(years,obj.rep$"Observed.survey.male.old.spawning.numbers"/THOUSAND);
    mtext("Mature old shell male",side=3,adj=0.1)
    
    tot.p.m<-obj.rep$"Predicted.Male.survey.new.mature.numbers"+
             obj.rep$"Predicted.Male.survey.old.mature.numbers";
    tot.o.m<-obj.rep$"Observed.survey.male.old.spawning.numbers"+
             obj.rep$"Observed.survey.male.new.spawning.numbers";
    plot(years,tot.p.m/THOUSAND,type="l",
      ylim=c(0,1000),xlim=range(obsyears),ylab="Survey Numbers (millions)",xlab="Year");
    points(years,tot.o.m/THOUSAND);
    mtext("All mature male",side=3,adj=0.1)
    #----------------------------------

    #----------------------------------
    # fraction mature by size
    #----------------------------------
    par(oma=c(0.5,1,1,0.5),mar=c(4,5,2,1)+0.2,mfrow=c(2,1))
    
    frac.p.f<-tot.p.f/obj.rep$"predicted.survey.numbers.female";
    frac.o.f<-tot.o.f/obj.rep$"Observed.survey.numbers.female";
    plot(years,frac.p.f,type="l",
      ylim=c(0,1.0),xlim=range(obsyears),ylab="fraction mature",xlab="Year");
    points(years,frac.o.f);
    mtext("Female",side=3,adj=0.1)
    legend("topright",
           c("predicted","observed"),
           lty=c(1,NA),pch=c(NA,21))
    
    frac.p.m<-tot.p.m/obj.rep$"predicted.survey.numbers.male";
    frac.o.m<-tot.o.m/obj.rep$"Observed.survey.numbers.male";
    plot(years,frac.p.m,type="l",
      ylim=c(0,1.0),xlim=range(obsyears),ylab="fraction mature",xlab="Year");
    points(years,frac.o.m);
    mtext("Male",side=3,adj=0.1)
    #----------------------------------
    
    #--------------------------------------------
    #Survey size comps: numbers at size
    #--------------------------------------------
    colnames(obj.rep$"Observed.Survey.Numbers.by.length.females")<-c("year",as.character(length.bins))
    plotSizeCompsComparisons(obj.rep$"Observed.Survey.Numbers.by.length.females",
                   obj.rep$"Predicted.Survey.Numbers.by.length.females",
                   nr = 8, nc = 6,nplot = length(obsyears));
    mtext("Survey numbers, females",side=3,adj=0.0,outer=TRUE);
    
    colnames(obj.rep$"Observed.Survey.Numbers.by.length.males")<-c("year",as.character(length.bins))
    plotSizeCompsComparisons(obj.rep$"Observed.Survey.Numbers.by.length.males",
                               obj.rep$"Predicted.Survey.Numbers.by.length.males",
                               nr = 8, nc = 6,nplot = length(obsyears));
    mtext("Survey numbers, males",side=3,adj=0.0,outer=TRUE);
    #----------------------------------

    #--------------------------------------------
    #Survey size comps: proportions at size
    #--------------------------------------------
    #--all females-----------
    colnames(obj.rep$"Observed.Length.Prop.survey.all.females")<-c("year",as.character(length.bins))
    plotSizeCompsComparisons(obj.rep$"Observed.Length.Prop.survey.all.females", 
                      obj.rep$"Predicted.length.prop.survey.all.females", nr = 7, nc = 6, 
                      nplot = length(obsyears),yearlb=obsyears,maxy=.12);
    mtext("Survey proportions, females",side=3,adj=0.0,outer=TRUE);
    
    par(oma=c(2,2,2,2),mar=c(7,4,2,1)+0.2,mfrow=c(1,1))
    plotBubbles(datao=obj.rep$"Observed.Length.Prop.survey.all.females",
                    datap=obj.rep$"Predicted.length.prop.survey.all.females",
                    nlen=15,sampsize=obj.rep$"Observed.Length.Prop.survey.all.females.sampsize"[1:15]);
    mtext("Survey proportions, females",side=3,adj=0.0);
    
    #--all males-------------
    par(col=1);
    colnames(obj.rep$"Observed.Length.Prop.survey.all.males")<-c("year",as.character(length.bins))
    plotSizeCompsComparisons(obj.rep$"Observed.Length.Prop.survey.all.males", 
                      obj.rep$"Predicted.length.prop.survey.all.males", nr = 7, nc = 6, 
                      nplot = length(obsyears),yearlb=obsyears,maxy=.07);
    mtext("Survey proportions, males",side=3,adj=0.0,outer=TRUE);
    
    par(oma=c(2,2,2,2),mar=c(7,4,2,1)+0.2,mfrow=c(1,1))
    plotBubbles(datao=obj.rep$"Observed.Length.Prop.survey.all.males",
                   datap=obj.rep$"Predicted.length.prop.survey.all.males",
                   nlen=32,sampsize=obj.rep$"Observed.Length.Prop.survey.all.males.sampsize");
    mtext("Survey proportions, males",side=3,adj=0.0);
    
    #--immature, new shell females-------
    par(col=1);
    colnames(obj.rep$"Observed.Length.Prop.survey.immature.new.females")<-c("year",as.character(length.bins))
    plotSizeCompsComparisons(obj.rep$"Observed.Length.Prop.survey.immature.new.females", 
            obj.rep$"Predicted.length.prop.survey.immature.new.females", nr = 7, nc = 6, 
            nplot = length(obsyears),maxy=.19)
    mtext("Survey proportions, immature new shell females",side=3,adj=0.0,outer=TRUE);
    
    par(oma=c(2,2,2,2),mar=c(7,4,2,1)+0.2,mfrow=c(1,1))
    plotBubbles(datao=obj.rep$"Observed.Length.Prop.survey.immature.new.females",
                    datap=obj.rep$"Predicted.length.prop.survey.immature.new.females",
                    nlen=15,sampsize=obj.rep$"Observed.Length.Prop.survey.immature.new.females.sampsize"[1:15]);
    mtext("Survey proportions, immature new shell females",side=3,adj=0.0);
    
    #--mature, new shell females---------
    colnames(obj.rep$"Observed.Length.Prop.survey.mature.new.females")<-c("year",as.character(length.bins))
    plotSizeCompsComparisons(obj.rep$"Observed.Length.Prop.survey.mature.new.females", 
                      obj.rep$"Predicted.length.prop.survey.mature.new.females", nr = 7, nc = 6, 
                      nplot = length(obsyears),maxy=.1)
    mtext("Survey proportions, mature new shell females",side=3,adj=0.0,outer=TRUE);
    
    par(oma=c(2,2,2,2),mar=c(7,4,2,1)+0.2,mfrow=c(1,1))
    plotBubbles(datao=obj.rep$"Observed.Length.Prop.survey.mature.new.females",
                    datap=obj.rep$"Predicted.length.prop.survey.mature.new.females",
                    nlen=15,sampsize=obj.rep$"Observed.Length.Prop.survey.mature.new.females.sampsize"[1:15]);
    mtext("Survey proportions, mature new shell females",side=3,adj=0.0);
    
    #--mature, old shell females----------
    par(col=1);
    colnames(obj.rep$"Observed.Length.Prop.survey.mature.old.females")<-c("year",as.character(length.bins))
    plotSizeCompsComparisons(obj.rep$"Observed.Length.Prop.survey.mature.old.females", 
                      obj.rep$"Predicted.length.prop.survey.mature.old.females", nr = 7, nc = 6, 
                      nplot = length(obsyears),maxy=.19)
    mtext("Survey proportions, mature old shell females",side=3,adj=0.0,outer=TRUE);
    
    par(oma=c(2,2,2,2),mar=c(7,4,2,1)+0.2,mfrow=c(1,1))
    plotBubbles(datao=obj.rep$"Observed.Length.Prop.survey.mature.old.females",
                    datap=obj.rep$"Predicted.length.prop.survey.mature.old.females",
                    nlen=15,sampsize=obj.rep$"Observed.Length.Prop.survey.mature.old.females.sampsize"[1:15]);
    mtext("Survey proportions, mature old shell females",side=3,adj=0.0);
    
    #--immature, new shell males-----------
    par(col=1);
    colnames(obj.rep$"Observed.Length.Prop.survey.immature.new.males")<-c("year",as.character(length.bins))
    plotSizeCompsComparisons(obj.rep$"Observed.Length.Prop.survey.immature.new.males", 
            obj.rep$"Predicted.length.prop.survey.immature.new.males", nr = 7, nc = 6, 
            nplot = length(obsyears),maxy=.08)
    mtext("Survey proportions, immature new shell males",side=3,adj=0.0,outer=TRUE);
    
    par(oma=c(2,2,2,2),mar=c(7,4,2,1)+0.2,mfrow=c(1,1))
    plotBubbles(datao=obj.rep$"Observed.Length.Prop.survey.immature.new.males",
                    datap=obj.rep$"Predicted.length.prop.survey.immature.new.males",
                    nlen=32,sampsize=obj.rep$"Observed.Length.Prop.survey.immature.new.males.sampsize");
    mtext("Survey proportions, immature new shell males",side=3,adj=0.0);
    
    #--mature, new shell males-----------------
    par(col=1);
    colnames(obj.rep$"Observed.Length.Prop.survey.mature.new.males")<-c("year",as.character(length.bins))
    plotSizeCompsComparisons(obj.rep$"Observed.Length.Prop.survey.mature.new.males", 
                      obj.rep$"Predicted.length.prop.survey.mature.new.males", 
                      nr = 7, nc = 6, 
                      nplot = length(obsyears),maxy=.025)
    mtext("Survey proportions, mature new shell males",side=3,adj=0.0,outer=TRUE);
    
    par(oma=c(2,2,2,2),mar=c(7,4,2,1)+0.2,mfrow=c(1,1))
    plotBubbles(datao=obj.rep$"Observed.Length.Prop.survey.mature.new.males",
                    datap=obj.rep$"Predicted.length.prop.survey.mature.new.males",
                    nlen=32,sampsize=obj.rep$"Observed.Length.Prop.survey.mature.new.males.sampsize");
    mtext("Survey proportions, mature new shell males",side=3,adj=0.0);
    
    #--mature, old shell males----------------
    colnames(obj.rep$"Observed.Length.Prop.survey.mature.old.males")<-c("year",as.character(length.bins))
    plotSizeCompsComparisons(obj.rep$"Observed.Length.Prop.survey.mature.old.males", 
                      obj.rep$"Predicted.length.prop.survey.mature.old.males", 
                      nr = 7, nc = 6, 
                      nplot = length(obsyears),maxy=.025)
    mtext("Survey proportions, mature old shell males",side=3,adj=0.0,outer=TRUE);
    
    par(oma=c(2,2,2,2),mar=c(7,4,2,1)+0.2,mfrow=c(1,1))
    plotBubbles(datao=obj.rep$"Observed.Length.Prop.survey.mature.old.males",
                    datap=obj.rep$"Predicted.length.prop.survey.mature.old.males",
                    nlen=32,sampsize=obj.rep$"Observed.Length.Prop.survey.mature.old.males.sampsize");
    mtext("Survey proportions, mature old shell males",side=3,adj=0.0);
    
    #--mature males---------------
    tmpobs=obj.rep$"Observed.Length.Prop.survey.mature.new.males"+obj.rep$"Observed.Length.Prop.survey.mature.old.males"
    colnames(tmpobs)=c("year",as.character(length.bins))
    tmpobs[,1]=obj.rep$"Observed.Length.Prop.survey.mature.new.males"[,1]
    plotSizeCompsComparisons(tmpobs, 
                      obj.rep$"Predicted.length.prop.survey.mature.new.males"+obj.rep$"Predicted.length.prop.survey.mature.old.males", 
                      nr = 7, nc = 6, 
                      nplot = length(obsyears),maxy=.05)
    mtext("Survey proportions, mature males",side=3,adj=0.0,outer=TRUE);
    
    par(oma=c(2,2,2,2),mar=c(7,4,2,1)+0.2,mfrow=c(1,1))
    plotBubbles(datao=tmpobs,
                    datap=(obj.rep$"Predicted.length.prop.survey.mature.old.males"+obj.rep$"Predicted.length.prop.survey.mature.new.males"),
                    nlen=32,sampsize=obj.rep$"Observed.Length.Prop.survey.mature.new.males.sampsize");
    mtext("Survey proportions, mature males",side=3,adj=0.0);
     
    #--sum of survey length proportions: all males, all females
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    obs.comps<-obj.rep$"Observed.Length.Prop.survey.all.females";
    prd.comps<-obj.rep$"Predicted.length.prop.survey.all.females";
    plotMeanSizeComps(length.bins,obs.comps,prd.comps,cols=2:33,CI=0.95,addToPlot=FALSE,pch=21,lty=1,clr='black',
                      xlab="Carapace Width(mm)",ylab="Mean of Size Proportions")
    obs.comps<-obj.rep$"Observed.Length.Prop.survey.all.males";
    prd.comps<-obj.rep$"Predicted.length.prop.survey.all.males";
    plotMeanSizeComps(length.bins+1,obs.comps,prd.comps,cols=2:33,CI=0.95,addToPlot=TRUE,pch=22,lty=2,clr='blue')
    mtext("Mean survey proportions, all individuals",side=3,adj=0.0);
    legend("topright",
           c("observed females","predicted females","observed males", "predicted males"),
           lty=c(NA,1,NA,2),pch=c(21,NA,22,NA),col=c('black','black','blue','blue'))

    #sum of survey length proportions: mature males, mature females
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    obs.comps<-obj.rep$"Observed.Length.Prop.survey.mature.new.females"+
               obj.rep$"Observed.Length.Prop.survey.mature.old.females";
    prd.comps<-obj.rep$"Predicted.length.prop.survey.mature.new.females"+
               obj.rep$"Predicted.length.prop.survey.mature.old.females";
    plotMeanSizeComps(length.bins,obs.comps,prd.comps,cols=2:33,CI=0.95,addToPlot=FALSE,pch=21,lty=1,clr='black',
                      xlab="Carapace Width(mm)",ylab="Mean of Size Proportions")
    obs.comps<-obj.rep$"Observed.Length.Prop.survey.mature.new.males"+
               obj.rep$"Observed.Length.Prop.survey.mature.old.males";
    prd.comps<-obj.rep$"Predicted.length.prop.survey.mature.new.males"+
               obj.rep$"Predicted.length.prop.survey.mature.old.males";
    plotMeanSizeComps(length.bins+1,obs.comps,prd.comps,cols=2:33,CI=0.95,addToPlot=TRUE,pch=22,lty=2,clr='blue')
    mtext("Mean survey proportions, mature individuals",side=3,adj=0.0);
    legend("topright",
           c("observed females","predicted females","observed males", "predicted males"),
           lty=c(NA,1,NA,2),pch=c(21,NA,22,NA),col=c('black','black','blue','blue'))

    #sum of survey length proportions: immature males, immature females
    obs.comps<-obj.rep$"Observed.Length.Prop.survey.immature.new.females"+
               obj.rep$"Observed.Length.Prop.survey.immature.old.females";
    prd.comps<-obj.rep$"Predicted.length.prop.survey.immature.new.females"+
               obj.rep$"Predicted.length.prop.survey.immature.old.females";
    plotMeanSizeComps(length.bins,obs.comps,prd.comps,cols=2:33,CI=0.95,addToPlot=FALSE,pch=21,lty=1,clr='black',
                      xlab="Carapace Width(mm)",ylab="Mean of Size Proportions")
    obs.comps<-obj.rep$"Observed.Length.Prop.survey.immature.new.males"+
               obj.rep$"Observed.Length.Prop.survey.immature.old.males";
    prd.comps<-obj.rep$"Predicted.length.prop.survey.immature.new.males"+
               obj.rep$"Predicted.length.prop.survey.immature.old.males";
    plotMeanSizeComps(length.bins+1,obs.comps,prd.comps,cols=2:33,CI=0.95,addToPlot=TRUE,pch=22,lty=2,clr='blue')
    mtext("Summed survey proportions, immature individuals",side=3,adj=0.0);
    legend("topright",
           c("observed females","predicted females","observed males", "predicted males"),
           lty=c(NA,1,NA,2),pch=c(21,NA,22,NA),col=c('black','black','blue','blue'))
    #----------------------------------

    #-------------------------------------------------
    # Directed fishery
    #-------------------------------------------------
    #--Directed fishery: retained males
    obs=obj.rep$"Observed.Prop.fishery.ret.new.males"+
        obj.rep$"Observed.Prop.fishery.ret.old.males"
    prd=obj.rep$"Predicted.length.prop.fishery.ret.new.males"+
        obj.rep$"Predicted.length.prop.fishery.ret.old.males"
    ss=obj.rep$"Observed.Prop.fishery.ret.new.males.sampsize"
    
    colnames(obs)=c("year",as.character(length.bins))
    colnames(prd)=c("year",as.character(length.bins))
    
    obs[,1]=obj.rep$"Observed.Prop.fishery.ret.new.males"[,1]
    prd[,1]=obj.rep$"Predicted.length.prop.fishery.ret.new.males"[,1]
    
    years.ret=obs[,1];
    
    #size comps
    plotSizeCompsComparisons(obs, prd, nr = 5, nc = 4, nplot = length(years.ret), yearlb=years.ret)
    mtext("directed fishery, all retained males",side=3,adj=0.0,outer=TRUE);
    
    #bubble plot
    par(oma=c(2,2,2,2),mar=c(7,4,2,1)+0.2,mfrow=c(1,1))
    plotBubbles(datao=obs, datap=prd, nlen=32, sampsize=ss);
    mtext("directed fishery, all retained males",side=3,adj=0.0);
    
    #summed proportions
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    plotMeanSizeComps(length.bins,obs,prd,cols=2:33,CI=0.95,addToPlot=FALSE,pch=21,lty=1,clr='black',
                      xlab="Carapace Width(mm)",ylab="Mean of Size Proportions")
    mtext("directed fishery, all retained males",side=3,adj=0.0);
    legend("topleft",
           c("observed males", "predicted males"),
           lty=c(NA,1),pch=c(21,NA),col=c('black','black'))

    #--Directed fishery: all males
    yrs.tcf<-obj.rep$"Observed.Prop.fishery.total.new.males"[,1];

    obs=obj.rep$"Observed.Prop.fishery.total.new.males"+
        obj.rep$"Observed.Prop.fishery.total.old.males"
    prd=obj.rep$"Predicted.length.prop.fishery.total.new.males"+ 
        obj.rep$"Predicted.length.prop.fishery.total.old.males"
    ss=obj.rep$"Observed.Prop.fishery.total.new.males.sampsize"
    
    colnames(obs)=c("year",as.character(length.bins))
    colnames(prd)=c("year",as.character(length.bins))
    obs[,1]=yrs.tcf
    prd[,1]=yrs.tcf
    
    #size comps
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(1,1))
    plotSizeCompsComparisons(obs,prd, nr = 4, nc = 3, nplot=nrow(obs), yearlb=obs[,1]);
    mtext("directed fishery, all males",side=3,adj=0.0,outer=TRUE);
    
    #bubble plot
    par(oma=c(2,2,2,2),mar=c(7,4,2,1)+0.2,mfrow=c(1,1))
    plotBubbles(datao=obs, datap=prd, nlen=32, sampsize=ss);
    mtext("directed fishery, all males",side=3,adj=0.0);
    
    #summed proportions
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    plotMeanSizeComps(length.bins,obs,prd,cols=2:33,CI=0.95,addToPlot=FALSE,pch=21,lty=1,clr='black',
                      xlab="Carapace Width(mm)",ylab="Mean of Size Proportions")
    mtext("directed fishery, all males",side=3,adj=0.0);
    legend("topleft",
           c("observed males", "predicted males"),
           lty=c(NA,1),pch=c(21,NA),col=c('black','black'))

    #--Directed fishery: all females
    obs<-(obj.rep$"Observed.length.prop.fishery.discard.all.females")
    prd=obj.rep$"Predicted.length.prop.fishery.discard.all.females"
    ss=obj.rep$"Observed.Prop.fishery.discard.all.females.sampsize"
    
    colnames(obs)=c("year",as.character(length.bins))
    colnames(prd)=c("year",as.character(length.bins))
    
    #size comps
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(1,1))
    plotSizeCompsComparisons(obs,prd,nr = 4,nc =3, nplot = nrow(obs), yearlb=obs[,1])
    mtext("directed fishery, all females",side=3,adj=0.0,outer=TRUE);
    
    #bubble plot
    par(oma=c(2,2,2,2),mar=c(7,4,2,1)+0.2,mfrow=c(1,1))
    plotBubbles(datao=obs, datap=prd,
                    nlen=32,sampsize=ss);
    mtext("directed fishery, all females",side=3,adj=0.0);
    
    #summed proportions
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    plotMeanSizeComps(length.bins,obs,prd,cols=2:33,CI=0.95,addToPlot=FALSE,pch=21,lty=1,clr='black',
                      xlab="Carapace Width(mm)",ylab="Mean of Size Proportions")
    mtext("directed fishery, all females",side=3,adj=0.0);
    legend("topright",
           c("observed females", "predicted females"),
           lty=c(NA,1),pch=c(21,NA),col=c('black','black'))
    #----------------------------------

    #----------------------------------------
    # Snow crab fishery discards comps
    #----------------------------------------
    yrs.scf=obj.rep$"Observed.length.prop.snow.fishery.males"[,1]
    
    obs.m=obj.rep$"Observed.length.prop.snow.fishery.males"
    colnames(obs.m)=c("year",as.character(length.bins))
    prd.m=obj.rep$"Predicted.length.prop.snow.fishery.males"
    colnames(prd.m)=c("year",as.character(length.bins))
    obs.f=obj.rep$"Observed.length.prop.snow.fishery.females"
    colnames(obs.f)=c("year",as.character(length.bins))
    prd.f=obj.rep$"Predicted.length.prop.snow.fishery.females"
    colnames(prd.f)=c("year",as.character(length.bins))
    
    #male size comps
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(1,1))
    plotSizeCompsComparisons(obs.m,prd.m,nr=6,nc=4,nplot=nrow(obs.m),yearlb=yrs.scf)
    mtext("snow crab fishery discards, all males",side=3,adj=0.0,outer=TRUE);
    
    #female size comps
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(1,1))
    plotSizeCompsComparisons(obs.f,prd.f,nr=6, nc=4,nplot=nrow(obs.f),yearlb=yrs.scf)
    mtext("snow crab fishery discards, all females",side=3,adj=0.0,outer=TRUE);
    
    #summed proportions
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    plotMeanSizeComps(length.bins,obs.f,prd.f,cols=2:33,CI=0.95,addToPlot=FALSE,pch=21,lty=1,clr='black',
                      xlab="Carapace Width(mm)",ylab="Mean of Size Proportions")
    plotMeanSizeComps(length.bins+1,obs.m,prd.m,cols=2:33,CI=0.95,addToPlot=TRUE,pch=22,lty=2,clr='blue')
    mtext("Mean proportions in the snow crab fishery, all individuals",side=3,adj=0.0);
    legend("topright",
           c("observed females","predicted females","observed males", "predicted males"),
           lty=c(NA,1,NA,2),pch=c(21,NA,22,NA),col=c('black','black','blue','blue'))
    #----------------------------------

    #---------------------------------------------
    # BBRKC fishery discard size comps
    #---------------------------------------------
    yrs.rkf=obj.rep$"Observed.length.prop.redk.fishery.females"[,1]
    
    obs.f=obj.rep$"Observed.length.prop.redk.fishery.females"
    colnames(obs.f)=c("year",as.character(length.bins))
    prd.f=obj.rep$"Predicted.length.prop.redk.fishery.females"
    colnames(prd.f)=c("year",as.character(length.bins))
    obs.m=obj.rep$"Observed.length.prop.redk.fishery.males"
    colnames(obs.m)=c("year",as.character(length.bins))
    prd.m=obj.rep$"Predicted.length.prop.redk.fishery.males"
    colnames(prd.m)=c("year",as.character(length.bins))
    
    #female size comps
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(1,1))
    plotSizeCompsComparisons(obs.f,prd.f,nr=5,nc=4,nplot=nrow(obs.f),yearlb=yrs.rkf)
    mtext("BBRKC fishery discards, all females",side=3,adj=0.0,outer=TRUE);
    
    #male size comps
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(1,1))
    plotSizeCompsComparisons(obs.m,prd.m,nr=5,nc=4,nplot=nrow(obs.m),yearlb=yrs.rkf)
    mtext("BBRKC fishery discards, all males",side=3,adj=0.0,outer=TRUE);
    
    #summed size frequency plots
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    plotMeanSizeComps(length.bins,obs.f,prd.f,cols=2:33,CI=0.95,addToPlot=FALSE,pch=21,lty=1,clr='black',
                      xlab="Carapace Width(mm)",ylab="Mean of Size Proportions")
    plotMeanSizeComps(length.bins+1,obs.m,prd.m,cols=2:33,CI=0.95,addToPlot=TRUE,pch=22,lty=2,clr='blue')
    mtext("Mean proportions in the BBRKC fishery, all individuals",side=3,adj=0.0);
    legend("topright",
           c("observed females","predicted females","observed males", "predicted males"),
           lty=c(NA,1,NA,2),pch=c(21,NA,22,NA),col=c('black','black','blue','blue'))

    #--------------------------------
    # groundfish fishery size comps
    #--------------------------------
    yrs.gtf<-obj.rep$"Observed.length.prop.trawl.females"[,1]

    obs.f=obj.rep$"Observed.length.prop.trawl.females"
    colnames(obs.f)=c("year",as.character(length.bins))
    prd.f=obj.rep$"Predicted.length.prop.trawl.females"
    colnames(prd.f)=c("year",as.character(length.bins))
    obs.m=obj.rep$"Observed.length.prop.trawl.males"
    colnames(obs.m)=c("year",as.character(length.bins))
    prd.m=obj.rep$"Predicted.length.prop.trawl.males"
    colnames(prd.m)=c("year",as.character(length.bins))
    
    #female size comps
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(1,1))
    plotSizeCompsComparisons(obs.f,prd.f,nr=7,nc=6,nplot=nrow(obs.f),yearlb=yrs.gtf)
    mtext("groundfish fishery: females",side=3,adj=0.0,outer=TRUE);
    
    #male size comps
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(1,1))
    plotSizeCompsComparisons(obs.m,prd.m,nr=7,nc=6,nplot=nrow(obs.m),yearlb=yrs.gtf)
    mtext("groundfish fishery: males",side=3,adj=0.0,outer=TRUE);
    
    #summed proportions
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    plotMeanSizeComps(length.bins,obs.f,prd.f,cols=2:33,CI=0.95,addToPlot=FALSE,pch=21,lty=1,clr='black',
                      xlab="Carapace Width(mm)",ylab="Mean of Size Proportions")
    plotMeanSizeComps(length.bins+1,obs.m,prd.m,cols=2:33,CI=0.95,addToPlot=TRUE,pch=22,lty=2,clr='blue')
    mtext("Mean proportions in the groundfish fisheries, all individuals",side=3,adj=0.0);
    legend("topright",
           c("observed females","predicted females","observed males", "predicted males"),
           lty=c(NA,1,NA,2),pch=c(21,NA,22,NA),col=c('black','black','blue','blue'))
    #----------------------------------

    #------------------------------------------
    # Predicted growth
    #------------------------------------------
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    plot(length.bins[1:24],
         obj.rep$"Predicted.mean.postmolt.length.females"[1:24],type="l",
         xlab="Pre-Molt Length",ylab="Post-Molt Length",ylim=c(35,185),xlim=c(25,200))
    points(length.bins[1:24],exp(0.5656024)*length.bins[1:24]^0.913266,pch=1)
    points(length.bins,exp(0.43794)*length.bins^0.948703,pch=3)
    lines(length.bins, obj.rep$"Predicted.mean.postmolt.length.males",lty=1)
    mtext("Growth",side=3,adj=0.0);
    #----------------------------------

    #---------------------------------------
    # probability of maturing
    #---------------------------------------
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    plot(length.bins,
         obj.rep$"Predicted.probability.of.maturing.females",
         xlab="Carapace Width (mm)", ylab="Probability of Maturing",type="l",lty=2)
    lines(length.bins,obj.rep$"Predicted.probability.of.maturing.males")
    #maturity 2009 assessment
    lines(length.bins, 1/(1+exp(-(0.07754*(length.bins-130.8540)))),lty=3)
    mtext("Maturation",side=3,adj=0.0);
    #----------------------------------
          
    #---------------------------------------
    # size distribution of recruits
    #---------------------------------------
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    plot(length.bins,
         obj.rep$"distribution.of.recruits.to.length.bins",
         type="b",xlab="Carapace Width (mm)",ylab="Proportion",lty=1,pch=21)
    mtext("Assumed size distribution at recruitment",side=3,adj=0.0);
    #----------------------------------

    #----------------------------------------------
    # TCF total selectivity for new shell males
    #----------------------------------------------
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    sel<-obj.rep$"selectivity.fishery.total.new.males";
    plot(length.bins,sel[1,],type="l",lwd=3,ylim=c(0,1.0),xlab="Carapace Width (mm)",ylab="Total Selectivity")
    rownames(sel)<-years.m1;
    ctr<-0
    for (i in as.character(yrs.tcf)) {
        lines(length.bins,sel[i,],lty=2,lwd=2)
        y<-0.75-ctr*0.5/length(yrs.tcf)
        iz<-which.min((sel[i,]-y)^2)
        text(length.bins[iz],y,labels=i,cex=0.9,col="dark blue")
        ctr<-ctr+1
    }
    mtext("Total directed fishery selectivity on new shell males",side=3,adj=0.0);
    #----------------------------------

    #----------------------------------------------
    # TCF retained selectivity for new shell males
    #----------------------------------------------
    sel<-obj.rep$"selectivity.fishery.ret.new.males"
    plot(length.bins,sel[1,],
         type="l",ylim=c(0,1.0),lwd=3,xlab="Carapace width (mm)",ylab="Retention")
    rownames(sel)<-years.m1;
    ctr<-0
    for (i in as.character(yrs.tcf)) {
        lines(length.bins,sel[i,],lty=2,lwd=2)
        y<-0.75-ctr*0.5/length(yrs.tcf)
        iz<-which.min((sel[i,]-y)^2)
        text(length.bins[iz],y,labels=i,cex=0.9,col="dark blue")
        ctr<-ctr+1
    }
    mtext("Directed fishery retention, new shell males",side=3,adj=0,outer=FALSE)
    #----------------------------------

    #----------------------------------------------
    # Survey selectivity
    #----------------------------------------------
    #tanner underbag selectivity parameters
    pa=0.8743/(1+ 10.95*exp(-.0504*(seq(27.5,182.5,by=5))))
    pb=1-(1/(1+185.9*exp(-.1742*(seq(27.5,182.5,by=5)))))
    ubx.x<-seq(27.5,182.5,by=5)
    ubx.y<-pa+(1-pa)*pb
    
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    pchs<-c(21,22,23,24)
    ltys<-c( 1, 1, 2,NA)
    
    #--males ??
    plot(length.bins,
         obj.rep$"selectivity.survey.males.1988.to.endyr",
         type="n",xlab="Carapace Width(mm)",ylab="Selectivity",ylim=c(0,1.5))
    points(length.bins,obj.rep$"selectivity.survey.males.1988.to.endyr",pch=pchs[1])
    lines( length.bins,obj.rep$"selectivity.survey.males.1988.to.endyr",lty=ltys[1])
    points(length.bins,obj.rep$"selectivity.survey.males.1982.to.1987", pch=pchs[2])
    lines( length.bins,obj.rep$"selectivity.survey.males.1982.to.1987", lty=ltys[2])
    points(length.bins,obj.rep$"selectivity.survey.males.1974.to.1981", pch=pchs[3])
    lines( length.bins,obj.rep$"selectivity.survey.males.1974.to.1981", lty=ltys[3])
    points(ubx.x,ubx.y,pch=pchs[4])  #underbag exp.
    #lines( ubx.x,ubx.y,lty=ltys[4]) #underbag exp.
    lines(c(140,140),c(0,1.0))       #ref size males
    mtext("Trawl survey selectivity, males",side=3,adj=0)
    legend("topright",
           c("males, 1988+","males, 1982-1987","males 1974-1981","Somerton underbag exp."),
           lty=ltys,pch=pchs,cex=0.7)
    
    #--females ??
    plot(length.bins,
         obj.rep$"selectivity.survey.females.1988.to.endyr",
         type="n",xlab="Carapace Width(mm)",ylab="Selectivity",ylim=c(0,1.5))
    points(length.bins,obj.rep$"selectivity.survey.females.1988.to.endyr",pch=pchs[1]) 
    lines( length.bins,obj.rep$"selectivity.survey.females.1988.to.endyr",lty=ltys[1]) 
    points(length.bins,obj.rep$"selectivity.survey.females.1982.to.1987", pch=pchs[2])
    lines( length.bins,obj.rep$"selectivity.survey.females.1982.to.1987", lty=ltys[2])
    points(length.bins,obj.rep$"selectivity.survey.females.1974.to.1981", pch=pchs[3])
    lines( length.bins,obj.rep$"selectivity.survey.females.1974.to.1981", lty=ltys[3])
    points(ubx.x,ubx.y,pch=pchs[4]) #underbag exp.
    #lines( ubx.x,ubx.y,lty=ltys[4]) #underbag exp.
    lines(c(100,100),c(0,1.0))    #ref size females
    mtext("Trawl survey selectivity, females",side=3,adj=0)
    legend("topright",
           c("females, 1988+","females, 1982-1987","females 1974-1981","Somerton underbag exp."),
           lty=ltys,pch=pchs,cex=0.7)
    
    #most recent period
    plot(length.bins,
         obj.rep$"selectivity.survey.males.1988.to.endyr",
         type="n",xlab="Carapace Width(mm)",ylab="Selectivity",ylim=c(0,1.5))
    points(length.bins,obj.rep$"selectivity.survey.males.1988.to.endyr",  pch=pchs[1])
    lines( length.bins,obj.rep$"selectivity.survey.males.1988.to.endyr",  lty=ltys[1])
    points(length.bins,obj.rep$"selectivity.survey.females.1988.to.endyr",pch=pchs[2]) 
    lines( length.bins,obj.rep$"selectivity.survey.females.1988.to.endyr",lty=ltys[2]) 
    points(ubx.x,ubx.y,pch=pchs[4])    #underbag exp.
    #lines( ubx.x,ubx.y,lty=ltys[4])   #underbag exp.
    lines(c(140,140),c(0,1.0),lty=2)   #ref size males
    lines(c(100,100),c(0,1.0),lty=3)   #ref size females
    mtext("Trawl survey selectivity, 1988+",side=3,adj=0)
    legend("topright",
           c("males","females","Somerton underbag exp."),
           lty=ltys[c(1,2,4)],pch=pchs[c(1,2,4)],cex=0.7)
    #----------------------------------
    
    #--------------------------------
    # snow crab fishery selectivity
    #--------------------------------
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    plot(length.bins,obj.rep$"selectivity.snow.females"[1,],
        type="l",lty=2,lwd=2,ylim=c(0,1.1),
        xlab="Carapace width(mm)",ylab="Selectivity")
    lines(length.bins,obj.rep$"selectivity.snow.females"[2,],lty=2,lwd=2,col="green")
    lines(length.bins,obj.rep$"selectivity.snow.females"[3,],lty=2,lwd=2,col="blue")
    lines(length.bins,obj.rep$"selectivity.snow.males"[1,],lty=1,lwd=2,col="black")
    lines(length.bins,obj.rep$"selectivity.snow.males"[2,],lty=1,lwd=2,col="green")
    lines(length.bins,obj.rep$"selectivity.snow.males"[3,],lty=1,lwd=2,col="blue")
    mtext("snow crab fishery",side=3,adj=0.0,outer=FALSE);
    legend("bottomright",
           c("females, era 1","females, era 2","females, era 3","males, era 1","males, era 2","males, era 3"),
           lty=c(2,2,2,1,1,1),col=rep(c("black","green","blue"),times=2),lwd=2,cex=0.7)
    #----------------------------------

    #--------------------------------
    # BBRKC fishery selectivity
    #--------------------------------
    plot(length.bins,
         obj.rep$"selectivity.redk.females"[1,],
         type="l",lty=2,lwd=2,ylim=c(0,1.1),
         xlab="Carapace width(mm)",ylab="Selectivity Red King Fishery")
    lines(as.numeric(length.bins),obj.rep$"selectivity.redk.females"[2,],lty=2,lwd=2,col="green")
    lines(as.numeric(length.bins),obj.rep$"selectivity.redk.females"[3,],lty=2,lwd=2,col="blue")
    lines(as.numeric(length.bins),obj.rep$"selectivity.redk.males"[1,],lwd=2)
    lines(as.numeric(length.bins),obj.rep$"selectivity.redk.males"[2,],lwd=2,col="green")
    lines(as.numeric(length.bins),obj.rep$"selectivity.redk.males"[3,],lwd=2,col="blue")
    mtext("BBRKC fishery",side=3,adj=0.0,outer=FALSE);
    legend("topleft",
           c("females, era 1","females, era 2","females, era 3","males, era 1","males, era 2","males, era 3"),
           lty=c(2,2,2,1,1,1),col=rep(c("black","green","blue"),times=2),lwd=2,cex=0.7)
    #----------------------------------

    #----------------------------------------------
    # Groundfish fishery selectivity
    #----------------------------------------------
    plot(length.bins, obj.rep$"selectivity.trawl.females"[1,],
    type="l",xlab="Carapace width(mm)",ylab="Selectivity",ylim=c(0,1.1),lty=2,lwd=2,col="black")
    lines(length.bins,obj.rep$"selectivity.trawl.females"[2,],lty=2,lwd=2,col="green")
    lines(length.bins,obj.rep$"selectivity.trawl.females"[3,],lty=2,lwd=2,col="blue")

    names(obj.rep$"selectivity.trawl.males")<-as.character(length.bins)
    lines(length.bins, obj.rep$"selectivity.trawl.males"[1,],lty=1,lwd=2)
    lines(length.bins, obj.rep$"selectivity.trawl.males"[2,],lty=1,lwd=2,col="green")
    lines(length.bins,obj.rep$"selectivity.trawl.males"[3,],lty=1,lwd=2,col="blue")
    mtext("groundfish fishery",side=3,adj=0.0,outer=FALSE);
    legend("bottomright",
           c("females, era 1","females, era 2","females, era 3","males, era 1","males, era 2","males, era 3"),
           lty=c(2,2,2,1,1,1),col=rep(c("black","green","blue"),times=2),lwd=2,cex=0.7)
    #----------------------------------
    
    #------------------------------------
    # Fits to retained & male discard mortalities
    #------------------------------------
    obs.rd<-obj.rep$"observed.retained.discard.male.catch.biomass"; #total mortality for TCF
    prd.rd<-obj.rep$"predicted.retained.discard.male.catch.biomass";#total mortality for TCF
    obs.r<-obj.rep$"observed.retained.catch.biomass";
    prd.r<-obj.rep$"predicted.retained.catch.biomass";
    prd.t<-obj.rep$"predicted.total.male.catch.biomass";#total over all fisheries
    
    plot(years.m1, prd.t,lty=3,lwd=2,col="green",
         type="l",xlab="Fishery Year",ylab="Catch (1000 t)",xlim=range(pretty(1965:endyr,h=2,n=(endyr-1965)/5)),
         ylim=c(0,max(prd.rd)));
    lines(years.m1,    prd.rd,lty=1,lwd=1)
    points(1992:(endyr-1),obs.rd,pch=22)
    lines(years.m1,    prd.r, lty=2,lwd=1)
    points(1965:(endyr-1),obs.r, pch=21)
    mtext("Male catch mortality",side=3,adj=0.0,outer=FALSE);
    legend("topright",legend=c("Predicted Total","Predicted Total: TCF","Observed Total: TCF","Predicted Retained","Observed Retained"),
           lty=c( 3, 1,NA, 2,NA),
           pch=c(NA,NA,22,NA,21),
           col=c('green','black','black','black','black'),cex=1)

    plot(years.m1, prd.r,
         type="l",xlab="Fishery Year",ylab="Retained Catch (1000 t)",xlim=range(pretty(1965:endyr,h=2,n=(endyr-1965)/5)),
         ylim=c(0,max(prd.r)))
    points(1965:(endyr-1), obs.r)
    legend("topright",legend=c("Predicted","Observed"),
           lty=c(1,NA),pch=c(NA,1),cex=1)
    mtext("Retained catch",side=3,adj=0.0,outer=FALSE);
    #----------------------------------

    #-------------------------------------------------
    # Fraction of male discard mortality in directed fishery
    #-------------------------------------------------
    par(oma=c(2,2,2,2),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    frc<-obj.rep$"predicted.discard.male.catch.biomass"/obj.rep$"predicted.retained.catch.biomass";
    plot(years.m1,frc,type='l',
         xlab="Fishery Year",ylab="ratio",
         xlim=range(years),ylim=c(0,2.0))
    mtext("Directed fishery male discard mortality biomass relative to retained catch biomass",side=3,adj=0.0,outer=FALSE);
    #----------------------------------

    #-------------------------------------------------
    #trawl bycatch
    #-------------------------------------------------
    prd<-obj.rep$"predicted.trawl.catch.biomass";
    obs<-obj.rep$"observed.trawl.catch.biomass"
    plot(years.m1,prd,type="l",
         xlab="Fishery Year",ylab="Discard Mortality (1000 t)",
         xlim=range(years),ylim=c(0,max(prd,obs,na.rm=TRUE)))
    points(1973:(endyr-1),obs)
    mtext("Groundfish discard mortality",side=3,adj=0.0,outer=FALSE);
    legend("topright",legend=c("Predicted","Observed"),
           lty=c(1,NA),pch=c(NA,1),cex=1)
    #----------------------------------

    #-------------------------------------------------
    # Directed fishery discards mortality
    #-------------------------------------------------
    par(oma=c(1,1,1,1),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    #male discard mortality
    obs<-obj.rep$"observed.male.discard.mortality.biomass"
    prd<-obj.rep$"predicted.discard.male.catch.biomass";
    plot(years.m1,prd,type="l",
         xlab="Year",ylab="Discard Mortality (1000 t)",
         xlim=range(years),ylim=c(0,max(prd,obs,na.rm=TRUE)))
    points(1992:(endyr-1),obs)
    mtext("Directed fishery, male discard mortality",side=3,adj=0.0,outer=FALSE);
    legend("topright",legend=c("Predicted","Observed"),
           lty=c(1,NA),pch=c(NA,1),cex=1)

    #female discard mortality
    obs<-obj.rep$"observed.female.discard.mortality.biomass"
    prd<-obj.rep$"predicted.female.discard.mortality.biomass";
    plot(years.m1,prd,type="l",
         xlab="Year",ylab="Discard Mortality (1000 t)",
         xlim=range(years),ylim=c(0,max(prd,obs,na.rm=TRUE)))
    points(1992:(endyr-1),obs)
    mtext("Directed fishery, female discards",side=3,adj=0.0,outer=FALSE);
    legend("topright",legend=c("Predicted","Observed"),
           lty=c(1,NA),pch=c(NA,1),cex=1)
    #----------------------------------

    #-------------------------------------------------
    #Snow crab fishery discard mortality
    #-------------------------------------------------
    par(oma=c(1,1,1,1),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    #--male discard mortality
    obs<-obj.rep$"observed.snow.male.discard.mortality.biomass"
    prd<-obj.rep$"predicted.snow.male.discard.mortality.biomass";
    plot(years.m1,prd,type="l",
         xlab="Year",ylab="Discard Mortality (1000 t)",
         xlim=range(years),ylim=c(0,max(prd,obs,na.rm=TRUE)))
    points(1992:(endyr-1),obs)
    mtext("Snow crab fishery, male discards",side=3,adj=0.0,outer=FALSE);
    legend("topright",legend=c("Predicted","Observed"),
           lty=c(1,NA),pch=c(NA,1),cex=1)
    
    #--female discard mortality
    obs<-obj.rep$"observed.snow.female.discard.mortality.biomass"
    prd<-obj.rep$"predicted.snow.female.discard.mortality.biomass";
    plot(years.m1,prd,type="l",
         xlab="Year",ylab="Discard Mortality (1000 t)",
         xlim=range(years),ylim=c(0,max(prd,obs,na.rm=TRUE)))
    points(1992:(endyr-1),obs)
    mtext("Snow crab fishery, female discards",side=3,adj=0.0,outer=FALSE);
    legend("topright",legend=c("Predicted","Observed"),
           lty=c(1,NA),pch=c(NA,1),cex=1)
    #----------------------------------

    #-------------------------------------------------
    #predicted red king crab fishery discard catch biomass
    #-------------------------------------------------
    par(oma=c(1,1,1,1),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    #--male discard mortality
    yrs<-obj.rep$"years.obs.bycatch.redk.fishery";
    if (is.null(yrs)) yrs<-1992:(endyr-1);
    obs<-obj.rep$"observed.redk.male.discard.mortality.biomass"
    prd<-obj.rep$"predicted.redk.male.discard.mortality.biomass";
    plot(years.m1,prd,type="l",
         xlab="Year",ylab="Discard Mortality (1000 t)",
         xlim=range(years),ylim=c(0,max(prd,obs,na.rm=TRUE)))
    points(yrs,obs)
    mtext("Red king crab fishery, male discards",side=3,adj=0.0,outer=FALSE);
    legend("topright",legend=c("Predicted","Observed"),
           lty=c(1,NA),pch=c(NA,1),cex=1)
    
    #--female discard mortality
    obs<-obj.rep$"observed.redk.female.discard.mortality.biomass"
    prd<-obj.rep$"predicted.redk.female.discard.mortality.biomass";
    plot(years.m1,prd,type="l",
         xlab="Year",ylab="Discard Mortality (1000 t)",
         xlim=range(years),ylim=c(0,max(prd,obs,na.rm=TRUE)))
    points(yrs,obs)
    mtext("Red king crab fishery, female discards",side=3,adj=0.0,outer=FALSE);
    legend("topright",legend=c("Predicted","Observed"),
           lty=c(1,NA),pch=c(NA,1),cex=1)
    #----------------------------------

    #-------------------------------------------------
    # Exploitation rate
    #-------------------------------------------------
    xpl<-obj.rep$"estimated.total.catch.divided.by.male.spawning.biomass.at.fishtime";
    plot(years.m1, xpl, type="l",
         xlab="Year",ylab="Exploitation Rate",xlim=range(plotyears),ylim=c(0,1.0))
    xpl<-obj.rep$"estimated.total.catch.of.legal.males.divided.by.legal.males.at.fishtime"
    lines(years.m1,xpl,,lty=2)
    legend("topright",legend=c("total catch","legal males"),
           lty=c(1,2),pch=c(NA,NA),cex=1)

    #-------------------------------------------------
    # Directed fishery fishing mortality
    #-------------------------------------------------
    par(oma=c(1,1,1,1),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
    if (isFRev){
        fm<-obj.rep$"estimated.annual.total.directed.fishing.capture.rate"
        plot(years.m1, fm, type="l",
             xlab="Year", ylab="Full Selection Total Fishery Capture Rate",
             xlim=range(plotyears),ylim=c(0,max(fm,na.rm=TRUE)))
        mtext("Directed fishery, males",side=3,adj=0.0,outer=FALSE);
    }
    
    fm.r<-obj.rep$"retained.fmTCFR_syz"
    fm.m<-obj.rep$"estimated.annual.male.total.directed.fishing.mortality"
    fm.f<-obj.rep$"estimated.annual.female.total.directed.fishing.mortality"
    plot(years.m1, fm.m, type="l",col='blue',lwd=3,
         xlab="Year", ylab="Full Selection Fishing Mortality Rate",
         xlim=range(plotyears),ylim=c(0,max(fm.m,fm.r,na.rm=TRUE)))
    lines(years.m1,fm.f,lty=2,col='green',lwd=3)
    mtext("Directed fishery",side=3,adj=0.0,outer=FALSE);
    legend("topright",legend=c("male (total)","female","retained"),pch=c(NA,NA,NA),
           lty=c(1,2,3),lwd=c(3,3,3),col=c('blue','green','cyan'),cex=1)
    #----------------------------------

    #-------------------------------------------------
    # Snow crab fishery fishing mortality
    #-------------------------------------------------
    fm.m<-obj.rep$"estimated.annual.snow.fishing.mortality"
    plot(years.m1, fm.m, type="l",col='blue',lwd=3,
         xlab="Year", ylab="Full Selection Fishing Mortality Rate",
         xlim=range(plotyears),ylim=c(0,max(fm.m,na.rm=TRUE)))
    mtext("Snow crab fishery",side=3,adj=0.0,outer=FALSE);
    #----------------------------------

    #-------------------------------------------------
    # Red king crab fishery fishing mortality
    #-------------------------------------------------    
    fm.m<-obj.rep$"estimated.annual.red.king.fishing.mortality"
    plot(years.m1, fm.m, type="l",col='blue',lwd=3,
         xlab="Year", ylab="Full Selection Fishing Mortality Rate",
         xlim=range(plotyears),ylim=c(0,max(fm.m,na.rm=TRUE)))
    mtext("BBRKC fishery",side=3,adj=0.0,outer=FALSE);
    #----------------------------------

    #-------------------------------------------------
    # Groundfish fishery fishing mortality
    #-------------------------------------------------
    fm.m<-obj.rep$"estimated.annual.fishing.mortality.trawl.bycatch"
    plot(years.m1, fm.m, type="l",col='blue',lwd=3,
         xlab="Year", ylab="Full Selection Fishing Mortality Rate",
         xlim=range(plotyears),ylim=c(0,max(fm.m,na.rm=TRUE)))
    mtext("Groundfish fishery",side=3,adj=0.0,outer=FALSE);
    #----------------------------------

    #-------------------------------------------------
    # Total fishing mortality
    #-------------------------------------------------
    fm.m<-obj.rep$"estimated.annual.max.male.total.fishing.mortality"
    fm.f<-obj.rep$"estimated.annual.max.female.total.fishing.mortality"
    plot(years.m1, fm.m, type="l",col='blue',lwd=3,
         xlab="Year", ylab="Full Selection Fishing Mortality Rate",
         xlim=range(plotyears),ylim=c(0,max(fm.m,fm.f,na.rm=TRUE)))
    lines(years.m1,fm.f,lty=2,col='green',lwd=3)
    mtext("Total (fully-selected) fishing mortality",side=3,adj=0.0,outer=FALSE);
    legend("topright",legend=c("males","females"),pch=c(NA,NA),
           lty=c(1,2),lwd=c(3,3),col=c('blue','green'),cex=1)
    #----------------------------------
    
    #-------------------------------------------------
    # Mature male biomass at mating time
    #-------------------------------------------------
    if (!is.null(obj.std)){
        par(oma=c(1,1,1,1),mar=c(4,4,2,1)+0.2,mfrow=c(2,1))
        sdf.mmb<-obj.std[obj.std[,2]=="sdrMMB",]
        mmb.sd<-sdf.mmb[,4]
        mmb.mn<-sdf.mmb[,3]
        plot(years[2:(length(years)-1)],mmb.mn,type="l",lty=1,
             xlab="Year",ylab="Biomass (1000's t)",
             ylim=c(0,1.2*max(mmb.mn+mmb.sd,na.rm=TRUE)))
        points(years[2:(length(years)-1)],mmb.mn,pch=1)
        plotErrorBars.V(years[2:(length(years)-1)],mmb.mn,
                        sigma=mmb.sd,CI=0.8)
        mtext("MMB at Mating",side=3,adj=0.0);
    }
    #----------------------------------

    #-------------------------------------------
    # Recruitment
    #-------------------------------------------
    if (!is.null(obj.std)){
        recLag<-5
        sdf.R<-obj.std[obj.std[,2]=="sdrLnRec",]
        R.mn<-2*exp(sdf.R[,3])/THOUSAND;#scale to total recruitment in millions
        R.sd<-sqrt(exp(sdf.R[,4]^2)-1)*R.mn;
        yrs.R<-years[2:(length(years)-recLag)]
        plot(yrs.R,R.mn,type="l",lty=1,
             xlab="Fertilization Year",ylab="numbers (millions)",
             ylim=c(0,1.2*max(R.mn+R.sd,na.rm=TRUE)))
        points(yrs.R,R.mn,pch=1)
        plotErrorBars.V(yrs.R,R.mn,sigma=R.sd,CI=0.8,lognormal=TRUE)
        mtext("Total recruitment",side=3,adj=0.0);
        rec.mn.1982<-mean(R.mn[(1982-recLag-1950):length(R.mn)]);
        lines((1982:endyr)-recLag,rec.mn.1982+0*((1982:endyr)-recLag),lty=2,lwd=2)
    }
    #----------------------------------
              
    #------------------------------------------------------------
    # Harvest control rule and fishing mortality
    #------------------------------------------------------------
    if (!is.na(F35)){
        par(oma=c(1,1,1,1),mar=c(4,4,2,1)+0.2,mfrow=c(1,1))
        mmb<-obj.rep$"Mating.time.Male.Spawning.Biomass";
        tfm<-obj.rep$"estimated.annual.max.male.total.fishing.mortality";
        if (isFRev){tfm<-obj.rep$"estimated.annual.total.fishing.mortality.males";}            
        yrs<-1949:(endyr-1)
        idx<-(yrs>=1968)
        plot(mmb[idx],tfm[idx],type='n',
             xlab=" MMB (1000 t)",ylab="Full Selection Fishing Mortality Rate",
             xlim=c(0,1.1*max(mmb[idx])),ylim=c(0,1.1*max(tfm[idx])))
        ctr<-0;
        for (i in idx[1:(length(idx)-1)]){
            if (i) {
                ctr<-ctr+1;
                text(mmb[ctr],tfm[ctr],as.character(yrs[ctr]),cex=0.7,adj=0)
            }
        }
        text(mmb[length(mmb)],tfm[length(tfm)],as.character(endyr-1),cex=1.1,col='red',adj=0)
        
        alpha<-0.1 
        beta<-0.25
        lines(rep(B35,length=20),seq(0,F35,length=20),lty=4,col="blue")
        lines(seq(0,max(mmb),length=20),rep(F35,length=20),lty=4,col="blue")
        text(B35,0,"B35%",cex=1,adj=0.5,col="blue")
        text(0,F35,"F35%",cex=1,adj=0.5,col="blue")
        lines(rep(B35*beta,length=20),
              seq(0,F35*((((B35*beta)/B35)-alpha)/(1.-alpha)),length=20),
              type="l",lty=1,lwd=2)
        
        lines(seq(B35*beta,B35,length=20),
              F35*(((seq(B35*beta,B35,length=20)/B35)-alpha)/(1.-alpha)),
              type="l",lty=1,lwd=2)
        
        lines(seq(B35,max(mmb),length=20),
              rep(F35,length=20),
              lty=1,lwd=2)    
    }
    
    #lines(seq(0,max(as.numeric(unlist(obj.rep$"Mating.time.Male.Spawning.Biomass"))),length=20),
    #     rep(1.1,20),lty=1)
    #text(50,1.15,"Pre-2000 Target F",cex=.7)
    par(cex=0.75)
    par(mai=c(.75,.75,.55,.35))
    
    # 5 year lag, don't use last male spb that is projected to spring of year after endyr in model
    #recruits enter in spring of styr+1.  so first male spb and first rec go together
    tmp=length(unlist((obj.rep$"Mating.time.Male.Spawning.Biomass")))
    plot(as.numeric(unlist(obj.rep$"Mating.time.Male.Spawning.Biomass"))[21:(tmp-5)],as.numeric(unlist(obj.rep$"estimated.number.of.recruitments.female")[25:(tmp-1)]),
    xlim=c(0,max(as.numeric(unlist(obj.rep$"Mating.time.Male.Spawning.Biomass")))),
    ylim=c(0,max(as.numeric(unlist(obj.rep$"estimated.number.of.recruitments.female")[25:(tmp-1)]))),
    xlab="Male Spawning Biomass(1000 t) at Feb. 15",ylab="Recruitment",type="n")
    for(i in 1:(tmp-5-20)){
      text(as.numeric(unlist((obj.rep$"Mating.time.Male.Spawning.Biomass")))[20+i],
      as.numeric(unlist(obj.rep$"estimated.number.of.recruitments.female"))[24+i],
      if((68+i)<100){as.character(68+i)}else{paste("0",as.character(i-32),sep="")},adj=0,cex=1.0)
    }
    
    dev.off()
    return(invisible(list(rep=obj.rep,std=obj.std,prs=obj.prs)));
}