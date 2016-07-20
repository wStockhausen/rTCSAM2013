#'
#'@title Compare estimated/predicted survey time series among several model runs
#'
#'@description Function to compare estimated/predicted survey time series among 
#'several model runs.
#'
#'@param reps - list of objects derived from Jack's R files for the models to be compared
#'@param numRecent - number of recent years to plot
#'@param styr  - start year for model run
#'@param endyr - final year of model run (assessment year)
#'@param plot1stObs - flag to plot observations from the first case, only
#'@param plotLegal  - flag to plot legal abundance
#'@param obsyr - start year for survey observations
#'@param pltyr - start year for plots with only model-predicted values
#'@param reclag - recruitment lag
#'@param pdf - name for output pdf file
#'
#'@details Uses \code{getMDFR.SurveyTimeSeries}.
#'
#'@return list of ggplot2 objects
#'
#'@export
#'
compareModelResults.SurveyTimeSeries<-function(reps=NULL,
                                               numRecent=15,
                                               styr=NULL,     #model start year
                                               endyr=NULL,    #model end year
                                               plot1stObs=TRUE,
                                               plotLegal=FALSE,
                                               obsyr=NULL,    #first year of survey observations
                                               pltyr=NULL,    #first year for plots
                                               pdf="ModelComparisons.SurveyTimeSeries.pdf"){
    
    #make sure reps is a list of tcsam2013.rep objects
    if (inherits(reps,"tcsam2013.rep")) reps<-list(tcsam2013=reps);
    cases<-names(reps);
    
    #set up time info
    endyr<-list();
    for (case in cases){
        if (!is.null(reps[[case]]$endyr)) {
            endyr[[case]]<-reps[[case]]$endyr;
        } else {            
            cat("'endyr' missing from rep file for case '",case,"'!\n")
            cat("Must set 'endyr' in rep file.\n","Aborting...\n");
            return(NULL);
        }
    }
    styr<-list();
    for (case in cases){
        if (!is.null(reps[[case]]$styr)) {
            styr[[case]]<-reps[[case]]$styr;
        } else {            
            cat("'styr' missing from rep file for case '",case,"'!\n")
            cat("Must set 'styr' in rep file.\n","Aborting...\n");
            return(NULL);
        }
    }
    obsyr<-list();
    for (case in cases){
        if (!is.null(reps[[case]]$obsyr)) {
            obsyr[[case]]<-reps[[case]]$obsyr;
        } else {            
            cat("'obsyr' missing from rep file for case '",case,"'!\n")
            cat("Must set 'obsyr' in rep file.\n","Aborting...\n");
            return(NULL);
        }
    }
    pltyr<-list();
    for (case in cases){
        if (!is.null(reps[[case]]$pltyr)) {
            pltyr[[case]]<-reps[[case]]$pltyr;
        } else {            
            cat("'pltyr' missing from rep file for case '",case,"'!\n")
            cat("Must set 'pltyr' in rep file.\n","Aborting...\n");
            return(NULL);
        }
    }
    
    #set some constants
#    THOUSAND <-1000;

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        oldpar<-par(oma=c(0.5,1,1,0.5),mar=c(2,5,1,1)+0.2,mfrow=c(3,1));
        on.exit(dev.off());
    } else {
        oldpar<-par(oma=c(0.5,1,1,0.5),mar=c(2,5,1,1)+0.2);
    }
    on.exit(par(oldpar),add=TRUE);
    
    years    <-list();
    years.m1 <-list();
    obsyears <-list();
    plotyears<-list();
    for (case in cases){
        years[[case]]    <-styr[[case]]:endyr[[case]];
        years.m1[[case]] <-styr[[case]]:(endyr[[case]]-1);
        obsyears[[case]] <-obsyr[[case]]:endyr[[case]];
        plotyears[[case]]<-pltyr[[case]]:endyr[[case]];
    }
    
    #----------------------------------
    # plot observed and predicted mature (spawning) biomass from survey
    #----------------------------------
    dfr<-NULL;
    types<-c('male','female');
    otypes<-c("Observed.survey.male.spawning.biomass",
              "Observed.survey.female.spawning.biomass");
    mtypes<-c("Predicted.Male.survey.mature.Biomass",
              "Predicted.Female.survey.mature.Biomass");
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            otype<-otypes[t];
            idx<-years[[case]] %in% obsyears[[case]];
            est <-(reps[[case]])[[otype]][idx];
            cv  <-(reps[[case]])[[paste(otype,"cv",sep='.')]];
            dfro<-data.frame(case=case,category='observed',year=obsyears[[case]],val=est,cv=cv);
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years[[case]],val=prd,cv=NA);
            if (plot1stObs){
                if (case==cases[1]){
                    dfrp<-rbind(dfrp,dfro,dfrm);#include observations
                } else {
                    dfrp<-rbind(dfrp,dfrm);     #exclude observations
                }
            } else {dfrp<-rbind(dfrp,dfro,dfrm);}
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    dfrp<-getMDFR.SurveyTimeSeries(reps,"MB_yx")
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
                                            showPlot=TRUE);

    return(invisible(plots));
}