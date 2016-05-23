#'
#'@title Get estimated/predicted time series from several model runs
#'
#'@description Function to get estimated/predicted time series fom 
#'several model runs.
#'
#'@param reps - list of objects derived from Jack's R files for the models to be compared
#'
#'@details Potential values for 'type' are:
#'\itemize{
#'  \item {'MB_xy' - mature biomass at mating, bysex (1000's t)}
#'  \item {'rec_y' - total recruitment (millions)}
#'}
#'
#'@return dataframe
#'
#'@export
#'
getMDFR.PopQuants<-function(reps,
                              type="MB_xy"){

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
    THOUSAND <-1000;

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
    
    dfr<-NULL;
    #----------------------------------
    #Mating Biomass
    #----------------------------------
    if (type=="MB_xy"){
        #--males
        types<-c('MMB');
        mtypes<-c("Mating.time.Male.Spawning.Biomass");
        for (t in 1:length(types)){
            dfrp<-NULL;
            for (case in cases){
                mtype<-mtypes[t];
                prd <-(reps[[case]])[[mtype]];
                dfrm<-data.frame(x='male',y=years.m1[[case]],val=prd,
                                 model=case,modeltype='TCSAM2013');
                dfrp<-rbind(dfrp,dfrm);
            }
            dfr<-rbind(dfr,dfrp);
        }
        #--females
        types<-c('MFB');
        mtypes<-c("Mating.time.Female.Spawning.Biomass");
        for (t in 1:length(types)){
            dfrp<-NULL;
            for (case in cases){
                mtype<-mtypes[t];
                prd <-(reps[[case]])[[mtype]];
                dfrm<-data.frame(x='female',y=years.m1[[case]],val=prd,
                                 model=case,modeltype='TCSAM2013');
                dfrp<-rbind(dfrp,dfrm);
            }
            dfr<-rbind(dfr,dfrp);
        }
        return(dfr);
    }

    #----------------------------------
    #recruitment
    #----------------------------------
    types<-c('Recruitment');
    mtypes<-c("estimated.number.of.recruits.male");
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-2*(reps[[case]])[[mtype]]/1000;#scale to millions, males+females
            dfrm<-data.frame(y=years[[case]],val=prd,
                             model=case,modeltype='TCSAM2013');
            dfrp<-rbind(dfrp,dfrm);
        }
        dfr<-rbind(dfr,dfrp);
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
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
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
    write.csv(dfr,file='ModelComparisons.MatureSurveyBiomass.csv',row.names=FALSE);
    
    #---------------------------------
    #retained catch
    #---------------------------------
    obsyrs <-"years.obs.retained.catch.directed.fishery";
    obstype<-"observed.retained.catch.biomass";
    vartype<-"predicted.retained.catch.biomass";
    title<-"Retained catch biomass";
    ylab<-"Retained catch biomass (1000's t)"; 
    dfr<-NULL;
    types<-c('male');
    otypes<-c(obstype);
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            otype<-otypes[t];
            oyrs<-reps[[case]][[obsyrs]][1]:(endyr[[case]]-1);
            est <-(reps[[case]])[[otype]];
            dfro<-data.frame(case=case,category='observed',year=oyrs,val=est,cv=NA);
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
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
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=TRUE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.RetainedCatch.csv',row.names=FALSE);
    
    #---------------------------------
    #total catch mortality, directed fishery, males
    #---------------------------------
    obsyrs <-"observed.TCF.years.discard.catch";
    obstype<-"observed.TCF.male.tot.biomass.mortality";
    vartype<-"predicted.TCF.male.tot.biomass.mortality";
    title="Males, directed fishery";
    ylab="Total mortality (1000's t)";   
    dfr<-NULL;
    types<-c('male');
    otypes<-c(obstype);
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            otype<-otypes[t];
            oyrs<-reps[[case]][[obsyrs]];
            est <-(reps[[case]])[[otype]];
            dfro<-data.frame(case=case,category='observed',year=oyrs,val=est,cv=NA);
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
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
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=TRUE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.TotalMortality.TCF.Males.csv',row.names=FALSE);
    
    #---------------------------------
    #total catch mortality, directed fishery, females
    #---------------------------------
    obsyrs <-"observed.TCF.years.discard.catch";
    obstype<-"observed.TCF.female.discard.mortality.biomass";
    vartype<-"predicted.TCF.female.discard.mortality.biomass";
    title="Females, directed fishery";
    ylab="Bycatch mortality (1000's t)";
    dfr<-NULL;
    types<-c('female');
    otypes<-c(obstype);
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            otype<-otypes[t];
            oyrs<-reps[[case]][[obsyrs]];
            est <-(reps[[case]])[[otype]];
            dfro<-data.frame(case=case,category='observed',year=oyrs,val=est,cv=NA);
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
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
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=TRUE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.TotalMortality.TCF.Females.csv',row.names=FALSE);
    
    #---------------------------------
    #bycatch mortality for males in the snow crab fishery
    #---------------------------------
    obsyrs <-"observed.SCF.years.discard.catch";
    obstype<-"observed.SCF.male.discard.mortality.biomass";
    vartype<-"predicted.SCF.male.discard.mortality.biomass";
    title="Males, snow crab fishery bycatch mortality";
    ylab="Discard mortality (1000's t)"; 
    dfr<-NULL;
    types<-c('male');
    otypes<-c(obstype);
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            otype<-otypes[t];
            oyrs<-reps[[case]][[obsyrs]];
            est <-(reps[[case]])[[otype]];
            dfro<-data.frame(case=case,category='observed',year=oyrs,val=est,cv=NA);
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
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
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=TRUE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.TotalMortality.SCF.Males.csv',row.names=FALSE);
    
    #---------------------------------
    #bycatchcatch mortality for females in the snow crab fishery
    #---------------------------------
    obsyrs <-"observed.SCF.years.discard.catch";
    obstype<-"observed.SCF.female.discard.mortality.biomass";
    vartype<-"predicted.SCF.female.discard.mortality.biomass"; 
    title="Females, snow crab fishery bycatch mortality";
    ylab="Discard mortality (1000's t)";
    dfr<-NULL;
    types<-c('female');
    otypes<-c(obstype);
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            otype<-otypes[t];
            oyrs<-reps[[case]][[obsyrs]];
            est <-(reps[[case]])[[otype]];
            dfro<-data.frame(case=case,category='observed',year=oyrs,val=est,cv=NA);
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
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
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=TRUE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.TotalMortality.SCF.Females.csv',row.names=FALSE);

    #---------------------------------
    #bycatch catch mortality for males in the BBRKC fishery
    #---------------------------------
    obsyrs <-"observed.RKF.years.discard.catch";
    obstype<-"observed.RKF.male.discard.mortality.biomass";
    vartype<-"predicted.RKF.male.discard.mortality.biomass";
    title="Males, BBRKC crab fishery bycatch mortality";
    ylab="Discard mortality (1000's t)";
    dfr<-NULL;
    types<-c('male');
    otypes<-c(obstype);
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            otype<-otypes[t];
            oyrs<-reps[[case]][[obsyrs]];
            est <-(reps[[case]])[[otype]];
            dfro<-data.frame(case=case,category='observed',year=oyrs,val=est,cv=NA);
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
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
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=TRUE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.TotalMortality.RKF.Males.csv',row.names=FALSE);
    
    #---------------------------------
    #bycatch mortality for females in the BBRKC fishery
    #---------------------------------
    obsyrs <-"observed.RKF.years.discard.catch";
    obstype<-"observed.RKF.female.discard.mortality.biomass";
    vartype<-"predicted.RKF.female.discard.mortality.biomass";
    title="Females, BBRKC fishery bycatch mortality";
    ylab="Discard mortality (1000's t)"; 
    dfr<-NULL;
    types<-c('female');
    otypes<-c(obstype);
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            otype<-otypes[t];
            oyrs<-reps[[case]][[obsyrs]];
            est <-(reps[[case]])[[otype]];
            dfro<-data.frame(case=case,category='observed',year=oyrs,val=est,cv=NA);
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
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
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=TRUE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.TotalMortality.RKF.Females.csv',row.names=FALSE);
    
    #---------------------------------
    #bycatch mortality in the groundfish fisheries
    #---------------------------------
    obsyrs <-"observed.GTF.years.discard.catch";
    obstype<-"observed.GTF.discard.mortality.biomass";
    vartype<-"predicted.GTF.discard.mortality.biomass";
    title="Groundfish fisheries bycatch mortality";
    ylab="Discard mortality (1000's t)";
    dfr<-NULL;
    types<-c('all');
    otypes<-c(obstype);
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            otype<-otypes[t];
            oyrs<-reps[[case]][[obsyrs]];
            est <-(reps[[case]])[[otype]];
            dfro<-data.frame(case=case,category='observed',year=oyrs,val=est,cv=NA);
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
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
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=TRUE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.TotalMortality.GTF.Males.csv',row.names=FALSE);
    
    #---------------------------------
    #max retained fishing mortality rate                 
    #---------------------------------
    vartype<-"max.retained.mortality.rate"
    title="Males, retained fishing mortality rate";
    ylab="Fully-selected Retained Mortality Rate";   
    dfr<-NULL;
    types<-c('male');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MaxFishingMortalityRate.TCFR.csv',row.names=FALSE);
    
    #---------------------------------
    #mean retained fishing mortality rate                 
    #---------------------------------
    vartype<-"mean.retained.mortality.rate";
    title="Males, retained fishing mortality rate";
    ylab="Mean Retained Mortality Rate";
    dfr<-NULL;
    types<-c('male');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MeanFishingMortalityRate.TCFR.csv',row.names=FALSE);

    #---------------------------------
    #"max.TOT.male.NS.mortality.rate"                 
    #---------------------------------
    vartype<-"max.TOT.male.NS.mortality.rate";
    title="Males, directed fishery total mortality rate";
    ylab="Fully-selected Fishing Mortality Rate";
    dfr<-NULL;
    types<-c('male');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MaxTotFishingMortalityRate.TCF.Males.csv',row.names=FALSE);

    #---------------------------------
    #"mean.TOT.male.NS.mortality.rate"                 
    #---------------------------------
    vartype<-"mean.TOT.male.NS.mortality.rate";
    title="Males, directed fishery total mortality rate";
    ylab="Mean Fishing Mortality Rate";
    dfr<-NULL;
    types<-c('male');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MeanTotFishingMortalityRate.TCF.Males.csv',row.names=FALSE);

    #---------------------------------
    #"max.TOT.female.NS.mortality.rate"               
    #---------------------------------
    vartype<-"max.TOT.female.NS.mortality.rate";
    title="Females, directed fishery bycatch mortality rate";
    ylab="Fully-selected Fishing Mortality Rate";
    dfr<-NULL;
    types<-c('female');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MaxTotFishingMortalityRate.TCF.Females.csv',row.names=FALSE);

    #---------------------------------
    #"mean.TOT.female.NS.mortality.rate"               
    #---------------------------------
    vartype<-"mean.TOT.female.NS.mortality.rate";
    title="Females, directed fishery bycatch mortality rate";
    ylab="Mean Fishing Mortality Rate";
    dfr<-NULL;
    types<-c('female');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MeanTotFishingMortalityRate.TCF.Females.csv',row.names=FALSE);

    #---------------------------------
    #"max.SCF.male.mortality.rate"                                
    #---------------------------------
    vartype<-"max.SCF.male.mortality.rate";
    title="Snow crab bycatch mortality rate on males";
    ylab="Fully-selected Fishing Mortality Rate";
    dfr<-NULL;
    types<-c('male');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MaxTotFishingMortalityRate.SCF.Males.csv',row.names=FALSE);

    #---------------------------------
    #"mean.SCF.male.mortality.rate"                                
    #---------------------------------
    vartype<-"mean.SCF.male.mortality.rate";
    title="Snow crab bycatch mortality rate on males";
    ylab="Mean Fishing Mortality Rate";
    dfr<-NULL;
    types<-c('male');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MeanTotFishingMortalityRate.SCF.Males.csv',row.names=FALSE);

    #---------------------------------
    #"max.SCF.female.mortality.rate"                                
    #---------------------------------
    vartype<-"max.SCF.female.mortality.rate";
    title="Snow crab bycatch mortality rate on females";
    ylab="Fully-selected Fishing Mortality Rate";   
    dfr<-NULL;
    types<-c('female');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MaxTotFishingMortalityRate.SCF.Females.csv',row.names=FALSE);

    #---------------------------------
    #"mean.SCF.female.mortality.rate"                                
    #---------------------------------
    vartype<-"mean.SCF.female.mortality.rate";
    title="Snow crab bycatch mortality rate on females";
    ylab="Mean Fishing Mortality Rate";   
    dfr<-NULL;
    types<-c('female');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MeanTotFishingMortalityRate.SCF.Females.csv',row.names=FALSE);

    #---------------------------------
    #"max.RKF.male.mortality.rate"                            
    #---------------------------------
    vartype<-"max.RKF.male.mortality.rate";
    title="BBRKC bycatch mortality rate on males";
    ylab="Fully-selected Fishing Mortality Rate";
    dfr<-NULL;
    types<-c('male');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MaxTotFishingMortalityRate.RKF.Males.csv',row.names=FALSE);

    #---------------------------------
    #"mean.RKF.male.mortality.rate"                            
    #---------------------------------
    vartype<-"mean.RKF.male.mortality.rate";
    title="BBRKC bycatch mortality rate on males";
    ylab="Mean Fishing Mortality Rate";
    dfr<-NULL;
    types<-c('male');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MeanTotFishingMortalityRate.RKF.Males.csv',row.names=FALSE);

    #---------------------------------
    #"max.RKF.female.mortality.rate"                            
    #---------------------------------
    vartype<-"max.RKF.female.mortality.rate";
    title="BBRKC bycatch mortality rate on females";
    ylab="Fully-selected Fishing Mortality Rate";
    dfr<-NULL;
    types<-c('female');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MaxTotFishingMortalityRate.RKF.Females.csv',row.names=FALSE);

    #---------------------------------
    #"mean.RKF.female.mortality.rate"                            
    #---------------------------------
    vartype<-"mean.RKF.female.mortality.rate";
    title="BBRKC bycatch mortality rate on females";
    ylab="Mean Fishing Mortality Rate";
    dfr<-NULL;
    types<-c('female');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MeanTotFishingMortalityRate.RKF.Females.csv',row.names=FALSE);

    #---------------------------------
    #"max.GTF.male.mortality.rate"                       
    #---------------------------------
    vartype<-"max.GTF.male.mortality.rate";
    title="Groundfish bycatch fishing mortality on males";
    ylab="Fully-selected Fishing Mortality Rate";
    dfr<-NULL;
    types<-c('male');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MaxTotFishingMortalityRate.GTF.Males.csv',row.names=FALSE);

    #---------------------------------
    #"mean.GTF.male.mortality.rate"                       
    #---------------------------------
    vartype<-"mean.GTF.male.mortality.rate";
    title="Groundfish bycatch fishing mortality on males";
    ylab="Mean Fishing Mortality Rate";
    dfr<-NULL;
    types<-c('male');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MeanTotFishingMortalityRate.GTF.Males.csv',row.names=FALSE);
    
    #---------------------------------
    #"max.GTF.female.mortality.rate"                       
    #---------------------------------
    vartype<-"max.GTF.female.mortality.rate";
    title="Groundfish bycatch fishing mortality on females";
    ylab="Fully-selected Fishing Mortality Rate";
    dfr<-NULL;
    types<-c('female');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MaxTotFishingMortalityRate.GTF.Females.csv',row.names=FALSE);

    #---------------------------------
    #"mean.GTF.female.mortality.rate"                       
    #---------------------------------
    vartype<-"mean.GTF.female.mortality.rate";
    title="Groundfish bycatch fishing mortality on females";
    ylab="Mean Fishing Mortality Rate";
    dfr<-NULL;
    types<-c('female');
    mtypes<-c(vartype);
    for (t in 1:length(types)){
        dfrp<-NULL;
        for (case in cases){
            mtype<-mtypes[t];
            prd <-(reps[[case]])[[mtype]]
            dfrm<-data.frame(case=case,category='predicted',year=years.m1[[case]],val=prd,cv=NA);
            dfrp<-rbind(dfrp,dfrm);
        }
        dfrp$type <- types[t];
        dfr<-rbind(dfr,dfrp);
    }
    #make 4-plot from observations & model results
    ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                            numRecent=numRecent,
                                            facets='type~.',
                                            plotObs=FALSE,
                                            plotMod=TRUE,
                                            ci=0.95,
                                            pdfType='lognormal',
                                            xlab='year',
                                            ylab=ylab,
                                            title=title,
                                            xlims=NULL,
                                            ylims=NULL,
                                            showPlot=TRUE);
    write.csv(dfr,file='ModelComparisons.MeanTotFishingMortalityRate.GTF.Females.csv',row.names=FALSE);
    
    #----------------------------------
    #legal male abundance at survey time
    #----------------------------------
    if (plotLegal){
        dfr<-NULL;
        types<-c('legal males');
        otypes<-c("observed.number.of.males.greater.than.101.mm");
        mtypes<-c("estimated.survey.numbers.of.males.101");
        for (t in 1:length(types)){
            dfrp<-NULL;
            for (case in cases){
                otype<-otypes[t];
                est <-(reps[[case]])[[otype]]/1000;#convert to millions
                dfro<-data.frame(case=case,category='observed',year=obsyears[[case]],val=est);
                mtype<-mtypes[t];
                prd <-(reps[[case]])[[mtype]]/1000;#convert to millions
                dfrm<-data.frame(case=case,category='predicted',year=years[[case]],val=prd);
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
        #make 4-plot from observations & model results
        ps<-plot4.ModelComparisonsGG.TimeSeries(dfr,
                                                numRecent=numRecent,
                                                facets='type~.',
                                                plotObs=TRUE,
                                                plotMod=TRUE,
                                                ci=0.95,
                                                pdfType='lognormal',
                                                xlab='year',
                                                ylab="'Legal' males (millions)",
                                                title="'Legal' males (> 138 mm CW)",
                                                xlims=NULL,
                                                ylims=NULL,
                                                showPlot=TRUE);
        write.csv(dfr,file='ModelComparisons.LegalMaleAbundance.csv',row.names=FALSE);
    }
    return(invisible(reps));
}