#'
#'@title Compare an estimated/predicted time series among several model runs.
#'
#'@description Function to compare an estimated/predicted time series among 
#'several model runs.
#'
#'@param objs - list of objects derived from Jack's R files for the models to be compared
#'@param cases - vector of labels for model cases (if 'objs' is not given)
#'@param styr - start year
#'@param endyr - end year
#'@param obsyr - start year for survey observations
#'@param pltyr - start year for plots with just model-predicted values
#'@param reclag - recruitment lag
#'@param clrs - colors to use for plots
#'@param pdf - name for output pdf file
#'
#'@details If 'objs' is not given, then the user is prompted to select Jack's R file output from each 
#'model to be compared. If 'cases' is given, the user is prompted to select the file
#'corresponding to each case. If 'cases' is not given, then the user may select an 
#'arbitrary number of files (one at a time), ending selection by pressing 'cancel' on the selection box'.\cr\cr
#'If 'objs' is not given, the working directory is set two levels above the 1st model case file selected.
#'
#'@return vector of list objects corresponding to the objects returned by each model R file.
#'
#'@importFrom PBSmodelling readList
#'@importFrom wtsUtilities selectFile
#'
#'@export
#'
compareModelResults.TimeSeries<-function(objs=NULL,
                                         cases=NULL,
                                         styr=1949,     #model start year
                                         endyr=2014,    #model end year
                                         obsyr=1974,    #first year of survey observations
                                         pltyr=1969,    #first year for plots
                                         recLag=5,
                                         clrs=c('blue','green','cyan','red','orange','darkgrey','darkseagreen'),
                                         pdf="ModelComparison.TimeSeries.pdf"){
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

    obj.rep<-objs[[1]];
    
    if (is.null(endyr)){
        if (is.null(obj.rep$endyr)){
            cat("'endyr' missing from rep file and not specified as an input.\n")
            cat("Must set 'endyr' to assessment year.\n",
                "Aborting...\n");
            return(NULL);
        } else {
            endyr<-obj.rep$endyr;
        }
    }
    if (is.null(styr)){
        if (is.null(obj.rep$styr)){
            cat("'styr' missing from rep file and not specified as an input.\n")
            cat("Must set 'styr' to assessment year.\n",
                "Aborting...\n");
            return(NULL);
        } else {
            styr<-obj.rep$styr;
        }
    }
    if (is.null(obsyr)){
        if (is.null(obj.rep$obsyr)){
            cat("'obsyr' missing from rep file and not specified as an input.\n")
            cat("Must set 'obsyr' to assessment year.\n",
                "Aborting...\n");
            return(NULL);
        } else {
            obsyr<-obj.rep$obsyr;
        }
    }
    if (is.null(pltyr)){
        if (is.null(obj.rep$pltyr)){
            cat("'pltyr' missing from rep file and not specified as an input.\n")
            cat("Must set 'pltyr' to assessment year.\n",
                "Aborting...\n");
            return(NULL);
        } else {
            pltyr<-obj.rep$pltyr;
        }
    }
    
    #set some constants
    THOUSAND <-1000;
    years    <-seq(styr,endyr);
    years.m1 <-seq(styr,endyr-1);
    obsyears <-seq(obsyr,endyr);
    plotyears<-seq(pltyr,endyr);
    
    length.bins<-obj.rep$length.bins;
    if (is.null(length.bins)) length.bins<- seq(27,182,length=32);

    if(!is.null(pdf)){
        pdf(file=pdf,width=6,height=8,onefile=TRUE);
        oldpar<-par(oma=c(0.5,1,1,0.5),mar=c(2,5,1,1)+0.2,mfrow=c(3,1));
        on.exit(dev.off());
    } else {
        oldpar<-par(oma=c(0.5,1,1,0.5),mar=c(2,5,1,1)+0.2);
    }
    on.exit(par(oldpar),add=TRUE);
    
    #----------------------------------
    # plot observed total and mature (spawning) biomass from survey
    #----------------------------------
    #observed spawning biomass
    idx<-(1:length(years))[years==min(obsyears)]:length(years)  
    spB.m.obs <-obj.rep$"Observed.survey.male.spawning.biomass"[idx]
    spB.f.obs <-obj.rep$"Observed.survey.female.spawning.biomass"[idx]
    spB.t.obs <-obj.rep$"Observed.survey.biomass"[idx]
    spB.tm.obs<-spB.m.obs+spB.f.obs;#total observed survey MATURE biomass
    #cv's    
    tanner.cv= cbind(obsyears,obj.rep$Observed.survey.female.spawning.biomass.cv,obj.rep$Observed.survey.male.spawning.biomass.cv)
    cv.f<-tanner.cv[,2];
    cv.m<-tanner.cv[,3];
    cv.t=sqrt((spB.m.obs*cv.m)^2 + (spB.f.obs*cv.f)^2)/(spB.tm.obs);

    #survey-time male mature biomass
    vartype<-"Predicted.Male.survey.mature.Biomass";
    plotModelComparisons.TimeSeries(obsyears,spB.m.obs,cv.m,
                                   years,vartype,objs,
                                   clrs=clrs,
                                   title="Mature male survey biomass",
                                   ylab="Biomass (1000's t)")    
    exportModelComparisons.TimeSeries(obsyears,spB.m.obs,cv.m,
                                   years,vartype,objs)    
    
    #survey-time mature female biomass
    vartype<-"Predicted.Female.survey.mature.Biomass";
    plotModelComparisons.TimeSeries(obsyears,spB.f.obs,cv.f,
                                   years,vartype,objs,
                                   clrs=clrs,
                                   title="Mature female survey biomass",
                                   ylab="Biomass (1000's t)")    
    exportModelComparisons.TimeSeries(obsyears,spB.f.obs,cv.f,
                                   years,vartype,objs)    
    
    #legal male abundance at survey time
    obs<-obj.rep$"observed.number.of.males.greater.than.101.mm"/1000;
    vartype<-"estimated.survey.numbers.of.males.101";
    plotModelComparisons.TimeSeries(obsyears,obs,NULL,
                                   years,vartype,objs,
                                   scaleBy=1000,
                                   clrs=clrs,
                                   title="'Legal' males (> 138 mm CW)",
                                   ylab="'Legal' males (millions)")    
    exportModelComparisons.TimeSeries(obsyears,obs,NULL,
                                   years,vartype,objs,scaleBy=1000)    
    
    #male spawning biomass
    vartype<-"Mating.time.Male.Spawning.Biomass";
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   clrs=clrs,
                                   title="Mature male spawning biomass (MMB)",
                                   ylab="Biomass (1000's t)")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs)    
    
    #female spawning biomass
    vartype<-"Mating.time.Female.Spawning.Biomass";                                     
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   clrs=clrs,
                                   title="Mature female spawning biomass",
                                   ylab="Biomass (1000's t)")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                     yrs,vartype,objs)    
    
    #recruitment
    vartype<-"estimated.number.of.recruits.male";
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   years,vartype,objs,
                                   clrs=clrs,
                                   scaleBy=1000,
                                   title="Male recruits",
                                   ylab="Recruitment (millions)")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   years,vartype,objs,scaleBy=1000)    
    
    #retained catch
    oyrs<-obj.rep$"years.obs.retained.catch.directed.fishery"[1]:(endyr-1);
    obs<-obj.rep$"observed.retained.catch.biomass"
    vartype<-"predicted.retained.catch.biomass"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(oyrs,obs,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Retained catch biomass",
                                   ylab="Retained catch biomass (1000's t)")    
    exportModelComparisons.TimeSeries(oyrs,obs,NULL,
                                   yrs,vartype,objs,scaleBy=1)    
    
    #total catch mortality, directed fishery, males
    oyrs<-obj.rep$"observed.TCF.years.discard.catch"
    obs<-obj.rep$"observed.TCF.male.tot.biomass.mortality"
    vartype<-"predicted.TCF.male.tot.biomass.mortality"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(oyrs,obs,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Males, directed fishery",
                                   ylab="Total mortality (1000's t)")    
    exportModelComparisons.TimeSeries(oyrs,obs,NULL,
                                      yrs,vartype,objs,scaleBy=1)    
    
    #total catch mortality, directed fishery, females
    oyrs<-obj.rep$"observed.TCF.years.discard.catch"
    obs<-obj.rep$"observed.TCF.female.discard.mortality.biomass"
    vartype<-"predicted.TCF.female.discard.mortality.biomass" 
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(oyrs,obs,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Females, directed fishery",
                                   ylab="Bycatch mortality (1000's t)")    
    exportModelComparisons.TimeSeries(oyrs,obs,NULL,
                                      yrs,vartype,objs,scaleBy=1)    
    
    #total catch mortality, directed fishery, males
    oyrs<-obj.rep$"observed.SCF.years.discard.catch"
    obs<-obj.rep$"observed.SCF.male.disccard.biomass.mortality"
    vartype<-"predicted.SCF.male.discard.biomass.mortality"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(oyrs,obs,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Males, snow crab fishery bycatch mortality",
                                   ylab="Discard mortality (1000's t)")    
    exportModelComparisons.TimeSeries(oyrs,obs,NULL,
                                      yrs,vartype,objs,scaleBy=1)    
    
    #total catch mortality for females snow crab fishery bycatch
    oyrs<-obj.rep$"observed.SCF.years.discard.catch"
    obs<-obj.rep$"observed.SCF.female.discard.mortality.biomass"
    vartype<-"predicted.SCF.female.discard.mortality.biomass" 
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(oyrs,obs,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Females, snow crab fishery bycatch mortality",
                                   ylab="Discard mortality (1000's t)")    
    exportModelComparisons.TimeSeries(oyrs,obs,NULL,
                                      yrs,vartype,objs,scaleBy=1)    
    
    #max retained fsihing mortality rate                 
    vartype<-"max.retained.mortality.rate"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Males, retained fishing mortality rate",
                                   ylab="Fully-selected Retained Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #bycatch catch mortality for males in the BBRKC fishery
    oyrs<-obj.rep$"observed.RKF.years.discard.catch"
    obs<-obj.rep$"observed.RKF.male.disccard.biomass.mortality"
    vartype<-"predicted.SCF.male.discard.biomass.mortality"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(oyrs,obs,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Males, BBRKC crab fishery bycatch mortality",
                                   ylab="Discard mortality (1000's t)")    
    exportModelComparisons.TimeSeries(oyrs,obs,NULL,
                                      yrs,vartype,objs,scaleBy=1)    
    
    #bycatch mortality for females in the BBRKC fishery
    oyrs<-obj.rep$"observed.RKF.years.discard.catch"
    obs<-obj.rep$"observed.RKF.female.discard.mortality.biomass"
    vartype<-"predicted.RKF.female.discard.mortality.biomass" 
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(oyrs,obs,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Females, BBRKC fishery bycatch mortality",
                                   ylab="Discard mortality (1000's t)")    
    exportModelComparisons.TimeSeries(oyrs,obs,NULL,
                                      yrs,vartype,objs,scaleBy=1)    
    
    #bycatch mortality in the groundfish fisheries
    oyrs<-obj.rep$"observed.GTF.years.discard.catch"
    obs<-obj.rep$"observed.GTF.discard.mortality.biomass"
    vartype<-"predicted.GTF.discard.mortality.biomass" 
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(oyrs,obs,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Groundfish fisheries bycatch mortality",
                                   ylab="Discard mortality (1000's t)")    
    exportModelComparisons.TimeSeries(oyrs,obs,NULL,
                                      yrs,vartype,objs,scaleBy=1)    
    
    #max retained fishing mortality rate                 
    vartype<-"max.retained.mortality.rate"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Males, retained fishing mortality rate",
                                   ylab="Fully-selected Retained Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    
    
    #mean retained fsihing mortality rate                 
    vartype<-"mean.retained.mortality.rate"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Males, retained fishing mortality rate",
                                   ylab="Mean Retained Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"max.TOT.male.NS.mortality.rate"                 
    vartype<-"max.TOT.male.NS.mortality.rate"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Males, directed fishery total mortality rate",
                                   ylab="Fully-selected Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"mean.TOT.male.NS.mortality.rate"                 
    vartype<-"mean.TOT.male.NS.mortality.rate"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Males, directed fishery total mortality rate",
                                   ylab="Mean Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"max.TOT.female.NS.mortality.rate"               
    vartype<-"max.TOT.female.NS.mortality.rate"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Females, directed fishery bycatch mortality rate",
                                   ylab="Fully-selected Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"mean.TOT.female.NS.mortality.rate"               
    vartype<-"mean.TOT.female.NS.mortality.rate"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Females, directed fishery bycatch mortality rate",
                                   ylab="Mean Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"max.SCF.male.mortality.rate"                                
    vartype<-"max.SCF.male.mortality.rate";
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Snow crab bycatch mortality rate on males",
                                   ylab="Fully-selected Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"mean.SCF.male.mortality.rate"                                
    vartype<-"mean.SCF.male.mortality.rate";
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Snow crab bycatch mortality rate on males",
                                   ylab="Mean Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"max.SCF.female.mortality.rate"                                
    vartype<-"max.SCF.female.mortality.rate";
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Snow crab bycatch mortality rate on females",
                                   ylab="Fully-selected Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"mean.SCF.female.mortality.rate"                                
    vartype<-"mean.SCF.female.mortality.rate";
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Snow crab bycatch mortality rate on females",
                                   ylab="Mean Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"max.RKF.male.mortality.rate"                            
    vartype<-"max.RKF.male.mortality.rate"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="BBRKC bycatch mortality rate on males",
                                   ylab="Fully-selected Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"mean.RKF.male.mortality.rate"                            
    vartype<-"mean.RKF.male.mortality.rate"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="BBRKC bycatch mortality rate on males",
                                   ylab="Mean Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"max.RKF.female.mortality.rate"                            
    vartype<-"max.RKF.female.mortality.rate"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="BBRKC bycatch mortality rate on females",
                                   ylab="Fully-selected Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"mean.RKF.female.mortality.rate"                            
    vartype<-"mean.RKF.female.mortality.rate"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="BBRKC bycatch mortality rate on females",
                                   ylab="Mean Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"max.GTF.male.mortality.rate"                       
    vartype<-"max.GTF.male.mortality.rate"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Groundfish bycatch fishing mortality",
                                   ylab="Fully-selected Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"mean.GTF.male.mortality.rate"                       
    vartype<-"mean.GTF.male.mortality.rate"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Groundfish bycatch fishing mortality",
                                   ylab="Mean Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    
    
    return(invisible(objs));
}