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
#'@importFrom wtsUtilities selectFiles
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
                                         pdf="ModelResultsComparisonPlots.pdf"){
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

    #set some constants
    THOUSAND<-1000;
    length.bins<- seq(27,182,length=32);
    years<-seq(styr,endyr);
    obsyears=seq(obsyr,endyr);
    plotyears=seq(pltyr,endyr);
    years.m1<-years[1:(length(years)-1)]; 
    
    obj.rep<-objs[[1]];
    
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

    if(is.character(pdf)){
        pdf(file=pdf,width=6,height=8,onefile=TRUE);
        oldpar<-par(oma=c(0.5,1,1,0.5),mar=c(2,5,1,1)+0.2,mfrow=c(3,1));
    } else {
        oldpar<-par(oma=c(0.5,1,1,0.5),mar=c(2,5,1,1)+0.2);
    }
    on.exit(par(oldpar));
    
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
    vartype<-"estimated.number.of.recruitments.male";
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
    oyrs<-obj.rep$"years.obs.total.catch.directed.fishery"[1]:(endyr-1)
    obs<-obj.rep$"observed.retained.discard.male.catch.biomass"
    vartype<-"predicted.retained.discard.male.catch.biomass"
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
    oyrs<-obj.rep$"years.obs.total.catch.directed.fishery"[1]:(endyr-1)
    obs<-obj.rep$"observed.female.discard.mortality.biomass"
    vartype<-"predicted.female.discard.mortality.biomass" 
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(oyrs,obs,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Females, directed fishery",
                                   ylab="Bycatch mortality (1000's t)")    
    exportModelComparisons.TimeSeries(oyrs,obs,NULL,
                                      yrs,vartype,objs,scaleBy=1)    
    
    #"retained.fTCFM_syz"                 
    vartype<-"retained.fTCFM_syz"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Males, retained fishing mortality rate",
                                   ylab="Fully-selected Retained Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"estimated.annual.male.total.directed.fishing.mortality"                 
    vartype<-"estimated.annual.male.total.directed.fishing.mortality"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Males, total directed fishing mortality rate",
                                   ylab="Fully-selected Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"estimated.annual.female.total.directed.fishing.mortality"               
    vartype<-"estimated.annual.female.total.directed.fishing.mortality"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Females, total directed fishing mortality rate",
                                   ylab="Fully-selected Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"estimated.annual.snow.fishing.mortality"                                
    vartype<-"estimated.annual.snow.fishing.mortality";
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Snow crab bycatch fishing mortality rate",
                                   ylab="Fully-selected Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"estimated.annual.red.king.fishing.mortality"                            
    vartype<-"estimated.annual.red.king.fishing.mortality"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="BBRKC bycatch fishing mortality",
                                   ylab="Fully-selected Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    #"estimated.annual.fishing.mortality.trawl.bycatch"                       
    vartype<-"estimated.annual.fishing.mortality.trawl.bycatch"
    yrs<-styr:(endyr-1)
    plotModelComparisons.TimeSeries(NULL,NULL,NULL,
                                   yrs,vartype,objs,
                                   scaleBy=1,
                                   clrs=clrs,
                                   title="Groundfish bycatch fishing mortality",
                                   ylab="Fully-selected Fishing Mortality Rate")    
    exportModelComparisons.TimeSeries(NULL,NULL,NULL,
                                      yrs,vartype,objs,scaleBy=1)    

    
    if(is.character(pdf)) dev.off();#close pdf file

    return(invisible(objs));
}