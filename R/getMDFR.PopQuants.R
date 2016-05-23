#'
#'@title Get predicted population quantities (time series) from several model runs
#'
#'@description Function to get predicted population quantities (time series) fom 
#'several model runs.
#'
#'@param reps - list of objects derived from Jack's R files for the models to be compared
#'
#'@details Potential values for 'type' are:
#'\itemize{
#'  \item {'MB_yx' - mature biomass at mating, bysex (1000's t)}
#'  \item {'R_y' - total recruitment (millions)}
#'  \item {'N_yxmsz' - numbers-at-size by x,m,s [TODO]}
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
    if (type=="MB_yx"){
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
    if (type=="R_y"){
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
        return(dfr);
    }

}