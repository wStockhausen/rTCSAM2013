#'
#'@title Get predicted population processes from several model runs
#'
#'@description Function to get predicted population processes from 
#'several model runs.
#'
#'@param obj - object with results for the models to be compared that can be converted to a list of tcsam2013.resLst objects
#'@param type - population process to retrieve
#'@param verbose - flag (T/F) to print debug info
#'
#'@details Potential values for 'type' are:
#'\itemize{
#'  \item {'M_yxm' - natural mortality rates}
#'  \item {'R_cz' - size distribution for recruitment}
#'  \item {'prM2M_cxz' - probability of molt-to-maturity}
#'  \item {'mnZAM_cxz' - mean growth increment}
#'  \item {'T_cxzz' - growth transition matrix}
#'}
#'Uses \code{reshape2::melt}.
#'
#'@return dataframe in canonical format.
#'
#'@export
#'
getMDFR.PopProcesses<-function(obj,
                               type=c('M_yxm',"R_cz",'prM2M_cxz','mnZAM_cxz','T_cxzz'),
                               verbose=FALSE){

    #----------------------------------
    #natural mortality rates
    #----------------------------------
    if (type[1]=="M_yxm"){
        dfr<-getMDFR.NaturalMortality(obj);
        return(dfr);
    }
    
    #----------------------------------
    #pr(molt-to-maturity|z)
    #----------------------------------
    if (type[1]=="prM2M_cxz"){
        dfr<-getMDFR.PrM2M(obj);
        return(dfr);
    }
    #----------------------------------
    #mean growth increments
    #----------------------------------
    if (type[1]=="mnZAM_cxz"){
        dfr<-getMDFR.meanGrowth(obj);
        return(dfr);
    }
    
    lst<-convertToListOfResults(obj);
    cases<-names(lst);
    
    tinfo<-getTimeInfo(lst);
    styr<-tinfo$styr;
    endyr<-tinfo$endyr;
    years<-tinfo$years;
    years.m1<-tinfo$years.m1;

    #----------------------------------
    #growth transition matrices
    #----------------------------------
    if (type[1]=="T_cxzz"){
        rws<-list();
        rws[["M"]]<-list(x=  'male');
        rws[["F"]]<-list(x='female');
        dfr<-NULL;
        for (case in cases){
            pc<-paste0(styr[[case]],"-",endyr[[case]]);
            for (nm in names(rws)) {
                rw<-rws[[nm]];
                val=(lst[[case]]$rep)[[paste0("pop.grw.prGr_xzz.",nm)]];
                dimnames(val)<-list(z =as.character(lst[[case]]$rep$mod.zBs),
                                    zp=as.character(lst[[case]]$rep$mod.zBs));
                dfrp<-reshape2::melt(val,value.name='val')
                dfrp<-cbind(case=case,
                            pc=pc,y=pc,x=rw$x,m='immature',s='all',dfrp);
                dfr<-rbind(dfr,dfrp);
            }
        }
        dfr<-getMDFR.CanonicalFormat(dfr);
        dfr$type<-"population";
        return(dfr);
    }
    
    #----------------------------------
    #recruitment size distribution
    #----------------------------------
    if (type[1]=="R_cz"){
        dfr<-NULL;
        for (case in cases){
            pc<-paste0(styr[[case]],"-",endyr[[case]]);
            dfrp<-data.frame(case=case,pc=pc,y=pc,x='all',m='immature',s='new shell',
                             z=lst[[case]]$rep$mod.zBs,
                             val=(lst[[case]]$rep)[["pop.prR_z"]]);
            dfr<-rbind(dfr,dfrp);
        }
        dfr<-getMDFR.CanonicalFormat(dfr);
        dfr$type<-"population";
        return(dfr);
    }

    cat("In getMDFR.PopProcesses()\n");
    cat("Type '",type[1],"' not recognized.\n",sep='');    
    cat("Returning NULL.\n");
    return(NULL);
}

