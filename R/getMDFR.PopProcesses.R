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
        dfr<-getMDFR.Pop.NaturalMortality(obj,verbose);
        return(dfr);
    }
    
    #----------------------------------
    #pr(molt-to-maturity|z)
    #----------------------------------
    if (type[1]=="prM2M_cxz"){
        dfr<-getMDFR.Pop.PrM2M(obj,verbose);
        return(dfr);
    }
    #----------------------------------
    #mean growth increments
    #----------------------------------
    if (type[1]=="mnZAM_cxz"){
        dfr<-getMDFR.Pop.MeanGrowth(obj,verbose);
        return(dfr);
    }
    
    #----------------------------------
    #growth transition matrices
    #----------------------------------
    if (type[1]=="T_cxzz"){
        dfr<-getMDFR.Pop.GrowthMatrices(obj,verbose);
        return(dfr);
    }
    
    #----------------------------------
    #recruitment size distribution
    #----------------------------------
    if (type[1]=="R_cz"){
        dfr<-getMDFR.Pop.RecSizeDistribution(obj,verbose);
        return(dfr);
    }

    cat("In getMDFR.PopProcesses()\n");
    cat("Type '",type[1],"' not recognized.\n",sep='');    
    cat("Returning NULL.\n");
    return(NULL);
}

