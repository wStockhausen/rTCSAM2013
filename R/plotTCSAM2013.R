#'
#' @title Plot TCSAM2013 model results.
#' 
#' @description Function to plot model results from TCSAM2013.
#' 
#' @param obj.rep - report file list object or filename for R-style report
#' @param obj.std - dataframe object with parameter std info or filename for std file
#' @param obj.prs - dataframe object w/ parameters info or csv file with parameters info
#' @param base.dir - path to base folder
#' @param mdl   - model name (optional if R report file is read)
#' @param styr  - model start year
#' @param endyr - assessment year
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
#' @importFrom wtsUtilities formatZeros
#' @importFrom wtsUtilities parseNum
#' @importFrom wtsUtilities selectFile
#' @importFrom wtsPlots plotErrorBars.V
#'
#'@export
#' 
#----------------------------------
# Set model variables for plots
plotTCSAM2013<-function(obj.rep=NULL,
                        obj.std=NULL,
                        obj.prs=NULL,
                        base.dir='./',
                        mdl='tcsam2013alta',#executable model name
                        endyr=NULL,    #assessment year
                        styr=NULL,     #model start year
                        obsyr=NULL,    #first year of survey observations
                        pltyr=1965,    #first year for plots
                        F35=0.73,      #F35 value
                        B35=33.54){    #B35 value

    #----------------------------------
    # Load files, if necessary
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
        if (!is.null(in.std)) {
            if (file.exists(in.std)) {
                cat("Reading file",in.std,'\n');
                obj.std = read.table(in.std,as.is=T,header=F,skip=1);
            } else {
                cat('File',in.std,'does not exist.\n');
            }
        }
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
        }    
    }
    
    #actually plot the results
    plotTCSAM2013I(obj.rep,
                   obj.std,
                   obj.prs,
                   base.dir=base.dir,
                   mdl=mdl,        #executable model name
                   endyr=endyr,    #assessment year
                   styr=styr,      #model start year
                   obsyr=obsyr,    #first year of survey observations
                   pltyr=pltyr,    #first year for plots
                   F35=F35,        #F35 value
                   B35=B35);       #B35 value
    
    #return the objects
    return(invisible(list(rep=obj.rep,std=obj.std,prs=obj.prs)));
}