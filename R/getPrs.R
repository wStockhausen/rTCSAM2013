#'
#'@title Get a TCSAM2013 prs object by reading initial and final parameters csv files
#'
#'@description Function to get a TCSAM2013 prs object by reading initial and final parameters csv file.
#'
#'@param type - 'all' or 'active'
#'@param inp.dir - folder for files (or NULL to select a folder)
#'
#'@return A prs model object (a dataframe). The returned object will be a dataframe of class 'tcsam2013.prs'.
#'
#'@details To create the prs object, this function reads 2 csv-type parameter value files,
#'one associated with the initial parameter values and one associated with the final values.
#'The user can select to return either all the parameters, or only the active parameters (ones
#'with phase > 0). The returned object will be a dataframe of class 'tcsam2013.prs'.
#'
#'@export
#'
getPrs<-function(type='all',inp.dir='.'){
    if (!any(type==c('all','active'))) {
        cat("type = '",type,"' undefined for function rTCSAM2013::getPrs(...).\n",sep='');
        cat("Returning NULL.\n\n");
        return(NULL);
    }
    if (is.null(inp.dir)){
        inp<-wtsUtilities::selectFile(ext='csv',caption="Select PRS file");
        inp.dir<-dirname(inp);
    }
    ##get initial parameter values
    iCSV<-file.path(inp.dir,paste0("TCSAM2013.params.",type,".init.csv"));
    iPRS<-read.csv(iCSV,stringsAsFactors=FALSE);
    ##get final parameter values
    fCSV<-file.path(inp.dir,paste0("TCSAM2013.params.",type,".final.csv"));
    fPRS<-read.csv(fCSV,stringsAsFactors=FALSE);
    
    ##combine initial and final values
    prsObj<-cbind(fPRS,init=iPRS$value);
    class(prsObj)<-c('tcsam2013.prs',class(prsObj));#set class attribute to 'tcsam2013.prs' for identification
    
    return(prsObj);
}
