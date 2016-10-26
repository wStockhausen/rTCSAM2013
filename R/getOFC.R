#'
#'@title Create a tcsam2013.ofc object from a TCSAM2013.final_likelihood_components.csv file
#'
#'@description Function to create a tcsam2013.ofc object from a TCSAM2013.final_likelihood_components.csv file.
#'
#'@param inp.dir = folder from which to read TCSAM2013.final_likelihood_components.csv file
#'
#'@return dataframe object corresponding to the TCSAM2013.final_likelihood_components.csv file, or NULL if file does not exist. 
#'Returned object has class 'tcsam2013.ofc'.
#'
#'@details Uses \code{wtsUtilities::selectFile} to open a file dialog if inp.dir is NULL.
#' 
#'@export
#' 
getOFC<-function(inp.dir="./"){
    options(stringsAsFactors=FALSE);
    if (is.null(inp.dir)){
        csv<-wtsUtilities::selectFile(ext="csv",caption="Select TCSAM2013.final_likelihood_components.csv file");
    } else {
        csv<-file.path(inp.dir,"TCSAM2013.final_likelihood_components.csv");
    }
    
    if ((is.null(csv))||!file.exists(csv)) return(NULL);
    
    dfr<-read.csv(csv,header=TRUE,skip=4);
    class(dfr)<-c('tcsam2013.ofc',class(dfr));
    
    return(invisible(dfr));
}
