#'
#'@title Create a TCSAM2013 wts object from a TCSAM_WTS R file
#'
#'@description Function to create a TCSAM2013 wts object from a TCSAM_WTS R file.
#'
#'@param inp - filename of TCSAM_WTS R file (or NULL)
#'
#'@return list object corresponding to the file, or NULL if file does not exist
#'
#'@details Uses \code{wtsUtilities::selectFile()}.
#' 
#'@export
#' 
getWTS<-function(inp=NULL){
    res<-NULL;
    if (is.null(inp)){
        inp<-wtsUtilities::selectFile(ext='R',caption="Select TCSAM_WTS R file");
    }
    if ((!is.null(inp))&&(file.exists(inp))){
        source(inp,local=TRUE);
    }    
    return(invisible(res));
}