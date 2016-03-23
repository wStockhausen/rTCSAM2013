#'
#'@title Create a TCSAM2013 prs object from a params csv file.
#'
#'@description Function to create a TCSAM2013 prs object from a params csv file.
#'
#'@param inp - filename for params csv file (or NULL)
#'
#'@return list object corresponding to the params file, or NULL if file does not exist
#'
#'@details Uses \code{wtsUtilities::selectFile()}.
#' 
#'@export
#' 
getPRS<-function(inp=NULL){
    obj.prs<-NULL;
    if (is.null(inp)){
        inp<-wtsUtilities::selectFile(ext='csv',caption="Select parameters info csv file");
    }
    obj.prs<-NULL;
    if ((!is.null(inp))&&(file.exists(inp))){
        obj.prs<-read.csv(inp,stringsAsFactors=FALSE);
    }    
    return(invisible(obj.prs))
}