#'
#'@title Create a TCSAM2013 prs object from an active params file.
#'
#'@description Function to create a TCSAM2013 prs object from an active params file.
#'
#'@param in.prs = filename of active params file
#'
#'@return list object corresponding to the active params file, or NULL if file does not exist
#'
#' @importFrom wtsUtilities selectFile
#' 
#' @export
#' 
getActiveParams<-function(in.prs=NULL){
    if (is.null(in.prs)){
        in.prs<-wtsUtilities::selectFile(ext='csv',caption="Select active parameters info csv file");
    }
    obj.prs<-NULL;
    if ((!is.null(in.prs))&&(file.exists(in.prs))){
        obj.prs<-read.csv(in.prs,stringsAsFactors=FALSE);
    }    
    return(invisible(obj.prs))
}