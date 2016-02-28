#'
#'@title Create a TCSAM2013 rep object from a rep file.
#'
#'@description Function to create a TCSAM2013 rep object from a rep file.
#'
#'@param in.rep = filename of rep file
#'
#'@return list object corresponding to the rep file, or NULL if file does not exist
#'
#'@details Uses \code{wtsUtilities::selectFile} to open a file dialog if in.par is NULL.
#'
#'@import PBSmodelling
#' 
#'@export
#' 
getRep<-function(in.rep=NULL){
    if (is.null(in.rep)){
        in.rep<-wtsUtilities::selectFile(ext="R",caption="Select Jack's R output file");
    }
    obj.rep<-NULL;
    if ((!is.null(in.rep))&&file.exists(in.rep)){
        obj.rep <- readList(in.rep);
    }
    return(invisible(obj.rep))
}