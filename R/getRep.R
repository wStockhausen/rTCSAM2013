#'
#'@title Create a TCSAM2013 rep object from a rep file.
#'
#'@description Function to create a TCSAM2013 rep object from a rep file.
#'
#'@param in.rep = filename of rep file
#'
#'@return list object corresponding to the rep file
#'
#' @import PBSmodelling
#' @importFrom wtsUtilities selectFile
#' 
#' @export
#' 
getRep<-function(in.rep=NULL){
    if (is.null(in.rep)){
        in.rep<-wtsUtilities::selectFile(ext="R",caption="Select Jack's R output file");
    }
    obj.rep<-NULLL;
    if (!is.null(in.rep)){
        obj.rep <- readList(in.rep);
    }
    return(invisible(obj.rep))
}