#'
#'@title Create a TCSAM2013 rep object from a TCSAM_OLDSTYLE R file.
#'
#'@description Function to create a TCSAM2013 rep object from a TCSAM_OLDSTYLE R file.
#'
#'@param inp = filename of TCSAM_OLDSTYLE R file
#'
#'@return list object corresponding to the TCSAM_OLDSTYLE R file, or NULL if file does not exist
#'
#'@details Uses \code{wtsUtilities::selectFile} to open a file dialog if in.par is NULL.
#'Uses \code{PBSmodelling::readList} to read file.
#' 
#'@export
#' 
getRep<-function(inp=NULL){
    if (is.null(inp)){
        inp<-wtsUtilities::selectFile(ext="R",caption="Select Jack's R output file");
    }
    obj.rep<-NULL;
    if ((!is.null(inp))&&file.exists(inp)){
        obj.rep <- PBSmodelling::readList(inp);
    }
    return(invisible(obj.rep))
}