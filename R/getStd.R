#'
#'@title Create a TCSAM2013 std object from a std file.
#'
#'@description Function to create a TCSAM2013 std object from a std file.
#'
#'@param in.std = filename of std file
#'
#'@return list object corresponding to the std file, or NULL if file does not exist
#'
#' @importFrom wtsUtilities selectFile
#' 
#' @export
#' 
getStd<-function(in.std=NULL){
    if (is.null(in.std)){
        in.std<-wtsUtilities::selectFile(ext='std',caption="Select std file");
    }
    obj.std<-NULL;
    if ((!is.null(in.std))&&file.exists(in.std)){
        obj.std = read.table(in.std,as.is=T,header=F,skip=1);
    }
    return(invisible(obj.std))
}