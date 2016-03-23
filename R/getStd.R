#'
#'@title Create a dataframe (an std object) from an ADMB std file.
#'
#'@description Function to create a dataframe (an std object) from an ADMB std file.
#'
#'@param inp - name of the ADMB std file
#'
#'@return list object corresponding to the std file, or NULL if file does not exist
#'
#'@details Uses \code{wtsUtilities::selectFile} to open a file dialog if in.par is NULL.
#'
#'@export
#' 
getStd<-function(inp=NULL){
    if (is.null(inp)){
        inp<-wtsUtilities::selectFile(ext='std',caption="Select std file");
    }
    obj.std<-NULL;
    if ((!is.null(inp))&&file.exists(inp)){
        obj.std = read.table(inp,as.is=T,header=F,skip=1);
    }
    return(invisible(obj.std))
}