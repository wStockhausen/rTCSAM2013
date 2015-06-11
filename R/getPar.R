#'
#'@title Create a TCSAM2013 par object from a par file.
#'
#'@description Function to create a TCSAM2013 par object from a par file.
#'
#'@param in.par = filename of par file
#'
#'@return list object corresponding to the par file
#'
#' @importFrom wtsUtilities selectFile
#' 
#' @export
#' 
getPar<-function(in.par=NULL){
    if (is.null(in.par)){
        in.par<-wtsUtilities::selectFile(ext='par',caption="Select par file");
    }
    r1<-readLines(con=in.par);
    
    #parse first line
    str <- gsub('[^[:digit:][:blank:].]','',r1[1],perl=TRUE);#remove all characters except numbers and blanks
    str <- str<-strsplit(str,'[[:blank:]]');#split str based on blanks
    str <- str[[1]][str[[1]]!=''];#remove empty array elements
    objfun <- as.numeric(str);
    names(objfun)<-c("number of parameters",'objective function','max gradient');
    dfr<-data.frame(name=names(objfun),value=objfun,stringsAsFactors=FALSE)
    
    #parse remaining lines
    nr<-length(r1);
    for (r in 1:((nr-1)/2)){
        nam  <- gsub('[^[:alnum:]_]','',r1[2*r],perl=TRUE);#get parameter name
        str  <- gsub('[^[:digit:][:blank:].]','',r1[2*r+1],perl=TRUE);#remove all characters except numbers and blanks
        str  <- str<-strsplit(str,'[[:blank:]]');#split str based on blanks
        str  <- str[[1]][str[[1]]!=''];#remove empty array elements
        valu <- as.numeric(str);
        nams <- nam;
        if (length(valu)>1) nams <- paste(nam,'[',1:length(valu),']',sep='');
        dfr <- rbind(dfr,data.frame(name=nams,value=valu,stringsAsFactors=FALSE));
    }
    
    return(dfr)
}