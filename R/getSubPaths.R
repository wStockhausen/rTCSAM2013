#'
#'@title Extract subpaths from a vector of file paths
#'
#'@description Function to extract subpaths from a vector of file paths.
#'
#'@param paths - vector of paths to extract from
#'@param last - number of final folder levels to retain
#'@param first - number of initial folder levels to retain
#'
#'@details If first and last are both given, only the "last" subpaths will be exported.
#'
#'@return character vector of subpaths
#'
#'@export
#'
getSubPaths<-function(paths,last=NULL,first=NULL){
    subpaths<-paths;
    fsep<-.Platform$file.sep;
    fldrs<-strsplit(paths,fsep,fixed=TRUE);
    for (i in 1:length(paths)){
        nf<-length(fldrs[[i]])
        if (!is.null(last)){
            lastp<-max(1,min(nf,last));
            lastp<-ifelse(fldrs[[i]][nf-lastp+1]!="",lastp,lastp-1);
            subpaths[i]<-fldrs[[i]][nf-lastp+1];
            if (lastp>1){
                for (j in seq.int(nf-lastp+2,nf,1)) subpaths[i]<-paste0(subpaths[i],fsep,fldrs[[i]][j]);
            }
        } else if (!is.null(first)){
            firstp<-ifelse(fldrs[[i]][1]!="",first,first+1);
            firstp<-max(1,min(nf,firstp));
            subpaths[i]<-fldrs[[i]][1];
            if (firstp>1){
                for (j in 2:firstp) subpaths[i]<-paste0(subpaths[i],fsep,fldrs[[i]][j]);
            }
        }
    }
    return(subpaths);
}
