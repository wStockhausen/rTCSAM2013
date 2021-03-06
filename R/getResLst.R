#'
#'@title Create a tcsam2013.resLst object from a model run
#'
#'@description Function to create a tcsam2013.resLst object from a model run.
#'
#'@param inp.dir - folder with model output
#'@param rep - filename to read for rep object
#'@param model - name of model executable
#'
#'@return a tcsam2013.resLst object.
#'
#'@details Uses \code{wtsUtilities::selectFile} to open a file dialog to select model directory 
#'if inp.dir is NULL. A tcsam2013.resLst object is a list with elements
#'\itemize{
#'  \item{rep - a tcsam2013.rep object}
#'  \item{prs - a tcsam2013.prs object}
#'  \item{std - a tcsam2013.std object, or NULL}
#'  \item{ofc - a tcsam2013.ofc object}
#'}
#' 
#'@export
#' 
getResLst<-function(inp.dir=NULL,
                    rep="TCSAM2013.OLDSTYLE.final.R",
                    model='tcsam2013alta',
                    prsType=c('all','active')){
    if (is.null(inp.dir)){
        inp.dir<-wtsUtilities::selectFile(ext='par',caption="Select par file");
        if (!file.exists(inp.dir)) return(NULL);
        inp.dir<-dirname(inp.dir);#determine folder
    }
    
    cat("rTCSAM2013::getResLst(...): getting model run from \n'",inp.dir,"'\n",sep='');
    if (!dir.exists(inp.dir)) {
        cat("Warning from getResLst(...).\n");
        cat("--The following folder does not exist:\n\t'",inp.dir,"'\n",sep='');
        cat("--Returning NULL.\n")
        return(NULL);
    }
    
    prs<-getPrs(inp.dir=inp.dir,type=prsType[1]);
    rep<-getRep(inp=file.path(inp.dir,rep));
    std<-getStd(inp=file.path(inp.dir,paste0(model,".std")));
    ofc<-getOFC(inp.dir=inp.dir)
    
    resLst<-list(rep=rep,prs=prs,std=std,ofc=ofc);
    class(resLst)<-c('tcsam2013.resLst',class(resLst));
    
    cat("rTCSAM2013::getResLst(...): Done!\n");
    return(resLst);
}
