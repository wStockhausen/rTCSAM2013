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
#'}
#' 
#'@export
#' 
getResLst<-function(inp.dir=NULL,
                    model='tcsam2013alta',
                    rep="TCSAM2013.OLDSTYLE.final.R"){
    if (is.null(inp.dir)){
        inp.dir<-wtsUtilities::selectFile(ext='par',caption="Select par file");
        if (!file.exists(inp.dir)) return(NULL);
        inp.dir<-dirname(inp.dir);#determine folder
    }
    
    if (!dir.exists(inp.dir)) return(NULL);
    
    prs<-getPrs(inp.dir=inp.dir,type='all');
    rep<-getRep(inp=file.path(inp.dir,rep));
    std<-getStd(inp=file.path(inp.dir,paste0(model,".std")));
    ofc<-getOFC(inp.dir=inp.dir)
    
    resLst<-list(rep=rep,prs=prs,std=std,ofc=ofc);
    class(resLst)<-c('tcsam2013.resLst',class(resLst));
    
    return(resLst);
}
