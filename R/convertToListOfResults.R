#'
#'@title Convert object to a list of tcsam2013.resLst objects
#'
#'@description Function to convert object to a list of tcsam2013.resLst objects.
#'
#'@param obj - object to convert (see details)
#'@param rep - a tcsam2013.rep object to convert
#'@param prs - a tcsam2013.prs object to convert
#'@param std - a tcsam2013.std object to convert
#'@param ofc - a tcsam2013.ofc object to convert
#'
#'@details Object can be one of the following:
#'\itemize{
#'  \item{a list of tcsam2013.resLst objects}
#'  \item{a single tcsam2013.resLst object}
#'  \item{a list of tcsam2013.rep objects}
#'  \item{a single tcsam2013.rep object}
#'}
#'
#'@return List of tcsam2013.resLst objects
#'
#'@export
#'
convertToListOfResults<-function(obj=NULL,
                                 rep=NULL,
                                 prs=NULL,
                                 std=NULL,
                                 ofc=NULL){
    if (is.null(obj)){
        lst<-list(rep=rep,prs=prs,std=std,ofc=ofc);
        class(lst)<-c('tcsam2013.resLst',class(lst));
        lst<-convertToListOfResults(lst);
    } else {
        #convert resLst to list of tcsam2013.resLst objects
        if (inherits(obj,"tcsam2013.rep")){
            #obj is a single tcsam2013.rep object
            #convert to list with a tcsam2013.resLst object
            lst<-list(tcsam2013=list(rep=obj,prs=NULL,std=NULL,ofc=NULL));
            class(lst[["tcsam2013"]])<-c('tcsam2013.resLst',class(lst[["tcsam2013"]]));
            cases<-names(lst);
        } else if (inherits(obj,"tcsam2013.ofc")){
            #obj is a single tcsam2013.ofc object
            #convert to list with a tcsam2013.resLst object
            lst<-list(tcsam2013=list(rep=NULL,prs=NULL,std=NULL,ofc=ofc));
            class(lst[["tcsam2013"]])<-c('tcsam2013.resLst',class(lst[["tcsam2013"]]));
            cases<-names(lst);
        } else if (inherits(obj,"tcsam2013.resLst")){
            #obj is a single tcsam2013.resLst object
            #convert to list with a tcsam2013.resLst object
            lst<-list(tcsam2013=obj);
            cases<-names(lst);
        } else if (class(obj)[1]=='list'){
            if (inherits(obj[[1]],"tcsam2013.rep")){
                #obj is a list of tcsam2013.rep objects
                #convert to a list of tcsam2013.resLst objects
                cases<-names(obj);
                lst<-list();
                for (case in cases) {
                    lst[[case]]<-list(rep=obj[[case]],prs=NULL,std=NULL,ofc=NULL);
                    class(lst[[case]])<-c('tcsam2013.resLst',class(lst[[case]]));
                }
            } else if (inherits(obj[[1]],"tcsam2013.ofc")){
                #obj is a list of tcsam2013.rep objects
                #convert to a list of tcsam2013.resLst objects
                cases<-names(obj);
                lst<-list();
                for (case in cases) {
                    lst[[case]]<-list(rep=NULL,prs=NULL,std=NULL,ofc=obj[[case]]);
                    class(lst[[case]])<-c('tcsam2013.resLst',class(lst[[case]]));
                }
            } else if (inherits(obj[[1]],"tcsam2013.resLst")){
                #assuming obj is list of tcsam2013.resLst objects
                #good to go
                lst<-obj;
            }  else {
                cat("---------------------------------------------------------\n");
                cat("--rTCSAM2013::convertToListOfResults:\n");
                cat("--class(obj[[1]]) is invalid: '",class(obj[[1]])[1],"'.\n",sep='');
                cat("--Returning NULL.\n");
                cat("---------------------------------------------------------\n");
                return(NULL);
            }
        } else {
            cat("In convertToListOfResults.\n");
            cat("class(obj) is invalid: '",class(obj),"'.\n",sep='');
            cat("Returning NULL.\n");
            return(NULL);
        }
    }
    
    return(lst);
}
