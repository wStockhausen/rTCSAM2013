#'
#'@title Get changes to objective function components relative to a base model for several models
#'
#'@description Function to get changes to objective function components 
#'relative to a base model for several models.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param base - name of case to use as base model, or NULL to use first
#'
#'@details Objective function component comparisons are relative to the base (or first) case. NEGATIVE
#'differences indicate a better fit (smaller value for the component in the alternative model).
#'
#'@return dataframe
#'
#'@export
#'
getMDFR.ObjFunComponents<-function(obj,base=NULL){
    
    lst <- convertToListOfResults(obj);
    cases<-names(lst);
    
    if (is.null(base)){
        base<-lst[[1]]$ofc;
    }
    
    dfr<-NULL;
    for (case in cases){
        dfrp<-lst[[case]]$ofc;
        dfrp$case<-case;
        dfrp$diff<-dfrp$objFun - base$objFun;
        dfr<-rbind(dfr,dfrp);
    }
    dfr<-dfr[,c("case","idx","weight","likelihood","objFun","diff","category","description")];
    #trim whitespace
    dfr$category<-trimws(dfr$category,which='both');
    dfr$description<-trimws(dfr$description,which='both');


    return(invisible(dfr));
}
