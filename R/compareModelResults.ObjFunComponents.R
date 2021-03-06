#'
#'@title Compare changes in objective function components from a base model for several models
#'
#'@description Function to compare changes in objective function components 
#'from a base model for several models.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param base - name of case to use as base model, or NULL to use first
#'@param y - 'diff' or 'objFun'
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - filename for pdf output
#'
#'@details If 'base' is not given, objective function component comparisons are relative to the first case. NEGATIVE
#'differences indicate a better fit (smaller value for the component in the alternative model). Selecting 
#'\code{y="diff"} plots relative objective function values--i.e., relative to the base model. Selecting \code{y="objFun"}
#'plots absolute values.
#'
#'@return list of ggplot2 objects
#'
#'@importFrom wtsUtilities printGGList
#'@import ggplot2
#'
#'@export
#'
compareModelResults.ObjFunComponents<-function(obj,
                                               base=NULL,
                                               y=c("diff","objFun"),
                                               showPlot=FALSE,
                                               pdf=NULL){
    
    #----------------------------------
    # 
    #----------------------------------
    obj<-convertToListOfResults(obj);
    cases<-names(obj);
    
    dfr<-getMDFR.ObjFunComponents(obj,base=base);
    dfr$case<-factor(dfr$case,levels=cases);
    
    if (length(cases)==1) y<-"objFun";

    if (y[1]=='diff') {
        #remove base case
        if (is.null(base)) base<-cases[1];
        ylab = paste0("diff. relative to '",base,"'");
        dfr<-dfr[dfr$case!=base,];
    }
    if (y[1]=='objFun') {
        ylab = "objective function";
    }
    
    cats <- as.character(unique(dfr$category));
    plots<-list();
    figno<-1;
    for (cat in cats){
        dfrp<-dfr[dfr$category==cat,];
        p <- ggplot(data=dfrp,mapping=aes_string(x="description",y=y[1],fill='case'));
        p <- p + geom_bar(stat='identity',position='dodge',group='case');
        p <- p + labs(x="component",y=ylab);
        p <- p + ggtitle(cat);
        p <- p + theme(axis.text.x=element_text(angle=30,hjust=1),plot.margin=margin(10,10,10,50));
        cap<-paste0("  \n  \nFigure &&fno. Objective function components related to ",cat,".  \n  \n");
        if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }

    return(invisible(plots));
}
