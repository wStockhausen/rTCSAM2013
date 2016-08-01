#'
#'@title Print a ggplot2 object, or list of ggplot2 objects
#'
#'@description Function to print a ggplot2 object or a list of ggplot2 objects.
#'
#'@param plots - a ggplot2 object or a list of ggplot2 objects
#'@param figno - figure number (or NULL)
#'@param cap - plot caption
#'
#'@return figure number for next plot, if figno was not NULL.
#'
#'@details Captions for each figure are taken from the name of the list element associated with the plot.
#'\code{figno} is substituted for '&&fno' in the caption and incremented for the next plot.
#'
#'@import ggplot2
#'
#'@export
#
printGGList<-function(plots,figno=NULL,cap=NULL){
    if (inherits(plots,"ggplot")){
        #plots is a ggplot object
        print(plots);
        if (!is.null(figno)) {
            cap<-gsub("&&fno",figno,cap);
            figno<-figno+1;
        }
        cat(cap,"\n"); ##do something to put caption with figure!!
    } else {
        #plots is a list
        for (nm in names(plots)){
            plot<-plots[[nm]];
            figno<-printGGList(plot,figno=figno,cap=nm);
        }
    }
    return(figno);
}