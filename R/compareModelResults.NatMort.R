#'
#'@title Function to plot natural mortality rates by year using ggplot2
#'
#'@description This function plots natural mortality estimates by year
#'   and sex for immatures, new shell matures and old shell matures.
#'   
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param showPlot - flag (T/F) to show plot
#'
#'@return ggplot2 object
#'
#'@details None.
#'
#'@import ggplot2
#'
#'@export
#'
compareModelResults.NatMort<-function(obj,
                                      showPlot=TRUE,
                                      pdf=NULL){
    lst<-convertToListOfResults(obj);
    cases<-names(lst);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }

    dfr<-getMDFR.NaturalMortality(lst);
    p <- ggplot(dfr,aes_string(x='y',y='val',colour='case'));
    p <- p + geom_line();
    p <- p + geom_point();
    if (any(!is.na(dfr$lci))) p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'));
    p <- p + labs(x='year',y="natural mortality");
    p <- p + ggtitle("Natural Mortality");
    p <- p + facet_grid(m+s~x);
    p <- p + ylim(c(0,NA));
    if (showPlot) print(p);
    
    return(p)
}