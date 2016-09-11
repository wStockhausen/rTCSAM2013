#'
#'@title Compare mean post-molt growth among several model runs
#'
#'@description Function to compare mean post-molt growth among several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param showPlot - flag to print plot to current device
#'@param pdf - name for output pdf file
#'
#'@details None.
#'
#'@return ggplot object
#'
#'@import ggplot2
#'
#'@export
#'
compareModelResults.MeanGrowth<-function(obj,
                                         showPlot=FALSE,
                                         pdf=NULL){

    lst<-convertToListOfResults(obj);
    cases<-names(lst);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }
    
    #-------------------------------------------#
    #plot mean growth
    #-------------------------------------------#
    dfr<-getMDFR.meanGrowth(lst);
    dfr$case<-factor(dfr$case,levels=cases);
    p <- ggplot(dfr,aes_string(x='z',y='val',colour='case'));
    p <- p + geom_line();
    p <- p + geom_point();
    if (any(!is.na(dfr$lci))) p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'));
    p <- p + geom_abline(slope=1,linetype=2);
    p <- p + labs(x='pre-molt size (mm CW)',y="post-molt size (mm CW)");
    p <- p + ggtitle("Mean Growth");
    p <- p + facet_grid(x~.);
    if (showPlot) print(p);

    return(p);
}