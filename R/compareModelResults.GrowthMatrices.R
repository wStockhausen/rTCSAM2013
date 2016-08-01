#'
#'@title Compare growth matrices among several model runs
#'
#'@description Function to compare growth matrices among several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param showPlot - flag to print plot to current device
#'@param pdf - name for output pdf file
#'
#'@details None.
#'
#'@return list of ggplot objects
#'
#'@import ggplot2
#'
#'@export
#'
compareModelResults.GrowthMatrices<-function(obj,
                                             showPlot=FALSE,
                                             pdf=NULL){

    #create pdf, if necessary
    if (!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }
    
    #----------------------------------
    # plot growth transition matrices
    #----------------------------------
    dfr<-getMDFR.PopProcesses(obj,type="T_cxzz");
    plots<-list();
    for (x in c('male','female')){
        dfrp<-dfr[dfr$x==x,];
        p <- ggplot(dfrp,aes_string(x='zp',y='val',colour='case'));
        p <- p + ggtitle(paste0(x,"s"));
        p <- p + geom_line();
        p <- p + facet_wrap(~z,ncol=4);
        p <- p + labs(y="probability", x="Post-molt Size (mm CW)");
        if (showPlot) print(p);
        plots[[x]]<-p;
    }

    return(plots);
}