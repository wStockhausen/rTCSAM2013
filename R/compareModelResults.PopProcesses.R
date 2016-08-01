#'
#'@title Compare predicted population processes among several model runs
#'
#'@description Function to compare predicted population processes among 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - name for output pdf file
#'
#'@details None
#'
#'@return (nested) list of ggplot2 objects, returned invisibly.
#'
#'@import ggplot2
#'
#'@export
#'
compareModelResults.PopProcesses<-function(obj,
                                           showPlot=FALSE,
                                           pdf=NULL){

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }
    plots<-list();#output list
    
    #-------------------------------------------#
    #plot natural mortality
    #-------------------------------------------#
    p<-compareModelResults.NatMort(obj,showPlot=FALSE);
    if (showPlot) print(p);
    plots[["NatMort"]]<-p;

    #-------------------------------------------#
    #plot prM2M
    #-------------------------------------------#
    p<-compareModelResults.PrM2M(obj,showPlot=FALSE);
    if (showPlot) print(p);
    plots[["prM2M"]]<-p;

    #-------------------------------------------#
    #plot mean growth
    #-------------------------------------------#
    p<-compareModelResults.MeanGrowth(obj,showPlot=FALSE);
    if (showPlot) print(p);
    plots[["mnGrw"]]<-p;

    #-------------------------------------------#
    #plot growth transition matrices
    #-------------------------------------------#
    p<-compareModelResults.GrowthMatrices(obj);
    if (showPlot) print(p);
    plots$GrTMs<-p;
    
    #-------------------------------------------#
    #plot recruitment size distribution
    #-------------------------------------------#
    dfr<-getMDFR.PopProcesses(obj,type="R_cz");
    p <- ggplot(dfr,aes_string(x='z',y='val',colour='case'));
    p <- p + geom_line();
    p <- p + geom_point();
    p <- p + ylim(c(0,NA));
    p <- p + labs(x="size (mm CW)",y="recruitment size distribution");
    if (showPlot) print(p);
    plots$R_z<-p;
    
    return(invisible(plots));
}
