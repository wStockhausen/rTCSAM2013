#'
#'@title Compare growth matrices among several model runs
#'
#'@description Function to compare growth matrices among several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param showPlot - flag to print plot to current device
#'@param pdf - name for output pdf file
#'
#'@details If multiple models are compared, then a set of sex-specific faceted line plots 
#'are created. If a single model is plotted, then sex-specific bubble plots are created.
#'
#'@return list of ggplot objects, by sex
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
    
    obj<-convertToListOfResults(obj);
    cases<-names(obj);
    
    #----------------------------------
    # plot growth transition matrices
    #----------------------------------
    dfr<-getMDFR.PopProcesses(obj,type="T_cxzz");
    plots<-list();
    if (length(cases)>1){
        for (x in c('male','female')){
            dfrp<-dfr[dfr$x==x,];
            p <- ggplot(dfrp,aes_string(x='zp',y='val',colour='case'));
            p <- p + ggtitle(paste0(x,"s"));
            p <- p + geom_line();
            p <- p + labs(y="probability", x="post-molt size (mm CW)");
            if (showPlot) print(p);
            plots[[x]]<-p;
        }
    } else {
        for (x in c('male','female')){
            dfrp<-dfr[dfr$x==x,];
            p <- ggplot(dfrp,aes_string(y='zp',x='z',size='val',fill='val'));
            p <- p + geom_abline(slope=1,colour='black',linetype=2)
            p <- p + geom_point(alpha=0.8,shape=21) + scale_size_area(max_size=10)
            p <- p + scale_fill_gradientn(colours=wtsUtilities::createColorPalette('jet',100,alpha=0.7))
            p <- p + ggtitle(paste0(x,"s"));
            p <- p + labs(x="pre-molt size (mm CW)", y="post-molt size (mm CW)");
            p <- p + guides(size=guide_legend("probability",order=1));
            p <- p + guides(fill=guide_colorbar("probability",alpha=1.0,order=2));
            if (showPlot) print(p);
            plots[[x]]<-p;
        }
    }

    return(plots);
}
