#'
#'@title Function to plot natural mortality rates by year using ggplot2
#'
#'@description This function plots natural mortality estimates by year
#'   and sex for immatures, new shell matures and old shell matures.
#'   
#'@param resLst - list of results objects
#'@param ci - confidence interval for error bars
#'@param min.yr - min year to plot
#'@param max.yr - max year to plot
#'
#'@return list with dataframe and list of plots as elements
#'
#'@import ggplot2
#'
#'@export
#'
compareModelResults.NatMort<-function(resLst,
                                      ci=0.8,
                                      min.yr=NULL,
                                      max.yr=NULL,
                                      showPlot=TRUE){
    dfr<-NULL;
    cases<-names(resLst);
    for (case in cases){
        dfrp<-getNatMort(resLst[[case]]);
        dfrp$case<-case;
        dfr<-rbind(dfr,dfrp);
    }
    
    cis<-calcCIs(dfr$val,)
    
    plots<-list();
    sexes<-c('male','female');  
    for (x in sexes){  
        dfrp<-dfr[dfr$sex %in% x,];
        p <- ggplot(dfrp,aes_string(x='year',y='val',color='case',shape='case'));
        p <- p + geom_point(size=4,alpha=0.8,position='dodge');
        p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position='dodge');
        p <- p + geom_line(position='dodge');
        p <- p + coord_cartesian(xlim=xlims,ylim=ylims)
        p <- p + labs(x='year',y='Natural Mortality');
        p <- p + ggtitle(title);
        p <- p + facet_grid(~maturity+shell);
        if (showPlot) print(p);
        plots[[x]]<-p;
    }
    return(invisible(list(dfr=dfr,plots=plots)))
}