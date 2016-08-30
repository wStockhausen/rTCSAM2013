#'
#'@title Plot z-scores from a set of model runs
#'
#'@description Function to plot z-scores from a set of model runs.
#'
#'@param dfr - dataframe
#'@param x - column name for x axis (default = 'year')
#'@param y - column name for y axis (default='z-score')
#'@param color - column name for color levels (or NULL)
#'@param shape - column name for shape levels (or NULL)
#'@param position - indicates ggplot2 position_ to use ('dodge','jitter','identity',)
#'@param facets - string giving faceting formula for facet_grid
#'@param facet.scales - ggplot2 scales option for facet_grid
#'@param xlab - label for x axis
#'@param ylab - label for y axis
#'@param title - title for plot
#'@param xlims - limits for x axis
#'@param ylims - limits for y axis
#'@param showPlot - flag (T/F) to show plot immediately
#'
#'@return ggplot2 object
#'
#'@details None.
#'
#'@import ggplot2
#'
#'@export
#'
plotZScores<-function(dfr,
                      x='y',
                      y='zscore',
                      color=NULL,
                      shape=NULL,
                      position='identity',
                      dodge=0.2,
                      facets=NULL,
                      facet.scales='fixed',
                      xlab='year',
                      ylab=NULL,
                      title=NULL,
                      legend=NULL,
                      xlims=NULL,
                      ylims=NULL,
                      showPlot=TRUE){
    p <- ggplot(dfr,aes_string(x=x,y=y));
    p <- p + geom_hline(yintercept=0.0,color='black',size=1);
    if (position=='dodge'){
        p <- p + geom_point(aes_string(shape=shape,color=color),size=4,alpha=0.5,
                            position=position_dodge(width=dodge));
    } else {
        p <- p + geom_point(aes_string(shape=shape,color=color),size=4,alpha=0.5,
                            position=position);
    }
    p <- p + coord_cartesian(xlim=xlims,ylim=ylims)
    p <- p + labs(x=xlab,y=ylab);
    p <- p + ggtitle(title);
    if (!is.null(legend)) p <- p + guides(color=guide_legend(legend),shape=guide_legend(legend))
    if (!is.null(facets)) p <- p + facet_grid(facets,scales=facet.scales);
    if (showPlot) print(p);
    
    return(p);
}