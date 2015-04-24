#'@title Plot multiple ggplot graphs on the same page.
#'
#'@param ...      - multiple ggplot objects
#'@param plotlist - a list of ggplot objects
#'@param cols     - number of columns in layout
#'@param layout   - a matrix specifying the layout. If present, 'cols' is ignored.
#'
#'@details If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#' 
#' Based on the 'multiplot' function from the "Cookbook for R" (www.cookbook-r.com).
#'
#'@import grid
#'@import ggplot2
#'
#'@export
#'
plotMulti.GG<-function(..., plotlist=NULL, file, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols', 'rows' to determine layout
  if (is.null(layout)) {
    if (is.null(rows)) rows <- ceiling(numPlots/cols);
    layout <- matrix(seq(1, cols * rows),ncol = cols, nrow = rows)
  }
    
    # Make each plot, in the correct location
    mxp<-max(layout);
    for (p in 1:numPlots) {
        i <- (p-1) %% mxp;#modulo
        if (i==0){
            # Set up a new page
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        }
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i+1, arr.ind = TRUE))
        
        print(plots[[p]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col));
    }
}