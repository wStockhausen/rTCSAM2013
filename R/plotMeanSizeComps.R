#'
#'@title Function to plot comparisons of mean size comps.
#'
#'@description This function plots comparisons of size comps using their averages.
#'
#'@param size.bins - 
#'@param obs.comp - 
#'@param prd.comp - 
#'@param cols - vector of column numbers to extract (NULL results in all columns used)
#'@param ymx - max y value
#'@param xlab - x axis title
#'@param ylab - y axis title
#'@param pch - vector of point symbols to use
#'@param lty - vector of line types to use
#'@param lwd - vector of line widths to use
#'@param clr - vector of colors to use
#'@param CI - confidence
#'@param bar.width - 
#'@param addToPlot - 
#'
#'@import stats
#'
#'@importFrom wtsPlots plotErrorBars.V
#'
plotMeanSizeComps<-function(size.bins,
                            obs.comp,
                            prd.comp,
                            cols=NULL,
                            ymx=NULL,
                            xlab=NULL,
                            ylab=NULL,
                            pch=21,
                            lty=1,
                            lwd=1,
                            clr='black',
                            CI=0.8,
                            bar.width=1,
                            addToPlot=FALSE){
    #set columns to extract
    if (is.null(cols)){cols<-1:ncol(obs.comp)}
    
    #calculate means and std. dev.s by size bin
    mns.obs<-colMeans(obs.comp,na.rm=TRUE)[cols];
    std.obs<-apply(obs.comp,2,sd,na.rm=TRUE)[cols]/sqrt(length(cols));#standard errors of mean
    mns.prd<-colMeans(prd.comp,na.rm=TRUE)[cols];
    std.prd<-apply(prd.comp,2,sd,na.rm=TRUE)[cols]/sqrt(length(cols));#standard errors of mean
    
    if (!addToPlot){
        if (is.null(ymx)){
            ymx<-max(mns.obs+std.obs);
        }
        plot(length.bins, mns.obs,
             type="n",pch=pch,col=clr,
             ylim=c(0,ymx),
             xlab=xlab,ylab=ylab);
    }
    plotErrorBars.V(size.bins,
                    mns.obs,
                    sigma=std.obs,
                    CI=CI,
                    width=bar.width,
                    pch=pch,col=clr)
    points(size.bins,mns.obs,pch=pch,col=clr)
    lines(size.bins,mns.prd,lty=lty,lwd=lwd,col=clr)
}