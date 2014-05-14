#'
#'@title Function to plot numbers-at-size from retained fishery data.
#'
#'@description This function plots retained catch numbers-at-size data by year as circles.
#'New shell crab are plotted in blue, old shells in green.
#'
#'@param nAtZ - retained catch numbers-at size 3d array (shell condition, year, size).
#'@param label - label for plot
#'
#'@importFrom wtsPlots plotCompsAsCircles
#'
plot.Data.Fishery.Retained.NatZ<-function(nAtZ,
                                          label="directed fishery: retained males"){
    #extract dimensions of nAtZ
    dim.names<-dimnames(nAtZ);
    scs<-dim.names[[1]];
    yrs<-as.numeric(dim.names[[2]]);
    zs<-as.numeric(dim.names[[3]]);
    
    n.ns<-as.matrix(nAtZ["NEW_SHELL",,])
    n.os<-as.matrix(nAtZ["OLD_SHELL",,])
        
    zscl<-max(n.ns,n.os)
    plotCompsAsCircles(z=t(n.ns),x=yrs,y=zs,overplot=FALSE,bg='blue',
                       xlab='',ylab='mm CW',
                       scale=zscl,maxRadius=0.8)
    plotCompsAsCircles(z=t(n.os),x=yrs,y=zs,overplot=TRUE,
                       bg='green',scale=zscl,maxRadius=0.8)
    mtext(label,side=3,adj=0.05)
}
