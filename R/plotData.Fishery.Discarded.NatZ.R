#'
#'@title Function to plot numbers-at-size from fishery discard data.
#'
#'@description This function plots discarded catch numbers-at-size data by year as circles.
#'New shell crab are plotted in blue, old shells in green. Plots are created by sex.
#'
#'@param nAtZ - discarded catch numbers-at size 3d array (sex,shell condition, year, size).
#'@param label - label for plot
#'
#'@importFrom wtsPlots plotCompsAsCircles
#'
plot.Data.Fishery.Discarded.NatZ<-function(nAtZ,
                                           label=''){
    #extract dimensions of nAtZ
    dim.names<-dimnames(nAtZ);
    sxs<-dim.names[[1]];
    scs<-dim.names[[2]];
    yrs<-as.numeric(dim.names[[3]]);
    zs<-as.numeric(dim.names[[4]]);
    
    for (sex in c("FEMALE","MALE")){
        n.ns<-as.matrix(nAtZ[sex,"NEW_SHELL",,])
        n.os<-as.matrix(nAtZ[sex,"OLD_SHELL",,])
        n.as<-as.matrix(nAtZ[sex,"ALL_SHELL",,])
        
        zscl<-max(n.ns,n.os,n.as)
        plotCompsAsCircles(z=t(n.ns),x=yrs,y=zs,overplot=FALSE,bg='blue',
                           xlab='',ylab='mm CW',
                           scale=zscl,maxRadius=0.8)
        plotCompsAsCircles(z=t(n.os),x=yrs,y=zs,overplot=TRUE,
                           bg='green',scale=zscl,maxRadius=0.8)
        plotCompsAsCircles(z=t(n.as),x=yrs,y=zs,overplot=TRUE,
                           bg='cyan',scale=zscl,maxRadius=0.8)
        mtext(paste(label,": discarded ",tolower(sex),'s',sep=''),side=3,adj=0.05)
    }
}
