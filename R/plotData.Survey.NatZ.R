#'
#'@title Function to plot numbers-at-size from survey data.
#'
#'@description This function plots survey numbers-at-size data by year for each sex as circles.
#'Immatures are plotted in blue, matures in green. Shell condition is summed over.
#'
#'@param nAtZ - survey numbers-at size 5d array 
#               '(sex, shell condition, maturity state, year, size).
#'@param label - label for plot
#'
#'@details Uses \code{wtsPlots::plotCompsAsCircles}.
#'
#'@export
#'
plotData.Survey.NatZ<-function(nAtZ,
                               label="trawl survey"){
    #extract dimensions of nAtZ
    dim.names<-dimnames(nAtZ);
    sxs<-dim.names[[1]];
    scs<-dim.names[[2]]
    mss<-dim.names[[3]]
    yrs<-as.numeric(dim.names[[4]]);
    zs<-as.numeric(dim.names[[5]]);
    
    for (sex in c("FEMALE","MALE")){
        n.imm<-as.matrix(nAtZ[sex,"NEW_SHELL","IMMATURE",,])
        n.mat<-as.matrix(nAtZ[sex,"NEW_SHELL","MATURE",,])+
                as.matrix(nAtZ[sex,"OLD_SHELL","MATURE",,]);
        
        zscl<-max(n.imm,n.mat)
        wtsPlots::plotCompsAsCircles(z=t(n.imm),x=yrs,y=zs,overplot=FALSE,bg='blue',
                                     xlab='',ylab='mm CW',
                                     scale=zscl,maxRadius=0.8)
        wtsPlots::plotCompsAsCircles(z=t(n.mat),x=yrs,y=zs,overplot=TRUE,
                                     bg='green',scale=zscl,maxRadius=0.8)
        mtext(paste(label,": ",tolower(sex),'s',sep=''),side=3,adj=0.05)
    }
}
