#'
#'@title Function to plot ABC and OFL calculations from calcABC(...)
#'
#'@description This function plots ABC and OFL from TCSAM2013 projection model output.
#'
#'@param x - empirical cdf of OFL
#'@param OFL - estimated OFL
#'@param ABC.pstar - ABC based on pstar method
#'@param ABC.buff  - ABC based on buffer method
#'@param buffer - buffer used for ABC.buff
#'@param title - title for plot
#'@import graphics
#'
#'@export
#'
plotABC<-function(x,OFL,ABC.pstar,ABC.buff,buffer=0.1,title=NULL,xlim=c(22.5,42.5)){
    plot(x,xlab="estimated OFL",ylab="empirical cdf",main="",lwd=2,xlim=xlim);
    abline(v=OFL,col="red",lwd=3)
    text(1.01*OFL,0.95,"OFL",col='red',adj=0)
    
    abline(v=ABC.pstar,col="blue",lwd=3,lty=2)
    text(ABC.pstar,0.85,"ABC\np*",col='blue',adj=1.01)
    
    abline(v=ABC.buff,col="green",lwd=3,lty=2)
    text(ABC.buff,0.85,paste("ABC\n",100*buffer,"%",sep=''),col='green',adj=1.01)
    
    if (!is.null(title)) mtext(title,side=3,adj=0.1);
}