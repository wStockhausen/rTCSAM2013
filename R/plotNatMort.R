#'
#'@title Function to plot natural mortality rates by year
#'
#'@description This function plots natural mortality estimates by year
#'   and sex for immatures, new shell matures and old shell matures.
#'   
#'@param sdobj - object (dataframe) obtained from reading the TCSAM2013 .std file
#'@param styr - model start year
#'@param endyr - model end year
#'@param min.yr - min year to plot
#'@param max.yr - max year to plot
#'@param clrs - colors to use
#'@param pchs - point types to use
#'@param ltys - line types to use
#'
#'@importFrom wtsPlots plotErrorBars.V
#'
#'@export
#'
plotNatMort<-function(sdobj,
                       styr=1949,
                       endyr=2013,
                       min.yr=1970,
                       max.yr=endyr,
                       clrs=c("green","blue","black"),
                       pchs=c(21,21,21),
                       ltys=c(1,1,1)){
  idx.M.imm<-which(sdobj[[2]]=="sdrNatMortImm",arr.ind=TRUE)
  idx.M.NS<-which(sdobj[[2]]=="sdrNatMortNS",arr.ind=TRUE)
  idx.M.OS<-which(sdobj[[2]]=="sdrNatMortOS",arr.ind=TRUE)
  
  M.imm.est<-matrix(data=sdobj[idx.M.imm,3],nrow=(endyr-styr+1),ncol=2)
  M.imm.std<-matrix(data=sdobj[idx.M.imm,4],nrow=(endyr-styr+1),ncol=2)
  M.NS.est<-matrix(data=sdobj[idx.M.NS,3],nrow=(endyr-styr+1),ncol=2)
  M.NS.std<-matrix(data=sdobj[idx.M.NS,4],nrow=(endyr-styr+1),ncol=2)
  M.OS.est<-matrix(data=sdobj[idx.M.OS,3],nrow=(endyr-styr+1),ncol=2)
  M.OS.std<-matrix(data=sdobj[idx.M.OS,4],nrow=(endyr-styr+1),ncol=2)
  
  yrs<-styr:endyr;
  ymx<-max(M.imm.est+M.imm.std,M.NS.est+M.NS.std,M.OS.est+M.OS.std,na.rm=TRUE)
  
  for (x in 1:2){    
    plot(yrs,M.imm.est[,1],type='n',
         xlab='year',xlim=c(min.yr,max.yr),
         ylab='mortality rate (yr-1)',ylim=c(0,1.2*ymx))
    
    plt<-1
    lines(yrs+0.15*(plt-2),M.imm.est[,x],col=clrs[plt],lty=ltys[plt])
    plotErrorBars.V(yrs+0.15*(plt-2),M.imm.est[,x],sigma=M.imm.std[,x],CI=0.8,width=0.2,col=clrs[plt]);
    points(yrs+0.15*(plt-2),M.imm.est[,x],col=clrs[plt],pch=pchs[plt])
    
    plt<-2
    lines(yrs+0.15*(plt-2),M.NS.est[,x],col=clrs[plt],lty=ltys[plt])
    plotErrorBars.V(yrs+0.15*(plt-2),M.NS.est[,x],sigma=M.NS.std[,x],CI=0.8,width=0.2,col=clrs[plt]);
    points(yrs+0.15*(plt-2),M.NS.est[,x],col=clrs[plt],pch=pchs[plt])
    
    plt<-3
    lines(yrs+0.15*(plt-2),M.OS.est[,x],col=clrs[plt],lty=ltys[plt])
    plotErrorBars.V(yrs+0.15*(plt-2),M.OS.est[,x],sigma=M.OS.std[,x],CI=0.8,width=0.2,col=clrs[plt]);
    points(yrs+0.15*(plt-2),M.OS.est[,x],col=clrs[plt],pch=pchs[plt])
    
    legend("topright",cex=0.8,
           c("immatures","new shell mature","old shell mature"),
           col=clrs[1:3],
           pch=pchs[1:3],
           lty=ltys[1:3])
    
    if (x==1){
      mtext("Females",side=3,adj=0.01,outer=FALSE)
    } else {
      mtext("Males",side=3,adj=0.01,outer=FALSE)
    }
  }
}