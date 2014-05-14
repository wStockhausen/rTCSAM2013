#'
#'@title Function to plot size comps comparisons by year
#'
#'@description This function compares observed and predicted size comps by year.
#'
#'@param data.o - observed size comps
#'@param data.p - predicted size comps
#'@param nr - number of rows per page
#'@param nc - number of columns per page
#'@param nplots - total number of plots
#'@param new - flag to set and return graphics parameters
#'@param len - flag indicating length comps
#'@param maxy - max y value to plot
#'@param yearlb - year labels, one per plot
#'
#'@import graphics
#'
#'@export
#'
plotSizeCompsComparisons<-function(data.o,data.p,
                                   nr=4,nc=4,nplots=nr*nc,
                                   new=TRUE,len=TRUE,
                                   maxy=NULL,yearlb=NULL){
  if(new){
    #for length comps
    #par.old<-par(mfrow=c(nr,nc),mai=c(0.3, 0.3, 0.0, 0.0))
    #age comps
    par.old<-par(mfrow=c(nr,nc),mai=c(0.0, 0.0, 0.0, 0.0),cex=.5,omi=c(0.5,0.5,0.5,0.5))
  }
  n.s<-length(data.o[1,])
  if(len){
    labx<-"Size (mm CW)"
    if(is.null(maxy)) {
      maxy<-max(as.numeric(unlist(data.o[,2:n.s])),
                as.numeric(unlist(data.p[,2:n.s])))
    }
  } else {labx<-"Age"}
  if(is.null(maxy)){
    maxy<-max(as.numeric(unlist(data.o[,2:n.s])),
         as.numeric(unlist(data.p[,2:n.s])))
  }
  size.bins<-as.numeric(colnames(data.o)[2:n.s])
  for(i in 1:nplots){
    plot(size.bins,as.numeric(unlist(data.p[i,2:n.s])),type="l",
    ylim=c(0,maxy),xlim=range(size.bins),yaxt="n",
    xlab=labx,ylab="")
    points(size.bins,as.numeric(unlist(data.o[i,2:n.s])))
    if(len){
      if(is.null(yearlb)){
        text(150,maxy-(maxy*.4),labels=as.character(data.o[i,1]),cex=1.0)
      } else {
        text(150,maxy-(maxy*.4),labels=as.character(yearlb[i]),cex=1.0)
        #browser()
      }
      #text(40,max(as.numeric(unlist(data.o[,2:n.s])),as.numeric(unlist(data.p[,2:n.s])))-.05,"dotted line predicted")    
    } else {
      #
      text(10,max(as.numeric(unlist(data.o[,2:n.s])),
      as.numeric(unlist(data.p[,2:n.s])))-.01,labels=as.character(data.o[i,1]),cex=1)
      #text(12,max(as.numeric(unlist(data.o[,2:n.s])),as.numeric(unlist(data.p[,2:n.s])))-.05,"dotted line predicted")
    }
  }
  if(new) return(par.old);
}

