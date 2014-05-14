#'
#'@title Function to calculate ABC and OFL from projection model output.
#'
#'@description This function calculates ABC and OFL from TCSAM2013 projection model output.
#'
#'@param res - r list from TCSAM2013 projection model run
#'@param fn - filename of TCSAM2013 projection model run r file
#'@param pstar - 
#'@param buffer - 
#'@param title - title for plot
#'
#'@import stats
#'@import graphics
#'
#'@export
#'
calcABC<-function(res=NULL,
                  fn=NULL,
                  pstar=0.49,
                  buffer=0.1,
                  title=NULL){
    if (is.null(res)){
        if (is.null(fn)) fn<-choose.files(caption="Select OFL-ABC sim file",multi=FALSE,filters=Filters[c("R")]);
        source(fn,local=TRUE); #reads in "res"
    }
    x<-ecdf(res$estOFL)
    OFL<-quantile(x,probs=0.50)
    ABC.pstar<-quantile(x,probs=pstar);
    b.y<-1-ABC.pstar/OFL;
    
    plot(x,xlab="estimated OFL",ylab="empirical cdf",main="",lwd=2);
    abline(v=OFL,col="red",lwd=3)
    text(1.01*OFL,0.95,"OFL",col='red',adj=0)
    
    abline(v=ABC.pstar,col="blue",lwd=3,lty=2)
    text(ABC.pstar,0.85,"ABC\np*",col='blue',adj=1.01)
    
    ABC.buff<-(1-buffer)*OFL;
    abline(v=ABC.buff,col="green",lwd=3,lty=2)
    
    if (!is.null(title)) mtext(title,side=3,adj=0.1);
    
    text(ABC.buff,0.85,"ABC\n10%",col='green',adj=1.01)
    cat("#--------------------------------------\n")
    cat("#  avg.rec   B   Fmsy   Bmsy   B/Bmsy  OFL   ABC.pstar   ABC.buffer\n")
    cat("\t",res$avg.rec,res$B.curr,res$FXX,res$BXX, res$B.curr/res$BXX,OFL,ABC.pstar,ABC.buff,"\n",sep="\t\t")
    
    return(res)
}
#
#res<-calcABC(title="R1 scenario");
