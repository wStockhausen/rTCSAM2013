#'
#'@title Function to calculate ABC and OFL from projection model output
#'
#'@description This function calculates ABC and OFL from TCSAM2013 projection model output.
#'
#'@param res - r list from TCSAM2013 projection model run
#'@param fn - filename of TCSAM2013 projection model run r file
#'@param pstar - 
#'@param buffer - buffer for ABC as fraction (i.e., 0.20 = 20 \% yields ABC = 0.8*OFL)
#'@param title - title for plot
#'
#'@details Uses standard graphics.
#'
#'@export
#'
calcABC<-function(res=NULL,
                  fn=NULL,
                  pstar=0.49,
                  buffer=0.1,
                  title=NULL){
    if (is.null(res)){
        if (is.null(fn)) fn<-wtsUtilities::selectFile(ext="R",caption="Select OFL-ABC sim file");
        source(fn,local=TRUE); #reads in "res"
    }
    
    if (is.null(fn)){
        fldr<-getwd();
    } else {
        fldr<-dirname(fn);
    }
    
    x<-ecdf(res$estOFL)
    OFL<-quantile(x,probs=0.50)
    ABC.pstar<-quantile(x,probs=pstar);
    b.y<-1-ABC.pstar/OFL;
    
    ABC.buff<-(1-buffer)*OFL;
    
    plotABC(x,OFL,ABC.pstar,ABC.buff,buffer=buffer,title=title);
    
    pfn<-file.path(fldr,"OFL_plot.png")
    png(pfn,width=660,height=420,units="px");
        plotABC(x,OFL,ABC.pstar,ABC.buff,buffer,title);
    dev.off();
    
    strl<-paste("avg.rec","B","Fmsy","Bmsy","B/Bmsy","OFL","ABC.pstar","ABC.buffer",sep="\t");
    strv<-paste(res$avg.rec,res$nextMMB,res$FXX,res$BXX, res$nextMMB/res$BXX,OFL,ABC.pstar,ABC.buff,sep="\t")
    cat(strl,strv,sep="\n");
    
    tfn<-file.path(fldr,"OFL_results.csv");
    strl<-paste("avg.rec","B","Fmsy","Bmsy","B/Bmsy","OFL","ABC.pstar","ABC.buffer",sep=",");
    strv<-paste(res$avg.rec,res$nextMMB,res$FXX,res$BXX, res$nextMMB/res$BXX,OFL,ABC.pstar,ABC.buff,sep=",")
    cat(strl,strv,sep="\n",file=tfn);
    
    return(invisible(res))
}
#
#res<-calcABC(title="R1 scenario");
