#'
#'@title Plot a quad plot from a TCSAM report object
#'
#'@description Function to plot a quad plot from a TCSAM report object.
#'
#'@param obj.rep - report file object
#'@param F35 - F35 from a projection model run
#'@param B35 - B35 from a projection model run
#'
#'@return nothing
#'
#'@export
#'
plotQuadPlot<-function(obj.rep,
                       F35=NA,
                       B35=NA){
    if (!is.na(F35)){
        styr<-obj.rep$styr;
        endyr<-obj.rep$endyr;
        par(oma=c(1,1,1,1),mar=c(4,4,2,1)+0.2,mfrow=c(1,1))
        yrs<-styr:(endyr-1)
        mmb<-obj.rep$"Mating.time.Male.Spawning.Biomass";
        tfm<-obj.rep$"max.TOT.male.NS.mortality.rate";
        idx<-(yrs>=1965)&(tfm>0);
        yrs<-yrs[idx];
        mmb<-mmb[idx];
        tfm<-tfm[idx];
        plot(mmb,tfm,type='n',
             xlab=" MMB (1000 t)",ylab="Full Selection Fishing Mortality Rate",
             xlim=c(0,1.1*max(mmb)),ylim=c(0,1.1*max(tfm)));
        n<-length(yrs);
        for (ctr in 1:(n-1)){
            if (yrs[ctr]<1980){
                clr<-'black';
            } else if (yrs[ctr]<1990){
                clr<-'blue';
            } else if (yrs[ctr]<2000){
                clr<-'cyan';
            } else if (yrs[ctr]<2010){
                clr<-'green';
            } else {
                clr<-'red'
            }
            text(mmb[ctr],tfm[ctr],as.character(yrs[ctr]),cex=0.7,col=clr,adj=0);
        }
        text(mmb[n],tfm[n],as.character(yrs[n]),cex=1.1,col=clr,adj=0)
        
        alpha<-0.1 
        beta<-0.25
        lines(rep(B35,length=20),seq(0,1.1*max(tfm),length=20),lty=4,col="blue")
        lines(seq(0,1.1*max(mmb),length=20),rep(F35,length=20),lty=4,col="blue")
        text(B35,1.1*max(tfm),expression(bold(B[35])),cex=0.8,adj=0.5,col="blue")
        text(1.1*max(mmb),F35,expression(bold(F[35])),cex=0.8,adj=0.5,col="blue")
        lines(rep(B35*beta,length=20),
              seq(0,F35*((((B35*beta)/B35)-alpha)/(1.-alpha)),length=20),
              type="l",lty=1,lwd=2)
        
        lines(seq(B35*beta,B35,length=20),
              F35*(((seq(B35*beta,B35,length=20)/B35)-alpha)/(1.-alpha)),
              type="l",lty=1,lwd=2)
        
        lines(seq(B35,max(mmb),length=20),
              rep(F35,length=20),
              lty=1,lwd=2)    
    }
}
#plotQuadPlot(repLst[["Model A"]],F35=0.61,B35=26.79)
