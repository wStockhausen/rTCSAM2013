plotModelComparisons.TimeSeries<-function(obsyrs=NULL,
                                         obs=NULL,
                                         obscv=NULL,
                                         prdyrs=NULL,
                                         vartype=NULL,
                                         objs=NULL,
                                         scaleBy=1,
                                         clrs=c('blue','green','cyan','red'),
                                         title='',
                                         ylab=''){
    
    ylim<-c(Inf,-Inf);
    #deal with observations first, if any
    if (!is.null(obs)){
        ylim<-range(obs,na.rm=TRUE,finite=TRUE)
        if (!is.null(obscv)){
            lower= obs*(exp(-1.96*sqrt(log(1+obscv^2)))-1);#lower error bar
            upper= obs*(exp( 1.96*sqrt(log(1+obscv^2)))-1);#upper error bar
            ylim<-range(ylim,lower,upper,na.rm=TRUE,finite=TRUE)
        }
    }
    
    #compile model time series
    nc<-length(objs);
    cases<-names(objs);
    vals<-vector(mode='list',length=nc);
    names(vals)<-cases;
    for (cs in cases){
        vals[[cs]]<-objs[[cs]][[vartype]]/scaleBy;
        cat(cs,": lims = ",range(vals[[cs]],na.rm=TRUE,finite=TRUE),"\n");
        ylim<-range(ylim,vals[[cs]],na.rm=TRUE,finite=TRUE);
    }
    
    cat("ylim = ",ylim,"\n",sep="\t");

    #plot
    plot(c(0,1),c(0,1),type="n",
         ylim=c(0,ylim[2]),xlim=c(min(prdyrs),max(prdyrs)),ylab=ylab,xlab='');
    if (!is.null(obscv)) wtsPlots::plotErrorBars.V(obsyrs,obs,upper=upper,lower=lower,col='black');
    if (!is.null(obs))   points(obsyrs,obs,pch=21,col='black',bg='black');
    mtext(title, side=3, adj=0.05,cex=0.9)
    for (ic in 1:nc){
        lines(prdyrs,vals[[ic]],col=clrs[ic],lty=1,lwd=2)
    }
    legend("topright", c("observed", cases),
           pch=c(21,NA*(1:nc)),pt.bg=c('black',NA*(1:nc)),
           col=c('black',clrs[1:nc]),lty=c(NA,1+0*(1:nc)),lwd=2);
}