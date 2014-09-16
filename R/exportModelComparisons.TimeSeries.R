#'
#'@title Export time series from a set of model runs to csv for comparison.
#'
#'@description Function to export time series from a set of model runs to csv for comparison.
#'
#'@export
#'
exportModelComparisons.TimeSeries<-function(obsyrs=NULL,
                                             obs=NULL,
                                             obscv=NULL,
                                             prdyrs=NULL,
                                             vartype=NULL,
                                             objs=NULL,
                                             scaleBy=1){
    
    #deal with observations first, if any
    if (!is.null(obs)){
        if (!is.null(obscv)){
            lower= obs*(exp(-1.96*sqrt(log(1+obscv^2)))-1);#lower error bar
            upper= obs*(exp( 1.96*sqrt(log(1+obscv^2)))-1);#upper error bar
        }
    }
    
    #compile model time series
    nc<-length(objs);
    cases<-names(objs);
    vals<-vector(mode='list',length=(nc+1));
    names(vals)<-c("year",cases);
    vals[["year"]]<-prdyrs;
    for (cs in cases){
        vals[[cs]]<-objs[[cs]][[vartype]]/scaleBy;
        cat(cs,": lims = ",range(vals[[cs]],na.rm=TRUE,finite=TRUE),"\n");
    }
    
    write.csv(vals,file=paste(vartype,".PRD.csv"),row.names=FALSE)
    if (!is.null(obs)){
        if (is.null(obscv)){
            write.csv(as.data.frame(list(years=obsyrs,obs=obs)),file=paste(vartype,".OBS.csv"),row.names=FALSE)
        } else {
            write.csv(as.data.frame(list(years=obsyrs,obs=obs,LCI=lower,UCI=upper)),file=paste(vartype,".OBS.csv"),row.names=FALSE)
        }
    }
}