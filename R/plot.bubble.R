#'
#'@title Compare size comps using bubble plots for Pearson residuals
#'
#'@description Function to compare size comps using bubble plots for Pearson residuals
#'
#'@details Assumes 5mm size bins starting at 27.5 as center of smallest bin. 
#'Filled circles represent pred > obs, empty circles represent pred. < obs.
#'
#'@param datao - dataframe of observed size frequencies (year in column 1)
#'@param datap - dataframe of predicted size frequencies (year in column 1)
#'@param nlen - number of size bins
#'@param sampsize - vector of sample sizes 
#'
#'@export
#'
plot.bubble<-function(datao,datap,nlen=22,sampsize){
    ny<-length(as.numeric(unlist(datao[,1])))
    #this is standardized pearson residual (o-p)/(sqrt(p(1-p)/sample size))
    sampsize[sampsize<1.0]=1.0
    res<-(datao[,2:(nlen+1)]-datap[,2:(nlen+1)])/sqrt((datap[,2:(nlen+1)]*(1-datap[,2:(nlen+1)]))/sampsize)
    res[is.na(res)]<-0
    resp<-res
    resp[resp<0]=0; #obs > pred
    respstar=resp
    respstar[respstar<=3]=0
    resp[resp>3]=0
    resn<-res
    resn[resn>=0]=0
    resnstar=resn
    resnstar[resnstar<=-3]=0
    resn[resn<(-3)]= 0  
    plot(rep(as.numeric(unlist(datao[,1])),rep(nlen,ny)),rep(seq(27.5,by=5,length=nlen),rep(ny,nlen)),
           type="n",ylab="Length bin",xlab="Year");
    
    symbols(rep(as.numeric(unlist(datao[,1])),rep(nlen,ny)),rep(seq(27.5,by=5,length=nlen),ny),
            circles=sqrt(as.vector(t(resp[,1:nlen]))),add=T,inches=0.1)
    if(max(resp[,1:nlen])>.0000001){
        symbols(rep(as.numeric(unlist(datao[,1])),rep(nlen,ny)),rep(seq(27.5,by=5,length=nlen),ny),
        circles=sqrt(-as.vector(t(resn[,1:nlen]))),add=T,
                    inches=0.1*(sqrt(max(-resn[,1:nlen]))/sqrt(max(resp[,1:nlen]))),bg=1)
    }
    x=rep(seq(27.5,by=5,length=nlen),ny)
    x[as.vector(unlist(t(respstar)))<=3]=0
    points((rep(as.numeric(unlist(datao[,1])),rep(nlen,ny))),x,pch="*")
    
    title(sub=paste(" Standardized Pearson Residual Range ",round(min(resn),3),round(max(resp),3)),cex.sub=1.0,adj=0)
}
