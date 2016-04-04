#'
#'@title Function to plot parameter values.
#'
#'@description This function plots parameters values, together with their limits
#'(if any) and the posterior distributions implied by their estimated standard
#'errors.
#'
#'@param obj.prs - TCSAM2013 prs object or 'active' or 'all'
#'@param obj.std - dataframe obtained from reading, or filename of, the .std file
#'@param dp - percent difference between parameter value and upper/lower limits used to color plot
#'
#'@return - dataframe
#'
#'@details  If obj.std is NULL, the user will NOT be prompted to select an
#'std file and std info will NOT be included in the output.
#'
#'@export
#'
checkParams<-function(obj.prs=NULL,
                      obj.std=NULL,
                      dp=0.01){
    if (!inherits(obj.prs,'tcsam2013.prs')){
        obj.prs<-getPrs(obj.prs);
    }
    if (!is.data.frame(obj.std)){
        if (is.character(obj.std)) {
            obj.std<-getStd(obj.std);
        } else {
            obj.std<-NULL;
        }
    }
    
    ##strip whitespace from obj.prs$names
    obj.prs$name<-gsub(" ","",obj.prs$name,fixed=TRUE);
    
    ##loop over parameters
    nr<-nrow(obj.prs);
    old.par<-par(mfcol=c(10,5),mai=c(0,0,0,0),omi=c(0.5,0.5,0.5,0.5));
    on.exit(par(old.par));
    k<-1;##counter for std table
    res<-NULL;
    for (r in 1:nr){
        pmn<-obj.prs$min[r];
        pmx<-obj.prs$max[r];
        pin<-obj.prs$init[r];
        pvl<-obj.prs$value[r];
        pnm<-obj.prs$name[r];
        ##cat(r,pnm,"\n")
        if (is.finite(pmn*pmx)&&(pmn!=pmx)){
            plot(c(pmn,pmx),c(0,1),type='n',ann=FALSE,xaxt='n',yaxt='n',ylim=c(0,1));
            clr<-'green';
            if (abs(pvl-pmn)/(pmx-pmn)<dp/100) {
                clr<-rgb(0,0,1,alpha=0.6);
                if (is.null(res)) {
                    res<-as.data.frame(list(name=pnm,type='low',idx=obj.prs$index[r],min=pmn,max=pmx,value=pvl),stringsAsFactors=FALSE);
                } else {
                    res<-rbind(res,list(name=pnm,type='low',idx=obj.prs$index[r],min=pmn,max=pmx,value=pvl));
                }
            }
            if (abs(pvl-pmx)/(pmx-pmn)<dp/100) {
                clr<-'red';
                if (is.null(res)) {
                    res<-as.data.frame(list(name=pnm,type='high',idx=obj.prs$index[r],min=pmn,max=pmx,value=pvl),stringsAsFactors=FALSE);
                } else {
                    res<-rbind(res,list(name=pnm,type='high',idx=obj.prs$index[r],min=pmn,max=pmx,value=pvl));
                }
            }
            if (obj.prs$phase[r]<1) clr<-grey(0.9);
            adj=0.01;
            if ((pvl-pmn)/(pmx-pmn)<0.5) adj<-0.99;
            rect(pmn,0,pmx,0.8,col=clr);
            lines(c(pin,pin),c(0,0.9),lwd=3,col=grey(0.4,alpha=0.8),lty=2);
            lines(c(pvl,pvl),c(0,1),lwd=3,col='black');
            if (!is.null(obj.std)){
                ##cat("--'",obj.std[k,2],"' =? '",pnm,"'\n",sep='');
                if (obj.std[k,2]==pnm){
                    stdv<-obj.std[k,4];
                    x<-seq(from=pmn,to=pmx,length.out=31);
                    y<-0.9*exp(-0.5*((pvl-x)/stdv)^2);
                    polygon(c(pmn,x,pmx),c(0,y,0),col=grey(0.5,alpha=0.5));
                    k<-k+1;
                }
            }
            mtext(pnm,side=3,line=-1,cex=0.6,adj=adj);
        } else {
            clr<-'white';
            if (obj.prs$phase[r]<1) clr<-grey(0.9);
            plot(c(0.9,1.1)*pvl,c(0,1),type='n',ann=FALSE,xaxt='n',yaxt='n');
            rect(0.9*pvl,0,1.1*pvl,0.8,col=clr);
            lines(c(pin,pin),c(0,0.9),lwd=3,col=grey(0.4,alpha=0.8),lty=2);
            lines(c(pvl,pvl),c(0,1),lwd=3,col='black');
            mtext(pnm,side=3,line=-1,cex=0.6,adj=0.01);
            if ((pmn==pmx)){
                mtext("min=max",side=1,line=-1,cex=0.6,adj=0.01,col='green');
            } else {
                mtext("no limits",side=1,line=-1,cex=0.6,adj=0.01,col='green');
            }
            ##check to advance obj.std counter
            if (!is.null(obj.std)){
                ##cat("--'",obj.std[k,2],"' =? '",pnm,"'\n",sep='');
                if (obj.std[k,2]==pnm) k<-k+1;
            }
        }
    }
    return(invisible(res));
}
