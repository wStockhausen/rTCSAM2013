#'
#'@title Function to plot parameter values.
#'
#'@description This function plots parameters values, together with their limits
#'(if any) and the posterior distributions implied by their estimated standard
#'errors.
#'
#'@param obj.prs - dataframe or filename for csv file containing parameters information
#'@param obj.std - dataframe obtained from reading, or filename of, the .std file
#'@param dp - percent difference between parameter value and upper/lower limits used to color plot
#'
#'@return - list
#'
#'@import graphics
#'
#'@export
#'
checkParams<-function(obj.prs=NULL,
                      obj.std=NULL,
                      dp=0.01){
    if (!is.data.frame(obj.prs)){
        if (!is.character(obj.prs)){
            in.prs<-wtsUtilities::selectFile(ext='csv',caption="Select parameters info csv file");
        } else {
            in.prs<-obj.prs;
        }
        obj.prs<-read.csv(in.prs,stringsAsFactors=FALSE);
    }
    if (!is.data.frame(obj.std)){
        if (!is.character(obj.std)){
            in.std<-wtsUtilities::selectFile(ext='std',caption="Select std file");
        } else {
            in.std<-obj.std;
        }
        obj.std<-NULL;
        if (!is.null(in.std)) obj.std = read.table(in.std,as.is=T,header=F,skip=1);
    }
    
    nr<-nrow(obj.prs);
    old.par<-par(mfcol=c(10,5),mai=c(0,0,0,0),omi=c(0.5,0.5,0.5,0.5));
    on.exit(par(old.par));
    res<-NULL;
    for (r in 1:nr){
        pmn<-obj.prs$min[r];
        pmx<-obj.prs$max[r];
        pvl<-obj.prs$value[r];
        pnm<-obj.prs$name[r];
        if (is.finite(pmn)){
            plot(c(pmn,pmx),c(0,1),type='n',ann=FALSE,xaxt='n',yaxt='n',ylim=c(0,1));
            clr<-'green';
            if (abs(pvl-pmn)/(pmx-pmn)<dp/100) {
                clr<-'blue';
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
            adj=0.01;
            if ((pvl-pmn<0.5)/(pmx-pmn)) adj<-0.99
            rect(pmn,0,pmx,0.8,col=clr);
            lines(c(pvl,pvl),c(0,1),lwd=3,col='black');
            if (!is.null(obj.std)){
                stdv<-obj.std[r,4];
                x<-seq(from=pmn,to=pmx,length.out=31);
                y<-0.9*exp(-0.5*((pvl-x)/stdv)^2);
                polygon(c(pmn,x,pmx),c(0,y,0),col=grey(0.5,alpha=0.5));
            }
            mtext(pnm,side=3,line=-1,cex=0.6,adj=adj);
        } else {
            plot(0.9*pvl,1.1*pvl,c(0,1),type='n',ann=FALSE,xaxt='n',yaxt='n');
            lines(c(pvl,pvl),c(0,1),lwd=3,col='black');
            mtext(pnm,side=3,line=-1,cex=0.6,adj=0.01);
            mtext("no limits",side=1,line=-1,cex=0.6,adj=0.01,col='green');
        }
    }
    return(invisible(res));
}
