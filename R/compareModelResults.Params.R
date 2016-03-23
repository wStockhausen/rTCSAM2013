#'
#'@title Function to compare parameter values from different models.
#'
#'@description This function extracts and plots parameters values, together with their limits
#'(if any) and the posterior distributions implied by their estimated standard
#'errors from several models.
#'
#'@param objs - model results objects (each is a list with elements 'prs' and 'std')
#'@param dp - percent difference between parameter value and upper/lower limits used to flag outliers
#'@param fac - number of std devs to extend uncertainty plots
#'@param nc - number of columns of plots per page
#'@param nr - number of rows of plots per page
#'@param showPlot - flag to show plots
#'@param pdf - file name for printing plots to a pdf file (or NULL to print to screen)
#'
#'@return - list with dfr, vfr, and plots as elements
#'
#'@details Uses \code{reshape2::melt()}.
#'
#'@export
#'
compareModelResults.Params<-function(objs,dp=0.01,fac=3,
                                     nc=3,nr=4,showPlot=TRUE,
                                     pdf="ModelComparisons.Params.pdf"){
    #extract dataframe with parameter estimates and info
    cat('Extracting params info\n')
    dfr<-extractModelResults.Params(objs,dp=dp);
    #extract dataframe with parameter uncertainty info
    cat("Extracting uncertainty info\n")
    lst<-extractModelResults.StdDevs(objs,fac=fac);
    #plot parameters as scalar values
    cat("Plotting parameter results\n")
    plots<-plotModelResults.ScalarParams(dfr,vfr=lst$vfr,nc=nc,nr=nr,showPlot=showPlot,pdf=pdf)
    
    #combine dfr and lst$sfr
    mdfr<-reshape2::melt(dfr,id.vars=c('case','type','par','param'),variable.name='variable',factorsAsStrings=TRUE);
    sdfr<-rbind(mdfr,lst$sfr);
    write.csv(sdfr,'ModelComparisons.ParameterEstimates.csv',row.names=FALSE);
    
    return(invisible(list(sdfr=sdfr,vfr=lst$vfr,plots=plots)));
}

#'
#'@title Function to extract parameter values from different models.
#'
#'@description This function extracts parameters values, together with their limits
#'(if any) from several models.
#'
#'@param objs - model results objects (each is a list with elements 'prs' and 'std')
#'@param dp - percent difference between parameter value and upper/lower limits used to flag outliers
#'
#'@return - dataframe with the parameter information
#'
#'@details Uses \code{wtsUtilities::formatZeros()}.
#'
#'@export
#'
extractModelResults.Params<-function(objs,dp=0.01){
    dfr<-NULL;
    cases<-names(objs);
    for (case in cases){
        #loop over model cases
        prs<-objs[[case]]$prs;        
        params<-unique(prs$name);
        for (param in params){
            prsp<-prs[prs$name==param,];
            nr<-nrow(prsp);
            if (nr>0){
                for (r in 1:nr){
                    rw<-list();
                    rw$case <-case;
                    rw$type <-'scalar';
                    rw$par  <- gsub("[[:blank:]]",'',param,fixed=FALSE);#need to remove whitespace
                    rw$param<-rw$par;
                    if (nr>1) {
                        rw$type <-'vector';
                        rw$param<-paste(rw$par,"[",wtsUtilities::formatZeros(r,width=2),"]",sep='');
                    }
                    rw$value<-prsp$value[r];
                    rw$min  <-prsp$min[r];
                    rw$max  <-prsp$max[r];
                    rw$scl  <- 0;
                    if (is.finite(rw$min)){
                        if (abs(rw$value-rw$min)/(rw$max-rw$min)<dp/100) rw$scl <- -1;
                        if (abs(rw$value-rw$max)/(rw$max-rw$min)<dp/100) rw$scl <-  1;
                    }#finite limits
                    dfr<-rbind(dfr,as.data.frame(rw,stringsAsFactors=FALSE));
                }#r
            }#nr>0
        }#params
    }#case
    return(invisible(dfr));
}

#'
#'@title Function to extract parameter uncertainty (std) info from different models.
#'
#'@description This function extracts the posterior distributions of parameters and
#'sdreport variables as implied by their estimated standard errors from several models.
#'
#'@param objs - model results objects (each is a list with elements 'std')
#'@param fac - number of std devs to extend uncertainty plots
#'
#'@return - list with elements vfr (dataframe with uncertainty info for plots) and 
#'sfr (dataframe with stdv info for combining w/ parameter estimates info)
#'
#'@details Uses \code{wtsUtilities::formatZeros()}.
#'
#'@export
#'
extractModelResults.StdDevs<-function(objs,fac=3){
    sfr<-NULL;
    vfr<-NULL;
    cases<-names(objs);
    for (case in cases){   
        #loop over model cases
        std<-objs[[case]]$std;
        params<-unique(std[,2])
        for (param in params){
            #loop over param names
            idx<-std[,2]==param;
            stdp<-std[idx,];
            nr<-nrow(stdp);
            if (nr>0){
                type<-'scalar';
                paramp<-param;
                for (r in 1:nr){
                    par  <-stdp[r,2];#param name
                    estp <-stdp[r,3];#model estimate
                    stdv <-stdp[r,4];#standard dev
                    if (stdv>0) {
                        x<-seq(from=estp-fac*stdv,to=estp+fac*stdv,length.out=51);
                        y<-0.9*exp(-0.5*((x-estp)/stdv)^2);
                        if (nr>1) {
                            type<-'vector';
                            paramp<-paste(par,"[",wtsUtilities::formatZeros(r,width=2),"]",sep='');
                        }
                        vri<-list(case=case,type=type,par=par,param=paramp,x=x,y=y);
                        vfr<-rbind(vfr,as.data.frame(vri,stringsAsFactors=FALSE));
                        vri<-list(case=case,type=type,par=par,param=paramp,variable='stdv',value=stdv);
                        sfr<-rbind(sfr,as.data.frame(vri,stringsAsFactors=FALSE));
                    }#stdv>0
                }#r
            }#nr>0
        }#params
    }#case
    return(invisible(list(vfr=vfr,sfr=sfr)));
}

#'
#'@title Function to plot parameter values and associated uncertainty from different models.
#'
#'@description This function plots parameters values, together with their limits
#'(if any) and the posterior distributions implied by their estimated standard,
#'errors from several models.
#'
#'@param dfr - dataframe from call to extractModelResults.Params
#'@param vfr - dataframe from call to extractModelResults.StdDevs
#'@param nc - number of columns of plots per page
#'@param nr - number of rows of plots per page
#'@param showPlot - flag to show plots
#'@param pdf - file name for printing plots to a pdf file (or NULL to print to screen)
#'
#'@return - list with plots as elements
#'
#'@import ggplot2
#'
#'@export
#'
plotModelResults.ScalarParams<-function(dfr,vfr=NULL,nc=3,nr=4,showPlot=TRUE,pdf=NULL){
    if (showPlot&!is.null(pdf)){
        pdf(file=pdf,width=8.5,height=11,onefile=TRUE);
        on.exit(dev.off());
    }
    ups<-unique(dfr$param);  #unique parameters
    np<-length(ups);         #number of unique parameters
    npg<-ceiling(np/(nc*nr));#number of pages
    plots<-list();
    for (pg in 1:npg){
       idx<- dfr$param %in% ups[1:(nc*nr)+(pg-1)*nc*nr]
       dfrsp<-dfr[idx,];
       if (!is.null(vfr)){
           idx<- vfr$param %in% ups[1:(nc*nr)+(pg-1)*nc*nr]
           vfrsp<-vfr[idx,];
       }
       p <- ggplot(data=dfrsp)
       p <- p + geom_rect(mapping=aes_string(xmin='min',xmax='max',ymin=I(0),ymax=I(1)),alpha=0.5,fill='grey')
       p <- p + geom_vline(aes_string(xintercept='value',colour='case',linetype='case'),size=1)
       p <- p + guides(colour=guide_legend())
       p <- p + labs(x='parameter value',y='')
       if (!is.null(vfr)&(nrow(vfrsp)>0)){
           p <- p + geom_area(aes(x=x,y=y,fill=case),data=vfrsp,alpha=0.3,position="identity")
           p <- p + guides(fill=guide_legend())
       }
       p <- p + facet_wrap(~param,ncol=nc,nrow=nr,drop=FALSE,scales="free_x");
       if (showPlot) print(p);
       plots[[pg]]<-p;
    }
    return(invisible(plots));
}

# resPar<-compareModelResults.Params(resLst,dp=0.01,fac=3,
#                                    nc=3,nr=5,showPlot=TRUE)
    
    