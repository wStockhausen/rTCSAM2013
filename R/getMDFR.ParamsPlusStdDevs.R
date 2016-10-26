#'
#'@title Function to extract parameter estimates and standard deviations from different TCSAM2013 models
#'
#'@description This function extracts parameters values and standard deviations, together with their limits
#'(if any) from several TCSAM2013 models.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param dp - percent difference between parameter value and upper/lower limits used to flag outliers
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return - dataframe with the parameter estimates and uncertainty info.
#'
#'@details Returned dataframe has columns 
#'\itemize{
#'  \item{case} 
#'  \item{type} 
#'  \item{param}
#'  \item{phase}
#'  \item{min}
#'  \item{max}
#'  \item{init}
#'  \item{value}
#'  \item{stdv}
#'  \item{check}
#'}
#'
#'@export
#'
getMDFR.ParamsPlusStdDevs<-function(obj,
                                    dp=0.01,
                                    verbose=FALSE){
    options(stringsAsFactors=FALSE);
    
    lst<-convertToListOfResults(obj);
    cases<-names(lst);

    pdfr<-NULL;
    for (case in cases){
        #loop over model cases
        prs<-lst[[case]]$prs;        
        std<-lst[[case]]$std;
        params<-as.character(unique(prs$name));
        for (param in params){
            if (verbose) cat("processing '",param,"'\n",sep='')
            prsp<-prs[prs$name==param,];
            nrp<-nrow(prsp);
            nrs<-0;
            prmw  <- gsub("[[:blank:]]",'',param,fixed=FALSE);#need to remove whitespace
            splt<-strsplit(prmw,"[\\[\\]]",perl=TRUE);#split on [ and ]
            #prm<-paste(splt[[1]][1],"[",wtsUtilities::formatZeros(splt[[1]][2],width=2),"]",sep='')
            prm<-splt[[1]][1];
            if (nrp>0){
                stdp<-NULL;
                if (!is.null(std)) {
                    idxs<-std[,2]==prmw;
                    stdp<-std[idxs,];
                    nrs<-nrow(stdp);
                    if ((nrs>0)&&verbose) cat("--Found std entries for '",prmw,"'!\n",sep='')
                }
                if (verbose) cat('--nrp =',nrp,' nrs =',nrs,'\n')
                for (r in 1:nrp){
                    ##process only defined parameters 
                    if (!((prsp$phase[r]==-1)&&(prsp$min[r]==-1)&&(prsp$max[r]==-1))){
                        if (verbose) cat('--Processing prs for row',r,'\n')
                        rw<-list();
                        rw$case <-case;
                        rw$type <-'scalar';
                        rw$param<-prm;
                        rw$index<-'';
                        rw$phase<-prsp$phase[r];
                        rw$min  <-prsp$min[r];
                        rw$max  <-prsp$max[r];
                        rw$init <-prsp$init[r];
                        rw$value<-prsp$value[r];
                        rw$stdv <-0.0;
                        rw$check  <-"";
                        rw$category   <-prsp$category[r];
                        rw$process    <-prsp$process[r];
                        rw$label      <-prsp$label[r];
                        rw$description<-prsp$description[r];
                        if (nrp>1) {
                            rw$type <-'vector';
                            rw$param<-paste(prm,"[",wtsUtilities::formatZeros(r,width=2),"]",sep='');
                            rw$index<-prsp$index[r];
                            rw$label<-paste(rw$label,wtsUtilities::formatZeros(rw$index,width=4),sep=' ');
                        }
                        if (nrs>0){
                            if (verbose) cat("--processing std\n")
                            rw$stdv <-stdp[r,4]; #standard dev
                        }#nrs>0
                        if (is.finite(rw$min)){
                            if (abs(rw$value-rw$min)/(rw$max-rw$min)<dp/100) rw$check <-"--";
                            if (abs(rw$value-rw$max)/(rw$max-rw$min)<dp/100) rw$check <-"++";
                        }#finite limits
                        pdfr<-rbind(pdfr,as.data.frame(rw,stringsAsFactors=FALSE));
                    }#parameter defined
                }#nrp
            }#nrp>0
        }#params
    }#case
    #trim leading/trailing whitespace
    pdfr$category   <-trimws(pdfr$category,which='both');
    pdfr$process    <-trimws(pdfr$process,which='both');
    pdfr$label      <-trimws(pdfr$label,which='both');
    pdfr$description<-trimws(pdfr$description,which='both');
    return(pdfr);
}
