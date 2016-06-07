#'
#'@title Compare estimated/predicted time series among several model runs
#'
#'@description Function to compare estimated/predicted time series among 
#'several model runs.
#'
#'@param reps - list of objects derived from Jack's R files for the models to be compared
#'@param cases - vector of labels for model cases (if 'reps' is not given)
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - name for output pdf file
#'
#'@details If 'reps' is not given, then the user is prompted to select Jack's R file output from each 
#'model to be compared. If 'cases' is given, the user is prompted to select the file
#'corresponding to each case. If 'cases' is not given, then the user may select an 
#'arbitrary number of files (one at a time), ending selection by pressing 'cancel' on the selection box'.\cr\cr
#'If 'reps' is not given, the working directory is set two levels above the 1st model case file selected.\cr\cr
#'Uses \code{PBSmodelling::readList} and \code{wtsUtilities::selectFile}.
#'
#'@return (nested) list of ggplot2 objects, returned invisibly.
#'
#'@import ggplot2
#'
#'@export
#'
compareModelResults.PopProcesses<-function(reps=NULL,
                                           cases=NULL,
                                           showPlot=FALSE,
                                           pdf="ModelComparison.PopProcesses.pdf"){
    if (is.null(reps)){
        #read in rep files
        in.obj<-0;
        in.reps<-vector(mode="character",length=0)
        nc<-0;
        nca<-Inf;
        if (!is.null(cases)) nca<-length(cases);
        cap<-"Select Jack's R model results file (or cancel to end)";
        while (!is.null(in.obj)&&(nc<nca)){
            nc<-nc+1;
            if (!is.null(cases)) {
                cap<-paste("Select Jack's R model results file for '",cases[nc],"'",sep='');
            }
            in.obj<-wtsUtilities::selectFile(ext="R",caption=cap);
            if(is.character(in.obj)) in.reps<-c(in.reps,in.obj);
        }
        setwd(dirname(in.reps[1])); 
        setwd('../..'); #set working dir to location two folder levels above 1st file
    
        nc<-length(in.reps);
        reps<-vector(mode='list',length=nc);
        if (is.null(cases)) cases<-in.reps;
        names(reps)<-cases;
        for (ic in 1:nc){
            reps[[ic]]<-PBSmodelling::readList(in.reps[ic]);
        }
    }
    
    #create cases, if necessary
    if (is.null(cases)) cases<-names(reps);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        oldpar<-par(oma=c(0.5,1,1,0.5),mar=c(2,5,1,1)+0.2,mfrow=c(3,1));
        on.exit(dev.off());
    } else {
        oldpar<-par(oma=c(0.5,1,1,0.5),mar=c(2,5,1,1)+0.2);
    }
    on.exit(par(oldpar),add=TRUE);
    
    plots<-list();#output list
    
    #-------------------------------------------#
    #plot natural mortality rates
    #-------------------------------------------#
    p<-compareModelResults.NatMort1(reps,cases);
    if (showPlot||!is.null(pdf)) print(p);
    plots$natMort<-p;
    
    #-------------------------------------------#
    #plot Pr(M2M|z)
    #-------------------------------------------#
    p<-compareModelResults.PrM2M(reps,cases);
    if (showPlot||!is.null(pdf)) print(p);
    plots$prM2M<-p;
    
    #-------------------------------------------#
    #plot mean growth rates
    #-------------------------------------------#
    p<-compareModelResults.MeanGrowth(reps,cases);
    if (showPlot||!is.null(pdf)) print(p);
    plots$mnGr<-p;
    
    #-------------------------------------------#
    #plot growth transition matrices
    #-------------------------------------------#
    p<-compareModelResults.GrowthMatrices(reps,cases);
    if (showPlot||!is.null(pdf)) print(p);
    plots$GrTMs<-p;
    
    #-------------------------------------------#
    #plot recruitment size distribution
    #-------------------------------------------#
    dfr<-getMDFR.PopProcesses(reps,type="R_cz");
    p <- ggplot(dfr,aes_string(x='z',y='val',colour='model'));
    p <- p + geom_line();
    p <- p + ylim(c(0,NA));
    p <- p + labs(x="size (mm CW)",y="recruitment size distribution");
    if (showPlot||!is.null(pdf)) print(p);
    plots$R_z<-p;
    
    return(invisible(plots));
}