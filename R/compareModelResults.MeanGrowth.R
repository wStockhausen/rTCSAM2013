#'
#'@title Compare mean post-molt growth among several model runs
#'
#'@description Function to compare mean post-molt growth among several model runs.
#'
#'@param reps - list of objects derived from Jack's R files for the models to be compared
#'@param cases - vector of labels for model cases (if 'reps' is not given)
#'@param showPlot - flag to print plot to current device
#'@param pdf - name for output pdf file
#'
#'@details If 'reps' is not given, then the user is prompted to select Jack's R file output from each 
#'model to be compared. If 'cases' is given, the user is prompted to select the file
#'corresponding to each case. If 'cases' is not given, then the user may select an 
#'arbitrary number of files (one at a time), ending selection by pressing 'cancel' on the selection box'.\cr\cr
#'If 'reps' is not given, the working directory is set two levels above the 1st model case file selected.\cr\cr
#'Uses \code{PBSmodelling::readList} and \code{wtsUtilities::selectFile}.
#'
#'@return ggplot object
#'
#'@import ggplot2
#'
#'@export
#'
compareModelResults.MeanGrowth<-function(reps=NULL,
                                         cases=NULL,
                                         showPlot=FALSE,
                                         pdf=NULL){
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
    } else {
        cases<-names(reps);
    }

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
    }
    
    #----------------------------------
    # plot mean growth
    #----------------------------------
    dfr<-getMDFR.PopProcesses(reps,type="mnZAM_cxz");
    
    p <- ggplot(dfr,aes_string(x='z',y='val',colour='model'));
    p <- p + geom_line(size=1.5);
    p <- p + geom_abline(slope=1,intercept=0,linetype=2)
    p <- p + facet_grid(x~.);
    p <- p + labs(x="Pre-molt Size (mm CW)", y="Mean Post-molt Size (mm CW)");
    if (showPlot||!is.null(pdf)) print(p);

    return(p);
}