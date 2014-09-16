#'
#'@title Compare changes in objective function components from a base model for several models.
#'
#'@description Function to compare changes in objective function components 
#'from a base model for several models.
#'
#'@param in.csvs - vector of paths to final likelihood components csv files for the models to be compared
#'@param case - plot labels for model cases (same order as in.csvs)
#'
#'@details If in.csvs is not given, then the user is prompted to select final likelihood files 
#'for comparison. If cases is given but not in.csvs, the user is prompted to select the file
#'corresponding to each case. If both 'cases' and 'in.csvs' are NULL, then the user may select an 
#'arbitrary number of files (one at a time), ending selection by pressing 'cancel' on the selection box'.\cr\cr
#'Objective function component comparisons are relative to the first (base) case. Positive
#'differences indicate a better fit (smaller value for the component in the alternative model).
#'
#'@return vector with selected file names as elements (can be used as in.csv on another
#'call to the function)
#'
#'@export
#'
compareModelResults.ObjFunComponents<-function(in.csvs=NULL,
                                               cases=NULL){
    if (is.null(in.csvs)){
        in.csv<-0;
        in.csvs<-vector(mode="character",length=0)
        nc<-0;
        nca<-Inf;
        if (!is.null(cases)) nca<-length(cases);
        cap<-"Select likelihood components csv file (or cancel to end)";
        while (!is.null(in.csv)&&(nc<nca)){
            nc<-nc+1;
            if (!is.null(cases)) {
                cap<-paste("Select likelihood components for '",cases[nc],"' (or cancel) to end",sep='');
            }
            in.csv<-wtsUtilities::selectFile(ext="csv",caption=cap);
            if(is.character(in.csv)) in.csvs<-c(in.csvs,in.csv);
        }
        setwd(dirname(in.csvs[1]));#set working dir to location of 1st file
    }
    
    nc<-length(in.csvs);
    dfrs<-vector(mode='list',length=nc);
    names(dfrs)<-in.csvs;
    for (csv in in.csvs){
        dfrs[[csv]]<-read.csv(csv,header=TRUE,skip=4)
    }

    base<-dfrs[[1]];
    nr<-nrow(base);
    lbls<-base$description;
    
    if (is.null(cases)){
        cases<-vector(mode='character',length=nc)
        for (cs in 1:nc){
            fldr<-dirname(in.csvs[cs])
            strs<-strsplit(fldr,"/",fixed=TRUE)
            ns<-length(strs[[1]]);
            cases[cs]<-strs[[1]][ns-1];
        }
    }
    
    nc1<-nc-1;
    difs<-matrix(nrow=nr,ncol=nc1);
    for (ic in 1:nc1){
        difs[,ic]<-base$objFun-dfrs[[ic+1]]$objFun;
    }
    
    xlim<-range(difs,na.rm=TRUE);
    
    old.par<-par(mai=c(1,6,1,0.5));
    barplot(t(difs[1:18,]),names.arg=lbls[1:18],xlim=xlim,
            beside=TRUE,horiz=TRUE,las=2,cex.names=0.7,
            legend.text=cases[2:nc],args.legend=list(cex=0.7))
    title(main=paste("relative to",cases[1]),cex.main=0.8)
    barplot(t(difs[19:nr,]),names.arg=lbls[19:nr],xlim=xlim,
            beside=TRUE,horiz=TRUE,las=2,cex.names=0.7,
            legend.text=cases[2:nc],args.legend=list(cex=0.7))
    title(main=paste("relative to",cases[1]),cex.main=0.8)
    
    return(in.csvs);
}