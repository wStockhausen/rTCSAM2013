#'
#'@title Function to plot TCSAM2013 input data from a .R file
#'
#'@description This function plots input data to TCSAM2013 from a .R output file
#'
#'@details Creates a set of plots (in a pdf file, if desired) from a TCSAM2013 .R
#'output file.
#'
#'@param lst - the data list object
#'@param fn - the file 
#'@param out.dir - directory to write pdf output to
#'@param pdfFile - (base) filename for output pdf file (if not NULL)
#'
#'@import graphics
#'
#'@export
#'
plotData<-function(lst=NULL,
                    fn=NULL,
                    out.dir=NULL,
                    pdfFile=NULL){
    
    if (is.null(lst)) {
        if (is.null(fn)) fn<-choose.files(default=getwd(),caption="Select TCSAM output .R file",index=1);
        if (length(fn)<1) {cat("No file chosen. canceled.\n"); return;}
        if (is.null(out.dir)) out.dir<-dirname(fn);
        source(fn,local=TRUE);
        lst<-res$model.data;
    }
    
    if (!is.null(pdfFile)){
        pdf(file=file.path(out.dir,pdfFile),width=8.5,height=11);
        on.exit(dev.off(),add=TRUE);
    }
    
    #biological info
    
    #surveys
    surveys<-lst[["surveys"]]
    for (survey in names(surveys)){
        #--abundance
        
        #--size comps
        plotData.Survey.NatZ(surveys[[survey]]$nAtZ$data,
                              label="trawl survey")
    }
    
    #fisheries
    fisheries<-lst[["fisheries"]]
    for (fishery in names(fisheries)){
        #--abundance
        
        #--size comps
        if (fishery=="tcf.r") {
            plotData.Fishery.Retained.NatZ(fisheries[[fishery]]$nAtZ$data,
                                            label="directed fishery: retained males");
        } else {
            plotData.Fishery.Discarded.NatZ(fisheries[[fishery]]$nAtZ$data,
                                             label=fisheries[[fishery]]$name);
        }
    }
    
}
