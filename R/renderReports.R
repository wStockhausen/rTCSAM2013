#'
#'@title Render reports
#'
#'@description renders reports.
#'
#'@param paths - vector of paths to model runs, with case names as names (optional)
#'@param obj - object convertible to a list of tcsam2013.resLst objects, with case names as names (optional)
#'@param out.dir - folder for output
#'@param figures - flag (T/F) to create 
#'@param tables - flag (T/F) to create 
#'@param format - output file format ("word_document","pdf_document", or "html_document") 
#'@param clean - flag (T/F) to create 
#'
#'@return vector with paths to model results as elements, if paths is not NULL.
#'
#'@details Renders createReport.Tables.Rmd and createReport.Figures.Rmd from 
#'the inst/Rmd folder in the rTCAM2013 package. 
#'Values for 'paths' and 'obj' should not be given simultaneously. 
#'Output files will be TCSAM2013ReportFigures.ext and TCSAM2013ReportTables.ext,
#'where 'ext' is 'pdf' or 'docx', depending on the format chosen.
#'
#'@export
#'
renderReports<-function(paths=NULL,
                        obj=NULL,
                        numRecent=15,
                        plot1stObs=TRUE,
                        out.dir=getwd(),
                        figures=TRUE,
                        tables=TRUE,
                        format="word_document",
                        clean=FALSE){
    if (is.null(paths)&is.null(obj)){
        paths<-wtsUtilities::selectFile(ext="par",caption="Select par file from model run");
        names(paths)<-'tcsam2013';
    }
    if (figures){
        rmarkdown::render(system.file("Rmd/createReport.Figures.Rmd", package="rTCSAM2013"),
                          output_format=format[1],
                          output_file=paste0("TCSAM2013ReportFigures",ifelse(format[1]=='word_document','.docx','.pdf')),
                          output_dir=out.dir,
                          intermediates_dir=out.dir,
                          params=list(paths=paths,obj=obj,numRecent=numRecent,plot1stObs=plot1stObs),
                          clean=FALSE);
    }
    if (tables){
        cat("out.dir = '",out.dir,"'\n",sep='')
        rmarkdown::render(system.file("Rmd/createReport.Tables.Rmd", package="rTCSAM2013"),
                          output_format=format[1],
                          output_file=paste0("TCSAM2013ReportTables",ifelse(format[1]=='word_document','.docx','.pdf')),
                          output_dir=out.dir,
                          intermediates_dir=out.dir,
                          params=list(paths=paths,obj=obj),
                          clean=FALSE);
    }
    return(paths);
}
