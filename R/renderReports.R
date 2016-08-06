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
#'@param format - output file format ("word_document","pdf_document","html_document") 
#'@param clean - flag (T/F) to create 
#'
#'@return vector with paths to model results as elements, if paths is not NULL.
#'
#'@details Renders createReport.Tables.Rmd and createReport.Figures.Rmd from 
#'the inst/Rmd folder in the rTCAM2013 package. 
#'Values for 'paths' and 'obj' should not be given simultaneously. 
#'
#'@export
#'
renderReports<-function(paths=NULL,obj=NULL,
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
                          output_format=format,
                          output_file="TCSAM2013ReportFigures.docx",
                          output_dir=out.dir,intermediates_dir=out.dir,
                          params=list(paths=paths,obj=obj),clean=FALSE);
    }
    if (tables){
        cat("out.dir = '",out.dir,"'\n",sep='')
        rmarkdown::render(system.file("Rmd/createReport.Tables.Rmd", package="rTCSAM2013"),
                          output_format=format,
                          output_file="TCSAM2013ReportTables.docx",
                          output_dir=out.dir,intermediates_dir=out.dir,
                          params=list(paths=paths,obj=obj),clean=FALSE);
    }
    return(paths);
}
