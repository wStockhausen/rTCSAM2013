#'
#'@title Render report
#'
#'@description renders report.
#'
#'@param paths - list of paths to model runs
#'
#'@return List with paths to model results as elements.
#'
#'@details Renders inst/Rmd/plotTCSAM2013.Rmd in rTCAM2013 package. Requires knittr, rmarkdown.
#'
#'@export
#'
renderReport<-function(paths=NULL,out.dir=getwd()){
    if (is.null(paths)){
        path<-wtsUtilities::selectFile(ext="par",caption="Select par file from model run");
        paths<-list(tcsam1=dirname(path));
    }
    rmarkdown::render(system.file("Rmd/plotTCSAM2013.Rmd", package="rTCSAM2013"),
                      output_dir=out.dir,intermediates_dir=out.dir,
                      params=list(paths=paths),clean=FALSE);
    return(paths);
}