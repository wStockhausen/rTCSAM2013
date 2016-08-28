#'
#' @title Plot TCSAM2013 model results.
#' 
#' @description Function to plot model results from TCSAM2013.
#' 
#' @param obj - object that can be converted into a list of tcsam2013.resLst objects
#' @param numRecent - number of "recent" years to include in "zoom" plots
#' @param plot1stObs - flag to plot observations from the first case, only
#' @param showPlot - flag (T/F) to print plots to screen (if pdf==FALSE)
#' @param pdf - flag (T/F) to print plots to a pdf
#' 
#' @return list of ggplot2 objects.
#' 
#' @details If pdf is TRUE, the file "TCSAM2013.ModelResults.pdf" is created in the 
#' working directory.
#'  
#' @export
#'
#----------------------------------
# Set model variables for plots
plotTCSAM2013I.GG<-function(obj,
                            numRecent=15,
                            plot1stObs=TRUE,
                            showPlot=TRUE,
                            pdf=FALSE){    
    if (is.null(obj)){
        cat("plotTCSAM2013I.GG: 'obj'  is NULL.\n")
        cat("Aborting...\n");
        return(NULL);
    }
    obj<-convertToListOfResults(obj);
    
    #----------------------------------
    # create lists for output
    #----------------------------------
    plots<-list();
    figno<-1;
    
    #----------------------------------
    # Set filename for pdf output
    #----------------------------------
    if (pdf){
        pdf(file="TCSAM2013.ModelResults.pdf",width=8.5,height=11);
        showPlot<-TRUE;
        on.exit(dev.off());
    }

    #----------------------------------
    # Plot objective funtion values
    #----------------------------------
    ps<-compareModelResults.ObjFunComponents(obj,y='objFun',
                                                showPlot=FALSE);
    if (showPlot) figno<-(printGGList(ps,figno=figno,show=TRUE))$figno;
    plots[["objective function components"]]<-ps;

    #----------------------------------
    # Plot parameter values w/ limits and std's
    #----------------------------------
    ps<-compareModelResults.Params(obj);
    if (showPlot) figno<-(printGGList(ps,figno=figno,show=TRUE))$figno;
    plots[["params"]]<-ps;

    #----------------------------------
    # plot population processes
    #----------------------------------
    ps<-compareModelResults.PopProcesses(obj);
    if (showPlot) figno<-(printGGList(ps,figno=figno,show=TRUE))$figno;
    plots[["population processes"]]<-ps;
    
    #----------------------------------
    # plot population quantities
    #----------------------------------
    ps<-compareModelResults.PopQuantities(obj,
                                          numRecent=numRecent);
    if (showPlot) figno<-(printGGList(ps,figno=figno,show=TRUE))$figno;
    plots[["population quantities"]]<-ps;
    
    #----------------------------------
    # plot survey quantities
    #----------------------------------
    ps<-compareModelResults.SurveyQuantities(obj,
                                             numRecent=numRecent,
                                             plot1stObs=plot1stObs,
                                             showPlot=FALSE,
                                             pdf=NULL);
    if (showPlot) figno<-(printGGList(ps,figno=figno,show=TRUE))$figno;
    plots[["surveys"]]<-ps;

    #----------------------------------
    # plot fishery quantities
    #----------------------------------
    ps<-compareModelResults.FisheryQuantities(obj,
                                              numRecent=numRecent,
                                              plot1stObs=plot1stObs,
                                              showPlot=FALSE,
                                              pdf=NULL);
    if (showPlot) figno<-(printGGList(ps,figno=figno,show=TRUE))$figno;
    plots[["fisheries"]]<-ps;

    return(invisible(plots));
}

