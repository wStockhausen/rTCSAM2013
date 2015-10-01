#'
#'@title Function to get natural mortality rates by year from an .std file or dataframe
#'
#'@description This function get natural mortality estimates by year
#'   and sex for immatures, new shell matures and old shell matures.
#'   
#'@param sdobj - object (dataframe) obtained from reading the TCSAM2013 .std file
#'@param styr - model start year
#'@param endyr - model end year
#'
#'@return dataframe with columns sex,shell,maturity,year,val,std
#'
#'@export
#'
getNatMort<-function(sdobj,
                       styr=NULL,
                       endyr=NULL){
  if (is.null(styr)){
      stop("getNatMort(): Must supply styr\nAborting...\n");
  }    
  if (is.null(endyr)){
      stop("getNatMort(): Must supply endyr\nAborting...\n");
  }    
  idx.M.imm<-which(sdobj[[2]]=="sdrNatMortImm",arr.ind=TRUE)
  idx.M.NS<-which(sdobj[[2]]=="sdrNatMortNS",arr.ind=TRUE)
  idx.M.OS<-which(sdobj[[2]]=="sdrNatMortOS",arr.ind=TRUE)
  
  M.imm.est<-matrix(data=sdobj[idx.M.imm,3],nrow=(endyr-styr+1),ncol=2)
  M.imm.std<-matrix(data=sdobj[idx.M.imm,4],nrow=(endyr-styr+1),ncol=2)
  M.NS.est<-matrix(data=sdobj[idx.M.NS,3],nrow=(endyr-styr+1),ncol=2)
  M.NS.std<-matrix(data=sdobj[idx.M.NS,4],nrow=(endyr-styr+1),ncol=2)
  M.OS.est<-matrix(data=sdobj[idx.M.OS,3],nrow=(endyr-styr+1),ncol=2)
  M.OS.std<-matrix(data=sdobj[idx.M.OS,4],nrow=(endyr-styr+1),ncol=2)
  
  yrs<-styr:endyr;
  
  dfr<-NULL;
  sexes<-c('female','male');
  for (x in 1:2){
      dfrp<-data.frame(sex=sexes[x],shell='',maturity='immature',year=yrs,
                       val=M.imm.est[,x],std=M.imm.std[,x]);
      dfr<-rbind(dfr,dfrp);
      dfrp<-data.frame(sex=sexes[x],shell='new',maturity='mature',year=yrs,
                       val=M.NS.est[,x],std=M.NS.std[,x]);
      dfr<-rbind(dfr,dfrp);
      dfrp<-data.frame(sex=sexes[x],shell='old',maturity='mature',year=yrs,
                       val=M.NS.est[,x],std=M.NS.std[,x]);
      dfr<-rbind(dfr,dfrp);
  }
  return(dfr);
}