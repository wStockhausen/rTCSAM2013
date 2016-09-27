#'
#'@title Get predicted population quantities (time series) from several model runs
#'
#'@description Function to get predicted population quantities (time series) from 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst objects
#'@param type - population quantity to retrieve
#'@param verbose - flag (T/F) to print debug info
#'
#'@details Potential values for 'type' are:
#'\itemize{
#'  \item {'MB_yx' - mature biomass at spawning time (1000's t)}
#'  \item {'R_y' - recruitment (millions)}
#'  \item {'iN_xmsz' - initial (July 1) abundance by x,m,s,z (millions)}
#'  \item {'fN_xmsz' - final (July 1) abundance by x,m,s,z (millions)}
#'  \item {'N_yxmsz' - annual (July 1) abundance by x,m,s,z (millions)}
#'  \item {'N_yxmz'  - annual (July 1) abundance by x,m,z (millions)}
#'  \item {'N_yxz'   - annual (July 1) abundance by x,z (millions)}
#'  \item {'N_yxms'  - annual (July 1) abundance by x,m,s (millions)}
#'  \item {'N_yxm'   - annual (July 1) abundance by x,m (millions)}
#'  \item {'N_yx'    - annual (July 1) abundance by x (millions)}
#'}
#'
#'@details Uses \code{reshape2::melt} and \code{reshape2::dcast}.
#'
#'@return dataframe
#'
#'@export
#'
getMDFR.PopQuantities<-function(obj,
                                type=c("MB_yx","R_y","iN_xmsz","fN_xmsz","N_yxmsz","N_yxmz","N_yxms","N_yxm","N_yx"),
                                verbose=FALSE){

    if (type[1]=="MB_yx"){
        dfr<-getMDFR.MatureBiomass(obj);
        return(dfr);
    }
    if (type[1]=="R_y"){
        dfr<-getMDFR.Recruitment(obj);
        return(dfr);
    }

    lst<-convertToListOfResults(obj);
    cases<-names(lst);
    
    tinfo<-getTimeInfo(lst);
    styr<-tinfo$styr;
    endyr<-tinfo$endyr;
    years<-tinfo$years;

    #----------------------------------
    #population abundance (millions)
    #----------------------------------
    if ((substr(type[1],1,4)=="N_yx")||(type[1]=="iN_xmsz")||(type[1]=="fN_xmsz")){
        dfr<-NULL;
        for (case in cases){
            #INF
            val=(lst[[case]]$rep)[["pop.NatZ.INF"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            if (type[1]=="iN_xmsz") dfrp<-dfrp[dfrp$y==styr[[case]],];
            if (type[1]=="fN_xmsz") dfrp<-dfrp[dfrp$y==endyr[[case]],];
            dfrp<-cbind(case=case,
                        x="female",m="immature",s="new shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
            #IOF
            val=(lst[[case]]$rep)[["pop.NatZ.IOF"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            if (type[1]=="iN_xmsz") dfrp<-dfrp[dfrp$y==styr[[case]],];
            if (type[1]=="fN_xmsz") dfrp<-dfrp[dfrp$y==endyr[[case]],];
            dfrp<-cbind(case=case,
                        x="female",m="immature",s="old shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
            #MNF
            val=(lst[[case]]$rep)[["pop.NatZ.MNF"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            if (type[1]=="iN_xmsz") dfrp<-dfrp[dfrp$y==styr[[case]],];
            if (type[1]=="fN_xmsz") dfrp<-dfrp[dfrp$y==endyr[[case]],];
            dfrp<-cbind(case=case,
                        x="female",m="mature",s="new shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
            #MOF
            val=(lst[[case]]$rep)[["pop.NatZ.MOF"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            if (type[1]=="iN_xmsz") dfrp<-dfrp[dfrp$y==styr[[case]],];
            if (type[1]=="fN_xmsz") dfrp<-dfrp[dfrp$y==endyr[[case]],];
            dfrp<-cbind(case=case,
                        x="female",m="mature",s="old shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
            #INM
            val=(lst[[case]]$rep)[["pop.NatZ.INM"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            if (type[1]=="iN_xmsz") dfrp<-dfrp[dfrp$y==styr[[case]],];
            if (type[1]=="fN_xmsz") dfrp<-dfrp[dfrp$y==endyr[[case]],];
            dfrp<-cbind(case=case,
                        x="male",m="immature",s="new shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
            #IOM
            val=(lst[[case]]$rep)[["pop.NatZ.IOM"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            if (type[1]=="iN_xmsz") dfrp<-dfrp[dfrp$y==styr[[case]],];
            if (type[1]=="fN_xmsz") dfrp<-dfrp[dfrp$y==endyr[[case]],];
            dfrp<-cbind(case=case,
                        x="male",m="immature",s="old shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
            #MNM
            val=(lst[[case]]$rep)[["pop.NatZ.MNM"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            if (type[1]=="iN_xmsz") dfrp<-dfrp[dfrp$y==styr[[case]],];
            if (type[1]=="fN_xmsz") dfrp<-dfrp[dfrp$y==endyr[[case]],];
            dfrp<-cbind(case=case,
                        x="male",m="mature",s="new shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
            #MOM
            val=(lst[[case]]$rep)[["pop.NatZ.MOM"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            if (type[1]=="iN_xmsz") dfrp<-dfrp[dfrp$y==styr[[case]],];
            if (type[1]=="fN_xmsz") dfrp<-dfrp[dfrp$y==endyr[[case]],];
            dfrp<-cbind(case=case,
                        x="male",m="mature",s="old shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
        }
        if (type[1]=="iN_xmsz") dfrp<-dfr;
        if (type[1]=="fN_xmsz") dfrp<-dfr;
        if (type[1]=="N_yxmsz") dfrp<-dfr;
        if (type[1]=="N_yxmz"){
            dfrp<-NULL;
            dfrp<-reshape2::dcast(dfr,"case+y+x+m+z~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[6]<-'val';
        }
        if (type[1]=="N_yxz"){
            dfrp<-NULL;
            dfrp<-reshape2::dcast(dfr,"case+y+x+z~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[5]<-'val';
        }
        if (type[1]=="N_yxms"){
            dfrp<-NULL;
            dfrp<-reshape2::dcast(dfr,"case+y+x+m+s~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[6]<-'val';
        }
        if (type[1]=="N_yxm"){
            dfrp<-NULL;
            dfrp<-reshape2::dcast(dfr,"case+y+x+m~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[5]<-'val';
        }
        if (type[1]=="N_yx"){
            dfrp<-NULL;
            dfrp<-reshape2::dcast(dfr,"case+y+x~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[4]<-'val';
        }
        dfrp<-getMDFR.CanonicalFormat(dfrp);
        dfrp$fleet<-'population';
        return(dfrp);
    }
}
