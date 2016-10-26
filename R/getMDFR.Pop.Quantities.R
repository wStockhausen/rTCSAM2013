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
#'  \item {'B_yxmsz' - annual (July 1) abundance by x,m,s,z (millions)}
#'  \item {'B_yxmz'  - annual (July 1) abundance by x,m,z (millions)}
#'  \item {'B_yxz'   - annual (July 1) abundance by x,z (millions)}
#'  \item {'B_yxms'  - annual (July 1) abundance by x,m,s (millions)}
#'  \item {'B_yxm'   - annual (July 1) abundance by x,m (millions)}
#'  \item {'B_yx'    - annual (July 1) abundance by x (millions)}
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
getMDFR.Pop.Quantities<-function(obj,
                                 type=c("MB_yx","R_y","iN_xmsz","fN_xmsz",
                                        "B_yxmsz","B_yxmz","B_yxms","B_yxm","B_yx",
                                        "N_yxmsz","N_yxmz","N_yxms","N_yxm","N_yx"),
                                 verbose=FALSE){
    options(stringsAsFactors=FALSE);

    if (type[1]=="MB_yx"){
        dfr<-getMDFR.Pop.MatureBiomass(obj);
        return(dfr);
    }
    if (type[1]=="R_y"){
        dfr<-getMDFR.Pop.Recruitment(obj);
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
    dfrp<-NULL;
    if ((substr(type[1],1,4)=="N_yx")||(type[1]=="iN_xmsz")||(type[1]=="fN_xmsz")){
        dfr<-NULL;
        for (case in cases){
            #INF
            val=(lst[[case]]$rep)[["pop.mod.NatZ.INF"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            if (type[1]=="iN_xmsz") dfrp<-dfrp[dfrp$y==styr[[case]],];
            if (type[1]=="fN_xmsz") dfrp<-dfrp[dfrp$y==endyr[[case]],];
            dfrp<-cbind(case=case,
                        x="female",m="immature",s="new shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
            #MNF
            val=(lst[[case]]$rep)[["pop.mod.NatZ.MNF"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            if (type[1]=="iN_xmsz") dfrp<-dfrp[dfrp$y==styr[[case]],];
            if (type[1]=="fN_xmsz") dfrp<-dfrp[dfrp$y==endyr[[case]],];
            dfrp<-cbind(case=case,
                        x="female",m="mature",s="new shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
            #MOF
            val=(lst[[case]]$rep)[["pop.mod.NatZ.MOF"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            if (type[1]=="iN_xmsz") dfrp<-dfrp[dfrp$y==styr[[case]],];
            if (type[1]=="fN_xmsz") dfrp<-dfrp[dfrp$y==endyr[[case]],];
            dfrp<-cbind(case=case,
                        x="female",m="mature",s="old shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
            #INM
            val=(lst[[case]]$rep)[["pop.mod.NatZ.INM"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            if (type[1]=="iN_xmsz") dfrp<-dfrp[dfrp$y==styr[[case]],];
            if (type[1]=="fN_xmsz") dfrp<-dfrp[dfrp$y==endyr[[case]],];
            dfrp<-cbind(case=case,
                        x="male",m="immature",s="new shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
            #MNM
            val=(lst[[case]]$rep)[["pop.mod.NatZ.MNM"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            if (type[1]=="iN_xmsz") dfrp<-dfrp[dfrp$y==styr[[case]],];
            if (type[1]=="fN_xmsz") dfrp<-dfrp[dfrp$y==endyr[[case]],];
            dfrp<-cbind(case=case,
                        x="male",m="mature",s="new shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
            #MOM
            val=(lst[[case]]$rep)[["pop.mod.NatZ.MOM"]];
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
        if (!is.null(dfrp)){
            dfrp<-rCompTCMs::getMDFR.CanonicalFormat(dfrp);
            dfrp$process<-'population';
        }
        return(dfrp);
    }
    #----------------------------------
    #population biomass (1000's t)
    #----------------------------------
    dfrp<-NULL;
    if ((substr(type[1],1,3)=="B_y")){
        dfr<-NULL;
        for (case in cases){
            #INF
            val=(lst[[case]]$rep)[["pop.mod.BatZ.INF"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            dfrp<-cbind(case=case,
                        x="female",m="immature",s="new shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
            #MNF
            val=(lst[[case]]$rep)[["pop.mod.BatZ.MNF"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            dfrp<-cbind(case=case,
                        x="female",m="mature",s="new shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
            #MOF
            val=(lst[[case]]$rep)[["pop.mod.BatZ.MOF"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            dfrp<-cbind(case=case,
                        x="female",m="mature",s="old shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
            #INM
            val=(lst[[case]]$rep)[["pop.mod.BatZ.INM"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            dfrp<-cbind(case=case,
                        x="male",m="immature",s="new shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
            #MNM
            val=(lst[[case]]$rep)[["pop.mod.BatZ.MNM"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            dfrp<-cbind(case=case,
                        x="male",m="mature",s="new shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
            #MOM
            val=(lst[[case]]$rep)[["pop.mod.BatZ.MOM"]];
            dimnames(val)<-list(y=as.character(years[[case]]),
                                z=as.character((lst[[case]]$rep)[["mod.zBs"]]));
            dfrp<-reshape2::melt(val,value.name='val')
            dfrp<-cbind(case=case,
                        x="male",m="mature",s="old shell",dfrp);
            dfr<-rbind(dfr,dfrp[,c('case','y','x','m','s','z','val')]);
        }
        if (type[1]=="B_yxmsz") dfrp<-dfr;
        if (type[1]=="B_yxmz"){
            dfrp<-NULL;
            dfrp<-reshape2::dcast(dfr,"case+y+x+m+z~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[6]<-'val';
        }
        if (type[1]=="B_yxz"){
            dfrp<-NULL;
            dfrp<-reshape2::dcast(dfr,"case+y+x+z~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[5]<-'val';
        }
        if (type[1]=="B_yxms"){
            dfrp<-NULL;
            dfrp<-reshape2::dcast(dfr,"case+y+x+m+s~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[6]<-'val';
        }
        if (type[1]=="B_yxm"){
            dfrp<-NULL;
            dfrp<-reshape2::dcast(dfr,"case+y+x+m~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[5]<-'val';
        }
        if (type[1]=="B_yx"){
            dfrp<-NULL;
            dfrp<-reshape2::dcast(dfr,"case+y+x~.",fun.aggregate=sum,value.var='val');
            names(dfrp)[4]<-'val';
        }
        if (!is.null(dfrp)){
            dfrp<-rCompTCMs::getMDFR.CanonicalFormat(dfrp);
            dfrp$process<-'population';
        }
        return(dfrp);
    }
}
