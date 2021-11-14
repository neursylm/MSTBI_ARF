R.Version4RUN<-343;
R.LibLocation <- "C:/Users/Dell/AppData/Roaming/EmpowerRCH/R343/library"
#***************** Regarding ALL Following R Functions ********************
#***** COPYRIGHT (c) 2010 X&Y Solutions, ALL RIGHT RESERVED ***************
#******************* www.EmpowerStats.com *********************************
#**************************************************************************
Sys.setlocale("LC_TIME", "C")
library(doBy,lib.loc=R.LibLocation)
library(plotrix,lib.loc=R.LibLocation)
library(stringi,lib.loc=R.LibLocation)
library(stringr,lib.loc=R.LibLocation)
library(survival,lib.loc=R.LibLocation)
library(rms,lib.loc=R.LibLocation)
library(nnet,lib.loc=R.LibLocation)
library(car,lib.loc=R.LibLocation)
library(mgcv,lib.loc=R.LibLocation)
pdfwd<-6; pdfht<-6
setwd("D:/datasets/ABI_RF/PROJ617_9_tbl")
load("D:/datasets/ABI_RF/data617_2.Rdata")
if (length(which(ls()=="EmpowerStatsR"))==0) EmpowerStatsR<-get(ls()[1])
names(EmpowerStatsR)<-toupper(names(EmpowerStatsR))


recodevar <- function (var,oldcode,newcode) {
  tmp.v <- var
  nc.tmp <- length(oldcode)
  for (i in (1:nc.tmp)) {tmp.v[(var==oldcode[i])]=newcode[i]}
  if (is.factor(tmp.v)) {tmp.v1<-as.numeric(as.character(tmp.v))} else {tmp.v1<-as.numeric(tmp.v)}
  rm(tmp.v);  return(tmp.v1)
} 


attach(EmpowerStatsR)
sink("D:/datasets/ABI_RF/datastep/PROJ617_datastep.lst")
print("Creating new variable: GCS.CONT")
GCS.CONT<-GCS
EmpowerStatsR<-cbind(EmpowerStatsR,GCS.CONT)
print("Creating new variable: VENTRICLE_SCORES.NEW")
VENTRICLE_SCORES.NEW<- recodevar(VENTRICLE_SCORES,c(0,1,2,3,4,5,6,7,8,9,10,13),c(0,1,1,1,1,1,1,1,1,1,1,1))
EmpowerStatsR<-cbind(EmpowerStatsR,VENTRICLE_SCORES.NEW)
print("Creating new variable: MLS")
MLS<-rep(NA,times=nrow(EmpowerStatsR))
tmp<- (is.na(MLS) & (MIDLINE.SHIFT==0))
tmp[is.na(tmp)]<-FALSE
MLS[tmp]<-0
tmp<- (is.na(MLS) & (MIDLINE.SHIFT>0))
tmp[is.na(tmp)]<-FALSE
MLS[tmp]<-1
EmpowerStatsR<-cbind(EmpowerStatsR,MLS)
rm(GCS.CONT,VENTRICLE_SCORES.NEW,MLS)
detach(EmpowerStatsR)
sink()
vname<-c("_N_","_STAT_","_TOTAL_","ID","RESPIRATORY.FAILURE","RESPIRATORY.FAILURE.0","RESPIRATORY.FAILURE.1")
vlabel<-c("样本量(%)","统计量","合计","ID","RESPIRATORY.FAILURE","  0","  1")
vname<-c(vname,"DISCHARGE.OUTCOME","DISCHARGE.OUTCOME.0","DISCHARGE.OUTCOME.1","DISCHARGE.OUTCOME.2")
vlabel<-c(vlabel,"DISCHARGE.OUTCOME","  0","  1","  2")
vname<-c(vname,"GENDER","GENDER.0","GENDER.1","AGE","GCS","GCS.3","GCS.4","GCS.5","GCS.6","GCS.7","GCS.8","GCS.9","GCS.10","GCS.11","GCS.12")
vlabel<-c(vlabel,"GENDER","  0","  1","AGE","GCS","  3","  4","  5","  6","  7","  8","  9","  10","  11","  12")
vname<-c(vname,"MUTIPLE","MUTIPLE.1","MUTIPLE.2","EDH","EDH.0","EDH.1")
vlabel<-c(vlabel,"MUTIPLE","  1","  2","EDH","  0","  1")
vname<-c(vname,"EDH_VOLUMN","SDH","SDH.0","SDH.1","SDH_VOLUMN")
vlabel<-c(vlabel,"EDH_VOLUMN","SDH","  0","  1","SDH_VOLUMN")
vname<-c(vname,"CONTUSION","CONTUSION.0","CONTUSION.1","CONTUSION_VOLUMN")
vlabel<-c(vlabel,"CONTUSION","  0","  1","CONTUSION_VOLUMN")
vname<-c(vname,"MIDLINE.SHIFT","RING.CISTERN","RING.CISTERN.0","RING.CISTERN.1","RING.CISTERN.2")
vlabel<-c(vlabel,"MIDLINE.SHIFT","RING.CISTERN","  0","  1","  2")
vname<-c(vname,"VENTRICLE_SCORES","VENTRICLE_SCORES.0","VENTRICLE_SCORES.1","VENTRICLE_SCORES.2","VENTRICLE_SCORES.3","VENTRICLE_SCORES.4","VENTRICLE_SCORES.5","VENTRICLE_SCORES.6","VENTRICLE_SCORES.7","VENTRICLE_SCORES.8","VENTRICLE_SCORES.9","VENTRICLE_SCORES.10","VENTRICLE_SCORES.13")
vlabel<-c(vlabel,"VENTRICLE_SCORES","  0","  1","  2","  3","  4","  5","  6","  7","  8","  9","  10","  13")
vname<-c(vname,"SAH_FISHER_GRADE","SAH_FISHER_GRADE.1","SAH_FISHER_GRADE.2","SAH_FISHER_GRADE.3","SAH_FISHER_GRADE.4")
vlabel<-c(vlabel,"SAH_FISHER_GRADE","  1","  2","  3","  4")
vname<-c(vname,"WITHIN_48","WITHIN_48.0","WITHIN_48.1","LOH")
vlabel<-c(vlabel,"WITHIN_48","  0","  1","LOH")
vname<-c(vname,"LOICU","MONEY","CAUSE","DM","DM.0","DM.1")
vlabel<-c(vlabel,"LOICU","MONEY","CAUSE","DM","  0","  1")
vname<-c(vname,"HTN","HTN.0","HTN.1","STROKE","STROKE.0","STROKE.1")
vlabel<-c(vlabel,"HTN","  0","  1","STROKE","  0","  1")
vname<-c(vname,"OTHERS","OTHERS.0","OTHERS.1","SMOKE","SMOKE.0","SMOKE.1")
vlabel<-c(vlabel,"OTHERS","  0","  1","SMOKE","  0","  1")
vname<-c(vname,"COPD","COPD.0","COPD.1","HEART","HEART.0","HEART.1")
vlabel<-c(vlabel,"COPD","  0","  1","HEART","  0","  1")
vname<-c(vname,"ADMISION.DATE","ADMIT.TIME","PREOSPITAL","PREOSPITAL.1","PREOSPITAL.1.5","PREOSPITAL.10","PREOSPITAL.11","PREOSPITAL.12","PREOSPITAL.13","PREOSPITAL.14","PREOSPITAL.15","PREOSPITAL.16","PREOSPITAL.17","PREOSPITAL.19","PREOSPITAL.2","PREOSPITAL.20","PREOSPITAL.21","PREOSPITAL.22","PREOSPITAL.23","PREOSPITAL.24","PREOSPITAL.26","PREOSPITAL.3","PREOSPITAL.30","PREOSPITAL.32","PREOSPITAL.33","PREOSPITAL.34","PREOSPITAL.36","PREOSPITAL.4","PREOSPITAL.40","PREOSPITAL.5","PREOSPITAL.6","PREOSPITAL.7","PREOSPITAL.8","PREOSPITAL.9")
vlabel<-c(vlabel,"ADMISION.DATE","ADMIT.TIME","PREOSPITAL","  1","  1.5","  10","  11","  12","  13","  14","  15","  16","  17","  19","  2","  20","  21","  22","  23","  24","  26","  3","  30","  32","  33","  34","  36","  4","  40","  5","  6","  7","  8","  9")
vname<-c(vname,"OR.DATE","DISCHARGE.DATE","MULITPLE_INJURY_TYPES")
vlabel<-c(vlabel,"OR.DATE","DISCHARGE.DATE","MULITPLE_INJURY_TYPES")
vname<-c(vname,"PRE.DISEASES.X","PRE.MEDCATION","GCS.CONT")
vlabel<-c(vlabel,"PRE.DISEASES.X","PRE.MEDCATION","GCS")
vname<-c(vname,"VENTRICLE_SCORES.NEW","VENTRICLE_SCORES.NEW.0","VENTRICLE_SCORES.NEW.1")
vlabel<-c(vlabel,"VENTRICLE_SCORES NEW","  0","  1")
vname<-c(vname,"MLS","MLS.0","MLS.1")
vlabel<-c(vlabel,"MLS","  0","  1")
slt.vname<-c()

library(gdata,lib.loc=R.LibLocation)
library(geepack,lib.loc=R.LibLocation)
library(mgcv,lib.loc=R.LibLocation)
 
ofname<-"PROJ617_9_tbl"; 
WD<-EmpowerStatsR; wd.subset=""; 
svy.DSN.YN <- FALSE; 
weights<-1;weights.var <- NA; 
WD<-cbind(WD,weights); WD<-WD[!is.na(weights),]; 
title<-"分层分析"; 
attach(WD) 
subjvname<-NA; 
yv<-cbind(RESPIRATORY.FAILURE); 
yvname<-c('RESPIRATORY.FAILURE'); 
yvar<-c('RESPIRATORY_FAILURE'); 
ydist<-c('binomial'); 
ylink<-c('logit'); 
ylv<-c(2); 
xv<-cbind(MUTIPLE); 
xvname<-c('MUTIPLE'); 
xvar<-c('MUTIPLE'); 
xlv<-c(2); 
sxf<-c(NA,0)[-1]; 
sv<-cbind(AGE,GENDER,GCS.CONT); 
svname<-c('AGE','GENDER','GCS.CONT'); 
svar<-c('AGE','GENDER','GCS_CONT'); 
sdf<-c(NA,0,0,0)[-1]; 
slv<-c(0,2,0); 
av<-cbind(MIDLINE.SHIFT,VENTRICLE_SCORES.NEW,SAH_FISHER_GRADE,RING.CISTERN); 
avname<-c('MIDLINE.SHIFT','VENTRICLE_SCORES.NEW','SAH_FISHER_GRADE','RING.CISTERN'); 
if (!is.na(avname[1])) avlbl<-vlabel[match(avname, vname)]; 
nadj<-length(avname);alv<-c(0,2,4,3); 
saf<-NA; 
timev<-NA; timevname<-NA; 
bv<-NA; bvar<-NA; 
colv<-NA; colvname<-NA; 
v.start<-NA; vname.start<-NA; 
v.stop<-NA; vname.stop<-NA; 
par1<-NA;dec<-2;parm<-c(NA, NA, 1,10, 0); 
if (!exists("pdfwd")) pdfwd<-6; 
if (!exists("pdfht")) pdfht<-6; 
##R package## gdata geepack mgcv ##R package##;
pvformat<-function(p,dec) {
  pp <- sprintf(paste("%.",dec,"f",sep=""),as.numeric(p))
  if (is.matrix(p)) {pp<-matrix(pp, nrow=nrow(p)); colnames(pp)<-colnames(p);rownames(pp)<-rownames(p);}
  lw <- paste("<",substr("0.00000000000",1,dec+1),"1",sep="");
  pp[as.numeric(p)<(1/10^dec)]<-lw
  return(pp)
}
numfmt<-function(p,dec) {
  if (is.list(p)) p<-as.matrix(p)
  pp <- sprintf(paste("%.",dec,"f",sep=""),as.numeric(p))
  if (is.matrix(p)) {pp<-matrix(pp, nrow=nrow(p));colnames(pp)<-colnames(p);rownames(pp)<-rownames(p);}
  pp[as.numeric(p)>10000000]<- "inf."
  pp[is.na(p) | gsub(" ","",p)==""]<- ""
  pp[p=="-Inf"]<-"-Inf"
  pp[p=="Inf"]<-"Inf"
  return(pp)
}

mat2htmltable<-function(mat) {
  t1<- apply(mat,1,function(z) paste(z,collapse="</td><td>"))
  t2<- paste("<tr><td>",t1,"</td></tr>")
  return(paste(t2,collapse=" "))
}
setgam<-function(fml,yi) {
  if (ydist[yi]=="") ydist[yi]<-"gaussian"
  if (ydist[yi]=="exact") ydist[yi]<-"binomial"
  if (ydist[yi]=="breslow") ydist[yi]<-"binomial"
  if (ydist[yi]=="gaussian") mdl<-try(gam(formula(fml),weights=wdtmp$weights,data=wdtmp, family=gaussian(link="identity")))
  if (ydist[yi]=="binomial") mdl<-try(gam(formula(fml),weights=wdtmp$weights,data=wdtmp, family=binomial(link="logit")))
  if (ydist[yi]=="poisson") mdl<-try(gam(formula(fml),weights=wdtmp$weights,data=wdtmp, family=poisson(link="log")))
  if (ydist[yi]=="gamma") mdl<-try(gam(formula(fml),weights=wdtmp$weights,data=wdtmp, family=Gamma(link="inverse")))
  if (ydist[yi]=="negbin") mdl<-try(gam(formula(fml),weights=wdtmp$weights,data=wdtmp, family=negbin(c(1,10), link="log")))
  return(mdl)
}
setgee<-function(fml,yi) {
  if (ydist[yi]=="") ydist[yi]<-"gaussian"
  if (ydist[yi]=="exact") ydist[yi]<-"binomial"
  if (ydist[yi]=="breslow") ydist[yi]<-"binomial"
  if (ydist[yi]=="gaussian") md<-try(geeglm(formula(fml),id=wdtmp[,subjvname],corstr=gee.TYPE,family="gaussian",weights=wdtmp$weights,data=wdtmp))
  if (ydist[yi]=="binomial") md<-try(geeglm(formula(fml),id=wdtmp[,subjvname],corstr=gee.TYPE,family="binomial",weights=wdtmp$weights,data=wdtmp))
  if (ydist[yi]=="poisson") md<-try(geeglm(formula(fml),id=wdtmp[,subjvname],corstr=gee.TYPE,family="poisson",weights=wdtmp$weights,data=wdtmp))
  if (ydist[yi]=="gamma") md<-try(geeglm(formula(fml),id=wdtmp[,subjvname],corstr=gee.TYPE,family="Gamma",weights=wdtmp$weights,data=wdtmp))
  if (ydist[yi]=="negbin") md<-try(geeglm.nb(formula(fml),id=wdtmp[,subjvname],corstr=gee.TYPE,weights=wdtmp$weights,data=wdtmp))
  return(md)
}
setglm<-function(fml,yi) {
  if (ydist[yi]=="") ydist[yi]<-"gaussian"
  if (ydist[yi]=="exact") ydist[yi]<-"binomial"
  if (ydist[yi]=="breslow") ydist[yi]<-"binomial"
  if (ydist[yi]=="gaussian") md<-try(glm(formula(fml),family="gaussian",weights=wdtmp$weights,data=wdtmp))
  if (ydist[yi]=="binomial") md<-try(glm(formula(fml),family="binomial",weights=wdtmp$weights,data=wdtmp))
  if (ydist[yi]=="poisson") md<-try(glm(formula(fml),family="poisson",weights=wdtmp$weights,data=wdtmp))
  if (ydist[yi]=="gamma") md<-try(glm(formula(fml),family="Gamma",weights=wdtmp$weights,data=wdtmp))
  if (ydist[yi]=="negbin") md<-try(glm.nb(formula(fml),weights=wdtmp$weights,data=wdtmp))
  return(md)
}
mdl2oo<-function(mdl, xxname, opt) {
  if (is.na(mdl[[1]][1])) return(list(rep("",times=length(xxname)),""))
  if (substr(mdl[[1]][1],1,5)=="Error") return(list(rep("",times=length(xxname)),""))
  decp<-dec+2; if (decp>4) decp<-4;
  gs<-summary(mdl); print(mdl$formula); print(gs)
  if (opt=="gam") {gsparm <- gs$p.table;tmpn<-gs$n;
  } else {gsparm <- gs$coefficients;tmpn <- sum(gs$df[c(1,2)]);}
  gsp<-gsparm[match(xxname,rownames(gsparm)),]
  if (length(xxname)==1) {beta<-gsp[1]; se<-gsp[2]; pv<-gsp[4]; 
  } else {beta<-gsp[,1]; se<-gsp[,2]; pv<-gsp[,4]; }
  ci1<- beta-1.96*se; ci2<- beta+1.96*se
  pvx<-substr(rep("****",length(pv)),1,(pv<=0.05)+(pv<=0.01)+(pv<=0.001)) 
  if (colprn==3) {pvv<-pvx;} else {pvv<-pvformat(pv,decp);}
  if ((colprn!=2) & (gs$family[[2]]=="log" | gs$family[[2]]=="logit")) {
    o1<-paste(numfmt(exp(beta),dec)," (",numfmt(exp(ci1),dec),", ",numfmt(exp(ci2),dec),")",sep="")
  } else {
    if (colprn<3) {o1<-paste(numfmt(beta,dec), " (",numfmt(ci1,dec),", ",numfmt(ci2,dec),")",sep="")
    } else {o1<-paste(numfmt(beta,dec), "+",numfmt(se,dec),sep="");}
  }   
  o1<-paste(o1,pvv)
  if (length(xxname)>1) {
    o1[1]<-""; o1[2]<-"0";
    if (gs$family[[2]]=="log" | gs$family[[2]]=="logit") o1[2]<-"1.0"
  }
  return(list(o1,tmpn))
}
recodevar <- function (var,oldcode,newcode) {
  tmp.v <- var
  nc.tmp <- length(oldcode)
  for (i in (1:nc.tmp)) {tmp.v[(var==oldcode[i])]=newcode[i]}
  if (is.factor(tmp.v)) {tmp.v1<-as.numeric(as.character(tmp.v))} else {tmp.v1<-as.numeric(tmp.v)}
  rm(tmp.v);  return(tmp.v1)
} 
rankvar <- function(var, num) {
  qprobs <- 1/num
  if (num>2) {for (i in (2:(num-1))) {qprobs <- c(qprobs, 1/num * i) } } 
  outvar <- rep(0, times=length(var))
  outvar[is.na(var)] <- NA
  cutpoints <- quantile(var,probs=qprobs, na.rm=TRUE)
  for (k in (1:length(cutpoints))) { outvar[var>=cutpoints[k]] <- k; }
  return(outvar)
}
removeNA<-function(i,j,k,wdf) {
 vvv<-c(yvname[i],avname[j],xvname[k],subjvname,bvar,vname.start,vname.stop,timevname); 
 vvv<-vvv[!is.na(vvv)]; vvv<-vvv[vvv>" "]
 tmp<-is.na(wdf[,vvv]); 
 return(wdf[apply(tmp,1,sum)==0,])
}
varstats<-function(k,j,wdtmp0) {
  vk<-xvname[k]; klv<-xxlvl_[[k]];nklv<-length(klv);
  if (!is.na(j)) {vj<-avname[j]; jlv<-aalvl_[[j]];njlv<-length(jlv);} else njlv<-0;
  if (njlv<=2) {
    a<-table(wdtmp0[,vk])
    return(cbind(xxlbl_[[k]],c(" ",a[match(klv,names(a))])))
  } else {
    a<-table(wdtmp0[,vk],wdtmp0[,vj])
    a<-a[match(klv,rownames(a)),match(jlv,colnames(a))]
    oon<-c(" ",matrix(t(cbind(" ",a)),ncol=1))
    t1<-matrix(rep(paste("&nbsp&nbsp",aalbl_[[j]][-1]),nklv),ncol=nklv)
    return(cbind(c(xb[k],matrix(rbind(xxlbl[[k]],t1),ncol=1)),oon))
  }
}
vlabelN<-(substr(vlabel,1,1)==" ");
vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN];
w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
if (!is.na(svname[1])) {
  if (sum((sdf=="s" | sdf=="S") & slv>0)>0) w<-c(w,"</br>Spline smoothing only applies for continuous variables")
  sdf[slv>0]<-"";
  if (!is.na(subjvname) & sum(sdf=="s" | sdf=="S")>0) {
    sdf<-rep(0,length(svname));
    w<-c(w,"</br>Generalized estimate equation could not be used with spline smoothing terms")
  }
}
if (is.na(sxf[1])) sxf<-rep(0,length(xvname))
if (sum(xlv==0 & sxf==0)>0) w<-c(w,"</br>Stratified variables should be categorical")
if (sum(sxf>1 & xlv>0)>0) w<-c(w,"Categorizing only applies to continuous variables");
allvname<-c(yvname,xvname,svname,bvar,avname,subjvname,vname.start,vname.stop,timevname,"weights"); 
allvname<-allvname[!is.na(allvname)]
WD<-WD[,allvname];
if (!is.na(subjvname)) WD<-WD[order(WD[,subjvname]),]
rm(xv,yv,bv,av,colv,v.start,v.stop)
sxf[xlv>0]<-0
sxf[xlv==0 & sxf==0]<-3
nx<-length(xvname)
if (sum(sxf>1 & xlv==0)>0) {
     t.xname<-NA;t.xlv<-NA; 
     for (i in 1:nx) {
       if (sxf[i]>1 & xlv[i]==0) {
          tmp.Xi<- rankvar(WD[,xvname[i]],sxf[i])
          tmp.newcode <- tapply(WD[,xvname[i]],tmp.Xi,function(z) median(z,na.rm=TRUE))
          tmp.low <- tapply(WD[,xvname[i]],tmp.Xi,function(z) min(z,na.rm=TRUE))
          tmp.upp <- tapply(WD[,xvname[i]],tmp.Xi,function(z) max(z,na.rm=TRUE))
          tmp.Xi2<- recodevar(tmp.Xi,(1:sxf[i])-1,tmp.newcode)
          tmp.Xi<-cbind(tmp.Xi,tmp.Xi2)
          tmp.NM<-paste(xvname[i],c("grp","grp.cont"),sep=".")
          colnames(tmp.Xi)<-tmp.NM
          WD<-cbind(WD,tmp.Xi)
          t.xname<-c(t.xname,tmp.NM)
          t.xlv<-c(t.xlv,sxf[i],0)
          vnameV<-c(vnameV,tmp.NM)
          vlabelV<-c(vlabelV,paste(vlabelV[vnameV==xvname[i]],c("group","group trend")))
          vnameZ<-c(vnameZ,paste(tmp.NM[1],(1:sxf[i])-1,sep="."))
          vlabelZ<-c(vlabelZ,paste(tmp.low,"-",tmp.upp))
       } else {
          t.xname<-c(t.xname,xvname[i]); t.xlv<-c(t.xlv,xlv[i])
       }
     }
     xvname<-t.xname[-1]; xlv<-t.xlv[-1];
}
xvname<-xvname[xlv>0]; xlv<-xlv[xlv>0]; nx<-length(xvname)
xxlbl_<-list(NA); xxlvl_<-list(NA); xxlbl<-list(NA); 
xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
for (j in 1:nx) {
  tmp<-levels(factor(WD[,xvname[j]])); ntmp<-length(tmp)
  xxlvl_[[j+1]]<-tmp;
  tmp<-paste(xvname[j],".",tmp,sep="")
  xxlbl_[[j+1]]<-paste(c("",rep("&nbsp&nbsp",ntmp)),c(xb[j],vlabelZ[match(tmp,vnameZ)]))
  xxlbl[[j+1]]<-paste(xb[j],"=",vlabelZ[match(tmp,vnameZ)])
}
xxlbl_<-xxlbl_[-1]; xxlvl_<-xxlvl_[-1]; xxlbl<-xxlbl[-1]

avname<-c(avname[alv<=2],avname[alv>2])
alv<-c(alv[alv<=2],alv[alv>2]); alv[alv<=2]<-0;
na<-length(avname); na0<-sum(alv<=2); na1<-sum(alv>2);
ab<-vlabelV[match(avname,vnameV)]
avname_<-avname
avname_[alv>2]<-paste("factor(",avname[alv>2],")",sep="")
aaname_<-list(NA); aalbl_<-list(NA); aalvl_<-list(NA)
for (j in (1:na)) {
  if (alv[j]==0) {
    aaname_[[j+1]]<-avname[j];aalbl_[[j+1]]<-ab[j];aalvl_[[j+1]]<-0
  } else {
    aalvl_[[j+1]]<-levels(factor(WD[,avname[j]]))
    tmp<-paste(avname[j],".",aalvl_[[j+1]],sep="")
    aalbl_[[j+1]]<-c(ab[j],vlabelZ[match(tmp,vnameZ)])
    aalbl_[[j+1]]<-paste(c("",rep("&nbsp&nbsp",length(aalbl_[[j+1]])-1)),aalbl_[[j+1]])
    aaname_[[j+1]]<-c(avname[j],paste("factor(",avname[j],")",aalvl_[[j+1]],sep=""))
  }
}
aaname_<-aaname_[-1]; aalbl_<-aalbl_[-1]; aalvl_<-aalvl_[-1]

ns=0; svb=""; smoothsv<-0; fmladj<-"";
if (!is.na(svname[1])) {
  svb<-vlabelV[match(svname,vnameV)];
  svname_ <- svname 
  smoothsvi<-((sdf=="s" | sdf=="S") & slv==0)
  smoothsv<-sum(smoothsvi)
  svname_[smoothsvi]<-paste("s(",svname[smoothsvi],")",sep="")
  svb1<-svb
  svb1[smoothsvi]<-paste(svb[smoothsvi],"(Smooth)",sep="")
  svname_[slv>0]<-paste("factor(",svname[slv>0],")",sep="")
  fmladj<-paste("+",paste(svname_,collapse="+"))
}

ny<-length(yvname);
yb<-vlabelV[match(yvname,vnameV)]; yb[is.na(yb)]<-yvname[is.na(yb)]

if (is.na(bvar)) {
  blvb<-"Total"; blvb_<-"Total"
} else {
  blv<-levels(factor(WD[,bvar])); nblv<-length(blv)+1
  blvb_<-vlabelZ[match(paste(bvar,".",blv,sep=""),vnameZ)]; blvb_[is.na(blvb_)]<-blv[is.na(blvb_)];
  blvb<-c(paste(vlabelV[vnameV==bvar],blvb_,sep="="),"Total");
  blvb_<-c(blvb_,"Total")
  WD<-WD[!is.na(WD[,bvar]),]
}
opt<-ifelse(!is.na(subjvname), "gee", ifelse(smoothsv>0, "gam", "glm"));

if (!is.na(bvar)) {prn<-"S";
} else if (na==1 & alv[1]>2) {prn<-"CX";
} else if (ny>1 & ny<5) {prn<-"Y";
} else if (na>1 & na<5 & sum(alv>2)==0) {prn<-"X";
} else {prn<-"Y";}
colprn<-parm[3];
if (is.na(parm[4]) < 5) parm[4] = 10
if (parm[4] == "" | parm[4] < 5) parm[4] = 5
minstsize = parm[4] + ns
sink(paste(ofname,".lst",sep=""))
w<-c(w,paste("<h2>", title, "</h2>"))

if (prn=="Y") {
  tt<-rep(NA,ny+2); nn<-c("Exposure","Sub-group",yb)
  for (j in 1:na) {
     tt<-rbind(tt,c(paste("X=",ab[j]),rep(" ",ny+1)),c("","N",yb));
     for (k in 1:nx) {
       wdtmp0<-removeNA(NA,j,k,WD)
       colk<-varstats(k,j,wdtmp0);
       nlvl<-length(xxlvl_[[k]])
       nnk<-cbind(ab[j],xxlbl[[k]])
       for (i in 1:ny) {
          wdtmp0<-removeNA(i,j,k,WD)
          fml<-paste(yvname[i],"~",avname_[j],fmladj)
          tmpc<-""; tmpnn<-"";
          for (v in 1:nlvl) {
            print(xxlbl[[k]][v]);
            wdtmp<-wdtmp0[wdtmp0[,xvname[k]]==xxlvl_[[k]][v],]
            tmpn<-nrow(wdtmp); tmpnn<-c(tmpnn,tmpn)
            if (tmpn>minstsize) {
              if (opt=="gam") tmp.mdl<-setgam(fml,i)
              if (opt=="gee") tmp.mdl<-setgee(fml,i)
              if (opt=="glm") tmp.mdl<-setglm(fml,i)
              tmpooi<-mdl2oo(tmp.mdl,aaname_[[j]],opt)
              tmpc<-c(tmpc,tmpooi[[1]]);
            } else {tmpc<-c(tmpc,rep(" ",length(aalbl_[[j]])));}
          }
          colk<-cbind(colk,tmpc); nnk<-cbind(nnk,tmpnn[-1])
       }
       tt<-rbind(tt,colk); nn<-rbind(nn,nnk)
     }
  }
}

if (prn=="X") {
  tt<-rep(NA,na+2); nn<-c("Outcome","Sub-group",ab)
  for (i in 1:ny) {
     tt<-rbind(tt,c(paste("Y=",yb[i]),rep(" ",na+1)),c("","N",ab));
     for (k in 1:nx) {
       wdtmp0<-removeNA(i,NA,k,WD)
       colk<-varstats(k,NA,wdtmp0);
       nlvl<-length(xxlvl_[[k]])
       nnk<-cbind(yb[i],xxlbl[[k]])
       for (j in 1:na) {
          wdtmp0<-removeNA(i,j,k,WD)
          fml<-paste(yvname[i],"~",avname_[j],fmladj)
          tmpc<-""; tmpnn<-"";
          for (v in 1:nlvl) {
            print(xxlbl[[k]][v]);
            wdtmp<-wdtmp0[wdtmp0[,xvname[k]]==xxlvl_[[k]][v],]
            tmpn<-nrow(wdtmp); tmpnn<-c(tmpnn,tmpn)
            if (tmpn>minstsize) {
              if (opt=="gam") tmp.mdl<-setgam(fml,i)
              if (opt=="gee") tmp.mdl<-setgee(fml,i)
              if (opt=="glm") tmp.mdl<-setglm(fml,i)
              tmpooi<-mdl2oo(tmp.mdl,aaname_[[j]],opt)
              tmpc<-c(tmpc,tmpooi[[1]])
            } else {tmpc<-c(tmpc," ");}
          }
          colk<-cbind(colk,tmpc); nnk<-cbind(nnk,tmpnn[-1])
       }
       tt<-rbind(tt,colk); nn<-rbind(nn,nnk)
     }
  }
}
if (prn=="CX") {
  nlv<-length(aalvl_[[1]]); tt<-rep(NA,nlv+2); nn<-c("Outcome","Sub-group",ab[1])
  for (i in 1:ny) {
     tt<-rbind(tt,c(paste("Y=",yb[i]),rep(" ",nlv+1)),c("","N",aalbl_[[1]][-1]));
     for (k in 1:nx) {
       wdtmp0<-removeNA(i,1,k,WD)
       colk<-varstats(k,NA,wdtmp0);
       nlvl<-length(xxlvl_[[k]])
       nnk<-cbind(yb[i],xxlbl[[k]])
       fml<-paste(yvname[i],"~",avname_[1],fmladj,sep="")
       tmpc<-rep("",nlv); tmpnn<-""
       for (v in 1:nlvl) {
          print(xxlbl[[k]][v]);
          wdtmp<-wdtmp0[wdtmp0[,xvname[k]]==xxlvl_[[k]][v],]
          tmpn<-nrow(wdtmp); tmpnn<-c(tmpnn,tmpn)
          if (tmpn>minstsize) {
            if (opt=="gam") tmp.mdl<-setgam(fml,i)
            if (opt=="gee") tmp.mdl<-setgee(fml,i)
            if (opt=="glm") tmp.mdl<-setglm(fml,i)
            tmpooi<-mdl2oo(tmp.mdl,aaname_[[1]],opt)
            tmpc<-rbind(tmpc,tmpooi[[1]][-1])
          } else {
            tmpc<-rbind(tmpc,rep(" ",nlv))
          }
       }
       colk<-cbind(colk,tmpc); nnk<-cbind(nnk,tmpnn[-1])
       tt<-rbind(tt,colk); nn<-rbind(nn,nnk)
     }
  }
}
if (prn=="S") {
  tt<-rep(NA,nblv+2); nn<-c("Outcome","Exposure","Sub-group",blvb)
  for (i in 1:ny) {
     tt<-rbind(tt,c(paste("Y=",yb[i]),rep(" ",nblv+1)));
     for (j in 1:na) {
       tt<-rbind(tt,c(paste("X=",ab[j]),rep(" ",nblv+1)),c("","Total N",blvb));
       for (k in 1:nx) {
          wdtmp0<-removeNA(i,j,k,WD)
          colk<-varstats(k,j,wdtmp0);
          nlvl<-length(xxlvl_[[k]])
          nnk<-cbind(yb[i],ab[j],xxlbl[[k]])
          fml<-paste(yvname[i],"~",avname_[j],fmladj)
          for (b in 1:nblv) {
            if (b<nblv) {wdtmp1<-wdtmp0[wdtmp0[,bvar]==blv[b],];} else {wdtmp1<-wdtmp0;}
            tmpc<-""; tmpnn<-""
            for (v in 1:nlvl) {
              print(paste(blvb[b],xxlbl[[k]][v],sep="; "));
              wdtmp<-wdtmp1[wdtmp1[,xvname[k]]==xxlvl_[[k]][v],]
              tmpn<-nrow(wdtmp); tmpnn<-c(tmpnn,tmpn)
              if (tmpn>minstsize) {
                if (opt=="gam") tmp.mdl<-setgam(fml,i)
                if (opt=="gee") tmp.mdl<-setgee(fml,i)
                if (opt=="glm") tmp.mdl<-setglm(fml,i)
                tmpooi<-mdl2oo(tmp.mdl,aaname_[[j]],opt)
                tmpc<-c(tmpc,tmpooi[[1]]);
              } else {tmpc<-c(tmpc,rep(" ",length(aalbl_[[j]])));}
            }
            colk<-cbind(colk,tmpc); nnk<-cbind(nnk,tmpnn[-1])
          }
          tt<-rbind(tt,colk); nn<-rbind(nn,nnk)
       }
     }
  }
}
tt<-tt[-1,]
sink()
w<-c(w,"</br><table border=3>", mat2htmltable(tt), "</table>")
prnopt<-c("β (95%CI) Pvalue / OR (95%CI) Pvalue", "β (95%CI) Pvalue", "β+se / OR (95%CI) *P<0.05 **P<0.01 ***P<0.001")


w<-c(w,"</br>表中数据：", prnopt[colprn])
w<-c(w,paste(c("</br>结果变量:",paste(yb,collapse="; ")),collapse=" "))
w<-c(w,paste(c("</br>暴露变量:",paste(ab,collapse="; ")),collapse=" "))
if (is.na(svname[1])) svb1<-"None";
w<-c(w,paste(c("</br>调整变量:",paste(svb1,collapse="; ")),collapse=" "))
if (smoothsv>0) w<-c(w,". Generalized additive models were applied")
if (opt=="gee") w<-c(w, paste("</br>Generalized estimate equation were used, subject ID=", subjvname, "(", gee.TYPE,")",sep=""))
w<-c(w,paste("</br>此表用易侕统计软件 (www.empowerstats.com) 和R软件生成，生成日期：",Sys.Date()))
w<-c(w,"</br></br>各模型所用的样本量</br><table border=3>", mat2htmltable(nn), "</table>")
w<-c(w,wd.subset)
w<-c(w,"</body></html>")
fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)

