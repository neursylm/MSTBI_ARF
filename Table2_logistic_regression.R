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
setwd("D:/datasets/ABI_RF/PROJ617_5_tbl")
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
 
ofname<-"PROJ617_5_tbl"; 
WD<-EmpowerStatsR; wd.subset=""; 
svy.DSN.YN <- FALSE; 
weights<-1;weights.var <- NA; 
WD<-cbind(WD,weights); WD<-WD[!is.na(weights),]; 
title<-"多个回归方程"; 
attach(WD) 
subjvname<-NA; 
yv<-cbind(RESPIRATORY.FAILURE); 
yvname<-c('RESPIRATORY.FAILURE'); 
yvar<-c('RESPIRATORY_FAILURE'); 
ydist<-c('binomial'); 
ylink<-c('logit'); 
ylv<-c(2); 
xv<-cbind(SDH,MLS,RING.CISTERN,SAH_FISHER_GRADE,VENTRICLE_SCORES); 
xvname<-c('SDH','MLS','RING.CISTERN','SAH_FISHER_GRADE','VENTRICLE_SCORES'); 
xvar<-c('SDH','MLS','RING_CISTERN','SAH_FISHER_GRADE','VENTRICLE_SCORES'); 
xlv<-c(2,2,3,4,12); 
sxf<-c(NA,0,0,0,0,0)[-1]; 
sv<-cbind(AGE,GENDER,MUTIPLE,SMOKE,GCS.CONT,COPD,DM,HTN,STROKE); 
svname<-c('AGE','GENDER','MUTIPLE','SMOKE','GCS.CONT','COPD','DM','HTN','STROKE'); 
svar<-c('AGE','GENDER','MUTIPLE','SMOKE','GCS_CONT','COPD','DM','HTN','STROKE'); 
sdf<-c(NA,0,0,0,0,0,0,0,0,0)[-1]; 
slv<-c(0,2,2,2,0,2,2,2,2); 
av<-cbind(AGE,GENDER); 
avname<-c('AGE','GENDER'); 
if (!is.na(avname[1])) avlbl<-vlabel[match(avname, vname)]; 
nadj<-length(avname);alv<-c(0,2); 
saf<-c(NA,0,0)[-1]; 
timev<-NA; timevname<-NA; 
bv<-NA; bvar<-NA; 
colv<-NA; colvname<-NA; 
v.start<-NA; vname.start<-NA; 
v.stop<-NA; vname.stop<-NA; 
par1<-1;dec<-2;parm<-c(1,NA, 1,1, 0); 
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

varstats<-function(var,vlvl,dec) {
  if (length(vlvl)==1 & vlvl[1]==0) {
    return(paste(numfmt(mean(var,na.rm=TRUE),dec),numfmt(sd(var,na.rm=TRUE),dec),sep="+"))
  } else {
    a<-table(var)
    b<-matrix(paste(a, " (", numfmt(a/sum(a)*100,dec), "%)",sep=""),ncol=1)
    return(c(" ",b[match(vlvl,names(a))]))
  }
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
  gs<-summary(mdl); print(mdl$formula); print(gs)
  if (opt=="gam") {gsparm <- gs$p.table;tmpn<-gs$n;
  } else {gsparm <- gs$coefficients;tmpn <- sum(gs$df[c(1,2)]);}
  gsp<-gsparm[match(xxname,rownames(gsparm)),]
  if (length(xxname)==1) {beta<-gsp[1]; se<-gsp[2]; pv<-gsp[4]; 
  } else {beta<-gsp[,1]; se<-gsp[,2]; pv<-gsp[,4]; }
  ci1<- beta-1.96*se; ci2<- beta+1.96*se
  pvx<-substr(rep("****",length(pv)),1,(pv<=0.05)+(pv<=0.01)+(pv<=0.001)) 
  if (colprn==3) {pvv<-pvx;} else {pvv<-pvformat(pv,dec+2);}
  if ((colprn!=2) & (gs$family[[2]]=="log" | gs$family[[2]]=="logit")) {
    o1<-paste(numfmt(exp(beta),dec)," (",numfmt(exp(ci1),dec),", ",numfmt(exp(ci2),dec),")",sep="")
  } else {
    if (colprn<3) {o1<-paste(numfmt(beta,dec), " (",numfmt(ci1,dec),", ",numfmt(ci2,dec),")",sep="")
    } else {o1<-paste(numfmt(beta,dec), "+",numfmt(se,dec),sep="");}
  }    
  o1<-paste(o1,pvv);  o1[is.na(beta)]<-NA
  if (length(xxname)>1) {
    if (gs$family[[2]]=="log" | gs$family[[2]]=="logit") {
       o1[is.na(o1) & substr(xxname,1,7)=="factor("]<-"1.0"
    } else {o1[is.na(o1) & substr(xxname,1,7)=="factor("]<-"0";}
    o1[is.na(o1)]<-"";
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
removeNA<-function(i,j,m,wdf) {
 vvv<-c(yvname[i],adjvv[[m]],subjvname,colvname,bvar,vname.start,vname.stop,timevname); 
 if (j<=nx) {vvv<-c(vvv,xvname[j]);} else {vvv<-c(vvv,xvname);}
 vvv<-vvv[!is.na(vvv)]; vvv<-vvv[vvv>" "]
 tmp<-is.na(wdf[,vvv]); 
 return(wdf[apply(tmp,1,sum)==0,])
}
vlabelN<-(substr(vlabel,1,1)==" ");
vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN]
w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
if (!is.na(avname[1])) {
 if (sum((saf=="s" | saf=="S") & alv>0)>0) w<-c(w,"</br>Spline smoothing only applies for continuous variables")
 if (!is.na(subjvname) & (sum((saf=="s" | saf=="S") & alv==0)>0)) w<-c(w,"</br>Generalized estimate equation could not be used with spline smoothing terms")
}
if (!is.na(svname[1])) {
 if (sum((sdf=="s" | sdf=="S") & slv>0)>0) w<-c(w,"</br>Spline smoothing only applies for continuous variables")
 if (!is.na(subjvname) & (sum((sdf=="s" | sdf=="S") & slv==0)>0)) w<-c(w,"</br>Generalized estimate equation could not be used with spline smoothing terms")
}
allvname<-c(yvname,xvname,colvname,bvar,avname,svname,subjvname,vname.start,vname.stop,timevname,"weights"); 
allvname<-allvname[!is.na(allvname)]
WD<-WD[,allvname];
if (!is.na(subjvname)) WD<-WD[order(WD[,subjvname]),]
if (!is.na(sxf[1])) {
  if (sum(sxf>1 & xlv>0)>0) w<-c(w,"Categorizing only applies to continuous variables");
  if (sum(sxf>1 & xlv==0)>0) {
     t.xname<-NA;t.xlv<-NA; nx<-length(xvname)
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
}
rm(xv,yv,bv,av,sv,colv,v.start,v.stop)
if (!is.na(subjvname)) {
 if (!is.na(avname[1])) saf<-rep(0,length(saf)); 
 if (!is.na(svname[1])) sdf<-rep(0,length(sdf)); 
 WD<-WD[order(WD[,subjvname]),];
}
fmlm<-" "; fmlb<-"Non-adjusted"; tmp<-""; adjvv<-list(NA); adjvb<-"None"; 
fmlp<-ifelse(!is.na(subjvname), "gee", "glm");
na=0; avb=""; smoothav<-0; nadjm<-0
if (!is.na(avname[1])) {
  na<-length(avname)
  avb<-vlabelV[match(avname,vnameV)];
  avname_ <- avname 
  smoothavi<-((saf=="s" | saf=="S") & alv==0)
  smoothav<-sum(smoothavi)
  smoothavname<-avname[smoothavi]
  avname_[smoothavi]<-paste("s(",avname[smoothavi],")",sep="")
  avb1<-avb
  avb1[smoothavi]<-paste(avb[smoothavi],"(Smooth)",sep="")
  avname_[alv>0]<-paste("factor(",avname[alv>0],")",sep="")
  fmlm<-c(fmlm,paste("+",paste(avname_,collapse="+")))
  fmlb<-c(fmlb,"Adjust")
  nadjm<-nadjm+1; tmp<-c(tmp,"I"); adjvv[[nadjm+1]]<-avname; 
  adjvb<-c(adjvb, paste(avb1, collapse="; "))
  fmlp<-c(fmlp,ifelse(!is.na(subjvname), "gee", ifelse(smoothav>0, "gam", "glm")))
}
ns=0; svb=""; smoothsv<-0
if (!is.na(svname[1])) {
  svb<-vlabelV[match(svname,vnameV)];
  svname_ <- svname 
  smoothsvi<-((sdf=="s" | sdf=="S") & slv==0)
  smoothsv<-sum(smoothsvi)
  smoothsvname<-svname[smoothsvi]
  svname_[smoothsvi]<-paste("s(",svname[smoothsvi],")",sep="")
  svb1<-svb
  svb1[smoothsvi]<-paste(svb[smoothsvi],"(Smooth)",sep="")
  svname_[slv>0]<-paste("factor(",svname[slv>0],")",sep="")
  fmlm<-c(fmlm,paste("+",paste(svname_,collapse="+")))
  fmlb<-c(fmlb,"Adjust")
  nadjm<-nadjm+1; tmp<-c(tmp,"II"); adjvv[[nadjm+1]]<-svname
  adjvb<-c(adjvb, paste(svb1, collapse="; "))
  fmlp<-c(fmlp,ifelse(!is.na(subjvname), "gee", ifelse(smoothsv>0, "gam", "glm")))
}
if (is.na(parm[1]) & length(fmlm)>1) {
  fmlm<-fmlm[-1]; fmlb<-fmlb[-1]; tmp<-tmp[-1]; adjvv<-adjvv[-1]; adjvb<-adjvb[-1]; fmlp<-fmlp[-1];
}
if (nadjm>1) fmlb<-paste(fmlb,tmp)
nmdl<-length(fmlm)

ny=length(yvname); nx=length(xvname); 
xb<-vlabelV[match(xvname,vnameV)]; xb[is.na(xb)]<-xvname[is.na(xb)]
yb<-vlabelV[match(yvname,vnameV)]; yb[is.na(yb)]<-yvname[is.na(yb)]
xvname_ <- xvname
xvname_[xlv>0]<-paste("factor(",xvname[xlv>0],")",sep="")
xxname_<-list(NA); xxlbl_<-list(NA); xxlvl_<-list(NA)
for (j in (1:nx)) {
  if (xlv[j]==0) {
    xxname_[[j+1]]<-xvname[j];xxlbl_[[j+1]]<-xb[j];xxlvl_[[j+1]]<-0
  } else {
    xxlvl_[[j+1]]<-levels(factor(WD[,xvname[j]]))
    tmp<-paste(xvname[j],".",xxlvl_[[j+1]],sep="")
    xxlbl_[[j+1]]<-c(xb[j],vlabelZ[match(tmp,vnameZ)])
    xxlbl_[[j+1]]<-paste(c("",rep("&nbsp&nbsp",length(xxlbl_[[j+1]])-1)),xxlbl_[[j+1]])
    xxname_[[j+1]]<-c(xvname[j],paste("factor(",xvname[j],")",xxlvl_[[j+1]],sep=""))
  }
}
xxname_<-xxname_[-1]; xxlbl_<-xxlbl_[-1]; xxlvl_<-xxlvl_[-1];
if (nx==1) par1<-1; 
if (is.na(par1)) par1<-1;
if (par1>1) {
  tmp1<-xxname_[[1]]; tmp2<-xxlbl_[[1]]
  for (j in 2:nx) {tmp1<-c(tmp1,xxname_[[j]]); tmp2<-c(tmp2,xxlbl_[[j]]);}
  xxname_[[nx+1]]<-tmp1; xxlbl_[[nx+1]]<-tmp2;
  xvname_<-c(xvname_,paste(xvname_,collapse="+"))
}
contx<-(sum(xlv>0)==0)
if (par1==3 & !is.na(bvar)) {w<-c(w,"</br>Column stratified variable was ignored"); bvar<-NA; bvname<-NA;}

if (is.na(bvar) & !is.na(colvname) & nmdl==1 & par1!=3) {if ((ny==1) | (nx==1 & contx)) {bvar<-colvname; colvname<-NA;}}
if (is.na(colvname)) {
  nclv<-1; clvb<-"Total"; clvb_<-"Total"
} else {
  clv<-levels(factor(WD[,colvname])); nclv<-length(clv)+1
  clvb_<-vlabelZ[match(paste(colvname,".",clv,sep=""),vnameZ)]; clvb_[is.na(clvb_)]<-clv[is.na(clvb_)];
  clvb<-c(paste(vlabelV[vnameV==colvname],clvb_,sep="="),"Total");
  clvb_<-c(clvb_,"Total")
  WD<-WD[!is.na(WD[,colvname]),]
} 
if (is.na(bvar)) {
  blvb<-"Total"; blvb_<-"Total"
} else {
  blv<-levels(factor(WD[,bvar])); nblv<-length(blv)+1
  blvb_<-vlabelZ[match(paste(bvar,".",blv,sep=""),vnameZ)]; blvb_[is.na(blvb_)]<-blv[is.na(blvb_)];
  blvb<-c(paste(vlabelV[vnameV==bvar],blvb_,sep="="),"Total");
  blvb_<-c(blvb_,"Total")
  WD<-WD[!is.na(WD[,bvar]),]
}
aa<-c(1,2,3,4)
for (i in 1:4) {
      for (j in c(1:4)[-i]) {
          for (k in c(1:4)[-c(i,j)]) aa<-rbind(aa,c(i,j,k,c(1:4)[-c(i,j,k)]))
      }
}
if (is.na(parm[4])) parm[4]<-1
rord<-aa[parm[4],]
if (!is.na(bvar)) {prn<-"S";
} else {
  if (parm[4]>1) {
    rordc<-ifelse(rord[4]==1,3,4)
    prn<-c("G","Y","M","X")[rord[rordc]]
    if (prn=="X") {
      if (!contx & nx>1) prn<-ifelse(nmdl>1, "M", ifelse(ny>nx & contx, "X", "Y"))
      if (!contx & nx==1) prn<-"CX"
      if (par1==2) {tmp<-ifelse(rordc==4,ifelse(rord[3]==1,2,3),2); prn<-c("G","Y","M","X")[rord[tmp]];}
    }
  } else {
    prn<-ifelse(nmdl>1, "M", ifelse(ny>nx & contx, "X", "Y"))
    if (par1==2) prn<-ifelse(nmdl>1, "M", "Y")
  }
  if (par1==3) prn<-"UM"
}
colprn<-parm[3];
sink(paste(ofname,".lst",sep=""))
if (par1==2) {xbgn<-nx+1; xend<-nx+1;} else {xbgn<-1; xend<-nx;}
if (prn=="Y") {
  tt<-c(0,0,0,0,"Exposure",yb); nn<-c(0,0,0,0,yb); 
  for (k in (1:nclv)) {
    wdtmp0<-WD;
    if (!is.na(colvname)) {
      if (k<nclv) wdtmp0<-WD[WD[,colvname]==clv[k],];
      print(paste("Stratified by",colvname, ":", clvb[k]))
    } 
    for (m in 1:nmdl) {
      for (j in (xbgn:xend)) {
        colj<-cbind(k,0,m,j,xxlbl_[[j]])
        nnj <-c(k,0,m,j)
        for (i in (1:ny)) {
          fml<-paste(yvname[i],"~",xvname_[j],fmlm[m]);
          wdtmp<-removeNA(i,j,m,wdtmp0)
          if (!is.na(colvname)) {if (k==nclv) fml<-paste(fml,"+factor(",colvname,")",sep="");}
          if (fmlp[m]=="gam") tmp.mdl<-setgam(fml,i)
          if (fmlp[m]=="gee") tmp.mdl<-setgee(fml,i)
          if (fmlp[m]=="glm") tmp.mdl<-setglm(fml,i)
          tmpooi<-mdl2oo(tmp.mdl,xxname_[[j]],fmlp[m])
          colj<-cbind(colj,tmpooi[[1]]); nnj<-c(nnj,tmpooi[[2]])
        }
        tt<-rbind(tt,colj); nn<-rbind(nn,nnj)
      }
    }
  }
} 
if (prn=="S") {
  tt<-c(0,0,0,0,"Exposure",blvb); nn<-c(0,0,0,0,blvb);
  for (k in (1:nclv)) {
    wdtmp0<-WD;
    if (!is.na(colvname)) {
      if (k<nclv) wdtmp0<-WD[WD[,colvname]==clv[k],];
      print(paste("Stratified by",colvname, ":", clvb[k]))
    } 
    for (i in (1:ny)) {
      for (m in 1:nmdl) {
        for (j in (xbgn:xend)) {
          colj<-cbind(k,i,m,j,xxlbl_[[j]]);
          nnj <- c(k,i,m,j)
          for (b in (1:nblv)) {
            print(paste("Stratified by",bvar, ":", blvb[b]))
            fml<-paste(yvname[i],"~",xvname_[j],fmlm[m]);
            if (b<nblv) {
              wdtmp1<-wdtmp0[wdtmp0[,bvar]==blv[b],];
            } else {
              wdtmp1<-wdtmp0; fml<-paste(fml,"+factor(",bvar,")",sep="");
            }
            wdtmp<-removeNA(i,j,m,wdtmp1)
            if (!is.na(colvname)) {if (k==nclv) fml<-paste(fml,"+factor(",colvname,")",sep="");}
            if (fmlp[m]=="gam") tmp.mdl<-setgam(fml,i)
            if (fmlp[m]=="gee") tmp.mdl<-setgee(fml,i)
            if (fmlp[m]=="glm") tmp.mdl<-setglm(fml,i)
            tmpooi<-mdl2oo(tmp.mdl,xxname_[[j]],fmlp[m])
            colj<-cbind(colj,tmpooi[[1]]); nnj<-c(nnj,tmpooi[[2]])
          }
          tt<-rbind(tt,colj); nn<-rbind(nn,nnj)
        }
      }
    }
  }
} 
if (prn=="M") {
  tt<-c(0,0,0,0,"Exposure",fmlb);  nn<-c(0,0,0,0,fmlb)
  for (k in (1:nclv)) {
    wdtmp0<-WD; 
    if (!is.na(colvname)) {
      if (k<nclv) wdtmp0<-WD[WD[,colvname]==clv[k],];
      print(paste("Stratified by",colvname, ":", clvb[k]))
    } 
    for (i in 1:ny) {
      for (j in xbgn:xend) {
        colj<-cbind(k,i,0,j,xxlbl_[[j]]); nnj<-c(k,i,0,j)
        for (m in 1:nmdl) {
          fml<-paste(yvname[i],"~",xvname_[j],fmlm[m]);
          wdtmp<-removeNA(i,j,m,wdtmp0)
          if (!is.na(colvname)) {if (k==nclv) fml<-paste(fml,"+factor(",colvname,")",sep="");}
          if (fmlp[m]=="gam") tmp.mdl<-setgam(fml,i)
          if (fmlp[m]=="gee") tmp.mdl<-setgee(fml,i)
          if (fmlp[m]=="glm") tmp.mdl<-setglm(fml,i)
          tmpooi<-mdl2oo(tmp.mdl,xxname_[[j]],fmlp[m])
          colj<-cbind(colj,tmpooi[[1]]); nnj<-c(nnj,tmpooi[[2]])
        }
        tt<-rbind(tt,colj); nn<-rbind(nn,nnj)
      }
    }
  }
}
if (prn=="X") {
  tt<-c(0,0,0,0,"Outcome",xb); nn<-c(0,0,0,0,xb); 
  for (k in (1:nclv)) {
    wdtmp0<-WD;
    if (!is.na(colvname)) {
      if (k<nclv) wdtmp0<-WD[WD[,colvname]==clv[k],];
      print(paste("Stratified by",colvname, ":", clvb[k]))
    } 
    for (m in 1:nmdl) {
      for (i in (1:ny)) {
        colj<-cbind(k,i,m,0,yb[i])
        nnj <-c(k,i,m,0)
        for (j in (1:nx)) {
          fml<-paste(yvname[i],"~",xvname_[j],fmlm[m]);
          wdtmp<-removeNA(i,j,m,wdtmp0)
          if (!is.na(colvname)) {if (k==nclv) fml<-paste(fml,"+factor(",colvname,")",sep="");}
          if (fmlp[m]=="gam") tmp.mdl<-setgam(fml,i)
          if (fmlp[m]=="gee") tmp.mdl<-setgee(fml,i)
          if (fmlp[m]=="glm") tmp.mdl<-setglm(fml,i)
          tmpooi<-mdl2oo(tmp.mdl,xxname_[[j]],fmlp[m])
          colj<-cbind(colj,tmpooi[[1]]); nnj<-c(nnj,tmpooi[[2]])
        }
        tt<-rbind(tt,colj); nn<-rbind(nn,nnj)
      }
    }
  }
} 
if (prn=="CX") {
  tt<-c(0,0,0,0,"Outcome",xxlbl_[[1]][-1]); nn<-c(0,0,0,0,xb[1]); 
  nxl<-length(xxlbl_[[1]])-1
  for (k in (1:nclv)) {
    wdtmp0<-WD;
    if (!is.na(colvname)) {
      if (k<nclv) wdtmp0<-WD[WD[,colvname]==clv[k],];
      print(paste("Stratified by",colvname, ":", clvb[k]))
    } 
    for (m in 1:nmdl) {
      for (i in (1:ny)) {
          colj<-c(k,i,m,0,yb[i])
          nnj <-c(k,i,m,0)
          fml<-paste(yvname[i],"~",xvname_[1],fmlm[m]);
          wdtmp<-removeNA(i,1,m,wdtmp0)
          if (!is.na(colvname)) {if (k==nclv) fml<-paste(fml,"+factor(",colvname,")",sep="");}
          if (fmlp[m]=="gam") tmp.mdl<-setgam(fml,i)
          if (fmlp[m]=="gee") tmp.mdl<-setgee(fml,i)
          if (fmlp[m]=="glm") tmp.mdl<-setglm(fml,i)
          tmpooi<-mdl2oo(tmp.mdl,xxname_[[1]],fmlp[m])
          colj<-c(colj,tmpooi[[1]][-1]); nnj<-c(nnj,tmpooi[[2]])
          tt<-rbind(tt,colj); nn<-rbind(nn,nnj)
      }
    }
  }
} 
if (prn=="UM") {
  tt<-c(0,0,0,0,"Exposure","Univariate","Multivariate");  nn<-c(0,0,0,0,"Univariate","Multivariate")
  for (k in (1:nclv)) {
    wdtmp0<-WD; 
    if (!is.na(colvname)) {
      if (k<nclv) wdtmp0<-WD[WD[,colvname]==clv[k],];
      print(paste("Stratified by",colvname, ":", clvb[k]))
    } 
    for (i in 1:ny) {
      for (m in 1:nmdl) {
        colm<-rep(NA,6); nnm<-rep(NA,5)
        for (j in 1:(nx+1)) {
          colj<-cbind(k,i,m,j,xxlbl_[[j]]); nnj<-c(k,i,m,j)
          fml<-paste(yvname[i],"~",xvname_[j],fmlm[m]);
          wdtmp<-removeNA(i,j,m,wdtmp0)
          if (!is.na(colvname)) {if (k==nclv) fml<-paste(fml,"+factor(",colvname,")",sep="");}
          if (fmlp[m]=="gam") tmp.mdl<-setgam(fml,i)
          if (fmlp[m]=="gee") tmp.mdl<-setgee(fml,i)
          if (fmlp[m]=="glm") tmp.mdl<-setglm(fml,i)
          tmpooi<-mdl2oo(tmp.mdl,xxname_[[j]],fmlp[m])
          colj<-cbind(colj,tmpooi[[1]]); nnj<-c(nnj,tmpooi[[2]])
          if (j<=nx) {colm<-rbind(colm,colj); nnm<-rbind(nnm,nnj);
          } else {colm<-cbind(colm[-1,],tmpooi[[1]]); nnm<-cbind(nnm[-1,],tmpooi[[2]]);}
        }
        tt<-rbind(tt,colm); nn<-rbind(nn,nnm)
      }
    }
  }
}
sink()
if (!contx & prn!="CX") rord<-c(rord[rord!=4],4)
if (prn=="X" | prn=="CX") rord<-rord[rord!=4]
if (prn=="Y") rord<-rord[rord!=2]
if (prn=="M") rord<-rord[rord!=3]
if (nx==1 & contx) rord<-rord[rord!=4]
if (nmdl==1) rord<-rord[rord!=3]
if (ny==1) rord<-rord[rord!=2]
if (is.na(colvname)) rord<-rord[rord!=1]
if (length(rord)==0) rord<-1
nrr<-length(rord)
for (i in nrr:1) {nn<-nn[order(as.numeric(nn[,rord[i]])),];tt<-tt[order(as.numeric(tt[,rord[i]])),];}
if (nn[2,4]>0) {nn[,4]<-c("Exposure",xb[as.numeric(nn[-1,4])]); } else {nn<-nn[,-4];}
if (nn[2,3]>0) {nn[,3]<-c("Model",fmlb[as.numeric(nn[-1,3])]); } else {nn<-nn[,-3];}
if (nn[2,2]>0) {nn[,2]<-c("Outcome",yb[as.numeric(nn[-1,2])]); } else {nn<-nn[,-2];}
if (!is.na(colvname)) {nn[,1]<-c(vlabel[vname==colvname],clvb_[as.numeric(nn[-1,1])]);} else {nn<-nn[,-1];}
tb<-matrix(as.numeric(tt[,c(1:4)]),ncol=4); 
if (!is.na(colvname)) {tt[,1]<-c(vlabelV[vnameV==colvname],clvb[tb[-1,1]]);}
if (ny>1) {tt[,2]<-c("Outcome",yb[tb[-1,2]]);}
if (nmdl>1) {tt[,3]<-c("Model",fmlb[tb[-1,3]]);}
nrr1<-nrr-1; oo<-tt[1,]; nc<-ncol(tt)-5; nr<-nrow(tt)
for (i in 2:nr) {
  if (nrr>1) {
    for (j in 1:nrr1) {
      if (tb[i,rord[j]]!=tb[i-1,rord[j]]) oo<-rbind(oo,c(rep(tt[i,rord[j]],5),rep(" ",nc)))
    }
  }
  oo<-rbind(oo,tt[i,])
}
if (rord[nrr]!=4 & tt[1,rord[nrr]]!="0") {oo<-cbind(oo[,rord[nrr]],oo[,-(1:5)]);} else {oo<-oo[,-(1:4)]}
w<-c(w,paste("<h2>", title, "</h2>"))
w<-c(w,"</br><table border=3>", mat2htmltable(oo), "</table>")
prnopt<-c("β (95%CI) Pvalue / OR (95%CI) Pvalue", "β (95%CI) Pvalue", "β+se / OR (95%CI) *P<0.05 **P<0.01 ***P<0.001")


w<-c(w,"</br>表中数据：", prnopt[colprn])
w<-c(w,paste(c("</br>结果变量:",paste(yb,collapse="; ")),collapse=" "))
w<-c(w,paste(c("</br>暴露变量:",paste(xb,collapse="; ")),collapse=" "))
for (m in 1:nmdl) w<-c(w,paste("</br>",fmlb[m],"model adjust for:", adjvb[m]))
if (smoothav>0 | smoothsv>0) w<-c(w,". Generalized additive models were applied")
if (!is.na(subjvname)) w<-c(w, paste("</br>Generalized estimate equation were used, subject ID=", subjvname, "(", gee.TYPE,")",sep=""))
w<-c(w,paste("</br>此表用易侕统计软件 (www.empowerstats.com) 和R软件生成，生成日期：",Sys.Date()))
w<-c(w,"</br></br>各模型所用的样本量</br><table border=3>", mat2htmltable(nn), "</table>")
w<-c(w,wd.subset)
w<-c(w,"</body></html>")
fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)

