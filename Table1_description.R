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
setwd("D:/datasets/ABI_RF/PROJ617_2_tbl")
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
rm(GCS.CONT,VENTRICLE_SCORES.NEW)
detach(EmpowerStatsR)
sink()
vname<-c("_N_","_STAT_","_TOTAL_","ID","RESPIRATORY.FAILURE","RESPIRATORY.FAILURE.0","RESPIRATORY.FAILURE.1")
vlabel<-c("样本量(%)","统计量","合计","ID","RESPIRATORY.FAILURE","  0","  1")
vname<-c(vname,"DISCHARGE.OUTCOME","DISCHARGE.OUTCOME.0","DISCHARGE.OUTCOME.1","DISCHARGE.OUTCOME.2")
vlabel<-c(vlabel,"DISCHARGE.OUTCOME","  0","  1","  2")
vname<-c(vname,"GENDER","GENDER.0","GENDER.1","AGE","MUTIPLE","MUTIPLE.1","MUTIPLE.2")
vlabel<-c(vlabel,"GENDER","  0","  1","AGE","MUTIPLE","  1","  2")
vname<-c(vname,"EDH","EDH.0","EDH.1","EDH_VOLUMN","SDH","SDH.0","SDH.1")
vlabel<-c(vlabel,"EDH","  0","  1","EDH_VOLUMN","SDH","  0","  1")
vname<-c(vname,"SDH_VOLUMN","CONTUSION","CONTUSION.0","CONTUSION.1")
vlabel<-c(vlabel,"SDH_VOLUMN","CONTUSION","  0","  1")
vname<-c(vname,"CONTUSION_VOLUMN","MIDLINE.SHIFT","RING.CISTERN","RING.CISTERN.0","RING.CISTERN.1","RING.CISTERN.2")
vlabel<-c(vlabel,"CONTUSION_VOLUMN","MIDLINE.SHIFT","RING.CISTERN","  0","  1","  2")
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
slt.vname<-c()

library(gdata,lib.loc=R.LibLocation)
 
ofname<-"PROJ617_2_tbl"; 
WD<-EmpowerStatsR; wd.subset=""; 
svy.DSN.YN <- FALSE; 
weights<-1;weights.var <- NA; 
WD<-cbind(WD,weights); WD<-WD[!is.na(weights),]; 
title<-"研究人群描述"; 
attach(WD) 
subjvname<-NA; 
xv<-cbind(GENDER,AGE,MUTIPLE,GCS.CONT,EDH,EDH_VOLUMN,SDH,SDH_VOLUMN,CONTUSION,CONTUSION_VOLUMN,MIDLINE.SHIFT,RING.CISTERN,SAH_FISHER_GRADE,WITHIN_48,LOH,LOICU,DM,HTN,STROKE,OTHERS,SMOKE,COPD,HEART,VENTRICLE_SCORES.NEW,DISCHARGE.OUTCOME); 
xvname<-c('GENDER','AGE','MUTIPLE','GCS.CONT','EDH','EDH_VOLUMN','SDH','SDH_VOLUMN','CONTUSION','CONTUSION_VOLUMN','MIDLINE.SHIFT','RING.CISTERN','SAH_FISHER_GRADE','WITHIN_48','LOH','LOICU','DM','HTN','STROKE','OTHERS','SMOKE','COPD','HEART','VENTRICLE_SCORES.NEW','DISCHARGE.OUTCOME'); 
xvar<-c('GENDER','AGE','MUTIPLE','GCS_CONT','EDH','EDH_VOLUMN','SDH','SDH_VOLUMN','CONTUSION','CONTUSION_VOLUMN','MIDLINE_SHIFT','RING_CISTERN','SAH_FISHER_GRADE','WITHIN_48','LOH','LOICU','DM','HTN','STROKE','OTHERS','SMOKE','COPD','HEART','VENTRICLE_SCORES_NEW','DISCHARGE_OUTCOME'); 
xlv<-c(2,0,2,0,2,0,2,0,2,0,0,3,4,2,0,0,2,2,2,2,2,2,2,2,3); 
sxf<-NA; 
svname<-NA; sv<-NA; slv<-NA; 
av<-NA; avname<-NA; avlbl<-NA; nadj<-0; alv<-NA; 
timev<-NA; timevname<-NA; 
bv<-RESPIRATORY.FAILURE;bvar<-"RESPIRATORY.FAILURE";bvname<-"RESPIRATORY_FAILURE"; 
colv<-NA; colvname<-NA; 
v.start<-NA; vname.start<-NA; 
v.stop<-NA; vname.stop<-NA; 
par1<-NA;dec<-2;parm<-c(NA, NA, 7,NA, 0); 
if (!exists("pdfwd")) pdfwd<-6; 
if (!exists("pdfht")) pdfht<-6; 
##R package## gdata ##R package##;
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
average<-function(mx) {return(mean(mx,na.rm=TRUE))}
mxsum<-function(mx) {return(sum(mx,na.rm=TRUE)) }
stdev<-function(mx) {return(sd(mx,na.rm=TRUE))}
stderr<-function(mx) {return(std.error(mx,na.rm=TRUE))}
mxmedian<-function(mx) {return(median(mx,na.rm=TRUE))}
mxmax<-function(mx) {return(ifelse(sum(!is.na(mx))==0,NA,max(mx,na.rm=TRUE)))}
mxmin<-function(mx) {return(ifelse(sum(!is.na(mx))==0,NA,min(mx,na.rm=TRUE)))}
mxq1<-function(mx) {return(quantile(mx,probs=0.25,na.rm=TRUE))}
mxq3<-function(mx) {return(quantile(mx,probs=0.75,na.rm=TRUE))}
mxp5<-function(mx) {return(quantile(mx,probs=0.05,na.rm=TRUE))}
mxp95<-function(mx) {return(quantile(mx,probs=0.05,na.rm=TRUE))}
mxp90<-function(mx) {return(quantile(mx,probs=0.05,na.rm=TRUE))}
mxp10<-function(mx) {return(quantile(mx,probs=0.05,na.rm=TRUE))}
mx.n<-function(mx) {return(sum(!is.na(mx)))}
gm.n<-function(mx) {return(length(mx[mx>0]))}
gm.mean<-function(mx) {return(exp(average(log(mx[mx>0]))))}
gm.low<-function(mx) {mm<-average(log(mx[mx>0]));  ss<-stderr(log(mx[mx>0]));  return(exp(mm-1.96*ss))}
gm.upp<-function(mx) {mm<-average(log(mx[mx>0]));  ss<-stderr(log(mx[mx>0]));  return(exp(mm+1.96*ss))}
matchbyrowname<-function(mx1,mx2,putColumn1,readColumn2) {
  tmp.1=rownames(mx1)
  if (is.matrix(mx2)) {tmp.2=rownames(mx2)} else {tmp.2=names(mx2)}
  for (i in (1:nrow(mx1))) {
    tmp.r<-which(tmp.2==tmp.1[i])
    if (length(tmp.r)>0) {
      if (is.matrix(mx2)) {mx1[i,putColumn1]<-mx2[tmp.r,readColumn2]} else {mx1[i,putColumn1]<-mx2[tmp.r]}
    }
  }
  return(mx1)
}
varfreqpercent<-function(var) {
  a<-table(var)
  b<-matrix(paste(a, " (", numfmt(a/sum(a)*100,dec), "%)",sep=""),ncol=1)
  rownames(b)<-levels(factor(var))
  return(b)
}
varmeanstd<-function(var) {
 if ((max(var,na.rm=TRUE)==1 | max(var,na.rm=TRUE)==0) & min(var,na.rm=TRUE)==0) {
    return(numfmt(average(var)*100,dec))
 } else {
   return(paste(numfmt(average(var),dec), " + ", numfmt(stdev(var),dec),sep=""))
 }
}
v_univariate<-function(x,group,dec,opt,vname,gname) {
  vnum <-tapply(x,factor(group),function(mx) sum(!is.na(mx)))
  vmean<-numfmt(tapply(x,factor(group),function(mx) mean(mx,na.rm=TRUE)),dec)
  vstd <-numfmt(tapply(x,factor(group),function(mx) sd(mx,na.rm=TRUE)),dec)
  vmedian <-numfmt(tapply(x,factor(group),function(mx) median(mx,na.rm=TRUE)),dec)
  vmin <-numfmt(tapply(x,factor(group),function(mx) ifelse(sum(!is.na(mx))==0,NA,min(mx,na.rm=TRUE))),dec)
  vq1 <-numfmt(tapply(x,factor(group),function(mx) quantile(mx,probs=0.25,na.rm=TRUE)),dec)
  vq3 <-numfmt(tapply(x,factor(group),function(mx) quantile(mx,probs=0.75,na.rm=TRUE)),dec)
  vmax <-numfmt(tapply(x,factor(group),function(mx) ifelse(sum(!is.na(mx))==0,NA,max(mx,na.rm=TRUE))),dec)
  tmp<-table(group)
  if ((length(tmp)>1) & (min(tmp)>1)) {
    pvalue<-try(summary(aov(x~factor(group)))[[1]]$"Pr(>F)"[1])
    if (substr(pvalue[1],1,5)=="Error") {
      pvalue1<-"      ";
    } else {
      pvalue1<-ifelse(pvalue<0.001,"P < 0.001", paste("P =",pvformat(pvalue,3)))
    }
    pvalue.npr<-try(kruskal.test(x~factor(group))$p.value)
    if (substr(pvalue.npr[1],1,5)=="Error") {
      pvalue.npr1<-"     ";
    } else {
      pvalue.npr1<-ifelse(pvalue.npr<0.001, "<0.001",pvformat(pvalue.npr,3))
    }
  } else { 
    pvalue1="        " 
    pvalue.npr1="        "
  }
  if (opt< 8) o1<-cbind(vnum,vmean,vstd,vmedian,vmin,vq1,vq3,vmax)
  if (opt==8) {
    vq1.q3<-paste(vq1,"-",vq3)
    vrange<-paste(vmin,"-",vmax)
    o1<-rbind(vnum,vmean,vstd,vmedian,vq1.q3,vrange)
    pvalue<-c(" ",pvalue1,rep(" ",4))
    o1<-cbind(o1,pvalue)
    rm(vnum,vmean,vstd,vmedian,vmin,vmax,vq1,vq3,pvalue,pvalue1,vq1.q3,vrange)
    o1<-rbind(rep(" ",ncol(o1)),o1)
    rownames(o1)<-c(vname,"  N","  Mean","  SD","  Median","  Q1-Q3","  Min-Max")
    return(list(o1,pvalue.npr1))
    } else {
    if (opt==1) o2<-paste(o1[,2], " &#177 ", o1[,3],sep="")
    if (opt==2) o2<-paste(o1[,2], " (", o1[,3], ") ", o1[,4], " ", o1[,6], "-", o1[,7], sep="")
    if (opt==3) o2<-paste(o1[,2], " (", o1[,3], ") ", o1[,4], " ", o1[,5], "-", o1[,8], sep="")
    if (opt==4) o2<-paste("(",o1[,1],") ",o1[,2], " &#177 ", o1[,3],sep="")
    if (opt==5) o2<-paste("(",o1[,1],") ",o1[,2], " (", o1[,3], ") ", o1[,4], " ", o1[,6], "-", o1[,7], sep="")
    if (opt==6) o2<-paste("(",o1[,1],") ",o1[,2], " (", o1[,3], ") ", o1[,4], " ", o1[,5], "-", o1[,8], sep="")
    if (opt==7) { oo<-rbind(c(rep(" ",8),pvalue1),cbind(o1," ")) } else {oo<-matrix(o2,ncol=1); rm(o2)}
    if (opt<7) {
      oo<-rbind(pvalue1,oo)
      colnames(oo)<-vname
      } else {
      colnames(oo)<-c("N","Mean","SD","Median","Min","Q1","Q3","Max","P value")
    }
    rownames(oo)<-c(gname,paste("  ",names(vnum)))
    rm(o1,vnum,vmean,vstd,vmedian,vmin,vmax,vq1,vq3,pvalue,pvalue1)
    return(oo)
  }
}
rowmatchbind<-function(mx1,mx2,fix9) {
  if (fix9) {
    p1<-mx1[,ncol(mx1)]; p2<-mx2[,ncol(mx2)]
    t1<-t(mx1[,-ncol(mx1)]); t2<-t(mx2[,-ncol(mx2)])
    tt<-t(merge(t1,t2,by="row.names",all=TRUE))
    tt=cbind(tt[-1,],c(p1,p2))
  } else {
    t1<-t(mx1);t2<-t(mx2)
    tt<-t(merge(t1,t2,by="row.names",all=TRUE))
    tt=tt[-1,]
  }
  rownames(tt)=c(rownames(mx1),rownames(mx2))
  return(tt)
}
t1_meanlist<-function(xname,gname,dec,WD1) {
  nc<-length(xname); mx<-WD1[,xname];if (nc==1) mx<-as.matrix(mx,ncol=nc)
  group<-WD1[,gname]; glv<-levels(factor(group))
  xlabel<-vlabelV[match(xname,vnameV)]; 
  xlabel[is.na(xlabel)]<-xname[is.na(xlabel)]
  for (i in (1:nc)) {
    rlist<-v_univariate(mx[,i],group,dec,8,xlabel[i],"")
    r1<-rlist[[1]]
    pv.npr1<-as.matrix(rlist[[2]],nrow=1)
    rownames(pv.npr1)<-xlabel[i]
    if (i==1) {
      rr<-r1;pv.npr<-pv.npr1
      } else {
      rr<-rowmatchbind(rr,r1,TRUE);pv.npr<-rbind(pv.npr,pv.npr1)
      }
    rm(r1,pv.npr1)
  }
  ng<-length(table(group))
  if (length(glv)>1) {
    gclabel<-vlabelZ[match(paste(gname,".",names(table(group)),sep=""),vnameZ)]
    if (ng>1) {colnames(rr)<-c(gclabel,"P value")} else {rr<-rr[,-ncol(rr)];colnames(rr)<-gclabel}
  } else {
    rr<-as.matrix(rr[,-ncol(rr)],ncol=1)
    colnames(rr)="Statistics"
  }
  return(list(rr,pv.npr))
}
t1_mean0 <- function(xname,dec,opt,WD1) {
  nc<-length(xname); mx<-WD1[,xname];if (nc==1) mx<-as.matrix(mx,ncol=nc)
  oo<-nrow(mx)
  vimean<-numfmt(apply(mx,2,average),dec)
  vistd<- numfmt(apply(mx,2,stdev),dec)
  vinn<-apply(!is.na(mx),2,sum)
  vimedian<-numfmt(apply(mx,2,mxmedian),dec)
  vimin<-numfmt(apply(mx,2,mxmin),dec)
  vimax<-numfmt(apply(mx,2,mxmax),dec)
  viq1<-numfmt(apply(mx,2,mxq1),dec)
  viq3<-numfmt(apply(mx,2,mxq3),dec)
  gn<-apply(mx,2,gm.n)
  gmean<-numfmt(apply(mx,2,gm.mean),dec)
  gmeanlow<-numfmt(apply(mx,2,gm.low),dec)
  gmeanupp<-numfmt(apply(mx,2,gm.upp),dec)
  if (opt==1) {oo<-paste(vimean," &#177 ",vistd,sep="")} 
  if (opt==2) {oo<-paste(vimean," (",vistd,") ",vimedian," (",vimin,"-",vimax,")",sep="")}
  if (opt==3) {oo<-paste(vimean," (",vistd,") ",vimedian," (",viq1,"-",viq3,")",sep="")}
  if (opt==4) {oo<-paste(gmean," (",gmeanlow," ",gmeanupp,")",sep="")}
  if (opt==5) {oo<-paste("(",vinn,") ",vimean," &#177 ",vistd,sep="")} 
  if (opt==6) {oo<-paste("(",vinn,") ",vimean," (",vistd,") ",vimedian," (",vimin,"-",vimax,")",sep="")}
  if (opt==7) {oo<-paste("(",vinn,") ",vimean," (",vistd,") ",vimedian," (",viq1,"-",viq3,")",sep="")}
  if (opt==8) {oo<-paste("(",gn,") ",gmean," (",gmeanlow," ",gmeanupp,")",sep="")}
  oo<-matrix(oo,nrow=ncol(mx))
  tmp<-vlabelV[match(xname,vnameV)]; tmp[is.na(tmp)]<-xname[is.na(tmp)]
  rownames(oo)<-tmp
  return(oo)
}
t1_mean <- function(xname, gname, dec, opt, WD1) {
  nc<-length(xname); mx<-WD1[,xname];if (nc==1) mx<-as.matrix(mx,ncol=nc)
  grp<-WD1[,gname]; ngrp<-length(levels(factor(grp)))
  oo<-summary(factor(grp));  oocomp<-is.na(cbind(grp,mx))
  pp<-"     "; st.diff<-"    ";
  for (i in (1:nc)) {
    vimean<-tapply(mx[,i],factor(grp),average)
    vistd<-tapply(mx[,i],factor(grp),stdev)
    vinn<-table(grp[!is.na(mx[,i])])
    vimedian<-numfmt(tapply(mx[,i],factor(grp),mxmedian),dec)
    vimin<-numfmt(tapply(mx[,i],factor(grp),mxmin),dec)
    vimax<-numfmt(tapply(mx[,i],factor(grp),mxmax),dec)
    viq1<-numfmt(tapply(mx[,i],factor(grp),mxq1),dec)
    viq3<-numfmt(tapply(mx[,i],factor(grp),mxq3),dec)
    gn<-tapply(mx[,i],factor(grp),gm.n)
    gmean<-numfmt(tapply(mx[,i],factor(grp),gm.mean),dec)
    gmeanlow<-numfmt(tapply(mx[,i],factor(grp),gm.low),dec)
    gmeanupp<-numfmt(tapply(mx[,i],factor(grp),gm.upp),dec)
    grpcomp<-grp[apply(oocomp[,c(1,i+1)],1,sum)==0]
    if (opt==4 | opt==8) {
       mxi<-mx[,i]; grpi<-grp
       mxi2<-log(mxi[mxi>0]);grpi2<-grpi[mxi>0]
       if (length(levels(factor(grpi2)))>1) {
         pvalue<-summary(aov(mxi2~factor(grpi2)))[[1]]$"Pr(>F)"[1]
         pp1<-ifelse(pvalue<0.001, "<0.001",pvformat(pvalue,3))
         } else {pp1<-"     "}
       } else {
       if (length(levels(factor(grpcomp)))>1) {  
         pvalue<-summary(aov(mx[,i]~factor(grp)))[[1]]$"Pr(>F)"[1]
         pp1<-ifelse(pvalue<0.001, "<0.001",pvformat(pvalue,3))
         } else {pp1<-"     "}    
    }
    if (length(levels(factor(grpcomp)))>1) {  
       pvalue.npr<-kruskal.test(mx[,i]~factor(grp))$p.value
       pp1.npr<-ifelse(pvalue.npr<0.001, "<0.001",pvformat(pvalue.npr,3))
    } else {pp1.npr<-"     "}
    if (ngrp==2) {
      stddiff <- abs(vimean[2] - vimean[1])/sqrt((vistd[2]^2 + vistd[1]^2)/2)
      se <- sqrt((vinn[1]+vinn[2])/(vinn[1]*vinn[2]) + stddiff^2/(2*(vinn[1]+vinn[2])))
      stddiff.l <- stddiff - 1.96 * se
      stddiff.u <- stddiff + 1.96 * se
      vi.stdiff<-paste(numfmt(stddiff,dec), " (", numfmt(stddiff.l,dec), ", ", numfmt(stddiff.u,dec), ")", sep="")
      st.diff<-rbind(st.diff,vi.stdiff)
    }
    pp<-rbind(pp,cbind(pp1,pp1.npr))
    if (opt==1) {ooi<-paste(numfmt(vimean,dec)," &#177 ",numfmt(vistd,dec),sep="")} 
    if (opt==2) {ooi<-paste(numfmt(vimean,dec)," (",numfmt(vistd,dec),") ",vimedian," (",vimin,"-",vimax,")",sep="")}
    if (opt==3) {ooi<-paste(numfmt(vimean,dec)," (",numfmt(vistd,dec),") ",vimedian," (",viq1,"-",viq3,")",sep="")}
    if (opt==4) {ooi<-paste(gmean," (",gmeanlow," ",gmeanupp,")",sep="")}
    if (opt==5) {ooi<-paste("(",vinn,") ",numfmt(vimean,dec)," &#177 ",numfmt(vistd,dec),sep="")} 
    if (opt==6) {ooi<-paste("(",vinn,") ",numfmt(vimean,dec)," (",numfmt(vistd,dec),") ",vimedian," (",vimin,"-",vimax,")",sep="")}
    if (opt==7) {ooi<-paste("(",vinn,") ",numfmt(vimean,dec)," (",numfmt(vistd,dec),") ",vimedian," (",viq1,"-",viq3,")",sep="")}
    if (opt==8) {ooi<-paste("(",gn,") ",gmean," (",gmeanlow," ",gmeanupp,")",sep="")}
    oo=rbind(oo,ooi)
  }
  tmp.cname<-vlabelZ[match(paste(gname,".",colnames(oo),sep=""),vnameZ)]
  if (ngrp==2) {
    oo<-cbind(oo,st.diff,pp)
    colnames(oo)<-c(tmp.cname,"Standardize diff.","P value", "P value*")
  } else {
    oo=cbind(oo,pp)
    colnames(oo)<-c(tmp.cname,"P value", "P value*")
  }
  tmp<-vlabelV[match(xname,vnameV)]; tmp[is.na(tmp)]<-xname[is.na(tmp)]
  rownames(oo)<-c("N", tmp) 
  return(oo)
}
t1_freq0 <- function(xname,WD1) {
  nc<-length(xname); mx<-WD1[,xname];if (nc==1) mx<-as.matrix(mx,ncol=nc)
  for (i in (1:nc)) {
    tt<-table(mx[,i],useNA="no")
    ss<-round(tt/sum(tt)*100,dec)
    ooi<-paste(format(tt)," (",numfmt(ss,dec),"%)",sep="")
    ooi<-matrix(c(" ",ooi),ncol=1)
    tmp.rname<-vlabelZ[match(paste(xname[i],".",names(tt),sep=""),vnameZ)]
    tmp.rname[is.na(tmp.rname)]<-names(tt)
    tmp<-vlabelV[match(xname[i],vnameV)]; if (is.na(tmp)) tmp<-xname[i]
    rownames(ooi)<-c(tmp,paste("    ",tmp.rname,sep=""))
    ifelse(i==1, oo<-ooi, oo<-rbind(oo,ooi))
  } 
  return(oo)
}
t1_freq <- function(xname, gname, WD1) {
  nc<-length(xname); mx<-WD1[,xname];if (nc==1) mx<-as.matrix(mx,ncol=nc)
  grp<-WD1[,gname];  ngrp<-length(levels(factor(grp)))
  for (i in (1:nc)) {
    t1<-table(mx[,i],factor(grp),useNA="no")
    pvalue<-chisq.test(t1,correct=FALSE)$p.value
    pp1<-ifelse(pvalue<0.001, "<0.001", pvformat(pvalue,3))
    ooi<-cbind(matrix(rep(" ",times=ncol(t1)),nrow=1), pp1)
    pp1.exact="-"
	coltot <- min(apply(t1,2,sum))
	rowtot <- min(apply(t1,1,sum))
	if (min(c(coltot, rowtot)) < 10) {
      if ((coltot*rowtot/sum(t1)<10) & max(dim(t1))<4) {
        pexact <- try(fisher.test(t1)$p.value)
        if (substr(pexact,1,5)!="Error") pp1.exact<-ifelse(pexact<0.001,"<0.001",pvformat(pexact,3))
      }
	}	 
    p1<-prop.table(t1,2)
    tb1<-matrix(paste(format(t1)," (", numfmt(p1*100,dec), "%)", sep=""),nrow=nrow(t1))
    if (ngrp==2) {
      t <- p1[-1, 2]; c <- p1[-1, 1];  k <- nrow(p1)-1;   r <- k
      s <- matrix(rep(0, k * r), ncol = k)
      for (ii in 1:k) {
        for (j in 1:r) {
          if (ii == j) {s[ii, j] <- 0.5 * (t[ii]*(1-t[ii])+c[ii]*(1-c[ii]))
          } else {s[ii, j] <- -0.5 * (t[ii]*t[j] + c[ii]*c[j])
          }
        }
      }
      e <- rep(1, k);  e <- diag(e);   s <- solve(s, e)
      tc1 <- t - c
      tc2 <- t - c
      stddiff <- sqrt(t(tc1) %*% s %*% tc2)
      n1 <- sum(t1[,1]); n2 <- sum(t1[,2]); n <- n1+n2
      se <- sqrt(1/(n1/n*n2) + stddiff^2/(2*n))
      stddiff.l <- stddiff - 1.96 * se
      stddiff.u <- stddiff + 1.96 * se
      vi.stdiff<-paste(numfmt(stddiff,dec), " (", numfmt(stddiff.l,dec), ", ", numfmt(stddiff.u,dec), ")", sep="")
      ooi<-cbind(matrix(rep(" ",times=ncol(t1)),nrow=1), vi.stdiff, pp1, pp1.exact)
      tb1<-cbind(tb1,matrix(" ",nrow=nrow(tb1),ncol=3))
    } else {
      ooi<-cbind(matrix(rep(" ",times=ncol(t1)),nrow=1), pp1, pp1.exact)
      tb1<-cbind(tb1,matrix(" ",nrow=nrow(tb1),ncol=2))
    }
    tmp<-vlabelV[match(xname[i],vnameV)]; if (is.na(tmp)) tmp<-xname[i]
    rownames(ooi)<-tmp 
    tmp.rname<-vlabelZ[match(paste(xname[i],".",rownames(t1),sep=""),vnameZ)]
    tmp.rname[is.na(tmp.rname)]<-rownames(t1)
    rownames(tb1)<-paste("    ",tmp.rname,sep="")  
    ooi=rbind(ooi,tb1)
    tmp.cname<-vlabelZ[match(paste(gname,".",colnames(t1),sep=""),vnameZ)]
    if (ngrp==2) {colnames(ooi)<-c(tmp.cname, "Standardize diff.", "P value", "P value*")
    } else {colnames(ooi)<-c(tmp.cname,"P value", "P value*"); }
    ifelse(i==1, oo<-ooi, oo<-rbind(oo,ooi))
  }
  return(oo)
}
vlabelN<-(substr(vlabel,1,1)==" ");
vlabelZ<-vlabel[vlabelN];vlabelV<-vlabel[!vlabelN]
vnameV<-vname[!vlabelN];vnameZ<-vname[vlabelN];
w<-c("<html><head>","<meta http-equiv=\"Content-Type\" content=\"text/html\" charset=\"gb2312\" /></head><body>")
w<-c(w,paste("<h2>", title, "</h2>"))
allvname<-c(xvname,bvar,colvname); allvname<-allvname[!is.na(allvname)]; 
WD<-data.frame(WD,TOT_=1)[,c(allvname,"TOT_")];

rm(xv,bv,colv)
if (is.na(colvname)) {
  nclv<-1; clvb<-"Total"; clvb_<-"Total"
} else {
  clv<-levels(factor(WD[,colvname])); nclv<-length(clv)+1
  clvb_<-vlabelZ[match(paste(colvname,".",clv,sep=""),vnameZ)]; 
  clvb_[is.na(clvb_)]<-clv[is.na(clvb_)];
  clvb<-c(paste(vlabelV[vnameV==colvname],clvb_,sep="="),"Total");
  clvb_<-c(clvb_,"Total")
  WD<-WD[!is.na(WD[,colvname]),]
}  

if (is.na(bvar)) {ncc<-1; tt00<-"";
} else {blv<-levels(factor(WD[,bvar])); ncc<-length(blv); 
  blvb_<-vlabelZ[match(paste(bvar,".",blv,sep=""),vnameZ)]; 
  blvb_[is.na(blvb_)]<-blv[is.na(blvb_)];
  if (ncc==2) {tt00<-c(blvb_,"Standardize diff.","P-value","P-value*")
  } else {tt00<-c(blvb_,"P-value","P-value*"); }
  WD<-WD[!is.na(WD[,bvar]),];
}

opt<-parm[3];
if (is.na(opt)) opt<-1
if (opt==0) opt<-1
prnopt<-c("Mean+SD / N(%)","Mean(SD) Median (Min-Max) / N(%)","Mean(SD) Median (Q1-Q3) / N(%)", "Geometric Mean (95% CI) / N(%)",
"(N) Mean+SD / N(%)","(N) Mean(SD) Median (Min-Max) / N(%)","(N) Mean(SD) Median (Q1-Q3) / N(%)", "(N) Geometric Mean (95% CI) / N(%)",
"List N Mean SD Median Q1-Q3 Min-Max")

xv0<-xvname[xlv==0]; xv1<-xvname[xlv>0]; nxv0<-sum(xlv==0); nxv1<-sum(xlv>0);

stprn<-strsplit(prnopt[opt],"/")[[1]]

for (k in 1:nclv) {
  tt<-tt00; ttnp<-c("","")
  if (!is.na(colvname) & k<nclv) {WD1<-WD[WD[,colvname]==clv[k],]; } else {WD1<-WD;}
  if (!is.na(bvar)) {
    if (opt<=8) {
      if (nxv0>0) tt<-rbind(tt,t1_mean(xv0,bvar,dec,opt,WD1))
      if (nxv1>0) tt<-rbind(tt,t1_freq(xv1,bvar,WD1))
    } else {
      if (nxv0>0) {
        t0.tmp<-t1_meanlist(xv0,bvar,dec,WD1); 
        tt<-rbind(tt,t0.tmp[[1]])
        tvnpr<-t0.tmp[[2]];  tvnpr<-cbind(rownames(tvnpr),tvnpr)
        ttnp<-rbind(ttnp,c("","Non-parametric(Kruskal Wallis) test P-value"), tvnpr)
      }
      if (nxv1>0) {
        t1.tmp<-t1_freq(xv1,bvar,WD1); 
        tt<-rbind(tt,t1.tmp[,-ncol(t1.tmp)])
        tvfish<-t1.tmp[,ncol(t1.tmp)]; tvfish<-as.matrix(tvfish[tvfish!=" "],ncol=1)
        tvfish<-cbind(rownames(tvfish),tvfish)
        ttnp<-rbind(ttnp,c("","Fisher exact test P-value"),tvfish)
      }
    }
  } else {
    if (opt<=8) {
      if (nxv0>0) tt<-rbind(tt,stprn[1],t1_mean0(xv0,dec,opt,WD1))
      if (nxv1>0) tt<-rbind(tt,"N (%)",t1_freq0(xv1,WD1))
    } else {
      if (nxv0>0) tt<-rbind(tt,"Statistics",t1_meanlist(xv0,"TOT_",dec,WD1)[[1]])
      if (nxv1>0) tt<-rbind(tt,"N (%)",t1_freq0(xv1,WD1))
    }
  }
  rname<-rownames(tt); tt<-cbind(rname,tt); 
  if (!is.na(bvar)) {tt[1,1]<-vlabel[vname==bvar];} else {tt[1,1]<-"";}
  if (!is.na(colvname)) w<-c(w,"</br>",clvb[k])
  w<-c(w,"</br><table border=3>", mat2htmltable(tt), "</table>")
  if (length(ttnp)>2) w<-c(w,"</br><table border=3>", mat2htmltable(ttnp), "</table>")
}


w<-c(w, "</br>表中结果: ", prnopt[opt])
if (!is.na(bvar)) w<-c(w, "</br>P值*: 如是连续变量，用Kruskal Wallis秩和检验得出, 如计数变量有理论数<10，用Fisher精确概率检验得出")
w<-c(w,paste("</br>此表用易侕统计软件 (www.empowerstats.com) 和R软件生成，生成日期：",Sys.Date()))
w<-c(w,wd.subset)
w<-c(w,"</body></html>")
fileConn<-file(paste(ofname,".htm",sep="")); writeLines(w, fileConn)



