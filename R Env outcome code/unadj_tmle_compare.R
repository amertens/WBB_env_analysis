
#---------------------------------------
# World Bank Environmental Analysis 
#
# andrew mertens (amertens@berkeley.edu)
#
# Environmental outcomes of World Bank data for
#---------------------------------------


#---------------------------------------
# Load data
#---------------------------------------

rm(list=ls())
library(foreign)
library(dplyr)
library(washb)

###Load in World Bank assessment data
setwd("C:/Users/andre/Dropbox/WASHB EML/Analysis Datasets/Andrew")
#worldbank<-read.dta("Env_worldbank_clean.dta")
worldbank<-read.csv("Env_worldbank_clean.csv",stringsAsFactors = TRUE)

head(worldbank)

#Load in blinded treatment
setwd("C:/Users/andre/Documents/WBB_env_analysis/Data/")
load("washb-bangladesh-tr.Rdata")
treatment<-d
levels(treatment$tr)
treatment$clusterid<-as.numeric(treatment$clusterid)

#load("washb-bangladesh-blind-tr.Rdata")
#treatment<-blind_tr
#treatment$clusterid<-as.numeric(treatment$clusterid)
#levels(treatment$tr)

#Load in enrollment data for adjusted analysis
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
enrol<-read.csv("washb-bangladesh-enrol+animals.csv",stringsAsFactors = TRUE)


#Load in Ayse's dataids, TRs, and outcomes
Ayse<-read.csv("C:/Users/andre/Dropbox/WASHB EML/Analysis Datasets/Ayse/washb-bangladesh-early-env-short.csv",stringsAsFactors = TRUE)
head(Ayse)

#Load in Ayse's saved unadj outcomes
load("C:/Users/andre/Dropbox/WASHB EML/Results/Ayse/env_prev_unadj_wb.RData")
#Load in Andrew's saved unadj outcomes
load("C:/Users/andre/Dropbox/WASHB EML/Results/Andrew/Env_Risk_diff_unadj_Andrew_WB.RData")




#Merge treatment information 
dim(worldbank)
d<-left_join(worldbank,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)
treatment$tr <- factor(treatment$tr,levels=c("Control","Sanitation","Nutrition + WSH"))
table(d$tr)
table(Ayse$tr)




#Create TMLE wrapper function:
apply_tmle<-function(Ys,tr=d$tr,W=NULL,id=d$block.x, contrast, family, measure="RR"){
  pair=NULL
  contrasts<-list(c("Control","Sanitation"), c("Control","WSH"),c("Sanitation","WSH"))
  library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")
  
  varlist<-colnames(Ys)
  res_list<-NULL
  for(i in 1:ncol(Ys)){
    cat("#",i,": ",varlist[i],"\n")
      if(measure=="RD"){
        temp<-matrix(NA, length(contrasts), 4)
      }else{
        temp<-matrix(NA, length(contrasts), 8)
      }
    rownames(temp)<-c("Control v Sanitation","Control v WSH","Sanitation v WSH")
    for(j in 1:length(contrasts)){
      fit<-washb_tmle(Y=Ys[,i], tr=tr, W=W[[i]], id=id, pair=pair, family=family, contrast= contrasts[[j]], Q.SL.library=library, seed=12345, print=F)
      temp[j,1:4] <-cbind(fit$estimates$ATE$psi,t(fit$estimates$ATE$CI),fit$estimates$ATE$pvalue)
      if(measure=="RR"){
        temp[j,5:8] <-cbind(fit$estimates$RR$psi,t(fit$estimates$RR$CI),fit$estimates$RR$pvalue)
        colnames(temp)<-c("ATE","ci.lb","ci.lb","P-val","RR","ci.lb","ci.lb","P-val")
      }else{
          colnames(temp)<-c("ATE","ci.lb","ci.lb","P-val")
      }
    }
    res_list[[i]]<-temp
    names(res_list)[[i]]<-varlist[i]
  }
  return(res_list)
}









###################
#Compare
###################


#Check overall means:
mean(d$ecposTW,na.rm=T)-mean(Ayse$ecposTW,na.rm=T)


colnames(d)

ec_tw_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposTW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1]   #tubewell
ec_tw_prev_A<-Ayse  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposTW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1]   #tubewell
ec_tw_prev_M-ec_tw_prev_A


ec_tw_mn_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecTW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #tubewell
ec_tw_mn_A<-Ayse  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecTW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #tubewell
ec_tw_mn_M-ec_tw_mn_A

(d$logecTW-Ayse$logecTW)[1:20]
round((d$logecTW-Ayse$logecTW)[1:20],3)











#Set contrasts
contrasts=list(c("Control","Sanitation"), c("Control","WSH"), c("Sanitation","WSH"))

#Order data
d<-d[order(d$block,d$ecposTW),]
Ayse<-Ayse[order(Ayse$block,Ayse$ecposTW),]
#Create lists of outcomes
Y.pos<-subset(d, select=c(ecposTW))
Y.pos.A<-subset(Ayse, select=c(ecposTW))
#------------
#Binary outcomes
#------------
pos_unadj<-apply_tmle(Ys=Y.pos,tr=d$tr,W=NULL,id=d$block, contrast=contrasts, family="binomial", measure="RR")

pos_unadj_A<-apply_tmle(Ys=Y.pos.A,tr=Ayse$tr,W=NULL,id=Ayse$block, contrast=contrasts, family="binomial", measure="RR")
pos_unadj
pos_unadj_A


d<-d[order(d$block,d$ecposTW),]
Ayse<-Ayse[order(Ayse$block,Ayse$ecposTW),]

fit_Mert<-washb_tmle(Y=d$ecposTW, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="binomial", contrast=c("Control","Sanitation"), Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=12345, print=F)
fit_Ayse<-washb_tmle(Y=Ayse$ecposTW, tr=Ayse$tr, W=NULL, id=Ayse$block, pair=NULL, family="binomial", contrast= c("Control","Sanitation"), Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=12345, print=F)


fit_Mert$estimates$ATE$psi
fit_Ayse$estimates$ATE$psi
fit_Mert$estimates$ATE$psi-fit_Ayse$estimates$ATE$psi

ec_tw_rd_h1_unadj_a
ec_tw_rd_h1_unadj_M



  pair=NULL
  contrasts<-list(c("Control","Sanitation"), c("Control","WSH"),c("Sanitation","WSH"))
  library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")
  Ys=Y.pos
  varlist<-colnames(Ys)
  res_list<-NULL
  measure="RR"
  family="binomial"
  for(i in 1:ncol(Ys)){
    cat("#",i,": ",varlist[i],"\n")
      if(measure=="RD"){
        temp<-matrix(NA, length(contrasts), 4)
      }else{
        temp<-matrix(NA, length(contrasts), 8)
      }

    rownames(temp)<-c("Control v Sanitation","Control v WSH","Sanitation v WSH")
    for(j in 1:length(contrasts)){
      fit<-washb_tmle(Y=Ys[,i], tr=d$tr, W=NULL, id=d$block, pair=NULL, family=family, contrast= contrasts[[j]], Q.SL.library=library, seed=12345, print=F)
 fit_Mert<-washb_tmle(Y=d$ecposTW, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="binomial", contrast=c("Control","Sanitation"), Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=12345, print=F)
     
      
      temp[j,1:4] <-cbind(fit$estimates$ATE$psi,t(fit$estimates$ATE$CI),fit$estimates$ATE$pvalue)
      if(measure=="RR"){
        temp[j,5:8] <-cbind(fit$estimates$RR$psi,t(fit$estimates$RR$CI),fit$estimates$RR$pvalue)
        colnames(temp)<-c("ATE","ci.lb","ci.lb","P-val","RR","ci.lb","ci.lb","P-val")
      }else{
          colnames(temp)<-c("ATE","ci.lb","ci.lb","P-val")
      }
    }
    res_list[[i]]<-temp
    names(res_list)[[i]]<-varlist[i]
  }
  
  
  
pos_unadj
pos_unadj_A
fit_Mert$estimates$ATE$psi
fit_Ayse$estimates$ATE$psi
fit_Mert$estimates$ATE$psi-fit_Ayse$estimates$ATE$psi

ec_tw_rd_h1_unadj_a
ec_tw_rd_h1_unadj_M


  
res_list




