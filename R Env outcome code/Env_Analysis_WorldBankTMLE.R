
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
enrol<-subset(enrol, select=-c(clusterid, block)) #drop so not duplicates after merge with treatment data

#Merge treatment information 
dim(worldbank)
d<-left_join(worldbank,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)
treatment$tr <- factor(treatment$tr,levels=c("Control","Sanitation","WSH"))
table(d$tr)



#Merge in enrollment information
dim(d)
dim(enrol)
d<-left_join(d,enrol, by="dataid")
dim(d)

#test that all rows are matched to enrollment data
table(is.na(d$svydate)) 

#NOTE: Looks like one unmatched dataid
d[is.na(d$svydate),]

#Set block as factor
d$block<-factor(d$block)

########################
#Prevalences and 95% CI by arm
########################

colnames(d)

ec_tw_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposTW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1]   #tubewell
ec_sw_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposSW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #stored water
ec_p_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposP, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #ponds
ec_h_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #child hands
ec_f_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #food
ec_s_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposS, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #soil
ec_y_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposY, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #flies
fc_tw_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposTW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_sw_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposSW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_p_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposP, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_h_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_f_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_s_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposS, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_y_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposY, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$flycaught, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mh_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$mhdirt, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ch_prev_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$chdirt, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 

mean(d$ecposTW, na.rm=T)
mean(d$ecposSW, na.rm=T)
mean(d$ecposP, na.rm=T)


########################
#Means and 95% CI by arm
########################

ec_tw_mn_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecTW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #tubewell
ec_sw_mn_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecSW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #stored water
ec_p_mn_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecP, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #ponds
ec_h_mn_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #child hands
ec_f_mn_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #food
ec_s_mn_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecS, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #soil
ec_y_mn_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecY, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #flies
fc_tw_mn_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcTW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_sw_mn_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcSW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_p_mn_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcP, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_h_mn_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_f_mn_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_s_mn_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcS, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_y_mn_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcY, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_mn_M<-d  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$numfly, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 





########################
#Unadjusted GLMs
########################

#Create TMLE wrapper function
apply_tmle<-function(Ys,tr=d$tr,W=NULL,id=d$block, clusterid=d$clusterid, dataid=d$dataid, contrast, family ,library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), measure="RR"){
  #contrasts<-list(c("Control","Sanitation"), c("Control","WSH"),c("Sanitation","WSH"))
  
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
    dat<-data.frame(Y=Ys[,i], tr=tr, id=id, clusterid=clusterid, dataid=dataid)
    for(j in 1:length(contrasts)){
      dat<-dat[order(dat$id, dat$clusterid, dat$dataid),]
      fit<-washb_tmle(Y=dat$Y, tr=dat$tr, W=W[[i]], id=dat$id, pair=NULL, family=family, contrast= contrasts[[j]], seed=12345, print=F)
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

apply_glm<-function(Ys,tr=d$tr,id=d$block, measure="RR"){
  contrasts<-list(c("Control","Sanitation"), c("Control","WSH"),c("Sanitation","WSH"))
  
  varlist<-colnames(Ys)
  res_list<-NULL
  for(i in 1:ncol(Ys)){
    cat("#",i,": ",varlist[i],"\n")
      if(measure=="RD"){
        temp<-matrix(NA, length(contrasts), 6)
      }else{
        temp<-matrix(NA, length(contrasts), 12)
      }
    rownames(temp)<-c("Control v Sanitation","Control v WSH","Sanitation v WSH")
    for(j in 1:length(contrasts)){
      if(measure=="RD"){
      temp[j,1:6]<-washb_glm(Y=Ys[,i], tr=tr, W=NULL, id=id, pair=NULL, family="gaussian", contrast= contrasts[[j]], seed=12345, print=F)$TR
                colnames(temp)<-c("ATE","ci.lb","ci.lb","SE","Zval","P-val")
      }
      if(measure=="RR"){
        temp[j,1:6]<-washb_glm(Y=Ys[,i], tr=tr, W=NULL, id=id, pair=NULL, family=binomial(link=`log`), contrast= contrasts[[j]], seed=12345, print=F)$TR
        temp[j,7:12]<-washb_glm(Y=Ys[,i], tr=tr, W=NULL, id=id, pair=NULL, family="gaussian", contrast= contrasts[[j]], seed=12345, print=F)$TR

        colnames(temp)<-c("ATE","ci.lb","ci.lb","SE","Zval","P-val","RR","ci.lb","ci.lb","SE","Zval","P-val")
      }
    }
    res_list[[i]]<-temp
    names(res_list)[[i]]<-varlist[i]
  }
  return(res_list)
}




#Order data for replicated SL
#d<-d[order(d$block,d$clusterid,d$dataid),]
d<-d[order(d$block,d$clusterid,d$dataid),]



#Create lists of outcomes
Y.pos<-subset(d, select=c(ecposTW,ecposSW,ecposP,ecposH,ecposF,ecposS,ecposY,
         fcposTW,fcposSW,fcposP,fcposH,fcposF,fcposS,fcposY,
         flycaught,mhdirt,chdirt))



#Set contrasts
contrasts=list(c("Control","Sanitation"), c("Control","WSH"), c("Sanitation","WSH"))


#d %>% group_by(tr) %>% summarize(meanec_S=mean(logecS, na.rm=T),meanfc_S=mean(logfcS, na.rm=T), minec_S=min(logecS, na.rm=T),minfc_S=min(logfcS, na.rm=T), maxec_S=max(logecS, na.rm=T),maxfc_S=max(logfcS, na.rm=T))
d<-d[order(d$block,d$clusterid,d$dataid),]
fit<-washb_tmle(Y=d$logfcTW, tr=d$tr, W=NULL, id=d$block, pair=NULL, Q.SL.library="SL.glm", family="gaussian", contrast= c("Sanitation","WSH"), seed=12345, print=F)
fit

glm<-washb_glm(Y=d$logfcTW, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= c("Sanitation","WSH"), print=T)
glm$TR

table(is.na(d$logfcTW))


test<-washb_glm(Y=d$logecS, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[1]],print=T)


#ec_s_dif_h1_unadj_M

#------------
#Binary outcomes
#------------
#pos_unadj<-apply_tmle(Ys=Y.pos,tr=d$tr,W=NULL,id=d$block, contrast=contrasts, family="binomial",library=NULL, measure="RR")
pos_unadj<-apply_glm(Ys=Y.pos,tr=d$tr,id=d$block, contrast=contrasts, measure="RR")


#Prevalence Ratio-unadjusted
#(h1): sanitation vs. control, combined WSH vs. control  
ec_tw_rr_h1_unadj_M<-pos_unadj[[1]][1:2,5:8]
ec_sw_rr_h1_unadj_M<-pos_unadj[[2]][1:2,5:8]
ec_p_rr_h1_unadj_M<-pos_unadj[[3]][1:2,5:8]
ec_h_rr_h1_unadj_M<-pos_unadj[[4]][1:2,5:8]
ec_f_rr_h1_unadj_M<-pos_unadj[[5]][1:2,5:8]
ec_s_rr_h1_unadj_M<-pos_unadj[[6]][1:2,5:8]
ec_y_rr_h1_unadj_M<-pos_unadj[[7]][1:2,5:8]
fc_tw_rr_h1_unadj_M<-pos_unadj[[8]][1:2,5:8]
fc_sw_rr_h1_unadj_M<-pos_unadj[[9]][1:2,5:8]
fc_p_rr_h1_unadj_M<-pos_unadj[[10]][1:2,5:8]
fc_h_rr_h1_unadj_M<-pos_unadj[[11]][1:2,5:8]
fc_f_rr_h1_unadj_M<-pos_unadj[[12]][1:2,5:8]
fc_s_rr_h1_unadj_M<-pos_unadj[[13]][1:2,5:8]
fc_y_rr_h1_unadj_M<-pos_unadj[[14]][1:2,5:8]
fly_rr_h1_unadj_M<-pos_unadj[[15]][1:2,5:8]
mh_rr_h1_unadj_M<-pos_unadj[[16]][1:2,5:8]
ch_rr_h1_unadj_M<-pos_unadj[[17]][1:2,5:8]

#(H2): combined WSH vs. sanitation.
ec_tw_rr_h2_unadj_M<-pos_unadj[[1]][3,5:8]
ec_sw_rr_h2_unadj_M<-pos_unadj[[2]][3,5:8]
ec_p_rr_h2_unadj_M<-pos_unadj[[3]][3,5:8]
ec_h_rr_h2_unadj_M<-pos_unadj[[4]][3,5:8]
ec_f_rr_h2_unadj_M<-pos_unadj[[5]][3,5:8]
ec_s_rr_h2_unadj_M<-pos_unadj[[6]][3,5:8]
ec_y_rr_h2_unadj_M<-pos_unadj[[7]][3,5:8]
fc_tw_rr_h2_unadj_M<-pos_unadj[[8]][3,5:8]
fc_sw_rr_h2_unadj_M<-pos_unadj[[9]][3,5:8]
fc_p_rr_h2_unadj_M<-pos_unadj[[10]][3,5:8]
fc_h_rr_h2_unadj_M<-pos_unadj[[11]][3,5:8]
fc_f_rr_h2_unadj_M<-pos_unadj[[12]][3,5:8]
fc_s_rr_h2_unadj_M<-pos_unadj[[13]][3,5:8]
fc_y_rr_h2_unadj_M<-pos_unadj[[14]][3,5:8]
fly_rr_h2_unadj_M<-pos_unadj[[15]][3,5:8]
mh_rr_h2_unadj_M<-pos_unadj[[16]][3,5:8]
ch_rr_h2_unadj_M<-pos_unadj[[17]][3,5:8]

#Prevalence Difference-unadjusted
#(h1): sanitation vs. control, combined WSH vs. control  
ec_tw_rd_h1_unadj_M<-pos_unadj[[1]][1:2,1:4]
ec_sw_rd_h1_unadj_M<-pos_unadj[[2]][1:2,1:4]
ec_p_rd_h1_unadj_M<-pos_unadj[[3]][1:2,1:4]
ec_h_rd_h1_unadj_M<-pos_unadj[[4]][1:2,1:4]
ec_f_rd_h1_unadj_M<-pos_unadj[[5]][1:2,1:4]
ec_s_rd_h1_unadj_M<-pos_unadj[[6]][1:2,1:4]
ec_y_rd_h1_unadj_M<-pos_unadj[[7]][1:2,1:4]
fc_tw_rd_h1_unadj_M<-pos_unadj[[8]][1:2,1:4]
fc_sw_rd_h1_unadj_M<-pos_unadj[[9]][1:2,1:4]
fc_p_rd_h1_unadj_M<-pos_unadj[[10]][1:2,1:4]
fc_h_rd_h1_unadj_M<-pos_unadj[[11]][1:2,1:4]
fc_f_rd_h1_unadj_M<-pos_unadj[[12]][1:2,1:4]
fc_s_rd_h1_unadj_M<-pos_unadj[[13]][1:2,1:4]
fc_y_rd_h1_unadj_M<-pos_unadj[[14]][1:2,1:4]
fly_rd_h1_unadj_M<-pos_unadj[[15]][1:2,1:4]
mh_rd_h1_unadj_M<-pos_unadj[[16]][1:2,1:4]
ch_rd_h1_unadj_M<-pos_unadj[[17]][1:2,1:4]

#(H2): combined WSH vs. sanitation.
ec_tw_rd_h2_unadj_M<-pos_unadj[[1]][3,1:4]
ec_sw_rd_h2_unadj_M<-pos_unadj[[2]][3,1:4]
ec_p_rd_h2_unadj_M<-pos_unadj[[3]][3,1:4]
ec_h_rd_h2_unadj_M<-pos_unadj[[4]][3,1:4]
ec_f_rd_h2_unadj_M<-pos_unadj[[5]][3,1:4]
ec_s_rd_h2_unadj_M<-pos_unadj[[6]][3,1:4]
ec_y_rd_h2_unadj_M<-pos_unadj[[7]][3,1:4]
fc_tw_rd_h2_unadj_M<-pos_unadj[[8]][3,1:4]
fc_sw_rd_h2_unadj_M<-pos_unadj[[9]][3,1:4]
fc_p_rd_h2_unadj_M<-pos_unadj[[10]][3,1:4]
fc_h_rd_h2_unadj_M<-pos_unadj[[11]][3,1:4]
fc_f_rd_h2_unadj_M<-pos_unadj[[12]][3,1:4]
fc_s_rd_h2_unadj_M<-pos_unadj[[13]][3,1:4]
fc_y_rd_h2_unadj_M<-pos_unadj[[14]][3,1:4]
fly_rd_h2_unadj_M<-pos_unadj[[15]][3,1:4]
mh_rd_h2_unadj_M<-pos_unadj[[16]][3,1:4]
ch_rd_h2_unadj_M<-pos_unadj[[17]][3,1:4]



#Differences in log-counts-unadjusted
colnames(Y.log)
d<-d[order(d$block,d$clusterid,d$dataid),]
Y.log<-subset(d, select=c(logecTW, logecSW, logecP, logecH, logecF, logecS, 
             logecY, logfcTW, logfcSW, logfcP, logfcH, logfcF,
             logfcS, logfcY, numfly))

log_unadj<-apply_tmle(Ys=Y.log,tr=d$tr,W=NULL,id=d$block, contrast=contrasts, family="gaussian",library=NULL, measure="RD")


#(h1): sanitation vs. control, combined WSH vs. control  
ec_tw_dif_h1_unadj_M<-log_unadj[[1]][1:2,1:4]
ec_sw_dif_h1_unadj_M<-log_unadj[[2]][1:2,1:4]
ec_p_dif_h1_unadj_M<-log_unadj[[3]][1:2,1:4]
ec_h_dif_h1_unadj_M<-log_unadj[[4]][1:2,1:4]
ec_f_dif_h1_unadj_M<-log_unadj[[5]][1:2,1:4]
ec_s_dif_h1_unadj_M<-log_unadj[[6]][1:2,1:4]
ec_y_dif_h1_unadj_M<-log_unadj[[7]][1:2,1:4]
fc_tw_dif_h1_unadj_M<-log_unadj[[8]][1:2,1:4]
fc_sw_dif_h1_unadj_M<-log_unadj[[9]][1:2,1:4]
fc_p_dif_h1_unadj_M<-log_unadj[[10]][1:2,1:4]
fc_h_dif_h1_unadj_M<-log_unadj[[11]][1:2,1:4]
fc_f_dif_h1_unadj_M<-log_unadj[[12]][1:2,1:4]
fc_s_dif_h1_unadj_M<-log_unadj[[13]][1:2,1:4]
fc_y_dif_h1_unadj_M<-log_unadj[[14]][1:2,1:4]
fly_dif_h1_unadj_M<-log_unadj[[15]][1:2,1:4]

#(H2): combined WSH vs. sanitation.
ec_tw_dif_h2_unadj_M<-log_unadj[[1]][3,1:4]
ec_sw_dif_h2_unadj_M<-log_unadj[[2]][3,1:4]
ec_p_dif_h2_unadj_M<-log_unadj[[3]][3,1:4]
ec_h_dif_h2_unadj_M<-log_unadj[[4]][3,1:4]
ec_f_dif_h2_unadj_M<-log_unadj[[5]][3,1:4]
ec_s_dif_h2_unadj_M<-log_unadj[[6]][3,1:4]
ec_y_dif_h2_unadj_M<-log_unadj[[7]][3,1:4]
fc_tw_dif_h2_unadj_M<-log_unadj[[8]][3,1:4]
fc_sw_dif_h2_unadj_M<-log_unadj[[9]][3,1:4]
fc_p_dif_h2_unadj_M<-log_unadj[[10]][3,1:4]
fc_h_dif_h2_unadj_M<-log_unadj[[11]][3,1:4]
fc_f_dif_h2_unadj_M<-log_unadj[[12]][3,1:4]
fc_s_dif_h2_unadj_M<-log_unadj[[13]][3,1:4]
fc_y_dif_h2_unadj_M<-log_unadj[[14]][3,1:4]
fly_dif_h2_unadj_M<-log_unadj[[15]][3,1:4]









########################
#Adjusted analysis
########################


########################
#Covariate setup
########################
#Include:
#	-ID of the lab staff member who performed the lab analysis
#	-Month of measurement, to account for seasonal variation 
#	-Most recent time it rained
#	-Child age (days)
#	-Child sex
#	-Mother's age (years)
#	-Mother's education level (no education, primary, secondary)
#	-Household food insecurity (4-level HFIAS categories) 
#	-Number of children < 18 years in the household
#	-Number of individuals living in the compound
#	-Distance (in minutes) to the household's primary drinking water source
#	-Housing materials (floor, walls, roof) and household assets
# ???-Assets measured: electricity, wardrobe, table, chair or bench, watch or clock, khat, chouki, working radi,o
# working black/white or color television, refrigerator, bicycle (not child's toy), motorcycle, sewing machine, mobile phone, land phone 

colnames(d)

#Make vectors of adjustment variable names used in all samples
Wvars<-c('sex', 'aged',   #
         'momage','momedu','hfiacat',
         'Nlt18','Ncomp','watmin',
         'roof', 'walls', 'floor',
         'elec', 'asset_wardrobe', 'asset_table', 'asset_chair', 'asset_clock', 
         'asset_khat', 'asset_chouki', 'asset_radio', 
         'asset_tv', 'asset_refrig', 'asset_bike',
         'asset_moto', 'asset_sewmach', 'asset_mobile', 'asset_phone',
         'month','raintime')


#subset time-constant W adjustment set
W<- subset(d, select=Wvars)

#Clean adjustment variables 
#Check missingness
for(i in 1:ncol(W)){
  print(colnames(W)[i])
  print(table(is.na(W[,i])))
}

#Replace missingness for factors with new level
#in main dataset 


d$asset_clock[is.na(d$asset_clock)]<-99
d$asset_clock<-factor(d$asset_clock)


#Re-subset W so new missing categories are included
W<- subset(d, select=Wvars)

#check that all the factor variables are set
for(i in 1:ncol(W)){
  print(colnames(W)[i])
  print(class(W[,i])  )
}

#set covariates as factors
W$sex<-as.factor(W$sex)
W$momedu<-as.factor(W$momedu)
W$elec<-as.factor(W$elec)
W$asset_wardrobe<-as.factor(W$asset_wardrobe)
W$asset_table<-as.factor(W$asset_table)
W$asset_chair<-as.factor(W$asset_chair)
W$asset_clock<-as.factor(W$asset_clock)
W$asset_radio<-as.factor(W$asset_radio)
W$asset_tv<-as.factor(W$asset_tv)
W$asset_refrig<-as.factor(W$asset_refrig)
W$asset_bike<-as.factor(W$asset_bike)
W$asset_moto<-as.factor(W$asset_moto)
W$asset_sewmach<-as.factor(W$asset_sewmach)
W$asset_mobile<-as.factor(W$asset_mobile)
W$asset_khat<-as.factor(W$asset_khat)
W$asset_chouki<-as.factor(W$asset_chouki)









#Add in sample-varying covariates
colnames(d)

#Drinking water
#	-Presence and integrity of tubewell platform and drainage 
wat_vars<-c("twplat", "twpooling")

W_wat<- subset(d, select=wat_vars)
for(i in 1:ncol(W_wat)){
  print(colnames(W_wat)[i])
  print(class(W_wat[,i])  )
  print(table(is.na(W_wat[,i])))
}

d$twplat<-addNA(d$twplat)
levels(d$twplat)[length(levels(d$twplat))]<-"Missing"

d$twpooling<-addNA(d$twpooling)
levels(d$twpooling)[length(levels(d$twpooling))]<-"Missing"



#Food
#	-Cover status of food storage container
#	-Location of food storage container (on the ground vs. elevated)
#	-Location of food storage area (inside vs. outside)
#	-Duration of food storage
#	-Temperature of food
#	-Temperature and humidity of food storage area
#food_vars<-c("foodtemp", "temp", "hum", "foodstorhrs", "foodsafestor", "foodsafeloc")
food_vars<-c("foodtemp", "temp", "hum", "foodstorhrs", "foodsafestor","foodroof","foodcover")

food_vars %in% colnames(d)
W_food<- subset(d, select=food_vars)
for(i in 1:ncol(W_food)){
  print(colnames(W_food)[i])
  print(class(W_food[,i])  )
  print(table(is.na(W_food[,i])))
}



d$foodtemp<-addNA(d$foodtemp)
levels(d$foodtemp)[length(levels(d$foodtemp))]<-"Missing"

d$foodsafestor<-addNA(d$foodsafestor)
levels(d$foodsafestor)[length(levels(d$foodsafestor))]<-"Missing"




#Soil
#	-Whether sampling location is sunny vs. in the shade
soil_vars<-c("soilsun")

W_soil<- subset(d, select=soil_vars)
for(i in 1:ncol(W_soil)){
  print(colnames(W_soil)[i])
  print(class(W_soil[,i])  )
  print(table(is.na(W_soil[,i])))
}



#Flies
#	-Number of hours fly tape was in place
#	-Condition of fly tape (intact vs. tampered with)
#	-Whether fly tape was protected from rain (e.g., under a roof)
#	-Number of steps from food preparation and latrine areas, respectively
#	-Location of food preparation area (inside vs. outside)
#fly_vars<-c("foodsafeloc", "tapeloc", "hanghrs", "tapetamper", "taperoof")
fly_vars<-c("foodroof", "tapeloc_kit", "hanghrs_kit", "tapeintact_kit", "taperoof_kit")

   
W_fly<- subset(d, select=fly_vars)
for(i in 1:ncol(W_fly)){
  print(colnames(W_fly)[i])
  print(class(W_fly[,i])  )
  print(table(is.na(W_fly[,i])))
}





#Check that prevalence >5% for all binary variables
W_all<-cbind(W,W_wat,W_food,W_soil,W_fly)
for(i in 1:ncol(W_all)){
  if(class(W_all[,i])=="factor"){
    for(j in 1:dim(table(W_all[,i]))){
      flag<-0
      if(sum(W_all[,i]==levels(W_all[,i])[j], na.rm=T)/nrow(W_all)*100<5){
        perc<-sum(W_all[,i]==levels(W_all[,i])[j], na.rm=T)/nrow(W_all)*100
        cat("\n>95% missing: ",colnames(W_all)[i]," level:",levels(W_all[,i])[j],"perc:",perc,"\n")
        flag<-1
      }
    }
      if(flag==1){
        print(table(W_all[,i]))
      }
  }else{
    if(sum(is.na(W_all[,i]))/nrow(W_all)*100>95){
      cat("\n>95% missing: ",colnames(W_all)[i],"\n")
    }
  }
}

store<-d
#d<-store

#Relevel all factors
d$sex=relevel(d$sex,ref="female")
d$momedu=relevel(d$momedu,ref="No education")
d$hfiacat=relevel(d$hfiacat,ref="Food Secure")
    d$hfiacat<-addNA(d$hfiacat)
d$wall<-factor(d$wall)
    d$wall<-addNA(d$wall)
    levels(d$wall)<-c("No improved wall","Improved wall","Missing")
    d$wall=relevel(d$wall,ref="No improved wall")
d$floor<-factor(d$floor)
    d$floor<-addNA(d$floor)
    levels(d$floor)<-c("No improved floor","Improved floor","Missing")
    d$floor=relevel(d$floor,ref="No improved floor")
d$elec<-factor(d$elec)
    d$elec<-addNA(d$elec)
    levels(d$elec)<-c("No electricity","Electricity","Missing")
    d$elec=relevel(d$elec,ref="No electricity")
    
    
d$asset_wardrobe<-factor(d$asset_wardrobe)
    d$asset_wardrobe<-addNA(d$asset_wardrobe)
    levels(d$asset_wardrobe)<-c("No wardrobe","Wardrobe","Missing")
    d$asset_wardrobe=relevel(d$asset_wardrobe,ref="No wardrobe")
d$asset_table<-factor(d$asset_table)
    d$asset_table<-addNA(d$asset_table)
    levels(d$asset_table)<-c("No table","Improved table","Missing")
    d$asset_table=relevel(d$asset_table,ref="No table")
d$asset_chair<-factor(d$asset_chair)
    d$asset_chair<-addNA(d$asset_chair)
    levels(d$asset_chair)<-c("No chair","Chair","Missing")
    d$asset_chair=relevel(d$asset_chair,ref="No chair")
d$asset_clock[is.na(d$asset_clock)]<-99
    d$asset_clock<-factor(d$asset_clock)
    d$asset_clock<-addNA(d$asset_clock)
    levels(d$asset_clock)<-c("No clock","Clock","Missing", "Missing")
    d$asset_clock=relevel(d$asset_clock,ref="No clock")
d$asset_khat<-factor(d$asset_khat)
    d$asset_khat<-addNA(d$asset_khat)
    levels(d$asset_khat)<-c("No khat","Khat","Missing")
    d$asset_khat=relevel(d$asset_khat,ref="No khat")
d$asset_chouki<-factor(d$asset_chouki)
    d$asset_chouki<-addNA(d$asset_chouki)
    levels(d$asset_chouki)<-c("No chouki","Chouki","Missing")
    d$asset_chouki=relevel(d$asset_chouki,ref="No chouki")
d$asset_tv<-factor(d$asset_tv)
    d$asset_tv<-addNA(d$asset_tv)
    levels(d$asset_tv)<-c("No TV","Improved TV","Missing")
    d$asset_tv=relevel(d$asset_tv,ref="No TV")
d$asset_refrig<-factor(d$asset_refrig)
    d$asset_refrig<-addNA(d$asset_refrig)
    levels(d$asset_refrig)<-c("No refrigerator","Refrigerator","Missing")
    d$asset_refrig=relevel(d$asset_refrig,ref="No refrigerator")
d$asset_bike<-factor(d$asset_bike)
    d$asset_bike<-addNA(d$asset_bike)
    levels(d$asset_bike)<-c("No bicycle","Bicycle","Missing")
    d$asset_bike=relevel(d$asset_bike,ref="No bicycle")
d$asset_moto<-factor(d$asset_moto)
    d$asset_moto<-addNA(d$asset_moto)
    levels(d$asset_moto)<-c("No motorcycle","Motorcycle","Missing")
    d$asset_moto=relevel(d$asset_moto,ref="No motorcycle")
d$asset_sewmach<-factor(d$asset_sewmach)
    d$asset_sewmach<-addNA(d$asset_sewmach)
    levels(d$asset_sewmach)<-c("No sewing machine","Sewing machine","Missing")
    d$asset_sewmach=relevel(d$asset_sewmach,ref="No sewing machine")
d$asset_mobile<-factor(d$asset_mobile)
    d$asset_mobile<-addNA(d$asset_mobile)
    levels(d$asset_mobile)<-c("No mobile phone","Mobile phone","Missing")
    d$asset_mobile=relevel(d$asset_mobile,ref="No mobile phone")    
d$raintime=relevel(d$raintime, ref="Rained today")
d$twpooling=relevel(d$twpooling, ref="No")
d$twplat=relevel(d$twplat, ref="No")
d$foodcover=relevel(d$foodcover, ref="No")
d$foodsafestor=relevel(d$foodsafestor, ref="No")
d$foodroof=relevel(d$foodroof, ref="No")
d$foodtemp=relevel(d$foodtemp, ref="Cold")
d$soilsun=relevel(d$soilsun, ref="No")
d$tapeintact_kit=relevel(d$tapeintact_kit, ref="No")
d$taperoof_kit=relevel(d$taperoof_kit, ref="No")
d$month<-factor(d$month)

#Re-subset W so new re-leveled factors are included
W<- subset(d, select=Wvars)
W_soil<- subset(d, select=soil_vars)
W_fly<- subset(d, select=fly_vars)
W_food<- subset(d, select=food_vars)
W_wat<- subset(d, select=wat_vars)


#Drop from Ayse:
#roof, radio, asset_phone, clock

#Drop variables with low prevalence
W<-subset(W, select= -c(roof, asset_radio, asset_phone, asset_clock))
#W_wat<-subset(W_wat, select= -c(twpooling)) Leave 
W_fly<-subset(W_fly, select= -c(tapeintact_kit))



#Combine constant covariates with sample-specific covariates
W_wat<- cbind(W,W_wat)
W_fly<- cbind(W,W_fly)
W_soil<- cbind(W,W_soil)
W_food<- cbind(W,W_food)







########################
#Adjusted analysis- TMLE
########################



#Create lists of Ws
colnames(Y.pos)
Wlist.pos<-list(W_wat,W_wat,W,W,W_food,W_soil,W,W_wat,W_wat,W,W,W_food,W_soil,W, W_fly, W, W)

#Run adjusted TMLEs
pos_adj<-apply_tmle(Ys=Y.pos,tr=d$tr,W=Wlist.pos,id=d$block, contrast=contrasts, family="binomial", measure="RR")

    
      
      
#Prevalence Ratio
#(H1): sanitation vs. control, combined WSH vs. control  
ec_tw_rr_h1_adj_M<-pos_adj[[1]][1:2,5:8]
ec_sw_rr_h1_adj_M<-pos_adj[[2]][1:2,5:8]
ec_p_rr_h1_adj_M<-pos_adj[[3]][1:2,5:8]
ec_h_rr_h1_adj_M<-pos_adj[[4]][1:2,5:8]
ec_f_rr_h1_adj_M<-pos_adj[[5]][1:2,5:8]
ec_s_rr_h1_adj_M<-pos_adj[[6]][1:2,5:8]
ec_y_rr_h1_adj_M<-pos_adj[[7]][1:2,5:8]
fc_tw_rr_h1_adj_M<-pos_adj[[8]][1:2,5:8]
fc_sw_rr_h1_adj_M<-pos_adj[[9]][1:2,5:8]
fc_p_rr_h1_adj_M<-pos_adj[[10]][1:2,5:8]
fc_h_rr_h1_adj_M<-pos_adj[[11]][1:2,5:8]
fc_f_rr_h1_adj_M<-pos_adj[[12]][1:2,5:8]
fc_s_rr_h1_adj_M<-pos_adj[[13]][1:2,5:8]
fc_y_rr_h1_adj_M<-pos_adj[[14]][1:2,5:8]
fly_rr_h1_adj_M<-pos_adj[[15]][1:2,5:8]
mh_rr_h1_adj_M<-pos_adj[[16]][1:2,5:8]
ch_rr_h1_adj_M<-pos_adj[[17]][1:2,5:8]

#(H2): combined WSH vs. sanitation.
ec_tw_rr_h2_adj_M<-pos_adj[[1]][3,5:8]
ec_sw_rr_h2_adj_M<-pos_adj[[2]][3,5:8]
ec_p_rr_h2_adj_M<-pos_adj[[3]][3,5:8]
ec_h_rr_h2_adj_M<-pos_adj[[4]][3,5:8]
ec_f_rr_h2_adj_M<-pos_adj[[5]][3,5:8]
ec_s_rr_h2_adj_M<-pos_adj[[6]][3,5:8]
ec_y_rr_h2_adj_M<-pos_adj[[7]][3,5:8]
fc_tw_rr_h2_adj_M<-pos_adj[[8]][3,5:8]
fc_sw_rr_h2_adj_M<-pos_adj[[9]][3,5:8]
fc_p_rr_h2_adj_M<-pos_adj[[10]][3,5:8]
fc_h_rr_h2_adj_M<-pos_adj[[11]][3,5:8]
fc_f_rr_h2_adj_M<-pos_adj[[12]][3,5:8]
fc_s_rr_h2_adj_M<-pos_adj[[13]][3,5:8]
fc_y_rr_h2_adj_M<-pos_adj[[14]][3,5:8]
fly_rr_h2_adj_M<-pos_adj[[15]][3,5:8]
mh_rr_h2_adj_M<-pos_adj[[16]][3,5:8]
ch_rr_h2_adj_M<-pos_adj[[17]][3,5:8]


#Prevalence Difference
#(h1): sanitation vs. control, combined WSH vs. control  
ec_tw_rd_h1_adj_M<-pos_adj[[1]][1:2,1:4]
ec_sw_rd_h1_adj_M<-pos_adj[[2]][1:2,1:4]
ec_p_rd_h1_adj_M<-pos_adj[[3]][1:2,1:4]
ec_h_rd_h1_adj_M<-pos_adj[[4]][1:2,1:4]
ec_f_rd_h1_adj_M<-pos_adj[[5]][1:2,1:4]
ec_s_rd_h1_adj_M<-pos_adj[[6]][1:2,1:4]
ec_y_rd_h1_adj_M<-pos_adj[[7]][1:2,1:4]
fc_tw_rd_h1_adj_M<-pos_adj[[8]][1:2,1:4]
fc_sw_rd_h1_adj_M<-pos_adj[[9]][1:2,1:4]
fc_p_rd_h1_adj_M<-pos_adj[[10]][1:2,1:4]
fc_h_rd_h1_adj_M<-pos_adj[[11]][1:2,1:4]
fc_f_rd_h1_adj_M<-pos_adj[[12]][1:2,1:4]
fc_s_rd_h1_adj_M<-pos_adj[[13]][1:2,1:4]
fc_y_rd_h1_adj_M<-pos_adj[[14]][1:2,1:4]
fly_rd_h1_adj_M<-pos_adj[[15]][1:2,1:4]
mh_rd_h1_adj_M<-pos_adj[[16]][1:2,1:4]
ch_rd_h1_adj_M<-pos_adj[[17]][1:2,1:4]

#(H2): combined WSH vs. sanitation.
ec_tw_rd_h2_adj_M<-pos_adj[[1]][3,1:4]
ec_sw_rd_h2_adj_M<-pos_adj[[2]][3,1:4]
ec_p_rd_h2_adj_M<-pos_adj[[3]][3,1:4]
ec_h_rd_h2_adj_M<-pos_adj[[4]][3,1:4]
ec_f_rd_h2_adj_M<-pos_adj[[5]][3,1:4]
ec_s_rd_h2_adj_M<-pos_adj[[6]][3,1:4]
ec_y_rd_h2_adj_M<-pos_adj[[7]][3,1:4]
fc_tw_rd_h2_adj_M<-pos_adj[[8]][3,1:4]
fc_sw_rd_h2_adj_M<-pos_adj[[9]][3,1:4]
fc_p_rd_h2_adj_M<-pos_adj[[10]][3,1:4]
fc_h_rd_h2_adj_M<-pos_adj[[11]][3,1:4]
fc_f_rd_h2_adj_M<-pos_adj[[12]][3,1:4]
fc_s_rd_h2_adj_M<-pos_adj[[13]][3,1:4]
fc_y_rd_h2_adj_M<-pos_adj[[14]][3,1:4]
fly_rd_h2_adj_M<-pos_adj[[15]][3,1:4]
mh_rd_h2_adj_M<-pos_adj[[16]][3,1:4]
ch_rd_h2_adj_M<-pos_adj[[17]][3,1:4]

#Log count difference
colnames(Y.log)
Wlist.log<-list(W_wat,W_wat,W,W,W_food,W_soil,W,W_wat,W_wat,W,W,W_food,W_soil,W, W_fly)
  

#Run adjusted TMLEs
log_adj<-apply_tmle(Ys=Y.log, tr=d$tr,W=Wlist.log,id=d$block, contrast=contrasts, family="gaussian", measure="RD")

#(h1): sanitation vs. control, combined WSH vs. control  
ec_tw_dif_h1_adj_M<-log_adj[[1]][1:2,1:4]
ec_sw_dif_h1_adj_M<-log_adj[[2]][1:2,1:4]
ec_p_dif_h1_adj_M<-log_adj[[3]][1:2,1:4]
ec_h_dif_h1_adj_M<-log_adj[[4]][1:2,1:4]
ec_f_dif_h1_adj_M<-log_adj[[5]][1:2,1:4]
ec_s_dif_h1_adj_M<-log_adj[[6]][1:2,1:4]
ec_y_dif_h1_adj_M<-log_adj[[7]][1:2,1:4]
fc_tw_dif_h1_adj_M<-log_adj[[8]][1:2,1:4]
fc_sw_dif_h1_adj_M<-log_adj[[9]][1:2,1:4]
fc_p_dif_h1_adj_M<-log_adj[[10]][1:2,1:4]
fc_h_dif_h1_adj_M<-log_adj[[11]][1:2,1:4]
fc_f_dif_h1_adj_M<-log_adj[[12]][1:2,1:4]
fc_s_dif_h1_adj_M<-log_adj[[13]][1:2,1:4]
fc_y_dif_h1_adj_M<-log_adj[[14]][1:2,1:4]
fly_dif_h1_adj_M<-log_adj[[15]][1:2,1:4]

#(H2): combined WSH vs. sanitation.
ec_tw_dif_h2_adj_M<-log_adj[[1]][3,1:4]
ec_sw_dif_h2_adj_M<-log_adj[[2]][3,1:4]
ec_p_dif_h2_adj_M<-log_adj[[3]][3,1:4]
ec_h_dif_h2_adj_M<-log_adj[[4]][3,1:4]
ec_f_dif_h2_adj_M<-log_adj[[5]][3,1:4]
ec_s_dif_h2_adj_M<-log_adj[[6]][3,1:4]
ec_y_dif_h2_adj_M<-log_adj[[7]][3,1:4]
fc_tw_dif_h2_adj_M<-log_adj[[8]][3,1:4]
fc_sw_dif_h2_adj_M<-log_adj[[9]][3,1:4]
fc_p_dif_h2_adj_M<-log_adj[[10]][3,1:4]
fc_h_dif_h2_adj_M<-log_adj[[11]][3,1:4]
fc_f_dif_h2_adj_M<-log_adj[[12]][3,1:4]
fc_s_dif_h2_adj_M<-log_adj[[13]][3,1:4]
fc_y_dif_h2_adj_M<-log_adj[[14]][3,1:4]
fly_dif_h2_adj_M<-log_adj[[15]][3,1:4]




########################
#Effect modification by rainfall
########################
#subset data to dry and wet months
wet<-subset(d, wet==1)
dry<-subset(d, wet==0)


#Re-subset W to dry and wet
W.wet<- subset(wet, select=Wvars)
W_soil.wet<- subset(wet, select=soil_vars)
W_fly.wet<- subset(wet, select=fly_vars)
W_food.wet<- subset(wet, select=food_vars)
W_wat.wet<- subset(wet, select=wat_vars)

#Drop variables with low prevalence
W.wet<-subset(W.wet, select= -c(roof, asset_radio, asset_phone, asset_clock))
W_fly.wet<-subset(W_fly.wet, select= -c(tapeintact_kit))

#Combine constant covariates with sample-specific covariates
W_wat.wet<- cbind(W.wet,W_wat.wet)
W_fly.wet<- cbind(W.wet,W_fly.wet)
W_soil.wet<- cbind(W.wet,W_soil.wet)
W_food.wet<- cbind(W.wet,W_food.wet)

#Re-subset W to dry and wet
W.dry<- subset(dry, select=Wvars)
W_soil.dry<- subset(dry, select=soil_vars)
W_fly.dry<- subset(dry, select=fly_vars)
W_food.dry<- subset(dry, select=food_vars)
W_wat.dry<- subset(dry, select=wat_vars)

#Drop variables with low prevalence
W.dry<-subset(W.dry, select= -c(roof, asset_radio, asset_phone, asset_clock))
W_fly.dry<-subset(W_fly.dry, select= -c(tapeintact_kit))

#Combine constant covariates with sample-specific covariates
W_wat.dry<- cbind(W.dry,W_wat.dry)
W_fly.dry<- cbind(W.dry,W_fly.dry)
W_soil.dry<- cbind(W.dry,W_soil.dry)
W_food.dry<- cbind(W.dry,W_food.dry)


#subset lists of outcomes to dry and wet months
Y.pos.wet<-subset(wet, select=c(ecposTW,ecposSW,ecposP,ecposH,ecposF,ecposS,ecposY,
         fcposTW,fcposSW,fcposP,fcposH,fcposF,fcposS,fcposY,
         flycaught,mhdirt,chdirt))
Y.log.wet<-subset(wet, select=c(logecTW, logecSW, logecP, logecH, logecF, logecS, 
             logecY, logfcTW, logfcSW, logfcP, logfcH, logfcF,
             logfcS, logfcY, numfly))
Y.pos.dry<-subset(dry, select=c(ecposTW,ecposSW,ecposP,ecposH,ecposF,ecposS,ecposY,
         fcposTW,fcposSW,fcposP,fcposH,fcposF,fcposS,fcposY,
         flycaught,mhdirt,chdirt))
Y.log.dry<-subset(dry, select=c(logecTW, logecSW, logecP, logecH, logecF, logecS, 
             logecY, logfcTW, logfcSW, logfcP, logfcH, logfcF,
             logfcS, logfcY, numfly))


#subset lists of covariate adjustments to dry and wet months
#-not needed as EM is unadjusted


#Run positive outcomes TMLEs
pos_wet<-apply_tmle(Ys=Y.pos.wet,tr=wet$tr,W=NULL,id=wet$block, contrast=contrasts, family="binomial", measure="RR")

    
      
#Prevalence Ratio
#(H1): sanitation vs. control, combined WSH vs. control  
ec_tw_rr_h1_wet_M<-pos_wet[[1]][1:2,5:8]
ec_sw_rr_h1_wet_M<-pos_wet[[2]][1:2,5:8]
ec_p_rr_h1_wet_M<-pos_wet[[3]][1:2,5:8]
ec_h_rr_h1_wet_M<-pos_wet[[4]][1:2,5:8]
ec_f_rr_h1_wet_M<-pos_wet[[5]][1:2,5:8]
ec_s_rr_h1_wet_M<-pos_wet[[6]][1:2,5:8]
ec_y_rr_h1_wet_M<-pos_wet[[7]][1:2,5:8]
fc_tw_rr_h1_wet_M<-pos_wet[[8]][1:2,5:8]
fc_sw_rr_h1_wet_M<-pos_wet[[9]][1:2,5:8]
fc_p_rr_h1_wet_M<-pos_wet[[10]][1:2,5:8]
fc_h_rr_h1_wet_M<-pos_wet[[11]][1:2,5:8]
fc_f_rr_h1_wet_M<-pos_wet[[12]][1:2,5:8]
fc_s_rr_h1_wet_M<-pos_wet[[13]][1:2,5:8]
fc_y_rr_h1_wet_M<-pos_wet[[14]][1:2,5:8]
fly_rr_h1_wet_M<-pos_wet[[15]][1:2,5:8]
mh_rr_h1_wet_M<-pos_wet[[16]][1:2,5:8]
ch_rr_h1_wet_M<-pos_wet[[17]][1:2,5:8]

#(H2): combined WSH vs. sanitation.
ec_tw_rr_h2_wet_M<-pos_wet[[1]][3,5:8]
ec_sw_rr_h2_wet_M<-pos_wet[[2]][3,5:8]
ec_p_rr_h2_wet_M<-pos_wet[[3]][3,5:8]
ec_h_rr_h2_wet_M<-pos_wet[[4]][3,5:8]
ec_f_rr_h2_wet_M<-pos_wet[[5]][3,5:8]
ec_s_rr_h2_wet_M<-pos_wet[[6]][3,5:8]
ec_y_rr_h2_wet_M<-pos_wet[[7]][3,5:8]
fc_tw_rr_h2_wet_M<-pos_wet[[8]][3,5:8]
fc_sw_rr_h2_wet_M<-pos_wet[[9]][3,5:8]
fc_p_rr_h2_wet_M<-pos_wet[[10]][3,5:8]
fc_h_rr_h2_wet_M<-pos_wet[[11]][3,5:8]
fc_f_rr_h2_wet_M<-pos_wet[[12]][3,5:8]
fc_s_rr_h2_wet_M<-pos_wet[[13]][3,5:8]
fc_y_rr_h2_wet_M<-pos_wet[[14]][3,5:8]
fly_rr_h2_wet_M<-pos_wet[[15]][3,5:8]
mh_rr_h2_wet_M<-pos_wet[[16]][3,5:8]
ch_rr_h2_wet_M<-pos_wet[[17]][3,5:8]


#Prevalence Difference
#(h1): sanitation vs. control, combined WSH vs. control  
ec_tw_rd_h1_wet_M<-pos_wet[[1]][1:2,1:4]
ec_sw_rd_h1_wet_M<-pos_wet[[2]][1:2,1:4]
ec_p_rd_h1_wet_M<-pos_wet[[3]][1:2,1:4]
ec_h_rd_h1_wet_M<-pos_wet[[4]][1:2,1:4]
ec_f_rd_h1_wet_M<-pos_wet[[5]][1:2,1:4]
ec_s_rd_h1_wet_M<-pos_wet[[6]][1:2,1:4]
ec_y_rd_h1_wet_M<-pos_wet[[7]][1:2,1:4]
fc_tw_rd_h1_wet_M<-pos_wet[[8]][1:2,1:4]
fc_sw_rd_h1_wet_M<-pos_wet[[9]][1:2,1:4]
fc_p_rd_h1_wet_M<-pos_wet[[10]][1:2,1:4]
fc_h_rd_h1_wet_M<-pos_wet[[11]][1:2,1:4]
fc_f_rd_h1_wet_M<-pos_wet[[12]][1:2,1:4]
fc_s_rd_h1_wet_M<-pos_wet[[13]][1:2,1:4]
fc_y_rd_h1_wet_M<-pos_wet[[14]][1:2,1:4]
fly_rd_h1_wet_M<-pos_wet[[15]][1:2,1:4]
mh_rd_h1_wet_M<-pos_wet[[16]][1:2,1:4]
ch_rd_h1_wet_M<-pos_wet[[17]][1:2,1:4]

#(H2): combined WSH vs. sanitation.
ec_tw_rd_h2_wet_M<-pos_wet[[1]][3,1:4]
ec_sw_rd_h2_wet_M<-pos_wet[[2]][3,1:4]
ec_p_rd_h2_wet_M<-pos_wet[[3]][3,1:4]
ec_h_rd_h2_wet_M<-pos_wet[[4]][3,1:4]
ec_f_rd_h2_wet_M<-pos_wet[[5]][3,1:4]
ec_s_rd_h2_wet_M<-pos_wet[[6]][3,1:4]
ec_y_rd_h2_wet_M<-pos_wet[[7]][3,1:4]
fc_tw_rd_h2_wet_M<-pos_wet[[8]][3,1:4]
fc_sw_rd_h2_wet_M<-pos_wet[[9]][3,1:4]
fc_p_rd_h2_wet_M<-pos_wet[[10]][3,1:4]
fc_h_rd_h2_wet_M<-pos_wet[[11]][3,1:4]
fc_f_rd_h2_wet_M<-pos_wet[[12]][3,1:4]
fc_s_rd_h2_wet_M<-pos_wet[[13]][3,1:4]
fc_y_rd_h2_wet_M<-pos_wet[[14]][3,1:4]
fly_rd_h2_wet_M<-pos_wet[[15]][3,1:4]
mh_rd_h2_wet_M<-pos_wet[[16]][3,1:4]
ch_rd_h2_wet_M<-pos_wet[[17]][3,1:4]


#Run log count TMLEs
log_wet<-apply_tmle(Ys=Y.log.wet, tr=wet$tr,W=NULL,id=wet$block, contrast=contrasts, family="gaussian", measure="RD")

#(h1): sanitation vs. control, combined WSH vs. control  
ec_tw_dif_h1_wet_M<-log_wet[[1]][1:2,1:4]
ec_sw_dif_h1_wet_M<-log_wet[[2]][1:2,1:4]
ec_p_dif_h1_wet_M<-log_wet[[3]][1:2,1:4]
ec_h_dif_h1_wet_M<-log_wet[[4]][1:2,1:4]
ec_f_dif_h1_wet_M<-log_wet[[5]][1:2,1:4]
ec_s_dif_h1_wet_M<-log_wet[[6]][1:2,1:4]
ec_y_dif_h1_wet_M<-log_wet[[7]][1:2,1:4]
fc_tw_dif_h1_wet_M<-log_wet[[8]][1:2,1:4]
fc_sw_dif_h1_wet_M<-log_wet[[9]][1:2,1:4]
fc_p_dif_h1_wet_M<-log_wet[[10]][1:2,1:4]
fc_h_dif_h1_wet_M<-log_wet[[11]][1:2,1:4]
fc_f_dif_h1_wet_M<-log_wet[[12]][1:2,1:4]
fc_s_dif_h1_wet_M<-log_wet[[13]][1:2,1:4]
fc_y_dif_h1_wet_M<-log_wet[[14]][1:2,1:4]
fly_dif_h1_wet_M<-log_wet[[15]][1:2,1:4]

#(H2): combined WSH vs. sanitation.
ec_tw_dif_h2_wet_M<-log_wet[[1]][3,1:4]
ec_sw_dif_h2_wet_M<-log_wet[[2]][3,1:4]
ec_p_dif_h2_wet_M<-log_wet[[3]][3,1:4]
ec_h_dif_h2_wet_M<-log_wet[[4]][3,1:4]
ec_f_dif_h2_wet_M<-log_wet[[5]][3,1:4]
ec_s_dif_h2_wet_M<-log_wet[[6]][3,1:4]
ec_y_dif_h2_wet_M<-log_wet[[7]][3,1:4]
fc_tw_dif_h2_wet_M<-log_wet[[8]][3,1:4]
fc_sw_dif_h2_wet_M<-log_wet[[9]][3,1:4]
fc_p_dif_h2_wet_M<-log_wet[[10]][3,1:4]
fc_h_dif_h2_wet_M<-log_wet[[11]][3,1:4]
fc_f_dif_h2_wet_M<-log_wet[[12]][3,1:4]
fc_s_dif_h2_wet_M<-log_wet[[13]][3,1:4]
fc_y_dif_h2_wet_M<-log_wet[[14]][3,1:4]
fly_dif_h2_wet_M<-log_wet[[15]][3,1:4]










#Run positive outcomes TMLEs-dry
pos_dry<-apply_tmle(Ys=Y.pos.dry,tr=dry$tr,W=NULL,id=dry$block, contrast=contrasts, family="binomial", measure="RR")

    
      
#Prevalence Ratio
#(H1): sanitation vs. control, combined WSH vs. control  
ec_tw_rr_h1_dry_M<-pos_dry[[1]][1:2,5:8]
ec_sw_rr_h1_dry_M<-pos_dry[[2]][1:2,5:8]
ec_p_rr_h1_dry_M<-pos_dry[[3]][1:2,5:8]
ec_h_rr_h1_dry_M<-pos_dry[[4]][1:2,5:8]
ec_f_rr_h1_dry_M<-pos_dry[[5]][1:2,5:8]
ec_s_rr_h1_dry_M<-pos_dry[[6]][1:2,5:8]
ec_y_rr_h1_dry_M<-pos_dry[[7]][1:2,5:8]
fc_tw_rr_h1_dry_M<-pos_dry[[8]][1:2,5:8]
fc_sw_rr_h1_dry_M<-pos_dry[[9]][1:2,5:8]
fc_p_rr_h1_dry_M<-pos_dry[[10]][1:2,5:8]
fc_h_rr_h1_dry_M<-pos_dry[[11]][1:2,5:8]
fc_f_rr_h1_dry_M<-pos_dry[[12]][1:2,5:8]
fc_s_rr_h1_dry_M<-pos_dry[[13]][1:2,5:8]
fc_y_rr_h1_dry_M<-pos_dry[[14]][1:2,5:8]
fly_rr_h1_dry_M<-pos_dry[[15]][1:2,5:8]
mh_rr_h1_dry_M<-pos_dry[[16]][1:2,5:8]
ch_rr_h1_dry_M<-pos_dry[[17]][1:2,5:8]

#(H2): combined WSH vs. sanitation.
ec_tw_rr_h2_dry_M<-pos_dry[[1]][3,5:8]
ec_sw_rr_h2_dry_M<-pos_dry[[2]][3,5:8]
ec_p_rr_h2_dry_M<-pos_dry[[3]][3,5:8]
ec_h_rr_h2_dry_M<-pos_dry[[4]][3,5:8]
ec_f_rr_h2_dry_M<-pos_dry[[5]][3,5:8]
ec_s_rr_h2_dry_M<-pos_dry[[6]][3,5:8]
ec_y_rr_h2_dry_M<-pos_dry[[7]][3,5:8]
fc_tw_rr_h2_dry_M<-pos_dry[[8]][3,5:8]
fc_sw_rr_h2_dry_M<-pos_dry[[9]][3,5:8]
fc_p_rr_h2_dry_M<-pos_dry[[10]][3,5:8]
fc_h_rr_h2_dry_M<-pos_dry[[11]][3,5:8]
fc_f_rr_h2_dry_M<-pos_dry[[12]][3,5:8]
fc_s_rr_h2_dry_M<-pos_dry[[13]][3,5:8]
fc_y_rr_h2_dry_M<-pos_dry[[14]][3,5:8]
fly_rr_h2_dry_M<-pos_dry[[15]][3,5:8]
mh_rr_h2_dry_M<-pos_dry[[16]][3,5:8]
ch_rr_h2_dry_M<-pos_dry[[17]][3,5:8]


#Prevalence Difference
#(h1): sanitation vs. control, combined WSH vs. control  
ec_tw_rd_h1_dry_M<-pos_dry[[1]][1:2,1:4]
ec_sw_rd_h1_dry_M<-pos_dry[[2]][1:2,1:4]
ec_p_rd_h1_dry_M<-pos_dry[[3]][1:2,1:4]
ec_h_rd_h1_dry_M<-pos_dry[[4]][1:2,1:4]
ec_f_rd_h1_dry_M<-pos_dry[[5]][1:2,1:4]
ec_s_rd_h1_dry_M<-pos_dry[[6]][1:2,1:4]
ec_y_rd_h1_dry_M<-pos_dry[[7]][1:2,1:4]
fc_tw_rd_h1_dry_M<-pos_dry[[8]][1:2,1:4]
fc_sw_rd_h1_dry_M<-pos_dry[[9]][1:2,1:4]
fc_p_rd_h1_dry_M<-pos_dry[[10]][1:2,1:4]
fc_h_rd_h1_dry_M<-pos_dry[[11]][1:2,1:4]
fc_f_rd_h1_dry_M<-pos_dry[[12]][1:2,1:4]
fc_s_rd_h1_dry_M<-pos_dry[[13]][1:2,1:4]
fc_y_rd_h1_dry_M<-pos_dry[[14]][1:2,1:4]
fly_rd_h1_dry_M<-pos_dry[[15]][1:2,1:4]
mh_rd_h1_dry_M<-pos_dry[[16]][1:2,1:4]
ch_rd_h1_dry_M<-pos_dry[[17]][1:2,1:4]

#(H2): combined WSH vs. sanitation.
ec_tw_rd_h2_dry_M<-pos_dry[[1]][3,1:4]
ec_sw_rd_h2_dry_M<-pos_dry[[2]][3,1:4]
ec_p_rd_h2_dry_M<-pos_dry[[3]][3,1:4]
ec_h_rd_h2_dry_M<-pos_dry[[4]][3,1:4]
ec_f_rd_h2_dry_M<-pos_dry[[5]][3,1:4]
ec_s_rd_h2_dry_M<-pos_dry[[6]][3,1:4]
ec_y_rd_h2_dry_M<-pos_dry[[7]][3,1:4]
fc_tw_rd_h2_dry_M<-pos_dry[[8]][3,1:4]
fc_sw_rd_h2_dry_M<-pos_dry[[9]][3,1:4]
fc_p_rd_h2_dry_M<-pos_dry[[10]][3,1:4]
fc_h_rd_h2_dry_M<-pos_dry[[11]][3,1:4]
fc_f_rd_h2_dry_M<-pos_dry[[12]][3,1:4]
fc_s_rd_h2_dry_M<-pos_dry[[13]][3,1:4]
fc_y_rd_h2_dry_M<-pos_dry[[14]][3,1:4]
fly_rd_h2_dry_M<-pos_dry[[15]][3,1:4]
mh_rd_h2_dry_M<-pos_dry[[16]][3,1:4]
ch_rd_h2_dry_M<-pos_dry[[17]][3,1:4]


#Run log count TMLEs
log_dry<-apply_tmle(Ys=Y.log.dry, tr=dry$tr,W=NULL,id=dry$block, contrast=contrasts, family="gaussian", measure="RD")

#(h1): sanitation vs. control, combined WSH vs. control  
ec_tw_dif_h1_dry_M<-log_dry[[1]][1:2,1:4]
ec_sw_dif_h1_dry_M<-log_dry[[2]][1:2,1:4]
ec_p_dif_h1_dry_M<-log_dry[[3]][1:2,1:4]
ec_h_dif_h1_dry_M<-log_dry[[4]][1:2,1:4]
ec_f_dif_h1_dry_M<-log_dry[[5]][1:2,1:4]
ec_s_dif_h1_dry_M<-log_dry[[6]][1:2,1:4]
ec_y_dif_h1_dry_M<-log_dry[[7]][1:2,1:4]
fc_tw_dif_h1_dry_M<-log_dry[[8]][1:2,1:4]
fc_sw_dif_h1_dry_M<-log_dry[[9]][1:2,1:4]
fc_p_dif_h1_dry_M<-log_dry[[10]][1:2,1:4]
fc_h_dif_h1_dry_M<-log_dry[[11]][1:2,1:4]
fc_f_dif_h1_dry_M<-log_dry[[12]][1:2,1:4]
fc_s_dif_h1_dry_M<-log_dry[[13]][1:2,1:4]
fc_y_dif_h1_dry_M<-log_dry[[14]][1:2,1:4]
fly_dif_h1_dry_M<-log_dry[[15]][1:2,1:4]

#(H2): combined WSH vs. sanitation.
ec_tw_dif_h2_dry_M<-log_dry[[1]][3,1:4]
ec_sw_dif_h2_dry_M<-log_dry[[2]][3,1:4]
ec_p_dif_h2_dry_M<-log_dry[[3]][3,1:4]
ec_h_dif_h2_dry_M<-log_dry[[4]][3,1:4]
ec_f_dif_h2_dry_M<-log_dry[[5]][3,1:4]
ec_s_dif_h2_dry_M<-log_dry[[6]][3,1:4]
ec_y_dif_h2_dry_M<-log_dry[[7]][3,1:4]
fc_tw_dif_h2_dry_M<-log_dry[[8]][3,1:4]
fc_sw_dif_h2_dry_M<-log_dry[[9]][3,1:4]
fc_p_dif_h2_dry_M<-log_dry[[10]][3,1:4]
fc_h_dif_h2_dry_M<-log_dry[[11]][3,1:4]
fc_f_dif_h2_dry_M<-log_dry[[12]][3,1:4]
fc_s_dif_h2_dry_M<-log_dry[[13]][3,1:4]
fc_y_dif_h2_dry_M<-log_dry[[14]][3,1:4]
fly_dif_h2_dry_M<-log_dry[[15]][3,1:4]












########################
#Save Objects for replication
########################
setwd("C:/Users/andre/Dropbox/WASHB EML/Results/Andrew")

#Save prevalences
save(ec_tw_prev_M,
ec_sw_prev_M,
ec_p_prev_M,
ec_h_prev_M,
ec_f_prev_M,
ec_s_prev_M,
ec_y_prev_M,
fc_tw_prev_M,
fc_sw_prev_M,
fc_p_prev_M,
fc_h_prev_M,
fc_f_prev_M,
fc_s_prev_M,
fc_y_prev_M,
fly_prev_M,
mh_prev_M,
ch_prev_M,
file="Env_Prevalence_Andrew_WB.Rdata")


#Save means
save(ec_tw_mn_M,
ec_sw_mn_M,
ec_p_mn_M,
ec_h_mn_M,
ec_f_mn_M,
ec_s_mn_M,
ec_y_mn_M,
fc_tw_mn_M,
fc_sw_mn_M,
fc_p_mn_M,
fc_h_mn_M,
fc_f_mn_M,
fc_s_mn_M,
fc_y_mn_M,
fly_mn_M,
file="Env_Means_Andrew_WB.Rdata")




#Save unadjusted RR
save(
ec_tw_rr_h1_unadj_M,
ec_sw_rr_h1_unadj_M,
ec_p_rr_h1_unadj_M,
ec_h_rr_h1_unadj_M,
ec_f_rr_h1_unadj_M,
ec_s_rr_h1_unadj_M,
ec_y_rr_h1_unadj_M,
fc_tw_rr_h1_unadj_M,
fc_sw_rr_h1_unadj_M,
fc_p_rr_h1_unadj_M,
fc_h_rr_h1_unadj_M,
fc_f_rr_h1_unadj_M,
fc_s_rr_h1_unadj_M,
fc_y_rr_h1_unadj_M,
fly_rr_h1_unadj_M,
mh_rr_h1_unadj_M,
ch_rr_h1_unadj_M,
ec_tw_rr_h2_unadj_M,
ec_sw_rr_h2_unadj_M,
ec_p_rr_h2_unadj_M,
ec_h_rr_h2_unadj_M,
ec_f_rr_h2_unadj_M,
ec_s_rr_h2_unadj_M,
ec_y_rr_h2_unadj_M,
fc_tw_rr_h2_unadj_M,
fc_sw_rr_h2_unadj_M,
fc_p_rr_h2_unadj_M,
fc_h_rr_h2_unadj_M,
fc_f_rr_h2_unadj_M,
fc_s_rr_h2_unadj_M,
fc_y_rr_h2_unadj_M,
fly_rr_h2_unadj_M,
mh_rr_h2_unadj_M,
ch_rr_h2_unadj_M,
file="Env_Risk_Ratio_unadj_Andrew_WB.Rdata")

#Save unadjusted RD
save(
ec_tw_rd_h1_unadj_M,
ec_sw_rd_h1_unadj_M,
ec_p_rd_h1_unadj_M,
ec_h_rd_h1_unadj_M,
ec_f_rd_h1_unadj_M,
ec_s_rd_h1_unadj_M,
ec_y_rd_h1_unadj_M,
fc_tw_rd_h1_unadj_M,
fc_sw_rd_h1_unadj_M,
fc_p_rd_h1_unadj_M,
fc_h_rd_h1_unadj_M,
fc_f_rd_h1_unadj_M,
fc_s_rd_h1_unadj_M,
fc_y_rd_h1_unadj_M,
fly_rd_h1_unadj_M,
mh_rd_h1_unadj_M,
ch_rd_h1_unadj_M,
ec_tw_rd_h2_unadj_M,
ec_sw_rd_h2_unadj_M,
ec_p_rd_h2_unadj_M,
ec_h_rd_h2_unadj_M,
ec_f_rd_h2_unadj_M,
ec_s_rd_h2_unadj_M,
ec_y_rd_h2_unadj_M,
fc_tw_rd_h2_unadj_M,
fc_sw_rd_h2_unadj_M,
fc_p_rd_h2_unadj_M,
fc_h_rd_h2_unadj_M,
fc_f_rd_h2_unadj_M,
fc_s_rd_h2_unadj_M,
fc_y_rd_h2_unadj_M,
fly_rd_h2_unadj_M,
mh_rd_h2_unadj_M,
ch_rd_h2_unadj_M,
file="Env_Risk_diff_unadj_Andrew_WB.Rdata")

#Save count difference
save(
ec_tw_dif_h1_unadj_M,
ec_sw_dif_h1_unadj_M,
ec_p_dif_h1_unadj_M,
ec_h_dif_h1_unadj_M,
ec_f_dif_h1_unadj_M,
ec_s_dif_h1_unadj_M,
ec_y_dif_h1_unadj_M,
fc_tw_dif_h1_unadj_M,
fc_sw_dif_h1_unadj_M,
fc_p_dif_h1_unadj_M,
fc_h_dif_h1_unadj_M,
fc_f_dif_h1_unadj_M,
fc_s_dif_h1_unadj_M,
fc_y_dif_h1_unadj_M,
fly_dif_h1_unadj_M,
ec_tw_dif_h2_unadj_M,
ec_sw_dif_h2_unadj_M,
ec_p_dif_h2_unadj_M,
ec_h_dif_h2_unadj_M,
ec_f_dif_h2_unadj_M,
ec_s_dif_h2_unadj_M,
ec_y_dif_h2_unadj_M,
fc_tw_dif_h2_unadj_M,
fc_sw_dif_h2_unadj_M,
fc_p_dif_h2_unadj_M,
fc_h_dif_h2_unadj_M,
fc_f_dif_h2_unadj_M,
fc_s_dif_h2_unadj_M,
fc_y_dif_h2_unadj_M,
fly_dif_h2_unadj_M,
file="Env_Count_Diff_unadj_Andrew_WB.Rdata")




#Save adjusted RR
save(
ec_tw_rr_h1_adj_M,
ec_sw_rr_h1_adj_M,
ec_p_rr_h1_adj_M,
ec_h_rr_h1_adj_M,
ec_f_rr_h1_adj_M,
ec_s_rr_h1_adj_M,
ec_y_rr_h1_adj_M,
fc_tw_rr_h1_adj_M,
fc_sw_rr_h1_adj_M,
fc_p_rr_h1_adj_M,
fc_h_rr_h1_adj_M,
fc_f_rr_h1_adj_M,
fc_s_rr_h1_adj_M,
fc_y_rr_h1_adj_M,
fly_rr_h1_adj_M,
mh_rr_h1_adj_M,
ch_rr_h1_adj_M,
ec_tw_rr_h2_adj_M,
ec_sw_rr_h2_adj_M,
ec_p_rr_h2_adj_M,
ec_h_rr_h2_adj_M,
ec_f_rr_h2_adj_M,
ec_s_rr_h2_adj_M,
ec_y_rr_h2_adj_M,
fc_tw_rr_h2_adj_M,
fc_sw_rr_h2_adj_M,
fc_p_rr_h2_adj_M,
fc_h_rr_h2_adj_M,
fc_f_rr_h2_adj_M,
fc_s_rr_h2_adj_M,
fc_y_rr_h2_adj_M,
fly_rr_h2_adj_M,
mh_rr_h2_adj_M,
ch_rr_h2_adj_M,
file="Env_Risk_Ratio_adj_Andrew_WB.Rdata")

#Save adjusted RD
save(
ec_tw_rd_h1_adj_M,
ec_sw_rd_h1_adj_M,
ec_p_rd_h1_adj_M,
ec_h_rd_h1_adj_M,
ec_f_rd_h1_adj_M,
ec_s_rd_h1_adj_M,
ec_y_rd_h1_adj_M,
fc_tw_rd_h1_adj_M,
fc_sw_rd_h1_adj_M,
fc_p_rd_h1_adj_M,
fc_h_rd_h1_adj_M,
fc_f_rd_h1_adj_M,
fc_s_rd_h1_adj_M,
fc_y_rd_h1_adj_M,
fly_rd_h1_adj_M,
mh_rd_h1_adj_M,
ch_rd_h1_adj_M,
ec_tw_rd_h2_adj_M,
ec_sw_rd_h2_adj_M,
ec_p_rd_h2_adj_M,
ec_h_rd_h2_adj_M,
ec_f_rd_h2_adj_M,
ec_s_rd_h2_adj_M,
ec_y_rd_h2_adj_M,
fc_tw_rd_h2_adj_M,
fc_sw_rd_h2_adj_M,
fc_p_rd_h2_adj_M,
fc_h_rd_h2_adj_M,
fc_f_rd_h2_adj_M,
fc_s_rd_h2_adj_M,
fc_y_rd_h2_adj_M,
fly_rd_h2_adj_M,
mh_rd_h2_adj_M,
ch_rd_h2_adj_M,
file="Env_Risk_diff_adj_Andrew_WB.Rdata")

#Save count difference
save(
ec_tw_dif_h1_adj_M,
ec_sw_dif_h1_adj_M,
ec_p_dif_h1_adj_M,
ec_h_dif_h1_adj_M,
ec_f_dif_h1_adj_M,
ec_s_dif_h1_adj_M,
ec_y_dif_h1_adj_M,
fc_tw_dif_h1_adj_M,
fc_sw_dif_h1_adj_M,
fc_p_dif_h1_adj_M,
fc_h_dif_h1_adj_M,
fc_f_dif_h1_adj_M,
fc_s_dif_h1_adj_M,
fc_y_dif_h1_adj_M,
fly_dif_h1_adj_M,
ec_tw_dif_h2_adj_M,
ec_sw_dif_h2_adj_M,
ec_p_dif_h2_adj_M,
ec_h_dif_h2_adj_M,
ec_f_dif_h2_adj_M,
ec_s_dif_h2_adj_M,
ec_y_dif_h2_adj_M,
fc_tw_dif_h2_adj_M,
fc_sw_dif_h2_adj_M,
fc_p_dif_h2_adj_M,
fc_h_dif_h2_adj_M,
fc_f_dif_h2_adj_M,
fc_s_dif_h2_adj_M,
fc_y_dif_h2_adj_M,
fly_dif_h2_adj_M,
file="Env_Count_Diff_adj_Andrew_WB.Rdata")

















#Save wet RD
save(
ec_tw_rr_h1_wet_M,
ec_sw_rr_h1_wet_M,
ec_p_rr_h1_wet_M,
ec_h_rr_h1_wet_M,
ec_f_rr_h1_wet_M,
ec_s_rr_h1_wet_M,
ec_y_rr_h1_wet_M,
fc_tw_rr_h1_wet_M,
fc_sw_rr_h1_wet_M,
fc_p_rr_h1_wet_M,
fc_h_rr_h1_wet_M,
fc_f_rr_h1_wet_M,
fc_s_rr_h1_wet_M,
fc_y_rr_h1_wet_M,
fly_rr_h1_wet_M,
mh_rr_h1_wet_M,
ch_rr_h1_wet_M,
ec_tw_rr_h2_wet_M,
ec_sw_rr_h2_wet_M,
ec_p_rr_h2_wet_M,
ec_h_rr_h2_wet_M,
ec_f_rr_h2_wet_M,
ec_s_rr_h2_wet_M,
ec_y_rr_h2_wet_M,
fc_tw_rr_h2_wet_M,
fc_sw_rr_h2_wet_M,
fc_p_rr_h2_wet_M,
fc_h_rr_h2_wet_M,
fc_f_rr_h2_wet_M,
fc_s_rr_h2_wet_M,
fc_y_rr_h2_wet_M,
fly_rr_h2_wet_M,
mh_rr_h2_wet_M,
ch_rr_h2_wet_M,
file="Env_Risk_Ratio_wet_Andrew_WB.Rdata")

save(
ec_tw_rd_h1_wet_M,
ec_sw_rd_h1_wet_M,
ec_p_rd_h1_wet_M,
ec_h_rd_h1_wet_M,
ec_f_rd_h1_wet_M,
ec_s_rd_h1_wet_M,
ec_y_rd_h1_wet_M,
fc_tw_rd_h1_wet_M,
fc_sw_rd_h1_wet_M,
fc_p_rd_h1_wet_M,
fc_h_rd_h1_wet_M,
fc_f_rd_h1_wet_M,
fc_s_rd_h1_wet_M,
fc_y_rd_h1_wet_M,
fly_rd_h1_wet_M,
mh_rd_h1_wet_M,
ch_rd_h1_wet_M,
ec_tw_rd_h2_wet_M,
ec_sw_rd_h2_wet_M,
ec_p_rd_h2_wet_M,
ec_h_rd_h2_wet_M,
ec_f_rd_h2_wet_M,
ec_s_rd_h2_wet_M,
ec_y_rd_h2_wet_M,
fc_tw_rd_h2_wet_M,
fc_sw_rd_h2_wet_M,
fc_p_rd_h2_wet_M,
fc_h_rd_h2_wet_M,
fc_f_rd_h2_wet_M,
fc_s_rd_h2_wet_M,
fc_y_rd_h2_wet_M,
fly_rd_h2_wet_M,
mh_rd_h2_wet_M,
ch_rd_h2_wet_M,
file="Env_Risk_diff_wet_Andrew_WB.Rdata")

#Save count difference
save(
ec_tw_dif_h1_wet_M,
ec_sw_dif_h1_wet_M,
ec_p_dif_h1_wet_M,
ec_h_dif_h1_wet_M,
ec_f_dif_h1_wet_M,
ec_s_dif_h1_wet_M,
ec_y_dif_h1_wet_M,
fc_tw_dif_h1_wet_M,
fc_sw_dif_h1_wet_M,
fc_p_dif_h1_wet_M,
fc_h_dif_h1_wet_M,
fc_f_dif_h1_wet_M,
fc_s_dif_h1_wet_M,
fc_y_dif_h1_wet_M,
fly_dif_h1_wet_M,
ec_tw_dif_h2_wet_M,
ec_sw_dif_h2_wet_M,
ec_p_dif_h2_wet_M,
ec_h_dif_h2_wet_M,
ec_f_dif_h2_wet_M,
ec_s_dif_h2_wet_M,
ec_y_dif_h2_wet_M,
fc_tw_dif_h2_wet_M,
fc_sw_dif_h2_wet_M,
fc_p_dif_h2_wet_M,
fc_h_dif_h2_wet_M,
fc_f_dif_h2_wet_M,
fc_s_dif_h2_wet_M,
fc_y_dif_h2_wet_M,
fly_dif_h2_wet_M,
file="Env_Count_Diff_wet_Andrew_WB.Rdata")





#Save dry RR
save(
ec_tw_rr_h1_dry_M,
ec_sw_rr_h1_dry_M,
ec_p_rr_h1_dry_M,
ec_h_rr_h1_dry_M,
ec_f_rr_h1_dry_M,
ec_s_rr_h1_dry_M,
ec_y_rr_h1_dry_M,
fc_tw_rr_h1_dry_M,
fc_sw_rr_h1_dry_M,
fc_p_rr_h1_dry_M,
fc_h_rr_h1_dry_M,
fc_f_rr_h1_dry_M,
fc_s_rr_h1_dry_M,
fc_y_rr_h1_dry_M,
fly_rr_h1_dry_M,
mh_rr_h1_dry_M,
ch_rr_h1_dry_M,
ec_tw_rr_h2_dry_M,
ec_sw_rr_h2_dry_M,
ec_p_rr_h2_dry_M,
ec_h_rr_h2_dry_M,
ec_f_rr_h2_dry_M,
ec_s_rr_h2_dry_M,
ec_y_rr_h2_dry_M,
fc_tw_rr_h2_dry_M,
fc_sw_rr_h2_dry_M,
fc_p_rr_h2_dry_M,
fc_h_rr_h2_dry_M,
fc_f_rr_h2_dry_M,
fc_s_rr_h2_dry_M,
fc_y_rr_h2_dry_M,
fly_rr_h2_dry_M,
mh_rr_h2_dry_M,
ch_rr_h2_dry_M,
file="Env_Risk_Ratio_dry_Andrew_WB.Rdata")

#Save dry RD
save(
ec_tw_rd_h1_dry_M,
ec_sw_rd_h1_dry_M,
ec_p_rd_h1_dry_M,
ec_h_rd_h1_dry_M,
ec_f_rd_h1_dry_M,
ec_s_rd_h1_dry_M,
ec_y_rd_h1_dry_M,
fc_tw_rd_h1_dry_M,
fc_sw_rd_h1_dry_M,
fc_p_rd_h1_dry_M,
fc_h_rd_h1_dry_M,
fc_f_rd_h1_dry_M,
fc_s_rd_h1_dry_M,
fc_y_rd_h1_dry_M,
fly_rd_h1_dry_M,
mh_rd_h1_dry_M,
ch_rd_h1_dry_M,
ec_tw_rd_h2_dry_M,
ec_sw_rd_h2_dry_M,
ec_p_rd_h2_dry_M,
ec_h_rd_h2_dry_M,
ec_f_rd_h2_dry_M,
ec_s_rd_h2_dry_M,
ec_y_rd_h2_dry_M,
fc_tw_rd_h2_dry_M,
fc_sw_rd_h2_dry_M,
fc_p_rd_h2_dry_M,
fc_h_rd_h2_dry_M,
fc_f_rd_h2_dry_M,
fc_s_rd_h2_dry_M,
fc_y_rd_h2_dry_M,
fly_rd_h2_dry_M,
mh_rd_h2_dry_M,
ch_rd_h2_dry_M,
file="Env_Risk_diff_dry_Andrew_WB.Rdata")

#Save count difference
save(
ec_tw_dif_h1_dry_M,
ec_sw_dif_h1_dry_M,
ec_p_dif_h1_dry_M,
ec_h_dif_h1_dry_M,
ec_f_dif_h1_dry_M,
ec_s_dif_h1_dry_M,
ec_y_dif_h1_dry_M,
fc_tw_dif_h1_dry_M,
fc_sw_dif_h1_dry_M,
fc_p_dif_h1_dry_M,
fc_h_dif_h1_dry_M,
fc_f_dif_h1_dry_M,
fc_s_dif_h1_dry_M,
fc_y_dif_h1_dry_M,
fly_dif_h1_dry_M,
ec_tw_dif_h2_dry_M,
ec_sw_dif_h2_dry_M,
ec_p_dif_h2_dry_M,
ec_h_dif_h2_dry_M,
ec_f_dif_h2_dry_M,
ec_s_dif_h2_dry_M,
ec_y_dif_h2_dry_M,
fc_tw_dif_h2_dry_M,
fc_sw_dif_h2_dry_M,
fc_p_dif_h2_dry_M,
fc_h_dif_h2_dry_M,
fc_f_dif_h2_dry_M,
fc_s_dif_h2_dry_M,
fc_y_dif_h2_dry_M,
fly_dif_h2_dry_M,
file="Env_Count_Diff_dry_Andrew_WB.Rdata")
