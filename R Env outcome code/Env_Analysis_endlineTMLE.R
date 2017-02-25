
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
endline<-read.csv("Env_endline_clean.csv",stringsAsFactors = TRUE)
#endline<-read.dta("Env_endline_clean.dta")
endline$clusterid<-factor(endline$clusterid)


head(endline)
#endline<-subset(endline, select=-c(clusterid)) #drop so not duplicates after merge with treatment data
d<-endline

#Load in unblinded treatment
#setwd("C:/Users/andre/Documents/WBB_env_analysis/Data/")
#load("washb-bangladesh-tr.Rdata")
#treatment<-d
#levels(treatment$tr)
#treatment$clusterid<-as.numeric(treatment$clusterid)
#treatment<-subset(treatment, select=-c(block)) 
#treatment$clusterid<-factor(treatment$clusterid)



#Load in enrollment data for adjusted analysis
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
enrol<-read.csv("washb-bangladesh-enrol+animals.csv",stringsAsFactors = TRUE)
enrol<-subset(enrol, select=-block)


#Merge treatment information 
#dim(endline)
#d<-left_join(endline,treatment, by="clusterid")
#dim(d)
#head(d)
#table(d$tr)
#treatment$tr <- factor(treatment$tr,levels=c("Control","Water","Handwashing","WSH","Nutrition","Nutrition + WSH"))
#table(d$tr)


#Merge in enrollment information
dim(d)
dim(enrol)
d<-left_join(d,enrol, by="dataid")
dim(d)


#Generate pooled treatment variables
d$tr.pool<-"NA"
d$tr.pool[d$tr=="WSH"|d$tr=="Nutrition + WSH"]<-"WSH+WSHN pooled"
d$tr.pool[d$tr=="Control"|d$tr=="Nutrition"]<-"C+N pooled"
d$tr.pool[d$tr=="Water"]<-"Water"
d$tr.pool[d$tr=="Handwashing"]<-"Handwashing"
d$tr.pool<-factor(d$tr.pool)
table(d$tr.pool)

#Set contrasts
contrasts=list(c("C+N pooled","WSH+WSHN pooled"), c("C+N pooled","Water"),c("C+N pooled","Handwashing"), 
               c("Water","WSH+WSHN pooled"),c("Handwashing","WSH+WSHN pooled"))


  
  
#test that all rows are matched to enrollment data
table(is.na(d$svydate)) 

#NOTE: Looks like one unmatched dataid
#d[is.na(d$svydate),]


#Set block as factor
table(is.na(d$block)) 

table(d$block)

d$block<-factor(d$block)


#Generate flycaught
#d$flycaught<-ifelse(d$numfly>0,1,0)

#Set treatment levels
d$tr=factor(d$tr,levels=c("Control","Nutrition","Water","Handwashing","WSH","Nutrition + WSH"))


########################
#Prevalences and 95% CI by arm
########################
colnames(d)
ec_t_prev_end_M<-d %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposT, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_h_prev_end_M<-d %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_dw_prev_end_M<-d %>% subset(tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_t_prev_end_M<-d %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposT, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_h_prev_end_M<-d %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_dw_prev_end_M<-d %>% subset(tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_kit_prev_end_M<-d  %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$flycaught_kit, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_lat_prev_end_M<-d  %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$flycaught_lat, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mh_prev_end_M<-d  %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$mhdirt, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ch_prev_end_M<-d  %>% subset(tr!="Water")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$chdirt, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 

ec_f_prev_end_M<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_f_prev_end_M<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 


########################
#Means and 95% CI by arm
########################

ec_t_mn_end_M<-d  %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecT, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_h_mn_end_M<-d  %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_dw_mn_end_M<-d  %>% subset(tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_t_mn_end_M<-d  %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcT, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_h_mn_end_M<-d  %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_dw_mn_end_M<-d  %>% subset(tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_kit_mn_end_M<-d  %>% subset(tr!="Water" & tr!="Handwashing")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$numfly_kit, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_lat_mn_end_M<-d  %>% subset(tr!="Water" & tr!="Handwashing")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$numfly_lat, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 

ec_f_mn_end_M<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_f_mn_end_M<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 





########################
#Unadjusted GLMs
########################

#Create TMLE wrapper function
apply_tmle<-function(Ys,tr=d$tr.pool,W=NULL,id=d$block, contrasts, family, measure="RR"){
  pair=NULL
  library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")
  fullcontrasts<-contrasts
  varlist<-colnames(Ys)
  res_list<-NULL
  for(i in 1:ncol(Ys)){
    contrasts<-fullcontrasts[[i]]
    cat("#",i,": ",varlist[i],"\n")
      if(measure=="RD"){
        temp<-matrix(NA, length(contrasts), 4)
      }else{
        temp<-matrix(NA, length(contrasts), 8)
      }
    rownames(temp)<-rep("a",length(contrasts))
    for(k in 1:length(contrasts)){
          rownames(temp)[k]<-paste0(contrasts[[k]][1], " v ",contrasts[[k]][2])
    }
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


#Order data for replicated SL
#d<-d[order(d$block,d$clusterid,d$dataid),]
d<-d[order(d$block,d$clusterid,d$dataid),]




#Create lists of outcomes
Y.pos<-subset(d, select=c(ecposT,ecposH,ecposW,fcposT,fcposH,fcposW,
         flycaught_kit,flycaught_lat,mhdirt,chdirt,ecposF,fcposF))
Y.log<-subset(d, select=c(logecT, logecH, logecW, logfcT, logfcH, logfcW, numfly_kit,numfly_lat,logecF,logfcF))


contrast1=contrasts
contrast2=list(c("C+N pooled","WSH+WSHN pooled"))
contrast3=list(c("C+N pooled","WSH+WSHN pooled"),c("C+N pooled","Handwashing"),c("Handwashing","WSH+WSHN pooled"))
contrast4=list(c("C+N pooled","WSH+WSHN pooled"),c("C+N pooled","Water"),c("Water","WSH+WSHN pooled"))
Y.pos.contrasts<-list(contrast2,contrast3,contrast4,contrast2,contrast3,contrast4,contrast2,contrast2,contrast3,contrast3,contrast1,contrast1)
Y.log.contrasts<-list(contrast2,contrast3,contrast4,contrast2,contrast3,contrast4,contrast2,contrast2,contrast1)


      #temp<-washb_tmle(Y=d$logecF, tr=d$tr.pool, W=NULL, id=d$block, pair=NULL, family="binomial", contrast=contrasts[[1]], Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=12345, print=F)



#Water
#envw=env[which(env$tr!="Handwashing"),]
#envw$tr=droplevels(envw$tr)
#envw$tr=factor(envw$tr,levels=c("Control","Nutrition","Water","WSH","Nutrition + WSH"))

#Hands
#envh=env[which(env$tr!="Water"),]
#envh$tr=droplevels(envh$tr)
#envh$tr=factor(envh$tr,levels=c("Control","Nutrition","Handwashing","WSH","Nutrition + WSH"))

#Toys
#envt=env[which(env$tr!="Water" & env$tr!="Handwashing"),]
#envt$tr=droplevels(envt$tr)
#envt$tr=factor(envh$tr,levels=c("Control","Nutrition","WSH","Nutrition + WSH"))

#Flies
#envy=env[which(env$tr!="Water" & env$tr!="Handwashing"),]
#envy$tr=droplevels(envy$tr)
#envy$tr=factor(envy$tr,levels=c("Control","Nutrition","WSH","Nutrition + WSH"))


#------------
#Binary outcomes
#------------
pos_unadj<-apply_tmle(Ys=Y.pos,tr=d$tr.pool,W=NULL,id=d$block, contrasts=Y.pos.contrasts, family="binomial", measure="RR")




#Prevalence Ratio-unadjusted
#(h1): sanitation vs. control, combined WSH vs. control  
ec_t_rr_h1_unadj_end_M<-pos_unadj[[1]][1,5:8]
ec_h_rr_h1_unadj_end_M<-pos_unadj[[2]][1:2,5:8]
ec_dw_rr_h1_unadj_end_M<-pos_unadj[[3]][1:2,5:8]
fc_t_rr_h1_unadj_end_M<-pos_unadj[[4]][1,5:8]
fc_h_rr_h1_unadj_end_M<-pos_unadj[[5]][1:2,5:8]
fc_dw_rr_h1_unadj_end_M<-pos_unadj[[6]][1:2,5:8]
fly_kit_rr_h1_unadj_end_M<-pos_unadj[[7]][1,5:8]
fly_lat_rr_h1_unadj_end_M<-pos_unadj[[8]][1,5:8]
mh_rr_h1_unadj_end_M<-pos_unadj[[9]][1:2,5:8]
ch_rr_h1_unadj_end_M<-pos_unadj[[10]][1:2,5:8]
ec_f_rr_h1_unadj_end_M<-pos_unadj[[11]][1:3,5:8]
fc_f_rr_h1_unadj_end_M<-pos_unadj[[12]][1:3,5:8]

#(H2): combined WSH vs. sanitation.
#ec_t_rr_h2_unadj_end_M<-pos_unadj[[1]][4:5,5:8]
ec_h_rr_h2_unadj_end_M<-pos_unadj[[2]][3,5:8]
ec_dw_rr_h2_unadj_end_M<-pos_unadj[[3]][3,5:8]
#fc_t_rr_h2_unadj_end_M<-pos_unadj[[4]][4:5,5:8]
fc_h_rr_h2_unadj_end_M<-pos_unadj[[5]][3,5:8]
fc_dw_rr_h2_unadj_end_M<-pos_unadj[[6]][3,5:8]
#fly_rr_h2_unadj_end_M<-pos_unadj[[7]][3,5:8]
mh_rr_h2_unadj_end_M<-pos_unadj[[9]][3,5:8]
ch_rr_h2_unadj_end_M<-pos_unadj[[10]][3,5:8]
ec_f_rr_h2_unadj_end_M<-pos_unadj[[11]][4:5,5:8]
fc_f_rr_h2_unadj_end_M<-pos_unadj[[12]][4:5,5:8]

#Prevalence Difference-unadjusted
#(h1): sanitation vs. control, combined WSH vs. control  
ec_t_rd_h1_unadj_end_M<-pos_unadj[[1]][1,1:4]
ec_h_rd_h1_unadj_end_M<-pos_unadj[[2]][1:2,1:4]
ec_dw_rd_h1_unadj_end_M<-pos_unadj[[3]][1:2,1:4]
fc_t_rd_h1_unadj_end_M<-pos_unadj[[4]][1,1:4]
fc_h_rd_h1_unadj_end_M<-pos_unadj[[5]][1:2,1:4]
fc_dw_rd_h1_unadj_end_M<-pos_unadj[[6]][1:2,1:4]
fly_kit_rd_h1_unadj_end_M<-pos_unadj[[7]][1,1:4]
fly_lat_rd_h1_unadj_end_M<-pos_unadj[[8]][1,1:4]
mh_rd_h1_unadj_end_M<-pos_unadj[[9]][1:2,1:4]
ch_rd_h1_unadj_end_M<-pos_unadj[[10]][1:2,1:4]
ec_f_rd_h1_unadj_end_M<-pos_unadj[[11]][1:3,1:4]
fc_f_rd_h1_unadj_end_M<-pos_unadj[[12]][1:3,1:4]


#(H2): combined WSH vs. sanitation.
#ec_t_rd_h2_unadj_end_M<-pos_unadj[[1]][4:5,1:4]
ec_h_rd_h2_unadj_end_M<-pos_unadj[[2]][3,1:4]
ec_dw_rd_h2_unadj_end_M<-pos_unadj[[3]][3,1:4]
#fc_t_rd_h2_unadj_end_M<-pos_unadj[[4]][3,1:4]
fc_h_rd_h2_unadj_end_M<-pos_unadj[[5]][3,1:4]
fc_dw_rd_h2_unadj_end_M<-pos_unadj[[6]][3,1:4]
#fly_rd_h2_unadj_end_M<-pos_unadj[[7]][3,1:4]
mh_rd_h2_unadj_end_M<-pos_unadj[[9]][3,1:4]
ch_rd_h2_unadj_end_M<-pos_unadj[[10]][3,1:4]
ec_f_rd_h2_unadj_end_M<-pos_unadj[[11]][4:5,1:4]
fc_f_rd_h2_unadj_end_M<-pos_unadj[[12]][4:5,1:4]


log_unadj<-apply_tmle(Ys=Y.log,tr=d$tr.pool,W=NULL,id=d$block, contrasts=Y.log.contrasts, family="binomial", measure="RR")


#(h1): sanitation vs. control, combined WSH vs. control  
ec_t_dif_h1_unadj_end_M<-log_unadj[[1]][1,1:4]
ec_h_dif_h1_unadj_end_M<-log_unadj[[2]][1:2,1:4]
ec_dw_dif_h1_unadj_end_M<-log_unadj[[3]][1:2,1:4]
fc_t_dif_h1_unadj_end_M<-log_unadj[[4]][1,1:4]
fc_h_dif_h1_unadj_end_M<-log_unadj[[5]][1:2,1:4]
fc_dw_dif_h1_unadj_end_M<-log_unadj[[6]][1:2,1:4]
fly_kit_dif_h1_unadj_end_M<-log_unadj[[7]][1,1:4]
fly_lat_dif_h1_unadj_end_M<-log_unadj[[8]][1,1:4]
ec_f_dif_h1_unadj_end_M<-log_unadj[[9]][1:3,1:4]

#(H2): combined WSH vs. sanitation.
ec_h_dif_h2_unadj_end_M<-log_unadj[[2]][3,1:4]
ec_dw_dif_h2_unadj_end_M<-log_unadj[[3]][3,1:4]
fc_h_dif_h2_unadj_end_M<-log_unadj[[5]][3,1:4]
fc_dw_dif_h2_unadj_end_M<-log_unadj[[6]][3,1:4]
ec_f_dif_h2_unadj_end_M<-log_unadj[[9]][4:5,1:4]





########################
#Save Objects for replication
########################

setwd("C:/Users/andre/Dropbox/WASHB EML/Results/Andrew")

#Save prevalences
save(ec_t_prev_end_M,
ec_h_prev_end_M,
ec_dw_prev_end_M,
fc_t_prev_end_M,
fc_h_prev_end_M,
fc_dw_prev_end_M,
fly_kit_prev_end_M,
fly_lat_prev_end_M,
mh_prev_end_M,
ch_prev_end_M,
ec_f_prev_end_M,
fc_f_prev_end_M,
file="Env_Prevalence_Andrew_end.Rdata")


#Save means
save(ec_t_mn_end_M,
ec_h_mn_end_M,
ec_dw_mn_end_M, 
fc_t_mn_end_M,
fc_h_mn_end_M, 
fc_dw_mn_end_M,
fly_kit_mn_end_M, 
fly_lat_mn_end_M, 
ec_f_mn_end_M,
fc_f_mn_end_M,
file="Env_Means_Andrew_end.Rdata")



#Save unadjusted RR
save(
ec_t_rr_h1_unadj_end_M,
ec_h_rr_h1_unadj_end_M,
ec_dw_rr_h1_unadj_end_M,
fc_t_rr_h1_unadj_end_M,
fc_h_rr_h1_unadj_end_M,
fc_dw_rr_h1_unadj_end_M,
fly_kit_rr_h1_unadj_end_M,
fly_lat_rr_h1_unadj_end_M,
mh_rr_h1_unadj_end_M,
ch_rr_h1_unadj_end_M,
ec_h_rr_h2_unadj_end_M,
ec_dw_rr_h2_unadj_end_M,
fc_h_rr_h2_unadj_end_M,
fc_dw_rr_h2_unadj_end_M,
mh_rr_h2_unadj_end_M,
ch_rr_h2_unadj_end_M,
ec_f_rr_h1_unadj_end_M,
fc_f_rr_h1_unadj_end_M,
ec_f_rr_h2_unadj_end_M,
fc_f_rr_h2_unadj_end_M,
file="Env_Risk_Ratio_unadj_Andrew_end.Rdata")

#Save unadjusted RD
save(
ec_t_rd_h1_unadj_end_M,
ec_h_rd_h1_unadj_end_M,
ec_dw_rd_h1_unadj_end_M,
fc_t_rd_h1_unadj_end_M,
fc_h_rd_h1_unadj_end_M,
fc_dw_rd_h1_unadj_end_M,
fly_kit_rd_h1_unadj_end_M,
fly_lat_rd_h1_unadj_end_M,
mh_rd_h1_unadj_end_M,
ch_rd_h1_unadj_end_M,
ec_h_rd_h2_unadj_end_M,
ec_dw_rd_h2_unadj_end_M,
fc_h_rd_h2_unadj_end_M,
fc_dw_rd_h2_unadj_end_M,
mh_rd_h2_unadj_end_M,
ch_rd_h2_unadj_end_M,
ec_f_rd_h1_unadj_end_M,
fc_f_rd_h1_unadj_end_M,
ec_f_rd_h2_unadj_end_M,
fc_f_rd_h2_unadj_end_M,
file="Env_Risk_diff_unadj_Andrew_end.Rdata")

#Save count difference
save(
ec_t_dif_h1_unadj_end_M,
ec_h_dif_h1_unadj_end_M,
ec_dw_dif_h1_unadj_end_M,
fc_t_dif_h1_unadj_end_M,
fc_h_dif_h1_unadj_end_M,
fc_dw_dif_h1_unadj_end_M,
fly_kit_dif_h1_unadj_end_M,
fly_lat_dif_h1_unadj_end_M,
ec_h_dif_h2_unadj_end_M,
ec_dw_dif_h2_unadj_end_M,
fc_h_dif_h2_unadj_end_M,
fc_dw_dif_h2_unadj_end_M,
ec_f_dif_h1_unadj_end_M,
#fc_f_dif_h1_unadj_end_M,
ec_f_dif_h2_unadj_end_M,
#fc_f_dif_h2_unadj_end_M,
file="Env_Count_Diff_unadj_Andrew_end.Rdata")






