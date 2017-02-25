
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
midline<-read.csv("Env_midline_clean.csv",stringsAsFactors = TRUE)
#midline<-read.dta("Env_midline_clean.dta")
midline$clusterid<-factor(midline$clusterid)


head(midline)
#midline<-subset(midline, select=-c(clusterid)) #drop so not duplicates after merge with treatment data
d<-midline

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
#dim(midline)
#d<-left_join(midline,treatment, by="clusterid")
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
contrasts=list(c("C+N pooled","Water"),c("C+N pooled","Handwashing"), c("C+N pooled","WSH+WSHN pooled"),
               c("Water","WSH+WSHN pooled"),c("Handwashing","WSH+WSHN pooled"))


  
  
#test that all rows are matched to enrollment data
table(is.na(d$svydate)) 

#NOTE: Looks like one unmatched dataid
#d[is.na(d$svydate),]


#Set block as factor
table(is.na(d$block)) 

d %>% group_by(tr) %>% summarize(n=n(), mean=mean(block, na.rm=T), sd=sd(block, na.rm=T) )

temp<-d %>% subset(tr=="Control") 
table(temp$block)

d$block<-factor(d$block)


#Generate flycaught
#d$flycaught<-ifelse(d$numfly>0,1,0)

#Set treatment levels
d$tr=factor(d$tr,levels=c("Control","Nutrition","Water","Handwashing","WSH","Nutrition + WSH"))


########################
#Prevalences and 95% CI by arm
########################
colnames(d)
ec_t_prev_mid_M<-d %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposT, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_h_prev_mid_M<-d %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_dw_prev_mid_M<-d %>% subset(tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_t_prev_mid_M<-d %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposT, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_h_prev_mid_M<-d %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_dw_prev_mid_M<-d %>% subset(tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_kit_prev_mid_M<-d  %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$flycaught_kit, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_lat_prev_mid_M<-d  %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$flycaught_lat, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mh_prev_mid_M<-d  %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$mhdirt, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ch_prev_mid_M<-d  %>% subset(tr!="Water")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$chdirt, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 



########################
#Means and 95% CI by arm
########################

ec_t_mn_mid_M<-d  %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecT, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_h_mn_mid_M<-d  %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_dw_mn_mid_M<-d  %>% subset(tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_t_mn_mid_M<-d  %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcT, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_h_mn_mid_M<-d  %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_dw_mn_mid_M<-d  %>% subset(tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_kit_mn_mid_M<-d  %>% subset(tr!="Water" & tr!="Handwashing")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$numfly_kit, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_lat_mn_mid_M<-d  %>% subset(tr!="Water" & tr!="Handwashing")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$numfly_lat, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 







########################
#Unadjusted GLMs
########################

#Create TMLE wrapper function
#Create GLM wrapper function
apply_glm<-function(Ys,tr=d$tr,id=d$block, contrasts.list, measure="RR"){
  #contrasts<-list(c("Control","Sanitation"), c("Control","WSH"),c("Sanitation","WSH"))

  varlist<-colnames(Ys)
  res_list<-NULL
  for(i in 1:ncol(Ys)){
    contrasts<-contrasts.list[[i]]
    cat("#",i,": ",varlist[i],"\n")
      if(measure=="RD" | measure=="neg.binom"){
        temp<-matrix(NA, length(contrasts), 7)
      }else{
        temp<-matrix(NA, length(contrasts), 13)
      }
      rownames(temp)<-rep("a",length(contrasts))
      for(k in 1:length(contrasts)){
          rownames(temp)[k]<-paste0(contrasts[[k]][1], " v ",contrasts[[k]][2])
      }    
    for(j in 1:length(contrasts)){
      if(measure=="RD"){
           family="gaussian"
           k=6
          if(length(grep("numfly",varlist[i],ignore.case=TRUE))>0){
                      family="neg.binom"
                      k=7
                      }
      temp[j,1:k]<-as.numeric(washb_glm(Y=Ys[,i], tr=tr, W=NULL, id=id, pair=NULL, family=family, contrast= contrasts[[j]], print=F)$TR)
               if(family=="gaussian"){colnames(temp)<-c("ATE","ci.lb","ci.lb","SE","Zval","P-val","na")} 
               if(family=="neg.binom"){colnames(temp)<-c("ATE","ci.lb","ci.lb","Estimate","SE","Zval","P-val")} 
            }
      if(measure=="RR"){
        temp[j,1:7]<-as.numeric(washb_glm(Y=Ys[,i], tr=tr, W=NULL, id=id, pair=NULL, family=poisson(link=`log`), contrast= contrasts[[j]], print=F)$TR)
        temp[j,8:13]<-as.numeric(washb_glm(Y=Ys[,i], tr=tr, W=NULL, id=id, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)

        colnames(temp)<-c("RR","ci.lb","ci.lb","estimate","SE","Zval","P-val","ATE","ci.lb","ci.lb","SE","Zval","P-val")
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
         flycaught_kit,flycaught_lat,mhdirt,chdirt))
Y.log<-subset(d, select=c(logecT, logecH, logecW, logfcT, logfcH, logfcW, numfly_kit,numfly_lat))


contrast1=contrasts
contrast2=list(c("C+N pooled","WSH+WSHN pooled"))
contrast3=list(c("C+N pooled","Handwashing"),c("C+N pooled","WSH+WSHN pooled"),c("Handwashing","WSH+WSHN pooled"))
contrast4=list(c("C+N pooled","Water"),c("C+N pooled","WSH+WSHN pooled"),c("Water","WSH+WSHN pooled"))
Y.pos.contrasts<-list(contrast2,contrast3,contrast4,contrast2,contrast3,contrast4,contrast2,contrast2,contrast3,contrast3)
Y.log.contrasts<-list(contrast2,contrast3,contrast4,contrast2,contrast3,contrast4,contrast2,contrast2)




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
pos_unadj<-apply_glm(Ys=Y.pos,tr=d$tr.pool,id=d$block, contrasts.list=Y.pos.contrasts,  measure="RR")




#Prevalence Ratio-unadjusted
#(h1): sanitation vs. control, combined WSH vs. control  
ec_t_rr_h1_unadj_mid_M<-pos_unadj[[1]][1,1:7]
ec_h_rr_h1_unadj_mid_M<-pos_unadj[[2]][1:2,1:7]
ec_dw_rr_h1_unadj_mid_M<-pos_unadj[[3]][1:2,1:7]
fc_t_rr_h1_unadj_mid_M<-pos_unadj[[4]][1,1:7]
fc_h_rr_h1_unadj_mid_M<-pos_unadj[[5]][1:2,1:7]
fc_dw_rr_h1_unadj_mid_M<-pos_unadj[[6]][1:2,1:7]
fly_kit_rr_h1_unadj_mid_M<-pos_unadj[[7]][1,1:7]
fly_lat_rr_h1_unadj_mid_M<-pos_unadj[[8]][1,1:7]
mh_rr_h1_unadj_mid_M<-pos_unadj[[9]][1:2,1:7]
ch_rr_h1_unadj_mid_M<-pos_unadj[[10]][1:2,1:7]

#(H2): combined WSH vs. sanitation.
#ec_t_rr_h2_unadj_mid_M<-pos_unadj[[1]][4:5,1:7]
ec_h_rr_h2_unadj_mid_M<-pos_unadj[[2]][3,1:7]
ec_dw_rr_h2_unadj_mid_M<-pos_unadj[[3]][3,1:7]
#fc_t_rr_h2_unadj_mid_M<-pos_unadj[[4]][4:5,1:7]
fc_h_rr_h2_unadj_mid_M<-pos_unadj[[5]][3,1:7]
fc_dw_rr_h2_unadj_mid_M<-pos_unadj[[6]][3,1:7]
#fly_rr_h2_unadj_mid_M<-pos_unadj[[7]][3,1:7]
mh_rr_h2_unadj_mid_M<-pos_unadj[[9]][3,1:7]
ch_rr_h2_unadj_mid_M<-pos_unadj[[10]][3,1:7]


#Prevalence Difference-unadjusted
#(h1): sanitation vs. control, combined WSH vs. control  
ec_t_rd_h1_unadj_mid_M<-pos_unadj[[1]][1,7:13]
ec_h_rd_h1_unadj_mid_M<-pos_unadj[[2]][1:2,7:13]
ec_dw_rd_h1_unadj_mid_M<-pos_unadj[[3]][1:2,7:13]
fc_t_rd_h1_unadj_mid_M<-pos_unadj[[4]][1,7:13]
fc_h_rd_h1_unadj_mid_M<-pos_unadj[[5]][1:2,7:13]
fc_dw_rd_h1_unadj_mid_M<-pos_unadj[[6]][1:2,7:13]
fly_kit_rd_h1_unadj_mid_M<-pos_unadj[[7]][1,7:13]
fly_lat_rd_h1_unadj_mid_M<-pos_unadj[[8]][1,7:13]
mh_rd_h1_unadj_mid_M<-pos_unadj[[9]][1:2,7:13]
ch_rd_h1_unadj_mid_M<-pos_unadj[[10]][1:2,7:13]

#(H2): combined WSH vs. sanitation.
#ec_t_rd_h2_unadj_mid_M<-pos_unadj[[1]][4:5,7:13]
ec_h_rd_h2_unadj_mid_M<-pos_unadj[[2]][3,7:13]
ec_dw_rd_h2_unadj_mid_M<-pos_unadj[[3]][3,7:13]
#fc_t_rd_h2_unadj_mid_M<-pos_unadj[[4]][3,7:13]
fc_h_rd_h2_unadj_mid_M<-pos_unadj[[5]][3,7:13]
fc_dw_rd_h2_unadj_mid_M<-pos_unadj[[6]][3,7:13]
#fly_rd_h2_unadj_mid_M<-pos_unadj[[7]][3,7:13]
mh_rd_h2_unadj_mid_M<-pos_unadj[[9]][3,7:13]
ch_rd_h2_unadj_mid_M<-pos_unadj[[10]][3,7:13]



log_unadj<-apply_glm(Ys=Y.log,tr=d$tr.pool,id=d$block, contrasts.list=Y.log.contrasts, measure="RD")


#(h1): sanitation vs. control, combined WSH vs. control  
ec_t_dif_h1_unadj_mid_M<-log_unadj[[1]][1,1:6]
ec_h_dif_h1_unadj_mid_M<-log_unadj[[2]][1:2,1:6]
ec_dw_dif_h1_unadj_mid_M<-log_unadj[[3]][1:2,1:6]
fc_t_dif_h1_unadj_mid_M<-log_unadj[[4]][1,1:6]
fc_h_dif_h1_unadj_mid_M<-log_unadj[[5]][1:2,1:6]
fc_dw_dif_h1_unadj_mid_M<-log_unadj[[6]][1:2,1:6]
fly_kit_dif_h1_unadj_mid_M<-log_unadj[[7]][1,1:7]
fly_lat_dif_h1_unadj_mid_M<-log_unadj[[8]][1,1:7]

#(H2): combined WSH vs. sanitation.
ec_h_dif_h2_unadj_mid_M<-log_unadj[[2]][3,1:6]
ec_dw_dif_h2_unadj_mid_M<-log_unadj[[3]][3,1:6]
fc_h_dif_h2_unadj_mid_M<-log_unadj[[5]][3,1:6]
fc_dw_dif_h2_unadj_mid_M<-log_unadj[[6]][3,1:6]


#Generate subgroups
#d$date
d$rain<-ifelse()


########################
#Save Objects for replication
########################

setwd("C:/Users/andre/Dropbox/WASHB EML/Results/Andrew")

#Save prevalences
save(ec_t_prev_mid_M,
ec_h_prev_mid_M,
ec_dw_prev_mid_M,
fc_t_prev_mid_M,
fc_h_prev_mid_M,
fc_dw_prev_mid_M,
fly_kit_prev_mid_M,
fly_lat_prev_mid_M,
mh_prev_mid_M,
ch_prev_mid_M,
file="Env_Prevalence_Andrew_mid.Rdata")


#Save means
save(ec_t_mn_mid_M,
ec_h_mn_mid_M,
ec_dw_mn_mid_M, 
fc_t_mn_mid_M,
fc_h_mn_mid_M, 
fc_dw_mn_mid_M,
fly_kit_mn_mid_M, 
fly_lat_mn_mid_M, 
file="Env_Means_Andrew_mid.Rdata")



#Save unadjusted RR
save(
ec_t_rr_h1_unadj_mid_M,
ec_h_rr_h1_unadj_mid_M,
ec_dw_rr_h1_unadj_mid_M,
fc_t_rr_h1_unadj_mid_M,
fc_h_rr_h1_unadj_mid_M,
fc_dw_rr_h1_unadj_mid_M,
fly_kit_rr_h1_unadj_mid_M,
fly_lat_rr_h1_unadj_mid_M,
mh_rr_h1_unadj_mid_M,
ch_rr_h1_unadj_mid_M,
ec_h_rr_h2_unadj_mid_M,
ec_dw_rr_h2_unadj_mid_M,
fc_h_rr_h2_unadj_mid_M,
fc_dw_rr_h2_unadj_mid_M,
mh_rr_h2_unadj_mid_M,
ch_rr_h2_unadj_mid_M,
file="Env_Risk_Ratio_unadj_Andrew_mid.Rdata")

#Save unadjusted RD
save(
ec_t_rd_h1_unadj_mid_M,
ec_h_rd_h1_unadj_mid_M,
ec_dw_rd_h1_unadj_mid_M,
fc_t_rd_h1_unadj_mid_M,
fc_h_rd_h1_unadj_mid_M,
fc_dw_rd_h1_unadj_mid_M,
fly_kit_rd_h1_unadj_mid_M,
fly_lat_rd_h1_unadj_mid_M,
mh_rd_h1_unadj_mid_M,
ch_rd_h1_unadj_mid_M,
ec_h_rd_h2_unadj_mid_M,
ec_dw_rd_h2_unadj_mid_M,
fc_h_rd_h2_unadj_mid_M,
fc_dw_rd_h2_unadj_mid_M,
mh_rd_h2_unadj_mid_M,
ch_rd_h2_unadj_mid_M,
file="Env_Risk_diff_unadj_Andrew_mid.Rdata")

#Save count difference
save(
ec_t_dif_h1_unadj_mid_M,
ec_h_dif_h1_unadj_mid_M,
ec_dw_dif_h1_unadj_mid_M,
fc_t_dif_h1_unadj_mid_M,
fc_h_dif_h1_unadj_mid_M,
fc_dw_dif_h1_unadj_mid_M,
fly_kit_dif_h1_unadj_mid_M,
fly_lat_dif_h1_unadj_mid_M,
ec_h_dif_h2_unadj_mid_M,
ec_dw_dif_h2_unadj_mid_M,
fc_h_dif_h2_unadj_mid_M,
fc_dw_dif_h2_unadj_mid_M,
file="Env_Count_Diff_unadj_Andrew_mid.Rdata")





########################
#Effect modification by rainfall
########################


########################
#Effect modification by rainfall
########################

wet<-subset(d, wet==1)
dry<-subset(d, wet==0)


ec_tw_prev_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposTW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1]   #tubewell
ec_sw_prev_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposSW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #stored water
ec_p_prev_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposP, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #ponds
ec_h_prev_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #child hands
ec_f_prev_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #food
ec_s_prev_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposS, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #soil
ec_y_prev_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposY, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #flies
fc_tw_prev_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposTW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_sw_prev_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposSW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_p_prev_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposP, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_h_prev_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_f_prev_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_s_prev_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposS, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_y_prev_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposY, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_prev_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$flycaught, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mh_prev_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$mhdirt, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ch_prev_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$chdirt, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 

ec_tw_mn_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecTW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #tubewell
ec_sw_mn_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecSW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #stored water
ec_p_mn_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecP, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #ponds
ec_h_mn_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #child hands
ec_f_mn_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #food
ec_s_mn_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecS, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #soil
ec_y_mn_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecY, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #flies
fc_tw_mn_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcTW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_sw_mn_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcSW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_p_mn_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcP, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_h_mn_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_f_mn_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_s_mn_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcS, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_y_mn_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcY, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_mn_wet_mid_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$numfly, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 


ec_tw_prev_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposTW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1]   #tubewell
ec_sw_prev_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposSW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #stored water
ec_p_prev_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposP, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #ponds
ec_h_prev_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #child hands
ec_f_prev_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #food
ec_s_prev_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposS, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #soil
ec_y_prev_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposY, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #flies
fc_tw_prev_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposTW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_sw_prev_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposSW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_p_prev_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposP, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_h_prev_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_f_prev_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_s_prev_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposS, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_y_prev_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposY, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_prev_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$flycaught, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mh_prev_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$mhdirt, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ch_prev_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$chdirt, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 

ec_tw_mn_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecTW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #tubewell
ec_sw_mn_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecSW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #stored water
ec_p_mn_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecP, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #ponds
ec_h_mn_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #child hands
ec_f_mn_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #food
ec_s_mn_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecS, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #soil
ec_y_mn_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecY, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] #flies
fc_tw_mn_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcTW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_sw_mn_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcSW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_p_mn_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcP, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_h_mn_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_f_mn_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_s_mn_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcS, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_y_mn_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcY, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_mn_dry_mid_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$numfly, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 






#Create GLM wrapper function
apply_glm_EM<-function(Ys,tr=d$tr,id=d$block, measure="RR", W=NULL, V=NULL){

  W[,1]<-factor(W[,1])
  contrasts<-list(c("Control","Sanitation"), c("Control","WSH"),c("Sanitation","WSH"))
  varlist<-colnames(Ys)
  res_list<-NULL
  
  for(i in 1:ncol(Ys)){
    cat("#",i,": ",varlist[i],"\n")
      if(measure=="RD" | measure=="neg.binom"){
        temp<-matrix(NA, length(contrasts)*2, 6)
      }else{
        temp<-matrix(NA, length(contrasts)*2, 12)
      }
    rownames(temp)<-c("Control v Sanitation dry","Control v Sanitation wet",
                      "Control v WSH dry","Control v WSH wet",
                      "Sanitation v WSH dry","Sanitation v WSH wet")
    for(j in 1:length(contrasts)){
      if(measure=="RD"){
           family="gaussian"
          if(length(grep("numfly",varlist[i],ignore.case=TRUE))>0){
                      family="neg.binom"
                      }
           temp2<-(washb_glm(Y=Ys[,i], tr=tr, W=W, V=V, id=id, pair=NULL, family=family, contrast= contrasts[[j]], print=F)$lincom)
           temp[2*j-1,]<-as.numeric(temp2[1,2:7])
           temp[2*j,]<-as.numeric(temp2[2,2:7])
               colnames(temp)<-c("ATE","ci.lb","ci.lb","SE","Zval","P-val")
            }
      if(measure=="RR"){
        temp2<-(washb_glm(Y=Ys[,i], tr=tr, W=W, V=V, id=id, pair=NULL, family=poisson(link=`log`), contrast= contrasts[[j]], print=F)$lincom)
        temp[2*j-1,1:6]<-as.numeric(temp2[1,2:7])
        temp[2*j,1:6]<-as.numeric(temp2[2,2:7])

        temp2<-(washb_glm(Y=Ys[,i], tr=tr, W=W, V=V, id=id, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$lincom)
        temp[2*j-1,7:12]<-as.numeric(temp2[1,2:7])
        temp[2*j,7:12]<-as.numeric(temp2[2,2:7])
        
        colnames(temp)<-c("RR","ci.lb","ci.lb","SE","Zval","P-val","ATE","ci.lb","ci.lb","SE","Zval","P-val")
          }
        }
    res_list[[i]]<-temp
    names(res_list)[[i]]<-varlist[i]
  }
  return(res_list)
}




#Binary outcomes- subgroup analysis
pos_em<-apply_glm_EM(Ys=Y.pos,tr=d$tr,id=d$block, measure="RR",W=subset(d, select=wet),V="wet")



      
#Prevalence Ratio
#(H1): sanitation vs. control, combined WSH vs. control  
ec_tw_rr_h1_wet_mid_M<-pos_em[[1]][c(1,3),1:6]
ec_sw_rr_h1_wet_mid_M<-pos_em[[2]][c(1,3),1:6]
ec_p_rr_h1_wet_mid_M<-pos_em[[3]][c(1,3),1:6]
ec_h_rr_h1_wet_mid_M<-pos_em[[4]][c(1,3),1:6]
ec_f_rr_h1_wet_mid_M<-pos_em[[5]][c(1,3),1:6]
ec_s_rr_h1_wet_mid_M<-pos_em[[6]][c(1,3),1:6]
ec_y_rr_h1_wet_mid_M<-pos_em[[7]][c(1,3),1:6]
fc_tw_rr_h1_wet_mid_M<-pos_em[[8]][c(1,3),1:6]
fc_sw_rr_h1_wet_mid_M<-pos_em[[9]][c(1,3),1:6]
fc_p_rr_h1_wet_mid_M<-pos_em[[10]][c(1,3),1:6]
fc_h_rr_h1_wet_mid_M<-pos_em[[11]][c(1,3),1:6]
fc_f_rr_h1_wet_mid_M<-pos_em[[12]][c(1,3),1:6]
fc_s_rr_h1_wet_mid_M<-pos_em[[13]][c(1,3),1:6]
fc_y_rr_h1_wet_mid_M<-pos_em[[14]][c(1,3),1:6]
fly_rr_h1_wet_mid_M<-pos_em[[15]][c(1,3),1:6]
mh_rr_h1_wet_mid_M<-pos_em[[16]][c(1,3),1:6]
ch_rr_h1_wet_mid_M<-pos_em[[17]][c(1,3),1:6]

#(H2): combined WSH vs. sanitation.
ec_tw_rr_h2_wet_mid_M<-pos_em[[1]][5,1:6]
ec_sw_rr_h2_wet_mid_M<-pos_em[[2]][5,1:6]
ec_p_rr_h2_wet_mid_M<-pos_em[[3]][5,1:6]
ec_h_rr_h2_wet_mid_M<-pos_em[[4]][5,1:6]
ec_f_rr_h2_wet_mid_M<-pos_em[[5]][5,1:6]
ec_s_rr_h2_wet_mid_M<-pos_em[[6]][5,1:6]
ec_y_rr_h2_wet_mid_M<-pos_em[[7]][5,1:6]
fc_tw_rr_h2_wet_mid_M<-pos_em[[8]][5,1:6]
fc_sw_rr_h2_wet_mid_M<-pos_em[[9]][5,1:6]
fc_p_rr_h2_wet_mid_M<-pos_em[[10]][5,1:6]
fc_h_rr_h2_wet_mid_M<-pos_em[[11]][5,1:6]
fc_f_rr_h2_wet_mid_M<-pos_em[[12]][5,1:6]
fc_s_rr_h2_wet_mid_M<-pos_em[[13]][5,1:6]
fc_y_rr_h2_wet_mid_M<-pos_em[[14]][5,1:6]
fly_rr_h2_wet_mid_M<-pos_em[[15]][5,1:6]
mh_rr_h2_wet_mid_M<-pos_em[[16]][5,1:6]
ch_rr_h2_wet_mid_M<-pos_em[[17]][5,1:6]


#Prevalence Difference
#(h1): sanitation vs. control, combined WSH vs. control  
ec_tw_rd_h1_wet_mid_M<-pos_em[[1]][c(1,3),7:12]
ec_sw_rd_h1_wet_mid_M<-pos_em[[2]][c(1,3),7:12]
ec_p_rd_h1_wet_mid_M<-pos_em[[3]][c(1,3),7:12]
ec_h_rd_h1_wet_mid_M<-pos_em[[4]][c(1,3),7:12]
ec_f_rd_h1_wet_mid_M<-pos_em[[5]][c(1,3),7:12]
ec_s_rd_h1_wet_mid_M<-pos_em[[6]][c(1,3),7:12]
ec_y_rd_h1_wet_mid_M<-pos_em[[7]][c(1,3),7:12]
fc_tw_rd_h1_wet_mid_M<-pos_em[[8]][c(1,3),7:12]
fc_sw_rd_h1_wet_mid_M<-pos_em[[9]][c(1,3),7:12]
fc_p_rd_h1_wet_mid_M<-pos_em[[10]][c(1,3),7:12]
fc_h_rd_h1_wet_mid_M<-pos_em[[11]][c(1,3),7:12]
fc_f_rd_h1_wet_mid_M<-pos_em[[12]][c(1,3),7:12]
fc_s_rd_h1_wet_mid_M<-pos_em[[13]][c(1,3),7:12]
fc_y_rd_h1_wet_mid_M<-pos_em[[14]][c(1,3),7:12]
fly_rd_h1_wet_mid_M<-pos_em[[15]][c(1,3),7:12]
mh_rd_h1_wet_mid_M<-pos_em[[16]][c(1,3),7:12]
ch_rd_h1_wet_mid_M<-pos_em[[17]][c(1,3),7:12]

#(H2): combined WSH vs. sanitation.
ec_tw_rd_h2_wet_mid_M<-pos_em[[1]][5,7:12]
ec_sw_rd_h2_wet_mid_M<-pos_em[[2]][5,7:12]
ec_p_rd_h2_wet_mid_M<-pos_em[[3]][5,7:12]
ec_h_rd_h2_wet_mid_M<-pos_em[[4]][5,7:12]
ec_f_rd_h2_wet_mid_M<-pos_em[[5]][5,7:12]
ec_s_rd_h2_wet_mid_M<-pos_em[[6]][5,7:12]
ec_y_rd_h2_wet_mid_M<-pos_em[[7]][5,7:12]
fc_tw_rd_h2_wet_mid_M<-pos_em[[8]][5,7:12]
fc_sw_rd_h2_wet_mid_M<-pos_em[[9]][5,7:12]
fc_p_rd_h2_wet_mid_M<-pos_em[[10]][5,7:12]
fc_h_rd_h2_wet_mid_M<-pos_em[[11]][5,7:12]
fc_f_rd_h2_wet_mid_M<-pos_em[[12]][5,7:12]
fc_s_rd_h2_wet_mid_M<-pos_em[[13]][5,7:12]
fc_y_rd_h2_wet_mid_M<-pos_em[[14]][5,7:12]
fly_rd_h2_wet_mid_M<-pos_em[[15]][5,7:12]
mh_rd_h2_wet_mid_M<-pos_em[[16]][5,7:12]
ch_rd_h2_wet_mid_M<-pos_em[[17]][5,7:12]


    
      
#Prevalence Ratio
#(H1): sanitation vs. control, combined WSH vs. control  
ec_tw_rr_h1_dry_mid_M<-pos_em[[1]][c(2,4),1:6]
ec_sw_rr_h1_dry_mid_M<-pos_em[[2]][c(2,4),1:6]
ec_p_rr_h1_dry_mid_M<-pos_em[[3]][c(2,4),1:6]
ec_h_rr_h1_dry_mid_M<-pos_em[[4]][c(2,4),1:6]
ec_f_rr_h1_dry_mid_M<-pos_em[[5]][c(2,4),1:6]
ec_s_rr_h1_dry_mid_M<-pos_em[[6]][c(2,4),1:6]
ec_y_rr_h1_dry_mid_M<-pos_em[[7]][c(2,4),1:6]
fc_tw_rr_h1_dry_mid_M<-pos_em[[8]][c(2,4),1:6]
fc_sw_rr_h1_dry_mid_M<-pos_em[[9]][c(2,4),1:6]
fc_p_rr_h1_dry_mid_M<-pos_em[[10]][c(2,4),1:6]
fc_h_rr_h1_dry_mid_M<-pos_em[[11]][c(2,4),1:6]
fc_f_rr_h1_dry_mid_M<-pos_em[[12]][c(2,4),1:6]
fc_s_rr_h1_dry_mid_M<-pos_em[[13]][c(2,4),1:6]
fc_y_rr_h1_dry_mid_M<-pos_em[[14]][c(2,4),1:6]
fly_rr_h1_dry_mid_M<-pos_em[[15]][c(2,4),1:6]
mh_rr_h1_dry_mid_M<-pos_em[[16]][c(2,4),1:6]
ch_rr_h1_dry_mid_M<-pos_em[[17]][c(2,4),1:6]

#(H2): combined WSH vs. sanitation.
ec_tw_rr_h2_dry_mid_M<-pos_em[[1]][6,1:6]
ec_sw_rr_h2_dry_mid_M<-pos_em[[2]][6,1:6]
ec_p_rr_h2_dry_mid_M<-pos_em[[3]][6,1:6]
ec_h_rr_h2_dry_mid_M<-pos_em[[4]][6,1:6]
ec_f_rr_h2_dry_mid_M<-pos_em[[5]][6,1:6]
ec_s_rr_h2_dry_mid_M<-pos_em[[6]][6,1:6]
ec_y_rr_h2_dry_mid_M<-pos_em[[7]][6,1:6]
fc_tw_rr_h2_dry_mid_M<-pos_em[[8]][6,1:6]
fc_sw_rr_h2_dry_mid_M<-pos_em[[9]][6,1:6]
fc_p_rr_h2_dry_mid_M<-pos_em[[10]][6,1:6]
fc_h_rr_h2_dry_mid_M<-pos_em[[11]][6,1:6]
fc_f_rr_h2_dry_mid_M<-pos_em[[12]][6,1:6]
fc_s_rr_h2_dry_mid_M<-pos_em[[13]][6,1:6]
fc_y_rr_h2_dry_mid_M<-pos_em[[14]][6,1:6]
fly_rr_h2_dry_mid_M<-pos_em[[15]][6,1:6]
mh_rr_h2_dry_mid_M<-pos_em[[16]][6,1:6]
ch_rr_h2_dry_mid_M<-pos_em[[17]][6,1:6]


#Prevalence Difference
#(h1): sanitation vs. control, combined WSH vs. control  
ec_tw_rd_h1_dry_mid_M<-pos_em[[1]][c(2,4),7:12]
ec_sw_rd_h1_dry_mid_M<-pos_em[[2]][c(2,4),7:12]
ec_p_rd_h1_dry_mid_M<-pos_em[[3]][c(2,4),7:12]
ec_h_rd_h1_dry_mid_M<-pos_em[[4]][c(2,4),7:12]
ec_f_rd_h1_dry_mid_M<-pos_em[[5]][c(2,4),7:12]
ec_s_rd_h1_dry_mid_M<-pos_em[[6]][c(2,4),7:12]
ec_y_rd_h1_dry_mid_M<-pos_em[[7]][c(2,4),7:12]
fc_tw_rd_h1_dry_mid_M<-pos_em[[8]][c(2,4),7:12]
fc_sw_rd_h1_dry_mid_M<-pos_em[[9]][c(2,4),7:12]
fc_p_rd_h1_dry_mid_M<-pos_em[[10]][c(2,4),7:12]
fc_h_rd_h1_dry_mid_M<-pos_em[[11]][c(2,4),7:12]
fc_f_rd_h1_dry_mid_M<-pos_em[[12]][c(2,4),7:12]
fc_s_rd_h1_dry_mid_M<-pos_em[[13]][c(2,4),7:12]
fc_y_rd_h1_dry_mid_M<-pos_em[[14]][c(2,4),7:12]
fly_rd_h1_dry_mid_M<-pos_em[[15]][c(2,4),7:12]
mh_rd_h1_dry_mid_M<-pos_em[[16]][c(2,4),7:12]
ch_rd_h1_dry_mid_M<-pos_em[[17]][c(2,4),7:12]

#(H2): combined WSH vs. sanitation.
ec_tw_rd_h2_dry_mid_M<-pos_em[[1]][6,7:12]
ec_sw_rd_h2_dry_mid_M<-pos_em[[2]][6,7:12]
ec_p_rd_h2_dry_mid_M<-pos_em[[3]][6,7:12]
ec_h_rd_h2_dry_mid_M<-pos_em[[4]][6,7:12]
ec_f_rd_h2_dry_mid_M<-pos_em[[5]][6,7:12]
ec_s_rd_h2_dry_mid_M<-pos_em[[6]][6,7:12]
ec_y_rd_h2_dry_mid_M<-pos_em[[7]][6,7:12]
fc_tw_rd_h2_dry_mid_M<-pos_em[[8]][6,7:12]
fc_sw_rd_h2_dry_mid_M<-pos_em[[9]][6,7:12]
fc_p_rd_h2_dry_mid_M<-pos_em[[10]][6,7:12]
fc_h_rd_h2_dry_mid_M<-pos_em[[11]][6,7:12]
fc_f_rd_h2_dry_mid_M<-pos_em[[12]][6,7:12]
fc_s_rd_h2_dry_mid_M<-pos_em[[13]][6,7:12]
fc_y_rd_h2_dry_mid_M<-pos_em[[14]][6,7:12]
fly_rd_h2_dry_mid_M<-pos_em[[15]][6,7:12]
mh_rd_h2_dry_mid_M<-pos_em[[16]][6,7:12]
ch_rd_h2_dry_mid_M<-pos_em[[17]][6,7:12]


#Binary outcomes- subgroup analysis
log_em<-apply_glm_EM(Ys=Y.log,tr=d$tr,id=d$block, measure="RD",W=subset(d, select=wet),V="wet")




#(h1): sanitation vs. control, combined WSH vs. control  
ec_tw_dif_h1_wet_mid_M<-log_em[[1]][c(1,3),1:6]
ec_sw_dif_h1_wet_mid_M<-log_em[[2]][c(1,3),1:6]
ec_p_dif_h1_wet_mid_M<-log_em[[3]][c(1,3),1:6]
ec_h_dif_h1_wet_mid_M<-log_em[[4]][c(1,3),1:6]
ec_f_dif_h1_wet_mid_M<-log_em[[5]][c(1,3),1:6]
ec_s_dif_h1_wet_mid_M<-log_em[[6]][c(1,3),1:6]
ec_y_dif_h1_wet_mid_M<-log_em[[7]][c(1,3),1:6]
fc_tw_dif_h1_wet_mid_M<-log_em[[8]][c(1,3),1:6]
fc_sw_dif_h1_wet_mid_M<-log_em[[9]][c(1,3),1:6]
fc_p_dif_h1_wet_mid_M<-log_em[[10]][c(1,3),1:6]
fc_h_dif_h1_wet_mid_M<-log_em[[11]][c(1,3),1:6]
fc_f_dif_h1_wet_mid_M<-log_em[[12]][c(1,3),1:6]
fc_s_dif_h1_wet_mid_M<-log_em[[13]][c(1,3),1:6]
fc_y_dif_h1_wet_mid_M<-log_em[[14]][c(1,3),1:6]
fly_dif_h1_wet_mid_M<-log_em[[15]][c(1,3),1:6]

#(H2): combined WSH vs. sanitation.
ec_tw_dif_h2_wet_mid_M<-log_em[[1]][5,1:6]
ec_sw_dif_h2_wet_mid_M<-log_em[[2]][5,1:6]
ec_p_dif_h2_wet_mid_M<-log_em[[3]][5,1:6]
ec_h_dif_h2_wet_mid_M<-log_em[[4]][5,1:6]
ec_f_dif_h2_wet_mid_M<-log_em[[5]][5,1:6]
ec_s_dif_h2_wet_mid_M<-log_em[[6]][5,1:6]
ec_y_dif_h2_wet_mid_M<-log_em[[7]][5,1:6]
fc_tw_dif_h2_wet_mid_M<-log_em[[8]][5,1:6]
fc_sw_dif_h2_wet_mid_M<-log_em[[9]][5,1:6]
fc_p_dif_h2_wet_mid_M<-log_em[[10]][5,1:6]
fc_h_dif_h2_wet_mid_M<-log_em[[11]][5,1:6]
fc_f_dif_h2_wet_mid_M<-log_em[[12]][5,1:6]
fc_s_dif_h2_wet_mid_M<-log_em[[13]][5,1:6]
fc_y_dif_h2_wet_mid_M<-log_em[[14]][5,1:6]
fly_dif_h2_wet_mid_M<-log_em[[15]][5,1:6]





#(h1): sanitation vs. control, combined WSH vs. control  
ec_tw_dif_h1_dry_mid_M<-log_em[[1]][c(2,4),1:6]
ec_sw_dif_h1_dry_mid_M<-log_em[[2]][c(2,4),1:6]
ec_p_dif_h1_dry_mid_M<-log_em[[3]][c(2,4),1:6]
ec_h_dif_h1_dry_mid_M<-log_em[[4]][c(2,4),1:6]
ec_f_dif_h1_dry_mid_M<-log_em[[5]][c(2,4),1:6]
ec_s_dif_h1_dry_mid_M<-log_em[[6]][c(2,4),1:6]
ec_y_dif_h1_dry_mid_M<-log_em[[7]][c(2,4),1:6]
fc_tw_dif_h1_dry_mid_M<-log_em[[8]][c(2,4),1:6]
fc_sw_dif_h1_dry_mid_M<-log_em[[9]][c(2,4),1:6]
fc_p_dif_h1_dry_mid_M<-log_em[[10]][c(2,4),1:6]
fc_h_dif_h1_dry_mid_M<-log_em[[11]][c(2,4),1:6]
fc_f_dif_h1_dry_mid_M<-log_em[[12]][c(2,4),1:6]
fc_s_dif_h1_dry_mid_M<-log_em[[13]][c(2,4),1:6]
fc_y_dif_h1_dry_mid_M<-log_em[[14]][c(2,4),1:6]
fly_dif_h1_dry_mid_M<-log_em[[15]][c(2,4),1:6]

#(H2): combined WSH vs. sanitation.
ec_tw_dif_h2_dry_mid_M<-log_em[[1]][6,1:6]
ec_sw_dif_h2_dry_mid_M<-log_em[[2]][6,1:6]
ec_p_dif_h2_dry_mid_M<-log_em[[3]][6,1:6]
ec_h_dif_h2_dry_mid_M<-log_em[[4]][6,1:6]
ec_f_dif_h2_dry_mid_M<-log_em[[5]][6,1:6]
ec_s_dif_h2_dry_mid_M<-log_em[[6]][6,1:6]
ec_y_dif_h2_dry_mid_M<-log_em[[7]][6,1:6]
fc_tw_dif_h2_dry_mid_M<-log_em[[8]][6,1:6]
fc_sw_dif_h2_dry_mid_M<-log_em[[9]][6,1:6]
fc_p_dif_h2_dry_mid_M<-log_em[[10]][6,1:6]
fc_h_dif_h2_dry_mid_M<-log_em[[11]][6,1:6]
fc_f_dif_h2_dry_mid_M<-log_em[[12]][6,1:6]
fc_s_dif_h2_dry_mid_M<-log_em[[13]][6,1:6]
fc_y_dif_h2_dry_mid_M<-log_em[[14]][6,1:6]
fly_dif_h2_dry_mid_M<-log_em[[15]][6,1:6]








#Save prevalences -wet
save(ec_tw_prev_wet_mid_M,
ec_sw_prev_wet_mid_M,
ec_p_prev_wet_mid_M,
ec_h_prev_wet_mid_M,
ec_f_prev_wet_mid_M,
ec_s_prev_wet_mid_M,
ec_y_prev_wet_mid_M,
fc_tw_prev_wet_mid_M,
fc_sw_prev_wet_mid_M,
fc_p_prev_wet_mid_M,
fc_h_prev_wet_mid_M,
fc_f_prev_wet_mid_M,
fc_s_prev_wet_mid_M,
fc_y_prev_wet_mid_M,
fly_prev_wet_mid_M,
mh_prev_wet_mid_M,
ch_prev_wet_mid_M,
file="Env_Prevalence_Andrew_mid_wet.Rdata")


#Save means -wet
save(ec_tw_mn_wet_mid_M,
ec_sw_mn_wet_mid_M,
ec_p_mn_wet_mid_M,
ec_h_mn_wet_mid_M,
ec_f_mn_wet_mid_M,
ec_s_mn_wet_mid_M,
ec_y_mn_wet_mid_M,
fc_tw_mn_wet_mid_M,
fc_sw_mn_wet_mid_M,
fc_p_mn_wet_mid_M,
fc_h_mn_wet_mid_M,
fc_f_mn_wet_mid_M,
fc_s_mn_wet_mid_M,
fc_y_mn_wet_mid_M,
fly_mn_wet_mid_M,
file="Env_wet_mid_Means_Andrew_mid_wet.Rdata")

#Save prevalences -dry
save(ec_tw_prev_dry_mid_M,
ec_sw_prev_dry_mid_M,
ec_p_prev_dry_mid_M,
ec_h_prev_dry_mid_M,
ec_f_prev_dry_mid_M,
ec_s_prev_dry_mid_M,
ec_y_prev_dry_mid_M,
fc_tw_prev_dry_mid_M,
fc_sw_prev_dry_mid_M,
fc_p_prev_dry_mid_M,
fc_h_prev_dry_mid_M,
fc_f_prev_dry_mid_M,
fc_s_prev_dry_mid_M,
fc_y_prev_dry_mid_M,
fly_prev_dry_mid_M,
mh_prev_dry_mid_M,
ch_prev_dry_mid_M,
file="Env_Prevalence_Andrew_mid_dry.Rdata")


#Save means -dry
save(ec_tw_mn_dry_mid_M,
ec_sw_mn_dry_mid_M,
ec_p_mn_dry_mid_M,
ec_h_mn_dry_mid_M,
ec_f_mn_dry_mid_M,
ec_s_mn_dry_mid_M,
ec_y_mn_dry_mid_M,
fc_tw_mn_dry_mid_M,
fc_sw_mn_dry_mid_M,
fc_p_mn_dry_mid_M,
fc_h_mn_dry_mid_M,
fc_f_mn_dry_mid_M,
fc_s_mn_dry_mid_M,
fc_y_mn_dry_mid_M,
fly_mn_dry_mid_M,
file="Env_dry_mid_Means_Andrew_mid_dry.Rdata")





#Save wet RD
save(
ec_tw_rr_h1_wet_mid_M,
ec_sw_rr_h1_wet_mid_M,
ec_p_rr_h1_wet_mid_M,
ec_h_rr_h1_wet_mid_M,
ec_f_rr_h1_wet_mid_M,
ec_s_rr_h1_wet_mid_M,
ec_y_rr_h1_wet_mid_M,
fc_tw_rr_h1_wet_mid_M,
fc_sw_rr_h1_wet_mid_M,
fc_p_rr_h1_wet_mid_M,
fc_h_rr_h1_wet_mid_M,
fc_f_rr_h1_wet_mid_M,
fc_s_rr_h1_wet_mid_M,
fc_y_rr_h1_wet_mid_M,
fly_rr_h1_wet_mid_M,
mh_rr_h1_wet_mid_M,
ch_rr_h1_wet_mid_M,
ec_tw_rr_h2_wet_mid_M,
ec_sw_rr_h2_wet_mid_M,
ec_p_rr_h2_wet_mid_M,
ec_h_rr_h2_wet_mid_M,
ec_f_rr_h2_wet_mid_M,
ec_s_rr_h2_wet_mid_M,
ec_y_rr_h2_wet_mid_M,
fc_tw_rr_h2_wet_mid_M,
fc_sw_rr_h2_wet_mid_M,
fc_p_rr_h2_wet_mid_M,
fc_h_rr_h2_wet_mid_M,
fc_f_rr_h2_wet_mid_M,
fc_s_rr_h2_wet_mid_M,
fc_y_rr_h2_wet_mid_M,
fly_rr_h2_wet_mid_M,
mh_rr_h2_wet_mid_M,
ch_rr_h2_wet_mid_M,
file="Env_Risk_Ratio_wet_Andrew_mid.Rdata")

save(
ec_tw_rd_h1_wet_mid_M,
ec_sw_rd_h1_wet_mid_M,
ec_p_rd_h1_wet_mid_M,
ec_h_rd_h1_wet_mid_M,
ec_f_rd_h1_wet_mid_M,
ec_s_rd_h1_wet_mid_M,
ec_y_rd_h1_wet_mid_M,
fc_tw_rd_h1_wet_mid_M,
fc_sw_rd_h1_wet_mid_M,
fc_p_rd_h1_wet_mid_M,
fc_h_rd_h1_wet_mid_M,
fc_f_rd_h1_wet_mid_M,
fc_s_rd_h1_wet_mid_M,
fc_y_rd_h1_wet_mid_M,
fly_rd_h1_wet_mid_M,
mh_rd_h1_wet_mid_M,
ch_rd_h1_wet_mid_M,
ec_tw_rd_h2_wet_mid_M,
ec_sw_rd_h2_wet_mid_M,
ec_p_rd_h2_wet_mid_M,
ec_h_rd_h2_wet_mid_M,
ec_f_rd_h2_wet_mid_M,
ec_s_rd_h2_wet_mid_M,
ec_y_rd_h2_wet_mid_M,
fc_tw_rd_h2_wet_mid_M,
fc_sw_rd_h2_wet_mid_M,
fc_p_rd_h2_wet_mid_M,
fc_h_rd_h2_wet_mid_M,
fc_f_rd_h2_wet_mid_M,
fc_s_rd_h2_wet_mid_M,
fc_y_rd_h2_wet_mid_M,
fly_rd_h2_wet_mid_M,
mh_rd_h2_wet_mid_M,
ch_rd_h2_wet_mid_M,
file="Env_Risk_diff_wet_Andrew_mid.Rdata")

#Save count difference
save(
ec_tw_dif_h1_wet_mid_M,
ec_sw_dif_h1_wet_mid_M,
ec_p_dif_h1_wet_mid_M,
ec_h_dif_h1_wet_mid_M,
ec_f_dif_h1_wet_mid_M,
ec_s_dif_h1_wet_mid_M,
ec_y_dif_h1_wet_mid_M,
fc_tw_dif_h1_wet_mid_M,
fc_sw_dif_h1_wet_mid_M,
fc_p_dif_h1_wet_mid_M,
fc_h_dif_h1_wet_mid_M,
fc_f_dif_h1_wet_mid_M,
fc_s_dif_h1_wet_mid_M,
fc_y_dif_h1_wet_mid_M,
fly_dif_h1_wet_mid_M,
ec_tw_dif_h2_wet_mid_M,
ec_sw_dif_h2_wet_mid_M,
ec_p_dif_h2_wet_mid_M,
ec_h_dif_h2_wet_mid_M,
ec_f_dif_h2_wet_mid_M,
ec_s_dif_h2_wet_mid_M,
ec_y_dif_h2_wet_mid_M,
fc_tw_dif_h2_wet_mid_M,
fc_sw_dif_h2_wet_mid_M,
fc_p_dif_h2_wet_mid_M,
fc_h_dif_h2_wet_mid_M,
fc_f_dif_h2_wet_mid_M,
fc_s_dif_h2_wet_mid_M,
fc_y_dif_h2_wet_mid_M,
fly_dif_h2_wet_mid_M,
file="Env_Count_Diff_wet_Andrew_mid.Rdata")





#Save dry RR
save(
ec_tw_rr_h1_dry_mid_M,
ec_sw_rr_h1_dry_mid_M,
ec_p_rr_h1_dry_mid_M,
ec_h_rr_h1_dry_mid_M,
ec_f_rr_h1_dry_mid_M,
ec_s_rr_h1_dry_mid_M,
ec_y_rr_h1_dry_mid_M,
fc_tw_rr_h1_dry_mid_M,
fc_sw_rr_h1_dry_mid_M,
fc_p_rr_h1_dry_mid_M,
fc_h_rr_h1_dry_mid_M,
fc_f_rr_h1_dry_mid_M,
fc_s_rr_h1_dry_mid_M,
fc_y_rr_h1_dry_mid_M,
fly_rr_h1_dry_mid_M,
mh_rr_h1_dry_mid_M,
ch_rr_h1_dry_mid_M,
ec_tw_rr_h2_dry_mid_M,
ec_sw_rr_h2_dry_mid_M,
ec_p_rr_h2_dry_mid_M,
ec_h_rr_h2_dry_mid_M,
ec_f_rr_h2_dry_mid_M,
ec_s_rr_h2_dry_mid_M,
ec_y_rr_h2_dry_mid_M,
fc_tw_rr_h2_dry_mid_M,
fc_sw_rr_h2_dry_mid_M,
fc_p_rr_h2_dry_mid_M,
fc_h_rr_h2_dry_mid_M,
fc_f_rr_h2_dry_mid_M,
fc_s_rr_h2_dry_mid_M,
fc_y_rr_h2_dry_mid_M,
fly_rr_h2_dry_mid_M,
mh_rr_h2_dry_mid_M,
ch_rr_h2_dry_mid_M,
file="Env_Risk_Ratio_dry_Andrew_mid.Rdata")

#Save dry RD
save(
ec_tw_rd_h1_dry_mid_M,
ec_sw_rd_h1_dry_mid_M,
ec_p_rd_h1_dry_mid_M,
ec_h_rd_h1_dry_mid_M,
ec_f_rd_h1_dry_mid_M,
ec_s_rd_h1_dry_mid_M,
ec_y_rd_h1_dry_mid_M,
fc_tw_rd_h1_dry_mid_M,
fc_sw_rd_h1_dry_mid_M,
fc_p_rd_h1_dry_mid_M,
fc_h_rd_h1_dry_mid_M,
fc_f_rd_h1_dry_mid_M,
fc_s_rd_h1_dry_mid_M,
fc_y_rd_h1_dry_mid_M,
fly_rd_h1_dry_mid_M,
mh_rd_h1_dry_mid_M,
ch_rd_h1_dry_mid_M,
ec_tw_rd_h2_dry_mid_M,
ec_sw_rd_h2_dry_mid_M,
ec_p_rd_h2_dry_mid_M,
ec_h_rd_h2_dry_mid_M,
ec_f_rd_h2_dry_mid_M,
ec_s_rd_h2_dry_mid_M,
ec_y_rd_h2_dry_mid_M,
fc_tw_rd_h2_dry_mid_M,
fc_sw_rd_h2_dry_mid_M,
fc_p_rd_h2_dry_mid_M,
fc_h_rd_h2_dry_mid_M,
fc_f_rd_h2_dry_mid_M,
fc_s_rd_h2_dry_mid_M,
fc_y_rd_h2_dry_mid_M,
fly_rd_h2_dry_mid_M,
mh_rd_h2_dry_mid_M,
ch_rd_h2_dry_mid_M,
file="Env_Risk_diff_dry_Andrew_mid.Rdata")

#Save count difference
save(
ec_tw_dif_h1_dry_mid_M,
ec_sw_dif_h1_dry_mid_M,
ec_p_dif_h1_dry_mid_M,
ec_h_dif_h1_dry_mid_M,
ec_f_dif_h1_dry_mid_M,
ec_s_dif_h1_dry_mid_M,
ec_y_dif_h1_dry_mid_M,
fc_tw_dif_h1_dry_mid_M,
fc_sw_dif_h1_dry_mid_M,
fc_p_dif_h1_dry_mid_M,
fc_h_dif_h1_dry_mid_M,
fc_f_dif_h1_dry_mid_M,
fc_s_dif_h1_dry_mid_M,
fc_y_dif_h1_dry_mid_M,
fly_dif_h1_dry_mid_M,
ec_tw_dif_h2_dry_mid_M,
ec_sw_dif_h2_dry_mid_M,
ec_p_dif_h2_dry_mid_M,
ec_h_dif_h2_dry_mid_M,
ec_f_dif_h2_dry_mid_M,
ec_s_dif_h2_dry_mid_M,
ec_y_dif_h2_dry_mid_M,
fc_tw_dif_h2_dry_mid_M,
fc_sw_dif_h2_dry_mid_M,
fc_p_dif_h2_dry_mid_M,
fc_h_dif_h2_dry_mid_M,
fc_f_dif_h2_dry_mid_M,
fc_s_dif_h2_dry_mid_M,
fc_y_dif_h2_dry_mid_M,
fly_dif_h2_dry_mid_M,
file="Env_Count_Diff_dry_Andrew_mid.Rdata")






















