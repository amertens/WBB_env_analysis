
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
mh_nail_prev_end_M <-d  %>% subset(tr!="Water")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$mhdirt_nail, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mh_fing_prev_end_M <-d  %>% subset(tr!="Water")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$mhdirt_fing, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mh_palm_prev_end_M <-d  %>% subset(tr!="Water")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$mhdirt_palm, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ch_nail_prev_end_M <-d  %>% subset(tr!="Water")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$chdirt_nail, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ch_fing_prev_end_M <-d  %>% subset(tr!="Water")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$chdirt_fing, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ch_palm_prev_end_M <-d  %>% subset(tr!="Water")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$chdirt_palm, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 





ec_f_prev_end_M<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
#fc_f_prev_end_M<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 

table(is.na(d$logecF))
table(is.na(d$ecposF))

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





########################
#Unadjusted GLMs
########################

#Create TMLE wrapper function
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
         flycaught_kit,flycaught_lat,mhdirt,chdirt,ecposF,
         mhdirt_nail, mhdirt_fing, mhdirt_palm, chdirt_nail, chdirt_fing, chdirt_palm))
Y.log<-subset(d, select=c(logecT, logecH, logecW, logfcT, logfcH, logfcW, numfly_kit,numfly_lat,logecF))

contrast1=list(c("C+N pooled","Water"),c("C+N pooled","Handwashing"),c("C+N pooled","WSH+WSHN pooled"),c("Water","WSH+WSHN pooled"),c("Handwashing","WSH+WSHN pooled"))
contrast2=list(c("C+N pooled","WSH+WSHN pooled"))
contrast3=list(c("C+N pooled","Handwashing"),c("C+N pooled","WSH+WSHN pooled"),c("Handwashing","WSH+WSHN pooled"))
contrast4=list(c("C+N pooled","Water"),c("C+N pooled","WSH+WSHN pooled"),c("Water","WSH+WSHN pooled"))
Y.pos.contrasts<-list(contrast2,contrast3,contrast4,contrast2,contrast3,contrast4,contrast2,contrast2,contrast3,contrast3,contrast1,contrast3,contrast3,contrast3,contrast3,contrast3,contrast3)
Y.log.contrasts<-list(contrast2,contrast3,contrast4,contrast2,contrast3,contrast4,contrast2,contrast2,contrast1)



#------------
#Binary outcomes
#------------
pos_unadj<-apply_glm(Ys=Y.pos,tr=d$tr.pool,id=d$block, contrasts.list=Y.pos.contrasts,  measure="RR")


#Prevalence Ratio-unadjusted
#(h1): sanitation vs. control, combined WSH vs. control  
ec_t_rr_h1_unadj_end_M<-pos_unadj[[1]][1,1:7]
ec_h_rr_h1_unadj_end_M<-pos_unadj[[2]][1:2,1:7]
ec_dw_rr_h1_unadj_end_M<-pos_unadj[[3]][1:2,1:7]
fc_t_rr_h1_unadj_end_M<-pos_unadj[[4]][1,1:7]
fc_h_rr_h1_unadj_end_M<-pos_unadj[[5]][1:2,1:7]
fc_dw_rr_h1_unadj_end_M<-pos_unadj[[6]][1:2,1:7]
fly_kit_rr_h1_unadj_end_M<-pos_unadj[[7]][1,1:7]
fly_lat_rr_h1_unadj_end_M<-pos_unadj[[8]][1,1:7]
mh_rr_h1_unadj_end_M<-pos_unadj[[9]][1:2,1:7]
ch_rr_h1_unadj_end_M<-pos_unadj[[10]][1:2,1:7]
ec_f_rr_h1_unadj_end_M<-pos_unadj[[11]][1:3,1:7]
mh_nail_rr_h1_unadj_end_M<-pos_unadj[[12]][1:2,1:7]
mh_fing_rr_h1_unadj_end_M<-pos_unadj[[13]][1:2,1:7]
mh_palm_rr_h1_unadj_end_M<-pos_unadj[[14]][1:2,1:7]
ch_nail_rr_h1_unadj_end_M<-pos_unadj[[15]][1:2,1:7]
ch_fing_rr_h1_unadj_end_M<-pos_unadj[[16]][1:2,1:7]
ch_palm_rr_h1_unadj_end_M<-pos_unadj[[17]][1:2,1:7]

#(H2): combined WSH vs. sanitation.
#ec_t_rr_h2_unadj_end_M<-pos_unadj[[1]][4:5,1:7]
ec_h_rr_h2_unadj_end_M<-pos_unadj[[2]][3,1:7]
ec_dw_rr_h2_unadj_end_M<-pos_unadj[[3]][3,1:7]
#fc_t_rr_h2_unadj_end_M<-pos_unadj[[4]][4:5,1:7]
fc_h_rr_h2_unadj_end_M<-pos_unadj[[5]][3,1:7]
fc_dw_rr_h2_unadj_end_M<-pos_unadj[[6]][3,1:7]
#fly_rr_h2_unadj_end_M<-pos_unadj[[7]][3,1:7]
mh_rr_h2_unadj_end_M<-pos_unadj[[9]][3,1:7]
ch_rr_h2_unadj_end_M<-pos_unadj[[10]][3,1:7]
ec_f_rr_h2_unadj_end_M<-pos_unadj[[11]][4:5,1:7]
mh_nail_rr_h2_unadj_end_M<-pos_unadj[[12]][3,1:7]
mh_fing_rr_h2_unadj_end_M<-pos_unadj[[13]][3,1:7]
mh_palm_rr_h2_unadj_end_M<-pos_unadj[[14]][3,1:7]
ch_nail_rr_h2_unadj_end_M<-pos_unadj[[15]][3,1:7]
ch_fing_rr_h2_unadj_end_M<-pos_unadj[[16]][3,1:7]
ch_palm_rr_h2_unadj_end_M<-pos_unadj[[17]][3,1:7]


#Prevalence Difference-unadjusted
#(h1): sanitation vs. control, combined WSH vs. control  
ec_t_rd_h1_unadj_end_M<-pos_unadj[[1]][1,8:13]
ec_h_rd_h1_unadj_end_M<-pos_unadj[[2]][1:2,8:13]
ec_dw_rd_h1_unadj_end_M<-pos_unadj[[3]][1:2,8:13]
fc_t_rd_h1_unadj_end_M<-pos_unadj[[4]][1,8:13]
fc_h_rd_h1_unadj_end_M<-pos_unadj[[5]][1:2,8:13]
fc_dw_rd_h1_unadj_end_M<-pos_unadj[[6]][1:2,8:13]
fly_kit_rd_h1_unadj_end_M<-pos_unadj[[7]][1,8:13]
fly_lat_rd_h1_unadj_end_M<-pos_unadj[[8]][1,8:13]
mh_rd_h1_unadj_end_M<-pos_unadj[[9]][1:2,8:13]
ch_rd_h1_unadj_end_M<-pos_unadj[[10]][1:2,8:13]
ec_f_rd_h1_unadj_end_M<-pos_unadj[[11]][1:3,8:13]
mh_nail_rd_h1_unadj_end_M<-pos_unadj[[12]][1:2,8:13]
mh_fing_rd_h1_unadj_end_M<-pos_unadj[[13]][1:2,8:13]
mh_palm_rd_h1_unadj_end_M<-pos_unadj[[14]][1:2,8:13]
ch_nail_rd_h1_unadj_end_M<-pos_unadj[[15]][1:2,8:13]
ch_fing_rd_h1_unadj_end_M<-pos_unadj[[16]][1:2,8:13]
ch_palm_rd_h1_unadj_end_M<-pos_unadj[[17]][1:2,8:13]

#(H2): combined WSH vs. sanitation.
#ec_t_rd_h2_unadj_end_M<-pos_unadj[[1]][4:5,8:13]
ec_h_rd_h2_unadj_end_M<-pos_unadj[[2]][3,8:13]
ec_dw_rd_h2_unadj_end_M<-pos_unadj[[3]][3,8:13]
#fc_t_rd_h2_unadj_end_M<-pos_unadj[[4]][3,8:13]
fc_h_rd_h2_unadj_end_M<-pos_unadj[[5]][3,8:13]
fc_dw_rd_h2_unadj_end_M<-pos_unadj[[6]][3,8:13]
#fly_rd_h2_unadj_end_M<-pos_unadj[[7]][3,8:13]
mh_rd_h2_unadj_end_M<-pos_unadj[[9]][3,8:13]
ch_rd_h2_unadj_end_M<-pos_unadj[[10]][3,8:13]
ec_f_rd_h2_unadj_end_M<-pos_unadj[[11]][4:5,8:13]
mh_nail_rd_h2_unadj_end_M<-pos_unadj[[12]][3,8:13]
mh_fing_rd_h2_unadj_end_M<-pos_unadj[[13]][3,8:13]
mh_palm_rd_h2_unadj_end_M<-pos_unadj[[14]][3,8:13]
ch_nail_rd_h2_unadj_end_M<-pos_unadj[[15]][3,8:13]
ch_fing_rd_h2_unadj_end_M<-pos_unadj[[16]][3,8:13]
ch_palm_rd_h2_unadj_end_M<-pos_unadj[[17]][3,8:13]

log_unadj<-apply_glm(Ys=Y.log,tr=d$tr.pool,id=d$block, contrasts.list=Y.log.contrasts, measure="RD")


#(h1): sanitation vs. control, combined WSH vs. control  
ec_t_dif_h1_unadj_end_M<-log_unadj[[1]][1,1:6]
ec_h_dif_h1_unadj_end_M<-log_unadj[[2]][1:2,1:6]
ec_dw_dif_h1_unadj_end_M<-log_unadj[[3]][1:2,1:6]
fc_t_dif_h1_unadj_end_M<-log_unadj[[4]][1,1:6]
fc_h_dif_h1_unadj_end_M<-log_unadj[[5]][1:2,1:6]
fc_dw_dif_h1_unadj_end_M<-log_unadj[[6]][1:2,1:6]
fly_kit_dif_h1_unadj_end_M<-log_unadj[[7]][1,1:7]
fly_lat_dif_h1_unadj_end_M<-log_unadj[[8]][1,1:7]
ec_f_dif_h1_unadj_end_M<-log_unadj[[9]][1:3,1:6]

#(H2): combined WSH vs. sanitation.
ec_h_dif_h2_unadj_end_M<-log_unadj[[2]][3,1:6]
ec_dw_dif_h2_unadj_end_M<-log_unadj[[3]][3,1:6]
fc_h_dif_h2_unadj_end_M<-log_unadj[[5]][3,1:6]
fc_dw_dif_h2_unadj_end_M<-log_unadj[[6]][3,1:6]
ec_f_dif_h2_unadj_end_M<-log_unadj[[9]][4:5,1:6]






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
mh_nail_prev_end_M,
mh_fing_prev_end_M,
mh_palm_prev_end_M,
ch_nail_prev_end_M,
ch_fing_prev_end_M,
ch_palm_prev_end_M,
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
#fc_f_mn_end_M,
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
#fc_f_rr_h1_unadj_end_M,
ec_f_rr_h2_unadj_end_M,
#fc_f_rr_h2_unadj_end_M,
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
#fc_f_rd_h1_unadj_end_M,
ec_f_rd_h2_unadj_end_M,
#fc_f_rd_h2_unadj_end_M,
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


#Save child and mom hand dirtiness stratified by hand location
save(
mh_nail_rr_h1_unadj_end_M,
mh_fing_rr_h1_unadj_end_M,
mh_palm_rr_h1_unadj_end_M,
ch_nail_rr_h1_unadj_end_M,
ch_fing_rr_h1_unadj_end_M,
ch_palm_rr_h1_unadj_end_M,
mh_nail_rr_h2_unadj_end_M,
mh_fing_rr_h2_unadj_end_M,
mh_palm_rr_h2_unadj_end_M,
ch_nail_rr_h2_unadj_end_M,
ch_fing_rr_h2_unadj_end_M,
ch_palm_rr_h2_unadj_end_M,
mh_nail_rd_h1_unadj_end_M,
mh_fing_rd_h1_unadj_end_M,
mh_palm_rd_h1_unadj_end_M,
ch_nail_rd_h1_unadj_end_M,
ch_fing_rd_h1_unadj_end_M,
ch_palm_rd_h1_unadj_end_M,
mh_nail_rd_h2_unadj_end_M,
mh_fing_rd_h2_unadj_end_M,
mh_palm_rd_h2_unadj_end_M,
ch_nail_rd_h2_unadj_end_M,
ch_fing_rd_h2_unadj_end_M,
ch_palm_rd_h2_unadj_end_M,
file="Env_M_C_hand_unadj_Andrew_mid.Rdata")




########################
#Effect modification by rainfall
########################



wet<-subset(d, wet==1)
dry<-subset(d, wet==0)


ec_t_prev_wet_end_M<-wet %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposT, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_h_prev_wet_end_M<-wet %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_dw_prev_wet_end_M<-wet %>% subset(tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_t_prev_wet_end_M<-wet %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposT, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_h_prev_wet_end_M<-wet %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_dw_prev_wet_end_M<-wet %>% subset(tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_kit_prev_wet_end_M<-wet  %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$flycaught_kit, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_lat_prev_wet_end_M<-wet  %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$flycaught_lat, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mh_prev_wet_end_M<-wet  %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$mhdirt, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ch_prev_wet_end_M<-wet  %>% subset(tr!="Water")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$chdirt, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_f_prev_wet_end_M<-wet %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 


ec_t_mn_wet_end_M<-wet  %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecT, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_h_mn_wet_end_M<-wet  %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_dw_mn_wet_end_M<-wet  %>% subset(tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_t_mn_wet_end_M<-wet  %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcT, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_h_mn_wet_end_M<-wet  %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_dw_mn_wet_end_M<-wet  %>% subset(tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_kit_mn_wet_end_M<-wet  %>% subset(tr!="Water" & tr!="Handwashing")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$numfly_kit, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_lat_mn_wet_end_M<-wet  %>% subset(tr!="Water" & tr!="Handwashing")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$numfly_lat, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_f_mn_wet_end_M<-wet  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 


ec_t_prev_dry_end_M<-dry %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposT, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_h_prev_dry_end_M<-dry %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_dw_prev_dry_end_M<-dry %>% subset(tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_t_prev_dry_end_M<-dry %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposT, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_h_prev_dry_end_M<-dry %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_dw_prev_dry_end_M<-dry %>% subset(tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$fcposW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_kit_prev_dry_end_M<-dry  %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$flycaught_kit, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_lat_prev_dry_end_M<-dry  %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$flycaught_lat, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mh_prev_dry_end_M<-dry  %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$mhdirt, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ch_prev_dry_end_M<-dry  %>% subset(tr!="Water")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$chdirt, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_f_prev_dry_end_M<-dry %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ecposF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 

ec_t_mn_dry_end_M<-dry  %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecT, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_h_mn_dry_end_M<-dry  %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_dw_mn_dry_end_M<-dry  %>% subset(tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_t_mn_dry_end_M<-dry  %>% subset(tr!="Water" & tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcT, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_h_mn_dry_end_M<-dry  %>% subset(tr!="Water") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcH, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fc_dw_mn_dry_end_M<-dry  %>% subset(tr!="Handwashing") %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logfcW, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_kit_mn_dry_end_M<-dry  %>% subset(tr!="Water" & tr!="Handwashing")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$numfly_kit, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
fly_lat_mn_dry_end_M<-dry  %>% subset(tr!="Water" & tr!="Handwashing")  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$numfly_lat, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
ec_f_mn_dry_end_M<-dry  %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$logecF, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 




#Create GLM wrapper function
apply_glm_EM<-function(Ys,tr=d$tr,id=d$block, contrasts.list, measure="RR", W=NULL ,V=NULL){

  W[,1]<-factor(W[,1])
  #contrasts<-list(c("Control","Sanitation"), c("Control","WSH"),c("Sanitation","WSH"))
  varlist<-colnames(Ys)
  res_list<-NULL
  
  for(i in 1:ncol(Ys)){
    contrasts<-contrasts.list[[i]]
    cat("#",i,": ",varlist[i],"\n")
      if(measure=="RD" | measure=="neg.binom"){
        temp<-matrix(NA, length(contrasts)*2, 8)
      }else{
        temp<-matrix(NA, length(contrasts)*2, 16)
      }

    for(j in 1:length(contrasts)){
      if(measure=="RD"){
           family="gaussian"
          if(length(grep("numfly",varlist[i],ignore.case=TRUE))>0){
                      family="neg.binom"
                      }
           temp2<-washb_glm(Y=Ys[,i], tr=tr, W=W, V=V, id=id, pair=NULL, family=family, contrast= contrasts[[j]], print=F)
           temp[2*j-1,1:7]<-as.numeric(temp2$lincom[1,1:7])
           temp[2*j,1:7]<-as.numeric(temp2$lincom[2,1:7])
           if(family=="gaussian"){
           temp[2*j-1,8]<-as.numeric(temp2$fit[4,6])
           temp[2*j,8]<-as.numeric(temp2$fit[4,6])             
           }
           if(family=="neg.binom"){
           temp[2*j-1,8]<-as.numeric(temp2$fit[4,7])
           temp[2*j,8]<-as.numeric(temp2$fit[4,7])   
           }
               colnames(temp)<-c("Subgroup","ATE","ci.lb","ci.lb","SE","Zval","P-val", "Int-P")
            }
      if(measure=="RR"){
        temp2<-(washb_glm(Y=Ys[,i], tr=tr, W=W, V=V, id=id, pair=NULL, family=poisson(link=`log`), contrast= contrasts[[j]], print=F))
        temp[2*j-1,1:7]<-as.numeric(temp2$lincom[1,1:7])
        temp[2*j,1:7]<-as.numeric(temp2$lincom[2,1:7])
        temp[2*j-1,8]<-as.numeric(temp2$fit[4,7])
        temp[2*j,8]<-as.numeric(temp2$fit[4,7])

        temp2<-(washb_glm(Y=Ys[,i], tr=tr, W=W, V=V, id=id, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F))
        temp[2*j-1,9:15]<-as.numeric(temp2$lincom[1,1:7])
        temp[2*j,9:15]<-as.numeric(temp2$lincom[2,1:7])
        temp[2*j-1,16]<-as.numeric(temp2$fit[4,6])
        temp[2*j,16]<-as.numeric(temp2$fit[4,6])
        
        colnames(temp)<-c("Subgroup","RR","ci.lb","ci.lb","SE","Zval","P-val","Int-P","subgroup","ATE","ci.lb","ci.lb","SE","Zval","P-val","Int-P")    
        }
      }
    temp<-data.frame(temp)
    temp$Subgroup<-temp$Subgroup-1
    if(measure=="RR"){temp$subgroup<-temp$subgroup-1}
    res_list[[i]]<-(temp)
    names(res_list)[[i]]<-varlist[i]
  }
  return(res_list)
}



#Binary outcomes- subgroup analysis
pos_em<-apply_glm_EM(Ys=Y.pos,tr=d$tr.pool,id=d$block, measure="RR",contrasts.list=Y.pos.contrasts,W=subset(d, select=wet),V="wet")

#WET
#Prevalence Ratio-unadjusted
#(h1): sanitation vs. control, combined WSH vs. control  
ec_t_rr_h1_em_end_M<-pos_em[[1]][1:2,1:8]
ec_h_rr_h1_em_end_M<-pos_em[[2]][1:4,1:8]
ec_dw_rr_h1_em_end_M<-pos_em[[3]][1:4,1:8]
fc_t_rr_h1_em_end_M<-pos_em[[4]][1:2,1:8]
fc_h_rr_h1_em_end_M<-pos_em[[5]][1:4,1:8]
fc_dw_rr_h1_em_end_M<-pos_em[[6]][1:4,1:8]
fly_kit_rr_h1_em_end_M<-pos_em[[7]][1:2,1:8]
fly_lat_rr_h1_em_end_M<-pos_em[[8]][1:2,1:8]
mh_rr_h1_em_end_M<-pos_em[[9]][1:4,1:8]
ch_rr_h1_em_end_M<-pos_em[[10]][1:4,1:8]
ec_f_rr_h1_em_end_M<-pos_em[[11]][1:6,1:8]


#(H2): combined WSH vs. sanitation.
ec_h_rr_h2_em_end_M<-as.matrix(pos_em[[2]][5:6,1:8])
ec_dw_rr_h2_em_end_M<-as.matrix(pos_em[[3]][5:6,1:8])
fc_h_rr_h2_em_end_M<-as.matrix(pos_em[[5]][5:6,1:8])
fc_dw_rr_h2_em_end_M<-as.matrix(pos_em[[6]][5:6,1:8])
mh_rr_h2_em_end_M<-as.matrix(pos_em[[9]][5:6,1:8])
ch_rr_h2_em_end_M<-as.matrix(pos_em[[10]][5:6,1:8])
ec_f_rr_h2_em_end_M<-pos_em[[11]][7:10,1:8]


#Prevalence Difference-wetusted
#(h1): sanitation vs. control, combined WSH vs. control  
ec_t_rd_h1_em_end_M<-pos_em[[1]][1:2,9:16]
ec_h_rd_h1_em_end_M<-pos_em[[2]][1:4,9:16]
ec_dw_rd_h1_em_end_M<-pos_em[[3]][1:4,9:16]
fc_t_rd_h1_em_end_M<-pos_em[[4]][1:2,9:16]
fc_h_rd_h1_em_end_M<-pos_em[[5]][1:4,9:16]
fc_dw_rd_h1_em_end_M<-pos_em[[6]][1:4,9:16]
fly_kit_rd_h1_em_end_M<-pos_em[[7]][1:2,9:16]
fly_lat_rd_h1_em_end_M<-pos_em[[8]][1:2,9:16]
mh_rd_h1_em_end_M<-pos_em[[9]][1:4,9:16]
ch_rd_h1_em_end_M<-pos_em[[10]][1:4,9:16]
ec_f_rd_h1_em_end_M<-pos_em[[11]][1:6,9:16]

#(H2): combined WSH vs. sanitation.
ec_h_rd_h2_em_end_M<-as.matrix(pos_em[[2]][5:6,9:16])
ec_dw_rd_h2_em_end_M<-as.matrix(pos_em[[3]][5:6,9:16])
fc_h_rd_h2_em_end_M<-as.matrix(pos_em[[5]][5:6,9:16])
fc_dw_rd_h2_em_end_M<-as.matrix(pos_em[[6]][5:6,9:16])
mh_rd_h2_em_end_M<-as.matrix(pos_em[[9]][5:6,9:16])
ch_rd_h2_em_end_M<-as.matrix(pos_em[[10]][5:6,9:16])
ec_f_rd_h2_em_end_M<-pos_em[[11]][7:10,9:16]



log_em<-apply_glm_EM(Ys=Y.log,tr=d$tr.pool,id=d$block, measure="RD", contrasts.list=Y.log.contrasts,W=subset(d, select=wet),V="wet")


#WET
#(h1): sanitation vs. control, combined WSH vs. control  
ec_t_dif_h1_em_end_M<-log_em[[1]][1:2,1:8]
ec_h_dif_h1_em_end_M<-log_em[[2]][1:4,1:8]
ec_dw_dif_h1_em_end_M<-log_em[[3]][1:4,1:8]
fc_t_dif_h1_em_end_M<-log_em[[4]][1:2,1:8]
fc_h_dif_h1_em_end_M<-log_em[[5]][1:4,1:8]
fc_dw_dif_h1_em_end_M<-log_em[[6]][1:4,1:8]
fly_kit_dif_h1_em_end_M<-log_em[[7]][1:2,1:8]
fly_lat_dif_h1_em_end_M<-log_em[[8]][1:2,1:8]
ec_f_dif_h1_em_end_M<-log_em[[9]][1:6,1:8]

#(H2): combined WSH vs. sanitation.
ec_h_dif_h2_em_end_M<-as.matrix(log_em[[2]][5:6,1:8])
ec_dw_dif_h2_em_end_M<-as.matrix(log_em[[3]][5:6,1:8])
fc_h_dif_h2_em_end_M<-as.matrix(log_em[[5]][5:6,1:8])
fc_dw_dif_h2_em_end_M<-as.matrix(log_em[[6]][5:6,1:8])
ec_f_dif_h2_em_end_M<-as.matrix(log_em[[9]][7:10,1:8])







#Save prevalences
save(ec_t_prev_wet_end_M,
ec_h_prev_wet_end_M,
ec_dw_prev_wet_end_M,
fc_t_prev_wet_end_M,
fc_h_prev_wet_end_M,
fc_dw_prev_wet_end_M,
fly_kit_prev_wet_end_M,
fly_lat_prev_wet_end_M,
mh_prev_wet_end_M,
ch_prev_wet_end_M,
ec_f_prev_wet_end_M,
file="Env_Prevalence_Andrew_wet_end.Rdata")


#Save means
save(ec_t_mn_wet_end_M,
ec_h_mn_wet_end_M,
ec_dw_mn_wet_end_M, 
fc_t_mn_wet_end_M,
fc_h_mn_wet_end_M, 
fc_dw_mn_wet_end_M,
fly_kit_mn_wet_end_M, 
fly_lat_mn_wet_end_M,
ec_f_mn_wet_end_M,
file="Env_Means_Andrew_wet_end.Rdata")


#Save prevalences
save(ec_t_prev_dry_end_M,
ec_h_prev_dry_end_M,
ec_dw_prev_dry_end_M,
fc_t_prev_dry_end_M,
fc_h_prev_dry_end_M,
fc_dw_prev_dry_end_M,
fly_kit_prev_dry_end_M,
fly_lat_prev_dry_end_M,
mh_prev_dry_end_M,
ch_prev_dry_end_M,
ec_f_prev_dry_end_M,
file="Env_Prevalence_Andrew_dry_end.Rdata")


#Save means
save(ec_t_mn_dry_end_M,
ec_h_mn_dry_end_M,
ec_dw_mn_dry_end_M, 
fc_t_mn_dry_end_M,
fc_h_mn_dry_end_M, 
fc_dw_mn_dry_end_M,
fly_kit_mn_dry_end_M, 
fly_lat_mn_dry_end_M,
ec_f_mn_dry_end_M,
file="Env_Means_Andrew_dry_end.Rdata")




#Save unadjusted RR
save(
ec_t_rr_h1_em_end_M,
ec_h_rr_h1_em_end_M,
ec_dw_rr_h1_em_end_M,
fc_t_rr_h1_em_end_M,
fc_h_rr_h1_em_end_M,
fc_dw_rr_h1_em_end_M,
fly_kit_rr_h1_em_end_M,
fly_lat_rr_h1_em_end_M,
mh_rr_h1_em_end_M,
ch_rr_h1_em_end_M,
ec_h_rr_h2_em_end_M,
ec_dw_rr_h2_em_end_M,
fc_h_rr_h2_em_end_M,
fc_dw_rr_h2_em_end_M,
mh_rr_h2_em_end_M,
ch_rr_h2_em_end_M,
ec_f_rr_h1_em_end_M,
ec_f_rr_h2_em_end_M,
file="Env_Risk_Ratio_em_Andrew_end.Rdata")

#Save unadjusted RD
save(
ec_t_rd_h1_em_end_M,
ec_h_rd_h1_em_end_M,
ec_dw_rd_h1_em_end_M,
fc_t_rd_h1_em_end_M,
fc_h_rd_h1_em_end_M,
fc_dw_rd_h1_em_end_M,
fly_kit_rd_h1_em_end_M,
fly_lat_rd_h1_em_end_M,
mh_rd_h1_em_end_M,
ch_rd_h1_em_end_M,
ec_h_rd_h2_em_end_M,
ec_dw_rd_h2_em_end_M,
fc_h_rd_h2_em_end_M,
fc_dw_rd_h2_em_end_M,
mh_rd_h2_em_end_M,
ch_rd_h2_em_end_M,
ec_f_rd_h1_em_end_M,
ec_f_rd_h2_em_end_M,
file="Env_Risk_diff_em_Andrew_end.Rdata")

#Save count difference
save(
ec_t_dif_h1_em_end_M,
ec_h_dif_h1_em_end_M,
ec_dw_dif_h1_em_end_M,
fc_t_dif_h1_em_end_M,
fc_h_dif_h1_em_end_M,
fc_dw_dif_h1_em_end_M,
fly_kit_dif_h1_em_end_M,
fly_lat_dif_h1_em_end_M,
ec_h_dif_h2_em_end_M,
ec_dw_dif_h2_em_end_M,
fc_h_dif_h2_em_end_M,
fc_dw_dif_h2_em_end_M,
ec_f_dif_h1_em_end_M,
ec_f_dif_h2_em_end_M,
file="Env_Count_Diff_em_Andrew_end.Rdata")









