
rm(list=ls())
library(dplyr)
library(ggplot2)
library(ggthemes) 
library(grid)
library(gridExtra)
library(scales)
library(lattice)

#Useful links
#Facet options:
     #http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
#Coloring X-axis text
     #https://stackoverflow.com/questions/22972478/color-axis-text-by-variable-in-ggplot
#Arranging ggplots:
     #https://github.com/baptiste/gridextra/wiki/arranging-ggplot
#Arranging grobs
     #https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
#Scaling in grid.arrange
     #https://stackoverflow.com/questions/16298599/keep-or-set-the-ratio-between-text-labels-and-size-of-plot-in-grid-arrange


#---------------------------------------
# Load data- Ayse's objects
#---------------------------------------

dir<-setwd("C:/Users/andre/Dropbox/WASHB EML/Results")

dir<-setwd("C:/Users/andre/Dropbox/WASHB EML/Results")
ays.res <- list.files(path=paste(dir,"/Ayse/", sep=""))
for(i in 1:length(ays.res)) {
  load(paste(dir,"/Ayse/",ays.res[i],sep=""))
}



#-----------------------------------
# World Bank prevalence processing
#-----------------------------------


ec_tw_prev_a<-cbind("Tubewell", rownames(ec_tw_prev_a), as.data.frame(ec_tw_prev_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_sw_prev_a<-cbind("Stored water", rownames(ec_sw_prev_a), as.data.frame( ec_sw_prev_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_p_prev_a<-cbind("Ponds", rownames(ec_p_prev_a), as.data.frame( ec_p_prev_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_h_prev_a<-cbind("Hands", rownames(ec_h_prev_a), as.data.frame( ec_h_prev_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_f_prev_a<-cbind("Food", rownames(ec_f_prev_a), as.data.frame( ec_f_prev_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ec_s_prev_a<-cbind("Soil", rownames(ec_s_prev_a), as.data.frame( ec_s_prev_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ec_y_prev_a<-cbind("Flies", rownames(ec_y_prev_a), as.data.frame( ec_y_prev_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
fly_prev_a<-cbind("FlyCount", rownames(fly_prev_a), as.data.frame( fly_prev_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 

#Construct dataframe of prevalence results
prev_wb<-(rbind(
ec_tw_prev_a,
ec_sw_prev_a,
ec_p_prev_a,
ec_h_prev_a,
ec_f_prev_a,
ec_s_prev_a,
ec_y_prev_a,
fly_prev_a))
rownames(prev_wb)<-NULL
colnames(prev_wb)<-c("Location","TR", "N", "Prevalence", "SD","Robust SE","lower.ci","upper.ci")

levels(prev_wb$TR)<-c("C","S","WSH")
#prev_wb[,3:ncol(prev_wb)]<-round(prev_wb[,3:ncol(prev_wb)],2)

prev_wb$TR.N<-paste0(prev_wb$TR, "\n(N=\n",prev_wb$N,")")
prev_wb$round<-"World Bank"
prev_wb$prev.perc<-paste0(prev_wb$Prevalence*100, "%")
#prev_wb$N<-paste0("(N=\n",prev_wb$N,")")

prev_wb$TR<-factor(prev_wb$TR)
prev_wb$TR = factor(prev_wb$TR,c("C","W","S","H","WSH","N","WSH+N"))

prev_wb$Prevalence<-prev_wb$Prevalence*100
prev_wb$lower.ci<-prev_wb$lower.ci*100
prev_wb$upper.ci<-prev_wb$upper.ci*100


#-----------------------------------
# Midline prevalence processing
#-----------------------------------

ec_tw_prev_mid_a<-cbind("Tubewell", rownames(ec_tw_prev_mid2_a), as.data.frame(ec_tw_prev_mid2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_sw_prev_mid_a<-cbind("Stored water", rownames(ec_sw_prev_mid2_a), as.data.frame( ec_sw_prev_mid2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_h_prev_mid_a<-cbind("Hands", rownames(ec_h_prev_mid2_a), as.data.frame( ec_h_prev_mid2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_t_prev_mid_a<-cbind("Toys", rownames(ec_t_prev_mid2_a), as.data.frame( ec_t_prev_mid2_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
fly_kit_prev_mid_a<-cbind("FlyCount_kit", rownames(fly_kit_prev_mid2_a), as.data.frame( fly_kit_prev_mid2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
fly_lat_prev_mid_a<-cbind("FlyCount_lat", rownames(fly_lat_prev_mid2_a), as.data.frame( fly_lat_prev_mid2_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 

#Construct dataframe of prevalence results
prev_mid<-(rbind(
ec_tw_prev_mid_a,
ec_sw_prev_mid_a,
ec_h_prev_mid_a,
ec_t_prev_mid_a,
fly_kit_prev_mid_a,
fly_lat_prev_mid_a
))
rownames(prev_mid)<-NULL
colnames(prev_mid)<-c("Location","TR", "N", "Prevalence", "SD","Robust SE","lower.ci","upper.ci")
levels(prev_mid$TR)
#levels(prev_mid$TR)<-c("C","N","WSH+N","W","WSH","H")
levels(prev_mid$TR)<-c("C","W","WSH","H", "N", "WSH+N")

#prev_mid[,3:ncol(prev_mid)]<-round(prev_mid[,3:ncol(prev_mid)],2)

prev_mid$TR.N<-paste0(prev_mid$TR, "\n(N=\n",prev_mid$N,")")
prev_mid$round<-"Year 1"
prev_mid$prev.perc<-paste0(prev_mid$Prevalence*100, "%")
#prev_mid$N<-paste0("(N=\n",prev_mid$N,")")

prev_mid$TR<-factor(prev_mid$TR)
prev_mid$TR = factor(prev_mid$TR,c("C","W","S","H","WSH","N","WSH+N"))

prev_mid$Prevalence<-prev_mid$Prevalence*100
prev_mid$lower.ci<-prev_mid$lower.ci*100
prev_mid$upper.ci<-prev_mid$upper.ci*100

#-----------------------------------
# Endline prevalence processing
#-----------------------------------

ec_tw_prev_end_a<-cbind("Tubewell", rownames(ec_tw_prev_end2_a), as.data.frame(ec_tw_prev_end2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_sw_prev_end_a<-cbind("Stored water", rownames(ec_sw_prev_end2_a), as.data.frame( ec_sw_prev_end2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_h_prev_end_a<-cbind("Hands", rownames(ec_h_prev_end2_a), as.data.frame( ec_h_prev_end2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_t_prev_end_a<-cbind("Toys", rownames(ec_t_prev_end2_a), as.data.frame( ec_t_prev_end2_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ec_f_prev_end_a<-cbind("Food", rownames(ec_f_prev_end2_a), as.data.frame( ec_f_prev_end2_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
fly_kit_prev_end_a<-cbind("FlyCount_kit", rownames(fly_kit_prev_end2_a), as.data.frame( fly_kit_prev_end2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
fly_lat_prev_end_a<-cbind("FlyCount_lat", rownames(fly_lat_prev_end2_a), as.data.frame( fly_lat_prev_end2_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 


#Construct dataframe of prevalence results
prev_end<-(rbind(
ec_tw_prev_end_a,
ec_sw_prev_end_a,
ec_h_prev_end_a,
ec_t_prev_end_a,
ec_f_prev_end_a,
fly_kit_prev_end_a,
fly_lat_prev_end_a))
rownames(prev_end)<-NULL
colnames(prev_end)<-c("Location","TR", "N", "Prevalence", "SD","Robust SE","lower.ci","upper.ci")

levels(prev_end$TR)
levels(prev_end$TR)<-c("C","W","WSH","H", "N", "WSH+N")

#prev_end[,3:ncol(prev_end)]<-round(prev_end[,3:ncol(prev_end)],2)

prev_end$TR.N<-paste0(prev_end$TR, "\n(N=\n",prev_end$N,")")
prev_end$round<-"Year 2"
prev_end$prev.perc<-paste0(prev_end$Prevalence*100, "%")
#prev_end$N<-paste0("(N=\n",prev_end$N,")")

prev_end$TR<-factor(prev_end$TR)
prev_end$TR = factor(prev_end$TR,c("C","W","S","H","WSH","N","WSH+N"))

prev_end$Prevalence<-prev_end$Prevalence*100
prev_end$lower.ci<-prev_end$lower.ci*100
prev_end$upper.ci<-prev_end$upper.ci*100

prev.dat<-rbind(prev_wb,prev_mid, prev_end)


#-----------------------------------
# World Bank log count processing
#-----------------------------------

ec_tw_mn<-cbind("Tubewell", rownames(ec_tw_mn_a), as.data.frame(ec_tw_mn_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_sw_mn<-cbind("Stored water", rownames(ec_sw_mn_a), as.data.frame( ec_sw_mn_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_p_mn<-cbind("Ponds", rownames(ec_p_mn_a), as.data.frame( ec_p_mn_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_h_mn<-cbind("Hands", rownames(ec_h_mn_a), as.data.frame( ec_h_mn_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_f_mn<-cbind("Food", rownames(ec_f_mn_a), as.data.frame( ec_f_mn_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ec_s_mn<-cbind("Soil", rownames(ec_s_mn_a), as.data.frame( ec_s_mn_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ec_y_mn<-cbind("Flies", rownames(ec_y_mn_a), as.data.frame( ec_y_mn_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
fly_mn<-cbind("FlyCount", rownames(fly_mn_a), as.data.frame( fly_mn_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 



#Construct dataframe of prevalence results
mn_wb<-(rbind(
ec_tw_mn,
ec_sw_mn,
ec_p_mn,
ec_h_mn,
ec_f_mn,
ec_s_mn,
ec_y_mn,
fly_mn))
rownames(mn_wb)<-NULL
colnames(mn_wb)<-c("Location","TR", "N", "Mean Log Count", "SD","Robust SE","lower.ci","upper.ci")

levels(mn_wb$TR)<-c("C","S","WSH")
#mn_wb[,3:ncol(mn_wb)]<-round(mn_wb[,3:ncol(mn_wb)],2)

mn_wb$TR.N<-paste0(mn_wb$TR, "\n(N=\n",mn_wb$N,")")
mn_wb$round<-"World Bank"

mn_wb$TR<-factor(mn_wb$TR)
mn_wb$TR = factor(mn_wb$TR,c("C","W","S","H","WSH","N","WSH+N"))

#-----------------------------------
# Midline log count processing
#-----------------------------------

ec_tw_mn_mid_a<-cbind("Tubewell", rownames(ec_tw_mn_mid2_a), as.data.frame(ec_tw_mn_mid2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_sw_mn_mid_a<-cbind("Stored water", rownames(ec_sw_mn_mid2_a), as.data.frame( ec_sw_mn_mid2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_h_mn_mid_a<-cbind("Hands", rownames(ec_h_mn_mid2_a), as.data.frame( ec_h_mn_mid2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_t_mn_mid_a<-cbind("Toys", rownames(ec_t_mn_mid2_a), as.data.frame( ec_t_mn_mid2_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
fly_kit_mn_mid_a<-cbind("FlyCount_kit", rownames(fly_kit_mn_mid2_a), as.data.frame( fly_kit_mn_mid2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
fly_lat_mn_mid_a<-cbind("FlyCount_lat", rownames(fly_lat_mn_mid2_a), as.data.frame( fly_lat_mn_mid2_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 


#Construct dataframe of mnalence results
mn_mid<-(rbind(
ec_tw_mn_mid_a,
ec_sw_mn_mid_a,
ec_h_mn_mid_a,
ec_t_mn_mid_a,
fly_kit_mn_mid_a,
fly_lat_mn_mid_a))
rownames(mn_mid)<-NULL
colnames(mn_mid)<-c("Location","TR", "N", "Mean Log Count", "SD","Robust SE","lower.ci","upper.ci")
levels(mn_mid$TR)
levels(mn_mid$TR)<-c("C","W","S","H","WSH","N","WSH+N")
#mn_mid[,3:ncol(mn_mid)]<-round(mn_mid[,3:ncol(mn_mid)],2)

mn_mid$TR.N<-paste0(mn_mid$TR, "\n(N=\n",mn_mid$N,")")
mn_mid$round<-"Year 1"
#mn_mid$N<-paste0("(N=\n",mn_mid$N,")")

mn_mid$TR<-factor(mn_mid$TR)
mn_mid$TR = factor(mn_mid$TR,c("C","W","S","H","WSH","N","WSH+N"))

#-----------------------------------
# Endline log count processing
#-----------------------------------


ec_tw_mn_end_a<-cbind("Tubewell", rownames(ec_tw_mn_end2_a), as.data.frame(ec_tw_mn_end2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_sw_mn_end_a<-cbind("Stored water", rownames(ec_sw_mn_end2_a), as.data.frame( ec_sw_mn_end2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_h_mn_end_a<-cbind("Hands", rownames(ec_h_mn_end2_a), as.data.frame( ec_h_mn_end2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_t_mn_end_a<-cbind("Toys", rownames(ec_t_mn_end2_a), as.data.frame( ec_t_mn_end2_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ec_f_mn_end_a<-cbind("Food", rownames(ec_f_mn_end2_a), as.data.frame( ec_f_mn_end2_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
fly_kit_mn_end_a<-cbind("FlyCount_kit", rownames(fly_kit_mn_end2_a), as.data.frame( fly_kit_mn_end2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
fly_lat_mn_end_a<-cbind("FlyCount_lat", rownames(fly_lat_mn_end2_a), as.data.frame( fly_lat_mn_end2_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 

#Construct dataframe of mnalence results
mn_end<-(rbind(
ec_tw_mn_end_a,
ec_sw_mn_end_a,
ec_h_mn_end_a,
ec_t_mn_end_a,
ec_f_mn_end_a,
fly_kit_mn_end_a,
fly_lat_mn_end_a))
rownames(mn_end)<-NULL
colnames(mn_end)<-c("Location","TR", "N", "Mean Log Count", "SD","Robust SE","lower.ci","upper.ci")

levels(mn_end$TR)
levels(mn_end$TR)<-c("C","W","S","H","WSH","N","WSH+N")
#mn_end[,3:ncol(mn_end)]<-round(mn_end[,3:ncol(mn_end)],2)

mn_end$TR.N<-paste0(mn_end$TR, "\n(N=\n",mn_end$N,")")
mn_end$round<-"Year 2"
#mn_end$mn.perc<-paste0(mn_end$mnalence*100, "%")
mn_end$N<-paste0("(N=\n",mn_end$N,")")

mn_end$TR<-factor(mn_end$TR)
mn_end$TR = factor(mn_end$TR,c("C","W","S","H","WSH","N","WSH+N"))


mn.dat<-rbind(mn_wb,mn_mid, mn_end)


#Add mean log10 count to prevalence dataset
prev_wb$log10<-mn_wb$`Mean Log Count`
prev_mid$log10<-mn_mid$`Mean Log Count`
prev_end$log10<-mn_end$`Mean Log Count`

prev.dat$log10<-mn.dat$`Mean Log Count`


#-----------------------------------
# World Bank prevalence ratio processing
#-----------------------------------


ec_tw_rr_h1_unadj<-cbind("Tubewell", rownames(ec_tw_rr_h1_unadj_a), as.data.frame(ec_tw_rr_h1_unadj_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_sw_rr_h1_unadj<-cbind("Stored water", rownames(ec_sw_rr_h1_unadj_a), as.data.frame( ec_sw_rr_h1_unadj_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_p_rr_h1_unadj<-cbind("Ponds", rownames(ec_p_rr_h1_unadj_a), as.data.frame( ec_p_rr_h1_unadj_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_h_rr_h1_unadj<-cbind("Hands", rownames(ec_h_rr_h1_unadj_a), as.data.frame( ec_h_rr_h1_unadj_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_f_rr_h1_unadj<-cbind("Food", rownames(ec_f_rr_h1_unadj_a), as.data.frame( ec_f_rr_h1_unadj_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ec_s_rr_h1_unadj<-cbind("Soil", rownames(ec_s_rr_h1_unadj_a), as.data.frame( ec_s_rr_h1_unadj_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ec_y_rr_h1_unadj<-cbind("Flies", rownames(ec_y_rr_h1_unadj_a), as.data.frame( ec_y_rr_h1_unadj_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
fly_rr_h1_unadj<-cbind("FlyCount", rownames(fly_rr_h1_unadj_a), as.data.frame( fly_rr_h1_unadj_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 


#Construct dataframe of prevalence results
pr_wb<-(rbind(
ec_tw_rr_h1_unadj,
ec_sw_rr_h1_unadj,
ec_p_rr_h1_unadj,
ec_h_rr_h1_unadj,
ec_f_rr_h1_unadj,
ec_s_rr_h1_unadj,
ec_y_rr_h1_unadj,
fly_rr_h1_unadj))
rownames(pr_wb)<-NULL
#pr_wb<-cbind(pr_wb[,1:2],rep(9999, nrow(pr_wb)),pr_wb[,3:9])
pr_wb<-cbind(pr_wb[,1:2],prev_wb[prev_wb$TR!="C","N"],pr_wb[,3:9])


colnames(pr_wb)<-c("Location","TR", "N", "PR","lower.ci","upper.ci","Estimate", "SD","Robust SE","P-value")
levels(pr_wb$TR)
levels(pr_wb$TR)<-c("S","WSH")
#pr_wb[,3:ncol(pr_wb)]<-round(pr_wb[,3:ncol(pr_wb)],2)

pr_wb$TR.N<-paste0(pr_wb$TR, "\n(N=\n",pr_wb$N,")")
pr_wb$round<-"World Bank"
#pr_wb$N<-paste0("(N=\n",pr_wb$N,")")

pr_wb$TR<-factor(pr_wb$TR)
pr_wb$TR = factor(pr_wb$TR,c("C","W","S","H","WSH","N","WSH+N"))

#Add in N's from prevalence objects
#prev_wb.N<-prev_wb[prev_wb$TR!="C",3]
#pr_wb$TR.N<-paste0(pr_wb$TR, "\n", prev_wb.N)

#Remove N's from axis labels
pr_wb$TR.N<-pr_wb$TR

#-----------------------------------
# Midline prevalence ratio processing
#-----------------------------------

ec_tw_rr_h1_unadj_mid<-cbind("Tubewell", rownames(ec_tw_rr_h1_unadj_mid_a), as.data.frame(ec_tw_rr_h1_unadj_mid_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_sw_rr_h1_unadj_mid<-cbind("Stored water", rownames(ec_sw_rr_h1_unadj_mid_a), as.data.frame( ec_sw_rr_h1_unadj_mid_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_h_rr_h1_unadj_mid<-cbind("Hands", rownames(ec_h_rr_h1_unadj_mid_a), as.data.frame( ec_h_rr_h1_unadj_mid_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_t_rr_h1_unadj_mid<-cbind("Toys", rownames(t(ec_t_rr_h1_unadj_mid_a)), as.data.frame( t(ec_t_rr_h1_unadj_mid_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
fly_kit_rr_h1_unadj_mid<-cbind("FlyCount_kit", rownames(t(fly_kit_rr_h1_unadj_mid_a)), as.data.frame( t(fly_kit_rr_h1_unadj_mid_a))) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
fly_lat_rr_h1_unadj_mid<-cbind("FlyCount_lat", rownames(t(fly_lat_rr_h1_unadj_mid_a)), as.data.frame( t(fly_lat_rr_h1_unadj_mid_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 

#Construct dataframe of mnalence results
pr_mid<-(rbind(
ec_tw_rr_h1_unadj_mid,
ec_sw_rr_h1_unadj_mid,
ec_h_rr_h1_unadj_mid,
ec_t_rr_h1_unadj_mid,
fly_kit_rr_h1_unadj_mid,
fly_lat_rr_h1_unadj_mid))
rownames(pr_mid)<-NULL
pr_mid<-cbind(pr_mid[,1:2],prev_mid[prev_mid$TR!="C","N"],pr_mid[,3:9])
colnames(pr_mid)<-c("Location","TR", "N", "PR","lower.ci","upper.ci","Estimate", "SD","Robust SE","P-value")
levels(pr_mid$TR)
levels(pr_mid$TR)<-c("W","WSH","H")
#pr_mid[,3:ncol(pr_mid)]<-round(pr_mid[,3:ncol(pr_mid)],2)

pr_mid$TR.N<-paste0(pr_mid$TR, "\n(N=\n",pr_mid$N,")")
pr_mid$round<-"Year 1"
#pr_mid$N<-paste0("(N=\n",pr_mid$N,")")

pr_mid$TR<-factor(pr_mid$TR)
pr_mid$TR = factor(pr_mid$TR,c("W","H","WSH"))

#Add in N's from prevalence objects
#prev_mid.N<-prev_mid[prev_mid$TR!="C",3]
#pr_mid$TR.N<-paste0(pr_mid$TR, "\n", prev_mid.N)

#Remove N's from axis labels
pr_mid$TR.N<-pr_mid$TR

#-----------------------------------
# Endline prevalence ratio processing
#-----------------------------------
ec_tw_rr_h1_unadj_end<-cbind("Tubewell", rownames(ec_tw_rr_h1_unadj_end_a), as.data.frame(ec_tw_rr_h1_unadj_end_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_sw_rr_h1_unadj_end<-cbind("Stored water", rownames(ec_sw_rr_h1_unadj_end_a), as.data.frame( ec_sw_rr_h1_unadj_end_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_h_rr_h1_unadj_end<-cbind("Hands", rownames(ec_h_rr_h1_unadj_end_a), as.data.frame( ec_h_rr_h1_unadj_end_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_t_rr_h1_unadj_end<-cbind("Toys", rownames(t(ec_t_rr_h1_unadj_end_a)), as.data.frame( t(ec_t_rr_h1_unadj_end_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ec_f_rr_h1_unadj_end<-cbind("Food", rownames((ec_f_rr_h1_unadj_end_a)), as.data.frame( (ec_f_rr_h1_unadj_end_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
fly_kit_rr_h1_unadj_end<-cbind("FlyCount_kit", rownames(t(fly_kit_rr_h1_unadj_end_a)), as.data.frame( t(fly_kit_rr_h1_unadj_end_a))) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
fly_lat_rr_h1_unadj_end<-cbind("FlyCount_lat", rownames(t(fly_lat_rr_h1_unadj_end_a)), as.data.frame( t(fly_lat_rr_h1_unadj_end_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 

#Construct dataframe of mnalence results
pr_end<-(rbind(
ec_tw_rr_h1_unadj_end,
ec_sw_rr_h1_unadj_end,
ec_h_rr_h1_unadj_end,
ec_t_rr_h1_unadj_end,
ec_f_rr_h1_unadj_end,
fly_kit_rr_h1_unadj_end,
fly_lat_rr_h1_unadj_end))
rownames(pr_end)<-NULL
pr_end<-cbind(pr_end[,1:2],prev_end[prev_end$TR!="C","N"],pr_end[,3:9])
colnames(pr_end)<-c("Location","TR", "N", "PR","lower.ci","upper.ci","Estimate", "SD","Robust SE","P-value")
levels(pr_end$TR)
levels(pr_end$TR)<-c("W","WSH","H")
#pr_end[,3:ncol(pr_end)]<-round(pr_end[,3:ncol(pr_end)],2)

pr_end$TR.N<-paste0(pr_end$TR, "\n(N=\n",pr_end$N,")")
pr_end$round<-"Year 2"
#pr_end$N<-paste0("(N=\n",pr_end$N,")")

pr_end$TR<-factor(pr_end$TR)
pr_end$TR = factor(pr_end$TR,c("W","H","WSH"))

#Add in N's from prevalence objects
#prev_end.N<-prev_end[prev_end$TR!="C",3]
#pr_end$TR.N<-paste0(pr_end$TR, "\n", prev_end.N)

#Remove N's from axis labels
pr_end$TR.N<-pr_end$TR


pr.dat<-rbind(pr_wb,pr_mid, pr_end)
pr.dat

#-----------------------------------
# World Bank log difference processing
#-----------------------------------

ec_tw_dif<-cbind("Tubewell", rownames(ec_tw_dif_h1_unadj_a), as.data.frame(ec_tw_dif_h1_unadj_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_sw_dif<-cbind("Stored water", rownames(ec_sw_dif_h1_unadj_a), as.data.frame( ec_sw_dif_h1_unadj_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_p_dif<-cbind("Ponds", rownames(ec_p_dif_h1_unadj_a), as.data.frame( ec_p_dif_h1_unadj_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_h_dif<-cbind("Hands", rownames(ec_h_dif_h1_unadj_a), as.data.frame( ec_h_dif_h1_unadj_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_f_dif<-cbind("Food", rownames(ec_f_dif_h1_unadj_a), as.data.frame( ec_f_dif_h1_unadj_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ec_s_dif<-cbind("Soil", rownames(ec_s_dif_h1_unadj_a), as.data.frame( ec_s_dif_h1_unadj_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ec_y_dif<-cbind("Flies", rownames(ec_y_dif_h1_unadj_a), as.data.frame( ec_y_dif_h1_unadj_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
fly_dif<-cbind("FlyCount", rownames(fly_dif_h1_unadj_a), as.data.frame( fly_dif_h1_unadj_a[,c(1:3,5:7)])) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 


#NOTE: Add in fly and mom/child dirty hands indicators into prevalence figures?

#Construct dataframe of prevalence results
dif_wb<-(rbind(
ec_tw_dif,
ec_sw_dif,
ec_p_dif,
ec_h_dif,
ec_f_dif,
ec_s_dif,
ec_y_dif,
fly_dif))
rownames(dif_wb)<-NULL
dif_wb<-cbind(dif_wb[,1:2],prev_wb[prev_wb$TR!="C","N"],dif_wb[,3:8])
colnames(dif_wb)<-c("Location","TR", "N", "Dif","lower.ci","upper.ci", "SD","Robust SE")

levels(dif_wb$TR)<-c("S","WSH")
#dif_wb[,3:ncol(dif_wb)]<-round(dif_wb[,3:ncol(dif_wb)],2)

dif_wb$TR.N<-paste0(dif_wb$TR, "\n(N=\n",dif_wb$N,")")
dif_wb$round<-"World Bank"
#dif_wb$dif.perc<-paste0(dif_wb$difalence*100, "%")
#dif_wb$N<-paste0("(N=\n",dif_wb$N,")")

dif_wb$TR<-factor(dif_wb$TR)
dif_wb$TR = factor(dif_wb$TR,c("C","W","S","H","WSH","N","WSH+N"))

#Add in N's from mn objects
#dif_wb.N<-mn_wb[mn_wb$TR!="C",3]
#dif_wb$TR.N<-paste0(dif_wb$TR, "\n", dif_wb.N)

#Remove N's from axis labels
dif_wb$TR.N<-dif_wb$TR



#-----------------------------------
# Midline log difference processing
#-----------------------------------

ec_tw_dif_mid_a<-cbind("Tubewell", rownames(ec_tw_dif_h1_unadj_mid_a), as.data.frame(ec_tw_dif_h1_unadj_mid_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_sw_dif_mid_a<-cbind("Stored water", rownames(ec_sw_dif_h1_unadj_mid_a), as.data.frame( ec_sw_dif_h1_unadj_mid_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_h_dif_mid_a<-cbind("Hands", rownames(ec_h_dif_h1_unadj_mid_a), as.data.frame( ec_h_dif_h1_unadj_mid_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_t_dif_mid_a<-cbind("Toys", rownames(t(ec_t_dif_h1_unadj_mid_a)), as.data.frame( t(ec_t_dif_h1_unadj_mid_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
fly_kit_dif_h1_unadj_mid<-cbind("FlyCount_kit", rownames(t(fly_kit_dif_h1_unadj_mid_a)), as.data.frame( t(fly_kit_dif_h1_unadj_mid_a[c(1:3,5:7),]))) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
fly_lat_dif_h1_unadj_mid<-cbind("FlyCount_lat", rownames(t(fly_lat_dif_h1_unadj_mid_a)), as.data.frame( t(fly_lat_dif_h1_unadj_mid_a[c(1:3,5:7),]))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 

#Construct dataframe of difalence results
dif_mid<-(rbind(
ec_tw_dif_mid_a,
ec_sw_dif_mid_a,
ec_h_dif_mid_a,
ec_t_dif_mid_a,
fly_kit_dif_h1_unadj_mid,
fly_lat_dif_h1_unadj_mid))
rownames(dif_mid)<-NULL
dif_mid<-cbind(dif_mid[,1:2],prev_mid[prev_mid$TR!="C","N"],dif_mid[,3:8])
colnames(dif_mid)<-c("Location","TR", "N", "Dif","lower.ci","upper.ci", "SD","Robust SE")
levels(dif_mid$TR)
levels(dif_mid$TR)<-c("W","WSH","H")
#dif_mid[,3:ncol(dif_mid)]<-round(dif_mid[,3:ncol(dif_mid)],2)

dif_mid$TR.N<-paste0(dif_mid$TR, "\n(N=\n",dif_mid$N,")")
dif_mid$round<-"Year 1"
#dif_mid$dif.perc<-paste0(dif_mid$difalence*100, "%")
#dif_mid$N<-paste0("(N=\n",dif_mid$N,")")

dif_mid$TR<-factor(dif_mid$TR)
dif_mid$TR = factor(dif_mid$TR,c("C","W","S","H","WSH","N","WSH+N"))

#Add in N's from mn objects
#dif_mid.N<-mn_mid[mn_mid$TR!="C",3]
#dif_mid$TR.N<-paste0(dif_mid$TR, "\n", dif_mid.N)

#Remove N's from axis labels
dif_mid$TR.N<-dif_mid$TR

#-----------------------------------
# Endline log difference processing
#-----------------------------------


ec_tw_dif_end_a<-cbind("Tubewell", rownames(ec_tw_dif_h1_unadj_end_a), as.data.frame(ec_tw_dif_h1_unadj_end_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_sw_dif_end_a<-cbind("Stored water", rownames(ec_sw_dif_h1_unadj_end_a), as.data.frame( ec_sw_dif_h1_unadj_end_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_h_dif_end_a<-cbind("Hands", rownames(ec_h_dif_h1_unadj_end_a), as.data.frame( ec_h_dif_h1_unadj_end_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_t_dif_end_a<-cbind("Toys", rownames(t(ec_t_dif_h1_unadj_end_a)), as.data.frame( t(ec_t_dif_h1_unadj_end_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ec_f_dif_end_a<-cbind("Food", rownames(ec_f_dif_h1_unadj_end_a), as.data.frame( ec_f_dif_h1_unadj_end_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
fly_kit_dif_h1_unadj_end<-cbind("FlyCount_kit", rownames(t(fly_kit_dif_h1_unadj_end_a)), as.data.frame( t(fly_kit_dif_h1_unadj_end_a[c(1:3,5:7),]))) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
fly_lat_dif_h1_unadj_end<-cbind("FlyCount_lat", rownames(t(fly_lat_dif_h1_unadj_end_a)), as.data.frame( t(fly_lat_dif_h1_unadj_end_a[c(1:3,5:7),]))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 

#Construct dataframe of difalence results
dif_end<-(rbind(
ec_tw_dif_end_a,
ec_sw_dif_end_a,
ec_h_dif_end_a,
ec_t_dif_end_a,
ec_f_dif_end_a,
fly_kit_dif_h1_unadj_end,
fly_lat_dif_h1_unadj_end))
rownames(dif_end)<-NULL
dif_end<-cbind(dif_end[,1:2],prev_end[prev_end$TR!="C","N"],dif_end[,3:8])
colnames(dif_end)<-c("Location","TR", "N", "Dif","lower.ci","upper.ci", "SD","Robust SE")

levels(dif_end$TR)
levels(dif_end$TR)<-c("W","WSH","H")
#dif_end[,3:ncol(dif_end)]<-round(dif_end[,3:ncol(dif_end)],2)

dif_end$TR.N<-paste0(dif_end$TR, "\n(N=\n",dif_end$N,")")
dif_end$round<-"Year 2"
#dif_end$dif.perc<-paste0(dif_end$difalence*100, "%")
#dif_end$N<-paste0("(N=\n",dif_end$N,")")

dif_end$TR<-factor(dif_end$TR)
dif_end$TR = factor(dif_end$TR,c("C","W","S","H","WSH","N","WSH+N"))

#Add in N's from mn objects
#dif_end.N<-mn_end[mn_end$TR!="C",3]
#dif_end$TR.N<-paste0(dif_end$TR, "\n", dif_end.N)

#Remove N's from axis labels
dif_end$TR.N<-dif_end$TR


dif.dat<-rbind(dif_wb,dif_mid, dif_end)




#-------------------------------------------
# Customize plot layout
#-------------------------------------------

# main study colors
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# cols <- c("gray30",cbPalette[c(2:4,6:8)])
# brighter color blind palette:  https://personal.sron.nl/~pault/ 
cblack <- "#000004FF"
cblue <- "#3366AA"
cteal <- "#11AA99"
cgreen <- "#66AA55"
cchartr <- "#CCCC55"
cmagent <- "#992288"
cred <- "#EE3333"
corange <- "#EEA722"
cyellow <- "#FFEE33"
cgrey <- "#777777"
cols=c(C=cblack,W=cblue,S=cteal,H=cgreen,WSH=corange,N=cred,"WSH+N"=cmagent)

cols=c(C=cblack,W=cblue,S=cteal,H=cgreen,WSH=corange)


arms=c("C", "W", "S", "H", "WSH")

# general label plot
ulabplot <- function(title) {
  plot(1,1,type="n",
       xaxt="n",xlab="",xlim=c(0,1),
       yaxt="n",ylab="",bty="n",ylim=c(0,1)
  )
  text(1,0.5,title,adj=1,cex=1.5)
}


#Order prevalence data
unique(prev.dat$Location)
unique(prev.dat$TR)
unique(prev.dat$round)
prev.dat$Location<-factor(prev.dat$Location)
table(prev.dat$Location)
prev.dat$Location<-factor(prev.dat$Location, c("Tubewell", "Stored water", "Hands", "Toys", "Food", "Ponds", "Soil", "Flies","FlyCount_kit", "FlyCount_lat"))
table(prev.dat$Location)

unique(pr.dat$Location)
unique(pr.dat$TR)
unique(pr.dat$round)
pr.dat$Location<-factor(pr.dat$Location)
table(pr.dat$Location)
pr.dat$Location<-factor(pr.dat$Location, c("Tubewell", "Stored water", "Hands", "Toys", "Food", "Ponds", "Soil", "Flies","FlyCount_kit", "FlyCount_lat"))
table(pr.dat$Location)

unique(dif.dat$Location)
unique(dif.dat$TR)
unique(dif.dat$round)
dif.dat$Location<-factor(dif.dat$Location)
table(dif.dat$Location)
dif.dat$Location<-factor(dif.dat$Location, c("Tubewell", "Stored water", "Hands", "Toys", "Food", "Ponds", "Soil", "Flies","FlyCount_kit", "FlyCount_lat"))
table(dif.dat$Location)









#-------------------------------------------
# Customize plot layout
#-------------------------------------------

# main study colors
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# cols <- c("gray30",cbPalette[c(2:4,6:8)])
# brighter color blind palette:  https://personal.sron.nl/~pault/ 
cblack <- "#000004FF"
cblue <- "#3366AA"
cteal <- "#11AA99"
cgreen <- "#66AA55"
cchartr <- "#CCCC55"
cmagent <- "#992288"
cred <- "#EE3333"
corange <- "#EEA722"
cyellow <- "#FFEE33"
cgrey <- "#777777"
cols=c(C=cblack,W=cblue,S=cteal,H=cgreen,WSH=corange,N=cred,"WSH+N"=cmagent)

cols=c(C=cblack,W=cblue,S=cteal,H=cgreen,WSH=corange)


arms=c("C", "W", "S", "H", "WSH")

# general label plot
ulabplot <- function(title) {
  plot(1,1,type="n",
       xaxt="n",xlab="",xlim=c(0,1),
       yaxt="n",ylab="",bty="n",ylim=c(0,1)
  )
  text(1,0.5,title,adj=1,cex=1.4, pos=2, offset = 0.5)
}


#Order prevalence data
unique(prev.dat$Location)
unique(prev.dat$TR)
unique(prev.dat$round)
prev.dat$Location<-factor(prev.dat$Location)
table(prev.dat$Location)
prev.dat$Location<-factor(prev.dat$Location, c("Tubewell", "Stored water", "Hands", "Toys", "Food", "Ponds", "Soil", "Flies","FlyCount_kit", "FlyCount_lat"))
table(prev.dat$Location)

unique(pr.dat$Location)
unique(pr.dat$TR)
unique(pr.dat$round)
pr.dat$Location<-factor(pr.dat$Location)
table(pr.dat$Location)
pr.dat$Location<-factor(pr.dat$Location, c("Tubewell", "Stored water", "Hands", "Toys", "Food", "Ponds", "Soil", "Flies","FlyCount_kit", "FlyCount_lat"))
table(pr.dat$Location)

unique(dif.dat$Location)
unique(dif.dat$TR)
unique(dif.dat$round)
dif.dat$Location<-factor(dif.dat$Location)
table(dif.dat$Location)
dif.dat$Location<-factor(dif.dat$Location, c("Tubewell", "Stored water", "Hands", "Toys", "Food", "Ponds", "Soil", "Flies","FlyCount_kit", "FlyCount_lat"))
table(dif.dat$Location)






#-------------------------------------------
# Prevalence plot function
#-------------------------------------------

prevplot<-function(d, i){
  
  
  xlabel_indices <- c(1,7,10,16,19,25)
  xaxis_indices <- c(2,8,11,17,20,26)
  xaxis_indices_primary <- c(1,10,19)
  

  if(nrow(d)==0){
       op <- par(mar=c(1,1,6,0)+0.1)
    	ulabplot("")
    	
         mtext(ifelse(i<9,
                      c("Tubewell", "Stored water", "Hands","Toys","Food","","Kitchen", "Latrine")[i-1],
                        ""),
                        side=3,line=0.25,col="gray20",cex=1)

  }else{
  
  ytics <- seq(0,100,by=10)  #<----------Set the Y-axis range here


if(i %in% xlabel_indices){
   op <- par(mar=c(1,0,6,1)+0.1)
  
  if(i %in% xaxis_indices_primary){
   ulabplot("E.coli\nprevalence\n(%)")
  }else{
   ulabplot("Fly\nprevalence\n(%)")
  }


	}else{
   op <- par(mar=c(1.5,1,6,0)+0.1)

   # set up an empty plot
MidPts <- barplot(1:5,names.arg=NA,border=NA,col=NA,
	ylim=c(range(ytics)[1],range(ytics)[2]+5),ylab="",yaxt="n",
	las=1,bty="n"
	)
  box()
	segments(x0=0,x1=6.2,y0=ytics[-1],lty=2,lwd=1,col="gray80")
	if(i %in% xaxis_indices){
	    axis(2,at=ytics,las=1)
	}
	#Vector of arms to plot:
	which.arms<-arms %in% d$TR
	num.arms<-sum(which.arms)
	if(num.arms==2){pos=c(MidPts[2],MidPts[4])}
	if(num.arms==3){pos=c(MidPts[1],MidPts[3],MidPts[5])}
	if(num.arms==4){pos=c(MidPts[1],
	                      MidPts[1] + (MidPts[5]-MidPts[1])/3,
	                      MidPts[5] - (MidPts[5]-MidPts[1])/3,
                        MidPts[5])}


	textsize=0.7 #<-------adjust text size here
	# plot estimates
	arrows(x0=pos, y0=d$lower.ci, y1=d$upper.ci, col=cols[which.arms],lwd=2,length=0.05,angle=90,code=3)
	points(pos,d$Prevalence,pch=21,cex=1.5,lwd=1,col=cols[which.arms],bg="white")
	points(pos,d$Prevalence,pch=21,cex=1.5,lwd=0,col=cols[which.arms],bg=alpha(cols[which.arms],alpha=0.5))
  	mtext(d$TR, side=1,line=0.5,at=pos,col=cols[which.arms],cex=textsize,las=1)

	  # X-axis labels
		if(i==2 | i==8){
  mtext(text="",side=1,line=0.5,at=-1.35,col=cblack,cex=textsize,las=1)
  mtext(text=c("N",d$N),side=3,line=2.25,at=c(-1,pos),col=c(cblack,cols[which.arms]),cex=textsize,las=1)
  mtext(text=c("Prevalence (%)",sprintf("%1.0f",d$Prevalence)),side=3,line=1.25,at=c(-2.6,pos),col=c(cblack,cols[which.arms]),cex=textsize,las=1)
  if(i==2) mtext(text=c("Log10 Mean",sprintf("%1.2f",d$log10)),side=3,line=0.25,at=c(-2.35,pos),col=c(cblack,cols[which.arms]),cex=textsize,las=1)
  if(i==8) mtext(text=c("Mean count",sprintf("%1.2f",d$log10)),side=3,line=0.25,at=c(-2.35,pos),col=c(cblack,cols[which.arms]),cex=textsize,las=1)
      }else{
		mtext(d$N, side=3,line=2.25,at=pos,col=cols[which.arms],cex=textsize,las=1)
    mtext(sprintf("%1.0f",d$Prevalence), side=3,line=1.25, at=pos,col=cols[which.arms],cex=textsize,las=1)
    mtext(sprintf("%1.2f",d$log10), side=3,line=0.25, at=pos,col=cols[which.arms],cex=textsize,las=1)
		}
         mtext(ifelse(i<10,
                      c("Tubewell", "Stored water", "Hands","Toys","Food","","Kitchen\nfly count", "Latrine\nfly count")[i-1],
                        ""),
                        side=3,line=3.5,col="gray20",cex=0.95)

  }
}

}



#-------------------------------------------
# Prevalence Ratio plot function
#-------------------------------------------


prplot<-function(d, i){
  
  xlabel_indices <- c(1,7,10,16,19,25)
  xaxis_indices <- c(2,8,11,17,20,26)
  xaxis_indices_primary <- c(1,10,19)
  
  if(nrow(d)==0){
       op <- par(mar=c(1,1,4,0)+0.1)
    	ulabplot("")
    	
         mtext(ifelse(i<11,
                      c("Tubewell", "Stored water", "Hands", "Food", "Ponds", "Soil", "Flies","", "Fly\ncount")[i-1],
                        ""),
                        side=3,line=0.25,col="gray20",cex=1)

  }else{
  
    exp_tics<-seq(-1.75,1,by=.25) #<----------Set the Y-axis range here
    ytics <- round(2^exp_tics,2)
               ytics <- c(0.25, 0.5, 1, 2)
              labs = c("1/4","1/2","1","2")


if(i %in% xlabel_indices){
   op <- par(mar=c(1,0,5,1)+0.1)

  if(i %in% xaxis_indices_primary){
  ulabplot("E.coli\nprevalence\nratio")
  }else{
   ulabplot("Fly\nprevalence\nratio")
  }


	}else{
   op <- par(mar=c(1.5,1,4,0)+0.1)

   # set up an empty plot
MidPts <- barplot(1:5,names.arg=NA,border=NA,col=NA,
	ylim=c(range(ytics)[1],range(ytics)[2]+diff(range(ytics))/20),ylab="",yaxt="n",
	las=1,bty="n", log="y"
	)
  box()
	segments(x0=0,x1=6.2,y0=ytics[-1],lty=2,lwd=1,col="gray80")
	segments(x0=0,x1=6.2,y0=1,lty=1,lwd=1,col="black")

		if(i %in% xaxis_indices){
       axis(2,at=ytics,las=1, labels = labs)
	}
	#Vector of arms to plot:
	which.arms<-arms %in% d$TR
	#pos<-MidPts[which.arms]
	num.arms<-sum(which.arms)
	if(num.arms==1){pos=c(MidPts[4])}
	if(num.arms==2){pos=c(MidPts[3],MidPts[5])}
	if(num.arms==3){pos=c(
	                      MidPts[1] + (MidPts[5]-MidPts[1])/3,
	                      MidPts[5] - (MidPts[5]-MidPts[1])/3,
                        MidPts[5])}

	
	
	textsize=0.7 #<-------adjust text size here
	# plot estimates
	arrows(x0=pos, y0=d$lower.ci, y1=d$upper.ci, col=cols[which.arms],lwd=2,length=0.05,angle=90,code=3)
	points(pos,d$PR,pch=21,cex=1.5,lwd=1,col=cols[which.arms],bg="white")
	points(pos,d$PR,pch=21,cex=1.5,lwd=0,col=cols[which.arms],bg=alpha(cols[which.arms],alpha=0.5))
  	mtext(d$TR, side=1,line=0.5,at=pos,col=cols[which.arms],cex=textsize,las=1)

	  # X-axis labels
 		if(i %in% xaxis_indices){
 		    mtext(text="",side=1,line=0.5,at=-1.35,col=cblack,cex=textsize,las=1)
    }

           mtext(ifelse(i<10,
                      c("Tubewell", "Stored water", "Hands", "Food", "Ponds", "Soil", "Flies")[i-1],
                        ""),
                        side=3,line=0.25,col="gray20",cex=1)
  }
}


}

#-------------------------------------------
# Prevalence Difference plot function
#-------------------------------------------

diffplot<-function(d, i){
  
  xlabel_indices <- c(1,7,10,16,19,25)
  xaxis_indices <- c(2,8,11,17,20,26)
  xaxis_indices_primary <- c(1,10,19)
  
  if(nrow(d)==0){
       op <- par(mar=c(1,1,4,0)+0.1)
    	ulabplot("")
    	
         mtext(ifelse(i<10,
                      c("Tubewell", "Stored water", "Hands", "Food", "Ponds", "Soil", "Flies")[i-1],
                        ""),
                        side=3,line=0.25,col="gray20",cex=1)

  }else{
  
  ytics <- seq(-1.4,0.6,by=.2) #<----------Set the Y-axis range here
   labs = NULL 
  if(i>25){
        exp_tics<-seq(-1.5,1,by=.25)
    ytics <- round(2^exp_tics,2)
               ytics <- c(0.25, 0.5, 1, 2)
     labs = c("1/4","1/2","1","2")
    }
  ytics<-round(ytics,2)

if(i %in% xlabel_indices){
   op <- par(mar=c(1,0,5,1)+0.1)

  if(i %in% xaxis_indices_primary){
ulabplot("E.coli\nlog 10\ndifference")
  }else{
   ulabplot("Fly\ncount\nratio")
  }

	}else{
   op <- par(mar=c(1.5,1,4,0)+0.1)

   # set up an empty plot
        if(i>25){
  MidPts <- barplot(1:5,names.arg=NA,border=NA,col=NA,
	ylim=c(range(ytics)[1],range(ytics)[2]+diff(range(ytics))/20),ylab="",yaxt="n",
	las=1,bty="n", log="y")
    }else{
  MidPts <- barplot(1:5,names.arg=NA,border=NA,col=NA,
	ylim=c(range(ytics)[1],range(ytics)[2]+diff(range(ytics))/20),ylab="",yaxt="n",
	las=1,bty="n")  
    }

  box()
	segments(x0=0,x1=6.2,y0=ytics[-1],lty=2,lwd=1,col="gray80")
	if(i<26){segments(x0=0,x1=6.2,y0=0,lty=1,lwd=1,col="black")}
	if(i>25){segments(x0=0,x1=max(MidPts+0.75),y0=1,lty=1,lwd=1,col="black")}
		if(i %in% xaxis_indices){
		  	if(is.null(labs)){
	        axis(2,at=ytics,las=1)
	      }else{
	        	axis(2,at=ytics,las=1, labels = labs)
	      }
		  }
	#Vector of arms to plot:
	which.arms<-arms %in% d$TR
	num.arms<-sum(which.arms)
	if(num.arms==1){pos=c(MidPts[4])}
	if(num.arms==2){pos=c(MidPts[3],MidPts[5])}
	if(num.arms==3){pos=c(
	                      MidPts[1] + (MidPts[5]-MidPts[1])/3,
	                      MidPts[5] - (MidPts[5]-MidPts[1])/3,
                        MidPts[5])}

	textsize=0.7 #<-------adjust text size here
	# plot estimates
	arrows(x0=pos, y0=d$lower.ci, y1=d$upper.ci, col=cols[which.arms],lwd=2,length=0.05,angle=90,code=3)
	points(pos,d$Dif,pch=21,cex=1.5,lwd=1,col=cols[which.arms],bg="white")
	points(pos,d$Dif,pch=21,cex=1.5,lwd=0,col=cols[which.arms],bg=alpha(cols[which.arms],alpha=0.5))
  	mtext(d$TR, side=1,line=0.5,at=pos,col=cols[which.arms],cex=textsize,las=1)

	  # X-axis labels
  	 		if(i %in% xaxis_indices){
  	 		mtext(text="",side=1,line=0.5,at=-1.35,col=cblack,cex=textsize,las=1)
    }

           mtext(ifelse(i<10,
                      c("Tubewell", "Stored water", "Hands", "Food", "Ponds", "Soil", "Flies")[i-1],
                        ""),
                        side=3,line=0.25,col="gray20",cex=1)
  }
}


}







  
setwd("C:/Users/andre/Dropbox/WASHB EML/Results/Figures")
pdf("EnvAnalysisEndlinePlot.pdf",width=10,height=8.5, paper="USr")


m <- rbind(c(1,2,3,4,5,6,7,8,9), c(10,11,12,13,14,15,16,17,18), c(19,20,21,22,23,24,25,26,27))
par(xpd=NA)

layout(m)


    i<-c(1,3,1)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(1,3,2)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(2,3,3)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(3,3,4)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(4,3,5)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(5,3,6)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(1,3,7)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(9,3,8)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(10,3,9)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])

    
    i<-c(1,3,10)
    prplot(d=pr.dat[pr.dat$Location==levels(pr.dat$Location)[i[1]] & pr.dat$round==unique(pr.dat$round)[i[2]],], i[3])
    i<-c(1,3,11)
    prplot(d=pr.dat[pr.dat$Location==levels(pr.dat$Location)[i[1]] & pr.dat$round==unique(pr.dat$round)[i[2]],], i[3])
    i<-c(2,3,12)
    prplot(d=pr.dat[pr.dat$Location==levels(pr.dat$Location)[i[1]] & pr.dat$round==unique(pr.dat$round)[i[2]],], i[3])
    i<-c(3,3,13)
    prplot(d=pr.dat[pr.dat$Location==levels(pr.dat$Location)[i[1]] & pr.dat$round==unique(pr.dat$round)[i[2]],], i[3])
    i<-c(4,3,14)
    prplot(d=pr.dat[pr.dat$Location==levels(pr.dat$Location)[i[1]] & pr.dat$round==unique(pr.dat$round)[i[2]],], i[3])
    i<-c(5,3,15)
    prplot(d=pr.dat[pr.dat$Location==levels(pr.dat$Location)[i[1]] & pr.dat$round==unique(pr.dat$round)[i[2]],], i[3])
    i<-c(1,3,16)
    prplot(d=pr.dat[pr.dat$Location==levels(pr.dat$Location)[i[1]] & pr.dat$round==unique(pr.dat$round)[i[2]],], i[3])
    i<-c(9,3,17)
    prplot(d=pr.dat[pr.dat$Location==levels(pr.dat$Location)[i[1]] & pr.dat$round==unique(pr.dat$round)[i[2]],], i[3])
    i<-c(10,3,18)
    prplot(d=pr.dat[pr.dat$Location==levels(pr.dat$Location)[i[1]] & pr.dat$round==unique(pr.dat$round)[i[2]],], i[3])

    i<-c(1,3,19)
    diffplot(d=dif.dat[dif.dat$Location==levels(dif.dat$Location)[i[1]] & dif.dat$round==unique(dif.dat$round)[i[2]],], i[3])
    i<-c(1,3,20)
    diffplot(d=dif.dat[dif.dat$Location==levels(dif.dat$Location)[i[1]] & dif.dat$round==unique(dif.dat$round)[i[2]],], i[3])
    i<-c(2,3,21)
    diffplot(d=dif.dat[dif.dat$Location==levels(dif.dat$Location)[i[1]] & dif.dat$round==unique(dif.dat$round)[i[2]],], i[3])
    i<-c(3,3,22)
    diffplot(d=dif.dat[dif.dat$Location==levels(dif.dat$Location)[i[1]] & dif.dat$round==unique(dif.dat$round)[i[2]],], i[3])
    i<-c(4,3,23)
    diffplot(d=dif.dat[dif.dat$Location==levels(dif.dat$Location)[i[1]] & dif.dat$round==unique(dif.dat$round)[i[2]],], i[3])
    i<-c(5,3,24)
    diffplot(d=dif.dat[dif.dat$Location==levels(dif.dat$Location)[i[1]] & dif.dat$round==unique(dif.dat$round)[i[2]],], i[3])
    i<-c(1,3,25)
    diffplot(d=dif.dat[dif.dat$Location==levels(dif.dat$Location)[i[1]] & dif.dat$round==unique(dif.dat$round)[i[2]],], i[3])
    i<-c(9,3,26)
    diffplot(d=dif.dat[dif.dat$Location==levels(dif.dat$Location)[i[1]] & dif.dat$round==unique(dif.dat$round)[i[2]],], i[3])
    i<-c(10,3,27)
    diffplot(d=dif.dat[dif.dat$Location==levels(dif.dat$Location)[i[1]] & dif.dat$round==unique(dif.dat$round)[i[2]],], i[3])

    
dev.off()



