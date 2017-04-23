
#---------------------------------------
# Environmental Analysis - figures
#
# andrew mertens (amertens@berkeley.edu)
#
#---------------------------------------

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
#fly_prev_a<-as.data.frame(cbind("Flies", ec_tw_prev_a))
#mh_prev_a<-as.data.frame(cbind("Mother Hands", ec_tw_prev_a))
#ch_prev_a<-as.data.frame(cbind("Child Hands", ec_tw_prev_a))

#NOTE: Add in fly and mom/child dirty hands indicators into prevalence figures?

#Construct dataframe of prevalence results
prev_wb<-(rbind(
ec_tw_prev_a,
ec_sw_prev_a,
ec_p_prev_a,
ec_h_prev_a,
ec_f_prev_a,
ec_s_prev_a,
ec_y_prev_a))
rownames(prev_wb)<-NULL
colnames(prev_wb)<-c("Location","TR", "N", "Prevalence", "SD","Robust SE","lower.ci","upper.ci")

levels(prev_wb$TR)<-c("C","S","WSH")
prev_wb[,3:ncol(prev_wb)]<-round(prev_wb[,3:ncol(prev_wb)],2)

prev_wb$TR.N<-paste0(prev_wb$TR, "\n(N=",prev_wb$N,")")
prev_wb$round<-"World Bank"
prev_wb$prev.perc<-paste0(prev_wb$Prevalence*100, "%")
prev_wb$N<-paste0("(N=",prev_wb$N,")")

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
#fly_prev_a<-as.data.frame(cbind("Flies", ec_tw_prev_a))
#mh_prev_a<-as.data.frame(cbind("Mother Hands", ec_tw_prev_a))
#ch_prev_a<-as.data.frame(cbind("Child Hands", ec_tw_prev_a))


#Construct dataframe of prevalence results
prev_mid<-(rbind(
ec_tw_prev_mid_a,
ec_sw_prev_mid_a,
ec_h_prev_mid_a,
ec_t_prev_mid_a))
rownames(prev_mid)<-NULL
colnames(prev_mid)<-c("Location","TR", "N", "Prevalence", "SD","Robust SE","lower.ci","upper.ci")
levels(prev_mid$TR)
#levels(prev_mid$TR)<-c("C","N","WSH+N","W","WSH","H")
levels(prev_mid$TR)<-c("C","W","WSH","H")

prev_mid[,3:ncol(prev_mid)]<-round(prev_mid[,3:ncol(prev_mid)],2)

prev_mid$TR.N<-paste0(prev_mid$TR, "\n(N=",prev_mid$N,")")
prev_mid$round<-"Year 1"
prev_mid$prev.perc<-paste0(prev_mid$Prevalence*100, "%")
prev_mid$N<-paste0("(N=",prev_mid$N,")")

prev_mid$TR<-factor(prev_mid$TR)
#prev_mid$TR = factor(prev_mid$TR,c("C","W","S","H","WSH","N","WSH+N"))
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

#Construct dataframe of prevalence results
prev_end<-(rbind(
ec_tw_prev_end_a,
ec_sw_prev_end_a,
ec_h_prev_end_a,
ec_t_prev_end_a,
ec_f_prev_end_a))
rownames(prev_end)<-NULL
colnames(prev_end)<-c("Location","TR", "N", "Prevalence", "SD","Robust SE","lower.ci","upper.ci")

levels(prev_end$TR)
#levels(prev_end$TR)<-c("C","N","WSH+N","W","WSH","H")
levels(prev_end$TR)<-c("C","W","WSH","H")

prev_end[,3:ncol(prev_end)]<-round(prev_end[,3:ncol(prev_end)],2)

prev_end$TR.N<-paste0(prev_end$TR, "\n(N=",prev_end$N,")")
prev_end$round<-"Year 2"
prev_end$prev.perc<-paste0(prev_end$Prevalence*100, "%")
prev_end$N<-paste0("(N=",prev_end$N,")")

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


#NOTE: Add in fly and mom/child dirty hands indicators into prevalence figures?

#Construct dataframe of prevalence results
mn_wb<-(rbind(
ec_tw_mn,
ec_sw_mn,
ec_p_mn,
ec_h_mn,
ec_f_mn,
ec_s_mn,
ec_y_mn))
rownames(mn_wb)<-NULL
colnames(mn_wb)<-c("Location","TR", "N", "Mean Log Count", "SD","Robust SE","lower.ci","upper.ci")

levels(mn_wb$TR)<-c("C","S","WSH")
mn_wb[,3:ncol(mn_wb)]<-round(mn_wb[,3:ncol(mn_wb)],2)

mn_wb$TR.N<-paste0(mn_wb$TR, "\n(N=",mn_wb$N,")")
mn_wb$round<-"World Bank"
#mn_wb$mn.perc<-paste0(mn_wb$mnalence*100, "%")
mn_wb$N<-paste0("(N=",mn_wb$N,")")

mn_wb$TR<-factor(mn_wb$TR)
mn_wb$TR = factor(mn_wb$TR,c("C","W","S","H","WSH","N","WSH+N"))

#-----------------------------------
# Midline log count processing
#-----------------------------------

ec_tw_mn_mid_a<-cbind("Tubewell", rownames(ec_tw_mn_mid2_a), as.data.frame(ec_tw_mn_mid2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_sw_mn_mid_a<-cbind("Stored water", rownames(ec_sw_mn_mid2_a), as.data.frame( ec_sw_mn_mid2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_h_mn_mid_a<-cbind("Hands", rownames(ec_h_mn_mid2_a), as.data.frame( ec_h_mn_mid2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_t_mn_mid_a<-cbind("Toys", rownames(ec_t_mn_mid2_a), as.data.frame( ec_t_mn_mid2_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 

#Construct dataframe of mnalence results
mn_mid<-(rbind(
ec_tw_mn_mid_a,
ec_sw_mn_mid_a,
ec_h_mn_mid_a,
ec_t_mn_mid_a))
rownames(mn_mid)<-NULL
colnames(mn_mid)<-c("Location","TR", "N", "Mean Log Count", "SD","Robust SE","lower.ci","upper.ci")
levels(mn_mid$TR)
#levels(mn_mid$TR)<-c("C","N","WSH+N","W","WSH","H")
levels(mn_mid$TR)<-c("C","W","WSH","H")
mn_mid[,3:ncol(mn_mid)]<-round(mn_mid[,3:ncol(mn_mid)],2)

mn_mid$TR.N<-paste0(mn_mid$TR, "\n(N=",mn_mid$N,")")
mn_mid$round<-"Year 1"
mn_mid$N<-paste0("(N=",mn_mid$N,")")

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

#Construct dataframe of mnalence results
mn_end<-(rbind(
ec_tw_mn_end_a,
ec_sw_mn_end_a,
ec_h_mn_end_a,
ec_t_mn_end_a,
ec_f_mn_end_a))
rownames(mn_end)<-NULL
colnames(mn_end)<-c("Location","TR", "N", "Mean Log Count", "SD","Robust SE","lower.ci","upper.ci")

levels(mn_end$TR)
levels(mn_end$TR)<-c("C","W","WSH","H")
mn_end[,3:ncol(mn_end)]<-round(mn_end[,3:ncol(mn_end)],2)

mn_end$TR.N<-paste0(mn_end$TR, "\n(N=",mn_end$N,")")
mn_end$round<-"Year 2"
#mn_end$mn.perc<-paste0(mn_end$mnalence*100, "%")
mn_end$N<-paste0("(N=",mn_end$N,")")

mn_end$TR<-factor(mn_end$TR)
mn_end$TR = factor(mn_end$TR,c("C","W","S","H","WSH","N","WSH+N"))


mn.dat<-rbind(mn_wb,mn_mid, mn_end)


#Add mean log10 count to prevalence dataset
prev_wb$log10<-mn_wb$`Mean Log Count`
prev_mid$log10<-mn_mid$`Mean Log Count`
prev_end$log10<-mn_end$`Mean Log Count`



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


#Construct dataframe of prevalence results
pr_wb<-(rbind(
ec_tw_rr_h1_unadj,
ec_sw_rr_h1_unadj,
ec_p_rr_h1_unadj,
ec_h_rr_h1_unadj,
ec_f_rr_h1_unadj,
ec_s_rr_h1_unadj,
ec_y_rr_h1_unadj))
rownames(pr_wb)<-NULL
pr_wb<-cbind(pr_wb[,1:2],rep(9999, nrow(pr_wb)),pr_wb[,3:9])
colnames(pr_wb)<-c("Location","TR", "N", "PR","lower.ci","upper.ci", "SD","Robust SE","P-value")
levels(pr_wb$TR)
levels(pr_wb$TR)<-c("S","WSH")
pr_wb[,3:ncol(pr_wb)]<-round(pr_wb[,3:ncol(pr_wb)],2)

pr_wb$TR.N<-paste0(pr_wb$TR, "\n(N=",pr_wb$N,")")
pr_wb$round<-"World Bank"
pr_wb$N<-paste0("(N=",pr_wb$N,")")

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

#Construct dataframe of mnalence results
pr_mid<-(rbind(
ec_tw_rr_h1_unadj_mid,
ec_sw_rr_h1_unadj_mid,
ec_h_rr_h1_unadj_mid,
ec_t_rr_h1_unadj_mid))
rownames(pr_mid)<-NULL
pr_mid<-cbind(pr_mid[,1:2],rep(9999, nrow(pr_mid)),pr_mid[,3:9])
colnames(pr_mid)<-c("Location","TR", "N", "PR","lower.ci","upper.ci", "SD","Robust SE")
levels(pr_mid$TR)
levels(pr_mid$TR)<-c("W","WSH","H")
pr_mid[,3:ncol(pr_mid)]<-round(pr_mid[,3:ncol(pr_mid)],2)

pr_mid$TR.N<-paste0(pr_mid$TR, "\n(N=",pr_mid$N,")")
pr_mid$round<-"Year 1"
pr_mid$N<-paste0("(N=",pr_mid$N,")")

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

#Construct dataframe of mnalence results
pr_end<-(rbind(
ec_tw_rr_h1_unadj_end,
ec_sw_rr_h1_unadj_end,
ec_h_rr_h1_unadj_end,
ec_t_rr_h1_unadj_end,
ec_f_rr_h1_unadj_end))
rownames(pr_end)<-NULL
pr_end<-cbind(pr_end[,1:2],rep(9999, nrow(pr_end)),pr_end[,3:9])
colnames(pr_end)<-c("Location","TR", "N", "PR","lower.ci","upper.ci", "SD","Robust SE")
levels(pr_end$TR)
levels(pr_end$TR)<-c("W","WSH","H")
pr_end[,3:ncol(pr_end)]<-round(pr_end[,3:ncol(pr_end)],2)

pr_end$TR.N<-paste0(pr_end$TR, "\n(N=",pr_end$N,")")
pr_end$round<-"Year 1"
pr_end$N<-paste0("(N=",pr_end$N,")")

pr_end$TR<-factor(pr_end$TR)
pr_end$TR = factor(pr_end$TR,c("W","H","WSH"))

#Add in N's from prevalence objects
#prev_end.N<-prev_end[prev_end$TR!="C",3]
#pr_end$TR.N<-paste0(pr_end$TR, "\n", prev_end.N)

#Remove N's from axis labels
pr_end$TR.N<-pr_end$TR

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


#NOTE: Add in fly and mom/child dirty hands indicators into prevalence figures?

#Construct dataframe of prevalence results
dif_wb<-(rbind(
ec_tw_dif,
ec_sw_dif,
ec_p_dif,
ec_h_dif,
ec_f_dif,
ec_s_dif,
ec_y_dif))
rownames(dif_wb)<-NULL
dif_wb<-cbind(dif_wb[,1:2],rep(9999, nrow(dif_wb)),dif_wb[,3:8])
colnames(dif_wb)<-c("Location","TR", "N", "Dif","lower.ci","upper.ci", "SD","Robust SE")

levels(dif_wb$TR)<-c("S","WSH")
dif_wb[,3:ncol(dif_wb)]<-round(dif_wb[,3:ncol(dif_wb)],2)

dif_wb$TR.N<-paste0(dif_wb$TR, "\n(N=",dif_wb$N,")")
dif_wb$round<-"World Bank"
#dif_wb$dif.perc<-paste0(dif_wb$difalence*100, "%")
dif_wb$N<-paste0("(N=",dif_wb$N,")")

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

#Construct dataframe of difalence results
dif_mid<-(rbind(
ec_tw_dif_mid_a,
ec_sw_dif_mid_a,
ec_h_dif_mid_a,
ec_t_dif_mid_a))
rownames(dif_mid)<-NULL
dif_mid<-cbind(dif_mid[,1:2],rep(9999, nrow(dif_mid)),dif_mid[,3:8])
colnames(dif_mid)<-c("Location","TR", "N", "Dif","lower.ci","upper.ci", "SD","Robust SE")
levels(dif_mid$TR)
levels(dif_mid$TR)<-c("W","WSH","H")
dif_mid[,3:ncol(dif_mid)]<-round(dif_mid[,3:ncol(dif_mid)],2)

dif_mid$TR.N<-paste0(dif_mid$TR, "\n(N=",dif_mid$N,")")
dif_mid$round<-"Year 1"
#dif_mid$dif.perc<-paste0(dif_mid$difalence*100, "%")
dif_mid$N<-paste0("(N=",dif_mid$N,")")

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

#Construct dataframe of difalence results
dif_end<-(rbind(
ec_tw_dif_end_a,
ec_sw_dif_end_a,
ec_h_dif_end_a,
ec_t_dif_end_a,
ec_f_dif_end_a))
rownames(dif_end)<-NULL
dif_end<-cbind(dif_end[,1:2],rep(9999, nrow(dif_end)),dif_end[,3:8])
colnames(dif_end)<-c("Location","TR", "N", "Dif","lower.ci","upper.ci", "SD","Robust SE")

levels(dif_end$TR)
levels(dif_end$TR)<-c("W","WSH","H")
dif_end[,3:ncol(dif_end)]<-round(dif_end[,3:ncol(dif_end)],2)

dif_end$TR.N<-paste0(dif_end$TR, "\n(N=",dif_end$N,")")
dif_end$round<-"Year 2"
#dif_end$dif.perc<-paste0(dif_end$difalence*100, "%")
dif_end$N<-paste0("(N=",dif_end$N,")")

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




ci.width<-0.5 #Width of confidence interval bars
ylim<-c(0.25,0.8) #Size of Y axis
vjust<- -1 #Vertical justification of printed prevalence
hjust<- -0.2 #Horizontal justification of printed prevalence

#-------------------------------------------
# Function for individual plots
#-------------------------------------------
plotfacet<-function(d, 
                    vars,
                    ci.width=0.5,
                    ylim=c(0.25,0.8),
                    vjust= -1,
                    hjust= -0.2){

  d<-subset(d, round==vars[1] & Location==vars[2])  
  
p<- ggplot(data = d) + 
  geom_point(mapping = aes(x=TR.N, y=Prevalence, color=TR),pch=21,cex=2,lwd=2)+  #, size=3) +
  geom_errorbar(mapping = aes(x=TR.N, y=Prevalence, ymin=lower.ci, ymax=upper.ci, color=TR), size=1, width=ci.width) +
  geom_text(aes(x=TR.N, y=Prevalence, label=prev.perc, color=TR),hjust=hjust, vjust=vjust, size=3,face="bold") +
  #geom_text(aes(x=TR.N, y=-Inf, label=N, color=TR),hjust=0, vjust=0, size=3,face="bold") +
  scale_y_continuous(limits = ylim, labels=percent)+ 
  #theme_bw() +
  theme_light() +
  #theme_minimal() +
  #theme_classic() +
  #theme_tufte() +
  ggtitle(ifelse(vars[1]=="World Bank",vars[2],"")) +
  labs(x="Subgroup", y=ifelse(vars[2]=="Tubewell",vars[1],""), color="Treatment")+
  scale_colour_manual(values = cols) +
  theme(#axis.text.x = element_text(color="#666666", face="bold"),   #angle = 45, hjust = 1),
        #axis.text.x=element_blank(),
        axis.text.y = element_text(color="#666666", face="bold"),
        axis.title.y = element_text(color="#666666", face="bold"),
        axis.title.x=element_blank(),
        strip.text.x = element_text(color="#666666", face="bold"),
        strip.text.y = element_text(color="#666666", face="bold"),
        strip.background = element_rect( fill=NA),
        #panel.grid.major=element_blank(),
        axis.line=element_line(),
        plot.title = element_text(hjust = 0.5),
        legend.position="none") 
  return(p)
}


#-------------------------------------------
# Combined prevalence plot
#-------------------------------------------

vars<- list(c("World Bank", "Tubewell"),
            c("World Bank", "Stored water"),
            c("World Bank", "Hands"),
            c("World Bank", "Food"),
            c("World Bank", "Ponds"),
            c("World Bank", "Soil"),
            c("World Bank", "Flies"),
            c("Year 1", "Tubewell"),
            c("Year 1", "Stored water"),
            c("Year 1", "Hands"),
            c("Year 1", "Toys"),
            c("Year 2", "Tubewell"),
            c("Year 2", "Stored water"),
            c("Year 2", "Hands"),
            c("Year 2", "Food"),
            c("Year 2", "Toys"))

prev.plots <- lapply(vars, plotfacet, 
                  d = prev.dat, 
                  ci.width=ci.width,
                  ylim=ylim,
                  vjust=vjust,
                  hjust=hjust)
#Make empty plot with just title for the toys panel
empty<-ggplot()+ theme_tufte() + ggtitle("Toys") + theme(plot.title = element_text(hjust = 0.5))
empty


#Print plots together 

lay <- rbind(c(1,2,3,4,5,6,7,8),
             c(9,10,11,12,13,14,15,16),
             c(17,18,19,20,21,22,23,24))
t <- textGrob("")
prev.plot<-grid.arrange(prev.plots[[1]], prev.plots[[2]], prev.plots[[3]], prev.plots[[4]], empty, prev.plots[[5]], prev.plots[[6]], prev.plots[[7]], 
             prev.plots[[8]], prev.plots[[9]], prev.plots[[10]], t, prev.plots[[11]], t, t, t,
             prev.plots[[12]], prev.plots[[13]], prev.plots[[14]], prev.plots[[15]], prev.plots[[16]], t, t, t,
             layout_matrix  = lay)

prev.plot[[9]]

setwd("C:/Users/andre/Dropbox/WASHB EML/Results/Figures")
pdf("Env Prevalence Plot.pdf",width=40,height=10)

grid.newpage()
grid.draw(prev.plot)

dev.off()



