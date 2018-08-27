

rm(list=ls())
library(dplyr)
library(ggplot2)
library(ggthemes) 
library(grid)
library(gridExtra)
library(scales)
library(lattice)

#---------------------------------------
# Load data- Ayse's objects
#---------------------------------------


setwd("C:/Users/andre/Dropbox/WASHB EML/Results")


dir<-setwd("C:/Users/andre/Dropbox/WASHB EML/Results")

dir<-setwd("C:/Users/andre/Dropbox/WASHB EML/Results")
ays.res <- list.files(path=paste(dir,"/Ayse/", sep=""))
for(i in 1:length(ays.res)) {
  load(paste(dir,"/Ayse/",ays.res[i],sep=""))
}

ls()
# mh_nail_rr_h1_unadj_a, mh_fing_rr_h1_unadj_a, mh_palm_rr_h1_unadj_a, ch_nail_rr_h1_unadj_a, ch_fing_rr_h1_unadj_a, ch_palm_rr_h1_unadj_a
# mh_nail_rr_h1_unadj_mid_a, mh_fing_rr_h1_unadj_mid_a, mh_palm_rr_h1_unadj_mid_a, ch_nail_rr_h1_unadj_mid_a, ch_fing_rr_h1_unadj_mid_a, ch_palm_rr_h1_unadj_mid_a 
# mh_nail_rr_h1_unadj_end_a, mh_fing_rr_h1_unadj_end_a, mh_palm_rr_h1_unadj_end_a, ch_nail_rr_h1_unadj_end_a, ch_fing_rr_h1_unadj_end_a, ch_palm_rr_h1_unadj_end_a






#-----------------------------------
# World Bank prevalence processing
#-----------------------------------

mh_nail_prev<-cbind("Caregiver's nails", rownames(mh_nail_prev_a), as.data.frame(mh_nail_prev_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
mh_fing_prev<-cbind("Caregiver's fingers", rownames(mh_fing_prev_a), as.data.frame( mh_fing_prev_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
mh_palm_prev<-cbind("Caregiver's palms", rownames(mh_palm_prev_a), as.data.frame( mh_palm_prev_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ch_nail_prev<-cbind("Child's nails", rownames(ch_nail_prev_a), as.data.frame((ch_nail_prev_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ch_fing_prev<-cbind("Child's fingers", rownames(ch_fing_prev_a), as.data.frame( (ch_fing_prev_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ch_palm_prev<-cbind("Child's palms", rownames(ch_palm_prev_a), as.data.frame((ch_palm_prev_a))) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 



#Construct dataframe of prevalence results
prev<-(rbind(
mh_nail_prev,
mh_fing_prev,
mh_palm_prev,
ch_nail_prev,
ch_fing_prev,
ch_palm_prev))

rownames(prev)<-NULL
colnames(prev)<-c("Location","TR", "N", "Prevalence", "SD","Robust SE","lower.ci","upper.ci")

levels(prev$TR)
levels(prev$TR) <- c("C", "S", "WSH")

prev$round<-"Year 2"
prev$prev.perc<-paste0(prev$Prevalence*100, "%")

prev$TR<-factor(prev$TR)
prev$TR = factor(prev$TR, c("C", "S", "WSH"))

prev$Prevalence<-prev$Prevalence*100
prev$lower.ci<-prev$lower.ci*100
prev$upper.ci<-prev$upper.ci*100


#-----------------------------------
# World Bank prevalence ratio processing
#-----------------------------------

mh_nail_rr_h1_unadj<-cbind("Caregiver's nails", rownames(mh_nail_rr_h1_unadj_a), as.data.frame(mh_nail_rr_h1_unadj_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
mh_fing_rr_h1_unadj<-cbind("Caregiver's fingers", rownames(mh_fing_rr_h1_unadj_a), as.data.frame( mh_fing_rr_h1_unadj_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
mh_palm_rr_h1_unadj<-cbind("Caregiver's palms", rownames(mh_palm_rr_h1_unadj_a), as.data.frame( mh_palm_rr_h1_unadj_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ch_nail_rr_h1_unadj<-cbind("Child's nails", rownames(ch_nail_rr_h1_unadj_a), as.data.frame((ch_nail_rr_h1_unadj_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ch_fing_rr_h1_unadj<-cbind("Child's fingers", rownames(ch_fing_rr_h1_unadj_a), as.data.frame( (ch_fing_rr_h1_unadj_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ch_palm_rr_h1_unadj<-cbind("Child's palms", rownames(ch_palm_rr_h1_unadj_a), as.data.frame((ch_palm_rr_h1_unadj_a))) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 

#Construct dataframe of mnalence results
pr<-(rbind(
mh_nail_rr_h1_unadj,
mh_fing_rr_h1_unadj,
mh_palm_rr_h1_unadj,
ch_nail_rr_h1_unadj,
ch_fing_rr_h1_unadj,
ch_palm_rr_h1_unadj))

rownames(pr)<-NULL
pr<-cbind(pr[,1:2],prev[prev$TR!="C","N"],pr[,3:9])
colnames(pr)<-c("Location","TR", "N", "PR","lower.ci","upper.ci","Estimate", "SD","Robust SE","P-value")
levels(pr$TR)
levels(pr$TR)<- c("S", "WSH")

pr$round<-"Year 2"

pr$TR<-factor(pr$TR)
pr$TR = factor(pr$TR, c("C","S", "WSH"))





#-----------------------------------
# Midline prevalence processing
#-----------------------------------

mh_nail_prev_mid<-cbind("Caregiver's nails", rownames(mh_nail_prev_mid2_a), as.data.frame(mh_nail_prev_mid2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
mh_fing_prev_mid<-cbind("Caregiver's fingers", rownames(mh_fing_prev_mid2_a), as.data.frame( mh_fing_prev_mid2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
mh_palm_prev_mid<-cbind("Caregiver's palms", rownames(mh_palm_prev_mid2_a), as.data.frame( mh_palm_prev_mid2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ch_nail_prev_mid<-cbind("Child's nails", rownames(ch_nail_prev_mid2_a), as.data.frame((ch_nail_prev_mid2_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ch_fing_prev_mid<-cbind("Child's fingers", rownames(ch_fing_prev_mid2_a), as.data.frame( (ch_fing_prev_mid2_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ch_palm_prev_mid<-cbind("Child's palms", rownames(ch_palm_prev_mid2_a), as.data.frame((ch_palm_prev_mid2_a))) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 



#Construct dataframe of prevalence results
prev_mid<-(rbind(
mh_nail_prev_mid,
mh_fing_prev_mid,
mh_palm_prev_mid,
ch_nail_prev_mid,
ch_fing_prev_mid,
ch_palm_prev_mid))

rownames(prev_mid)<-NULL
colnames(prev_mid)<-c("Location","TR", "N", "Prevalence", "SD","Robust SE","lower.ci","upper.ci")

levels(prev_mid$TR)
levels(prev_mid$TR) <- c("C","H", "WSH")

prev_mid$round<-"Year 2"
prev_mid$prev.perc<-paste0(prev_mid$Prevalence*100, "%")

prev_mid$TR<-factor(prev_mid$TR)
prev_mid$TR = factor(prev_mid$TR, c("C","H", "WSH"))

prev_mid$Prevalence<-prev_mid$Prevalence*100
prev_mid$lower.ci<-prev_mid$lower.ci*100
prev_mid$upper.ci<-prev_mid$upper.ci*100


#-----------------------------------
# Midline prevalence ratio processing
#-----------------------------------

mh_nail_rr_h1_unadj_mid<-cbind("Caregiver's nails", rownames(mh_nail_rr_h1_unadj_mid_a), as.data.frame(mh_nail_rr_h1_unadj_mid_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
mh_fing_rr_h1_unadj_mid<-cbind("Caregiver's fingers", rownames(mh_fing_rr_h1_unadj_mid_a), as.data.frame( mh_fing_rr_h1_unadj_mid_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
mh_palm_rr_h1_unadj_mid<-cbind("Caregiver's palms", rownames(mh_palm_rr_h1_unadj_mid_a), as.data.frame( mh_palm_rr_h1_unadj_mid_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ch_nail_rr_h1_unadj_mid<-cbind("Child's nails", rownames(ch_nail_rr_h1_unadj_mid_a), as.data.frame((ch_nail_rr_h1_unadj_mid_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ch_fing_rr_h1_unadj_mid<-cbind("Child's fingers", rownames(ch_fing_rr_h1_unadj_mid_a), as.data.frame( (ch_fing_rr_h1_unadj_mid_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ch_palm_rr_h1_unadj_mid<-cbind("Child's palms", rownames(ch_palm_rr_h1_unadj_mid_a), as.data.frame((ch_palm_rr_h1_unadj_mid_a))) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 

#Construct dataframe of mnalence results
pr_mid<-(rbind(
mh_nail_rr_h1_unadj_mid,
mh_fing_rr_h1_unadj_mid,
mh_palm_rr_h1_unadj_mid,
ch_nail_rr_h1_unadj_mid,
ch_fing_rr_h1_unadj_mid,
ch_palm_rr_h1_unadj_mid))

rownames(pr_mid)<-NULL
pr_mid<-cbind(pr_mid[,1:2],prev_mid[prev_mid$TR!="C","N"],pr_mid[,3:9])
colnames(pr_mid)<-c("Location","TR", "N", "PR","lower.ci","upper.ci","Estimate", "SD","Robust SE","P-value")
levels(pr_mid$TR)
levels(pr_mid$TR)<- c("H", "WSH")

pr_mid$round<-"Year 2"

pr_mid$TR<-factor(pr_mid$TR)
pr_mid$TR = factor(pr_mid$TR, c("C","H", "WSH"))





#-----------------------------------
# Endline prevalence processing
#-----------------------------------

mh_nail_prev_end<-cbind("Caregiver's nails", rownames(mh_nail_prev_end2_a), as.data.frame(mh_nail_prev_end2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
mh_fing_prev_end<-cbind("Caregiver's fingers", rownames(mh_fing_prev_end2_a), as.data.frame( mh_fing_prev_end2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
mh_palm_prev_end<-cbind("Caregiver's palms", rownames(mh_palm_prev_end2_a), as.data.frame( mh_palm_prev_end2_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ch_nail_prev_end<-cbind("Child's nails", rownames(ch_nail_prev_end2_a), as.data.frame((ch_nail_prev_end2_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ch_fing_prev_end<-cbind("Child's fingers", rownames(ch_fing_prev_end2_a), as.data.frame( (ch_fing_prev_end2_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ch_palm_prev_end<-cbind("Child's palms", rownames(ch_palm_prev_end2_a), as.data.frame((ch_palm_prev_end2_a))) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 



#Construct dataframe of prevalence results
prev_end<-(rbind(
mh_nail_prev_end,
mh_fing_prev_end,
mh_palm_prev_end,
ch_nail_prev_end,
ch_fing_prev_end,
ch_palm_prev_end))

rownames(prev_end)<-NULL
colnames(prev_end)<-c("Location","TR", "N", "Prevalence", "SD","Robust SE","lower.ci","upper.ci")

levels(prev_end$TR)
levels(prev_end$TR) <- c("C","H", "WSH")

prev_end$round<-"Year 2"
prev_end$prev.perc<-paste0(prev_end$Prevalence*100, "%")

prev_end$TR<-factor(prev_end$TR)
prev_end$TR = factor(prev_end$TR, c("C","H", "WSH"))

prev_end$Prevalence<-prev_end$Prevalence*100
prev_end$lower.ci<-prev_end$lower.ci*100
prev_end$upper.ci<-prev_end$upper.ci*100


#-----------------------------------
# Endline prevalence ratio processing
#-----------------------------------

mh_nail_rr_h1_unadj_end<-cbind("Caregiver's nails", rownames(mh_nail_rr_h1_unadj_end_a), as.data.frame(mh_nail_rr_h1_unadj_end_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
mh_fing_rr_h1_unadj_end<-cbind("Caregiver's fingers", rownames(mh_fing_rr_h1_unadj_end_a), as.data.frame( mh_fing_rr_h1_unadj_end_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
mh_palm_rr_h1_unadj_end<-cbind("Caregiver's palms", rownames(mh_palm_rr_h1_unadj_end_a), as.data.frame( mh_palm_rr_h1_unadj_end_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ch_nail_rr_h1_unadj_end<-cbind("Child's nails", rownames(ch_nail_rr_h1_unadj_end_a), as.data.frame((ch_nail_rr_h1_unadj_end_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ch_fing_rr_h1_unadj_end<-cbind("Child's fingers", rownames(ch_fing_rr_h1_unadj_end_a), as.data.frame( (ch_fing_rr_h1_unadj_end_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ch_palm_rr_h1_unadj_end<-cbind("Child's palms", rownames(ch_palm_rr_h1_unadj_end_a), as.data.frame((ch_palm_rr_h1_unadj_end_a))) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 

#Construct dataframe of mnalence results
pr_end<-(rbind(
mh_nail_rr_h1_unadj_end,
mh_fing_rr_h1_unadj_end,
mh_palm_rr_h1_unadj_end,
ch_nail_rr_h1_unadj_end,
ch_fing_rr_h1_unadj_end,
ch_palm_rr_h1_unadj_end))

rownames(pr_end)<-NULL
pr_end<-cbind(pr_end[,1:2],prev_end[prev_end$TR!="C","N"],pr_end[,3:9])
colnames(pr_end)<-c("Location","TR", "N", "PR","lower.ci","upper.ci","Estimate", "SD","Robust SE","P-value")
levels(pr_end$TR)
levels(pr_end$TR)<- c("H", "WSH")

pr_end$round<-"Year 2"

pr_end$TR<-factor(pr_end$TR)
pr_end$TR = factor(pr_end$TR, c("C","H", "WSH"))




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



#Order prevalence data
unique(prev.dat$Location)
unique(prev.dat$TR)
unique(prev.dat$round)
prev.dat$Location<-factor(prev.dat$Location)
table(prev.dat$Location)
prev.dat$Location<-factor(prev.dat$Location, levels=unique(prev.dat$Location))
table(prev.dat$Location)

unique(pr.dat$Location)
unique(pr.dat$TR)
unique(pr.dat$round)
pr.dat$Location<-factor(pr.dat$Location)
table(pr.dat$Location)
pr.dat$Location<-factor(pr.dat$Location, levels=unique(pr.dat$Location))
table(pr.dat$Location)







#-------------------------------------------
# Prevalence plot function
#-------------------------------------------

# general label plot
ulabplot <- function(title) {
  plot(1,1,type="n",
       xaxt="n",xlab="",xlim=c(0,1),
       yaxt="n",ylab="",bty="n",ylim=c(0,1)
  )
  text(1,0.5,title,adj=1,cex=1.5)
}


prevplot<-function(d, i, axis=F, ytics = seq(0,100,by=10) ){
  
  if(i==1) axis=T
  
  op <- par(mar=c(1.5,1,6,0)+0.1)

   # set up an empty plot
MidPts <- barplot(1:5,names.arg=NA,border=NA,col=NA,
	ylim=c(range(ytics)[1],range(ytics)[2]+5),ylab="",yaxt="n",
	las=1,bty="n"
	)
  box()
	segments(x0=0,x1=6.2,y0=ytics[-1],lty=2,lwd=1,col="gray80")
	if(axis==T){
	    axis(2, at=ytics, las=1)
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
		if(i==1){
  mtext(text="",side=1,line=0.5,at=-1.35,col=cblack,cex=textsize,las=1)
  mtext(text=c("N",d$N),side=3,line=1.25,at=c(-1,pos),col=c(cblack,cols[which.arms]),cex=textsize,las=1)
  mtext(text=c("Prevalence (%)",sprintf("%1.0f",d$Prevalence)),side=3,line=0.25,at=c(-2.6,pos),col=c(cblack,cols[which.arms]),cex=textsize,las=1)
      }else{
		mtext(d$N, side=3,line=1.25,at=pos,col=cols[which.arms],cex=textsize,las=1)
    mtext(sprintf("%1.0f",d$Prevalence), side=3,line=0.25, at=pos,col=cols[which.arms],cex=textsize,las=1)
		}

           mtext(c("Caregiver's nails", "Caregiver's fingers", "Caregiver's palms", "Child's nails", "Child's fingers", "Child's palms")[i],
                        side=3,line=2.5,col="gray20",cex=0.95)
}



#-------------------------------------------
# Prevalence Ratio plot function
#-------------------------------------------


prplot<-function(d, i, exp_tics = seq(-2,1,by=.5), labs = c("0.25","","0.5","","1","","2")){
  

    ytics = round(2^exp_tics,2)

    
   op <- par(mar=c(1.5,1,4,0)+0.1)

   # set up an empty plot
MidPts <- barplot(1:5,names.arg=NA,border=NA,col=NA,
	ylim=c(range(ytics)[1],range(ytics)[2]+diff(range(ytics))/20),ylab="",yaxt="n",
	las=1,bty="n", log="y"
	)
  box()
	segments(x0=0,x1=6.2,y0=ytics[-1],lty=2,lwd=1,col="gray80")
	segments(x0=0,x1=6.2,y0=1,lty=1,lwd=1,col="black")

	if(i==1){
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
 		#if(i %in% xaxis_indices){
 		    mtext(text="",side=1,line=0.5,at=-1.35,col=cblack,cex=textsize,las=1)
    #}
}



  
setwd("C:/Users/andre/Dropbox/WASHB EML/Results/Figures")

pdf("EnvAnalysisWorldBankPlot_Hands.pdf",width=10,height=8.5, paper="USr")

  m <- rbind(c(1,2,3,4,5,6,7), c(8,9,10,11,12,13,14))
  par(xpd=NA)
  
  layout(m)

    #prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]],], i[2])
       op <- par(mar=c(1,0,6,1)+0.1)
       ulabplot("Visible\ndirt\nprevalence\n(%)")

    for(i in 1:6){
    prevplot(d=prev[prev$Location==levels(prev$Location)[i],], i, ytics = seq(0,60,by=10))
    }
     
       op <- par(mar=c(1,0,6,1)+0.1)
       ulabplot("Visible\ndirt\nprevalence\nratio")  
    
    for(i in 1:6){
    prplot(d=pr[pr$Location==levels(pr$Location)[i],], i, exp_tics = seq(-1.5,1,by=.5), labs = c("","0.5","","1","","2"))
    }
    
dev.off()





pdf("EnvAnalysisMidlinePlot_Hands.pdf",width=10,height=8.5, paper="USr")

  m <- rbind(c(1,2,3,4,5,6,7), c(8,9,10,11,12,13,14))
  par(xpd=NA)
  
  layout(m)

    #prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]],], i[2])
       op <- par(mar=c(1,0,6,1)+0.1)
       ulabplot("Visible\ndirt\nprevalence\n(%)")

    for(i in 1:6){
    prevplot(d=prev_mid[prev_mid$Location==levels(prev_mid$Location)[i],], i)
    }
     
       op <- par(mar=c(1,0,6,1)+0.1)
       ulabplot("Visible\ndirt\nprevalence\nratio")  
    
    for(i in 1:6){
    prplot(d=pr_mid[pr_mid$Location==levels(pr_mid$Location)[i],], i, exp_tics = seq(-3,1,by=.5), labs = c("0.125","","0.25","","0.5","","1","","2"))
    }
    
dev.off()





pdf("EnvAnalysisEndlinePlot_Hands.pdf",width=10,height=8.5, paper="USr")

  m <- rbind(c(1,2,3,4,5,6,7), c(8,9,10,11,12,13,14))
  par(xpd=NA)
  
  layout(m)

       op <- par(mar=c(1,0,6,1)+0.1)
       ulabplot("Visible\ndirt\nprevalence\n(%)")

    for(i in 1:6){
    prevplot(d=prev_end[prev_end$Location==levels(prev_end$Location)[i],], i)
    }
     
       op <- par(mar=c(1,0,6,1)+0.1)
       ulabplot("Visible\ndirt\nprevalence\nratio")  
    
    for(i in 1:6){
    prplot(d=pr_end[pr_end$Location==levels(pr_end$Location)[i],], i, exp_tics = seq(-3,1,by=.5), labs = c("0.125","","0.25","","0.5","","1","","2"))
    }
    
dev.off()




