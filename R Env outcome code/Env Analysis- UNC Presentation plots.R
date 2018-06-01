 
# Env. analysis presentation figures




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


fly_prev_a
fly_mn_a
fly_rr_h1_unadj_a
fly_rd_h1_unadj_a
fly_dif_h1_unadj_a

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
fly_prev_a<-cbind("FlyCount_kit", rownames(fly_prev_a), as.data.frame( fly_prev_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 

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
prev_wb[,3:ncol(prev_wb)]<-round(prev_wb[,3:ncol(prev_wb)],2)

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

prev_mid[,3:ncol(prev_mid)]<-round(prev_mid[,3:ncol(prev_mid)],2)

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

prev_end[,3:ncol(prev_end)]<-round(prev_end[,3:ncol(prev_end)],2)

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
#corange <- "#EEA722"
corange <- "#ce5600"
cyellow <- "#FFEE33"
cgrey <- "#777777"
cols=c(C=cblack,W=cblue,S=cteal,H=cgreen,WSH=corange,N=cred,"WSH+N"=cmagent)

cols=c(C=cblack,W=cblue,S=cteal,H=cgreen,WSH=corange)


arms=c("C", "W", "S", "H", "WSH")

# general label plot
ulabplot <- function(title) {
  plot(0.1,1,type="n",
       xaxt="n",xlab="",xlim=c(0,0.1),
       yaxt="n",ylab="",bty="n",ylim=c(0,1)
  )
  text(0.05,0.05,title,adj=0,cex=1.5, srt = 90)
}


#Order prevalence data
unique(prev.dat$Location)
unique(prev.dat$TR)
unique(prev.dat$round)
prev.dat$Location<-factor(prev.dat$Location)
table(prev.dat$Location)
prev.dat$Location<-factor(prev.dat$Location, c("Tubewell", "Stored water", "Hands", "Toys", "Food", "Ponds", "Soil", "Flies","FlyCount_kit", "FlyCount_lat"))
table(prev.dat$Location)




#-------------------------------------------
# Prevalence plot function
#-------------------------------------------

prevplot<-function(d, i, j){
  
  d <- d[!is.na(d[,1]),]

          if(j==1){
  xlabel_indices <- c(1,9,11,19,21,29)
  xaxis_indices <- c(2,10,12,20,22,30)
  xaxis_indices_primary <- c(1,11,21)
          }
          if(j==2){
  xlabel_indices <- c(1,6,9,14,17,22)
  xaxis_indices <- c(2,7,10,15,18,23)
  xaxis_indices_primary <- c(1,9,17)
          }
    	    if(j==3){
  xlabel_indices <- c(1,7,10,16,19,25)
  xaxis_indices <- c(2,8,11,17,20,26)
  xaxis_indices_primary <- c(1,10,19)  
    	    } 

  
  ytics <- seq(0,100,by=10)  #<----------Set the Y-axis range here


if(i %in% xlabel_indices){
   op <- par(mar=c(0,0,0,0)+0)
  
  if(i %in% xaxis_indices_primary){
   ulabplot("E.coli prevalence (%)")
  }else{
   ulabplot("Fly prevalence (%)")
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
	#pos<-MidPts[which.arms]
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

  	    if(j==1){
           mtext(ifelse(i<11,
                      c("Tubewell", "Stored water", "Hands", "Food", "Ponds", "Soil", "Flies","","Kitchen\nfly count")[i-1],
                        ""),
                        side=3,line=1,col="gray20",cex=0.95)
  	    }   
  	  	if(j==2){
           mtext(ifelse(i<9,
                      c("Tubewell", "Stored water", "Hands","Toys","","Kitchen\nfly count", "Latrine\nfly count")[i-1],
                        ""),
                        side=3,line=1,col="gray20",cex=0.95)
  	  	}
  	  	if(j==3){  	     
           mtext(ifelse(i<10,
                      c("Tubewell", "Stored water", "Hands","Toys","Food","","Kitchen\nfly count", "Latrine\nfly count")[i-1],
                        ""),
                        side=3,line=1,col="gray20",cex=0.95)
  	  	}
  
}

}





  
setwd("C:/Users/andre/Dropbox/WASHB EML/Results/Figures")
pdf("EnvAnalysisUNC_Presentation_plots.pdf",width=10,height=8.5, paper="USr")


m <- rbind(c(1,2,3,4,5,6,7,8,9,10),
           c(11,12,13,14,15,16,17,18,19,20),
           c(21,22,23,24,25,26,27,28,29,30))

#widths
w <- c(0.5, 1, 1, 1, 1, 1, 1, 1, 0.5, 1)


# nf <- layout(m, w)
# layout.show(nf) 


par(xpd=NA)

layout(m, w)


    i<-c(1,1,1)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 1)
    i<-c(1,1,2)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 1)
    i<-c(2,1,3)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 1)
    i<-c(3,1,4)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 1)
    i<-c(5,1,5)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 1)
    i<-c(6,1,6)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 1)
    i<-c(7,1,7)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 1)
    i<-c(8,1,8)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 1)
    i<-c(1,1,9)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 1)
    i<-c(9,1,10)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 1)
 
    

m <- rbind(c(1,2,3,4,5,6,7,8), c(9,10,11,12,13,14,15,16), c(17,18,19,20,21,22,23,24))

#widths
w <- c(0.5, 1, 1, 1, 1, 0.5, 1, 1)    
layout(m, w)

    i<-c(1,2,1)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 2)
    i<-c(1,2,2)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 2)
    i<-c(2,2,3)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 2)
    i<-c(3,2,4)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 2)
    i<-c(4,2,5)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 2)
    i<-c(1,2,6)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 2)
    i<-c(9,2,7)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 2)
    i<-c(10,2,8)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 2)

      
    
m <- rbind(c(1,2,3,4,5,6,7,8,9), c(10,11,12,13,14,15,16,17,18), c(19,20,21,22,23,24,25,26,27))

#widths
w <- c(0.5, 1, 1, 1, 1, 1, 0.5, 1, 1)    
layout(m, w)

    i<-c(1,3,1)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 3)
    i<-c(1,3,2)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 3)
    i<-c(2,3,3)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 3)
    i<-c(3,3,4)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 3)
    i<-c(4,3,5)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 3)
    i<-c(5,3,6)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 3)
    i<-c(1,2,7)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 3)
    i<-c(9,2,8)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 3)
    i<-c(10,2,9)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3], 3)
        
    

dev.off()


