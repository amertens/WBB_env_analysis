
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

#Useful links
#Facet options:
     #http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
#Coloring X-axis text
     #https://stackoverflow.com/questions/22972478/color-axis-text-by-variable-in-ggplot


#---------------------------------------
# Load data- Ayse's objects
#---------------------------------------

dir<-setwd("C:/Users/andre/Dropbox/WASHB EML/Results")
# load everything in each directory
# dir=substr(dir, 1, nchar(dir)-17)

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

#-----------------------------------
# Midline prevalence processing
#-----------------------------------

ec_tw_prev_mid_a<-cbind("Tubewell", rownames(ec_tw_prev_mid_a), as.data.frame(ec_tw_prev_mid_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_sw_prev_mid_a<-cbind("Stored water", rownames(ec_sw_prev_mid_a), as.data.frame( ec_sw_prev_mid_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_h_prev_mid_a<-cbind("Hands", rownames(ec_h_prev_mid_a), as.data.frame( ec_h_prev_mid_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_t_prev_mid_a<-cbind("Toys", rownames(ec_t_prev_mid_a), as.data.frame( ec_t_prev_mid_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
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
levels(prev_mid$TR)<-c("C","N","WSH+N","W","WSH","H")
prev_mid[,3:ncol(prev_mid)]<-round(prev_mid[,3:ncol(prev_mid)],2)

prev_mid$TR.N<-paste0(prev_mid$TR, "\n(N=",prev_mid$N,")")
prev_mid$round<-"Year 1"
prev_mid$prev.perc<-paste0(prev_mid$Prevalence*100, "%")




#-----------------------------------
# Endline prevalence processing
#-----------------------------------

ec_tw_prev_end_a<-cbind("Tubewell", rownames(ec_tw_prev_end_a), as.data.frame(ec_tw_prev_end_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_sw_prev_end_a<-cbind("Stored water", rownames(ec_sw_prev_end_a), as.data.frame( ec_sw_prev_end_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_h_prev_end_a<-cbind("Hands", rownames(ec_h_prev_end_a), as.data.frame( ec_h_prev_end_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_t_prev_end_a<-cbind("Toys", rownames(ec_t_prev_end_a), as.data.frame( ec_t_prev_end_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ec_f_prev_end_a<-cbind("Food", rownames(ec_f_prev_end_a), as.data.frame( ec_f_prev_end_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 



#NOTE: Add in fly and mom/child dirty hands indicators into prevalence figures?

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
levels(prev_end$TR)<-c("C","N","WSH+N","W","WSH","H")
prev_end[,3:ncol(prev_end)]<-round(prev_end[,3:ncol(prev_end)],2)

prev_end$TR.N<-paste0(prev_end$TR, "\n(N=",prev_end$N,")")
prev_end$round<-"Year 2"
prev_end$prev.perc<-paste0(prev_end$Prevalence*100, "%")


prev.dat<-rbind(prev_wb,prev_mid, prev_end)


#-----------------------------------
# Prevalence Plot
#-----------------------------------

ci.width<-0.5
ylim<-c(0.3,0.7)
vjust<- -1
hjust<- -0.2

full.plot<- ggplot(data = prev.dat) + 
  geom_point(mapping = aes(x=TR, y=Prevalence, color=TR), size=3) +
  geom_errorbar(mapping = aes(x=TR, y=Prevalence, ymin=lower.ci, ymax=upper.ci, color=TR), size=1, width=ci.width) +
  geom_text(aes(x=TR, y=Prevalence, label=prev.perc, color=TR),hjust=hjust, vjust=vjust, size=3,  colour="#666666",face="bold") +
  #facet_grid(round ~ Location, scale="free") +
  facet_grid(round ~ Location) +
  scale_y_continuous(limits = ylim, labels=percent)+ 
  #theme_bw() +
  #theme_light() +
  #theme_minimal() +
  #theme_classic() +
  theme_tufte() +
  labs(x="Subgroup", y="E. coli prevalence", color="Treatment")+
  theme(axis.text.x = element_text(color="#666666", face="bold"),   #angle = 45, hjust = 1),
        axis.text.y = element_text(color="#666666", face="bold"),
        axis.title.y = element_text(color="#666666", face="bold"),
        axis.title.x=element_blank(),
        strip.text.x = element_text(color="#666666", face="bold"),
        legend.position="none") 
full.plot


wb.plot<-  
  ggplot(data = prev_wb) + 
  geom_point(mapping = aes(x=TR.N, y=Prevalence, color=TR), size=3) +
  geom_errorbar(mapping = aes(x=TR.N, y=Prevalence, ymin=lower.ci, ymax=upper.ci, color=TR), size=1, width=ci.width) +
  geom_text(aes(x=TR.N, y=Prevalence, label=prev.perc, color=TR),hjust=hjust, vjust=vjust, size=3,face="bold") +
  facet_grid( ~ Location, scale="free") +
  scale_y_continuous(limits = ylim, labels=percent)+ 
  #theme_bw() +
  theme_light() +
  #theme_minimal() +
  #theme_classic() +
  #theme_tufte() +
  labs(x="Subgroup", y="E. coli prevalence", color="Treatment")+
  theme(axis.text.x = element_text(color="#666666", face="bold"),   #angle = 45, hjust = 1),
        axis.text.y = element_text(color="#666666", face="bold"),
        axis.title.y = element_text(color="#666666", face="bold"),
        axis.title.x=element_blank(),
        strip.text.x = element_text(color="#666666", face="bold"),
        strip.background = element_rect( fill="white"),
        legend.position="none") 
wb.plot


mid.plot<-   ggplot(data = prev_mid) + 
  geom_point(mapping = aes(x=TR.N, y=Prevalence, color=TR), size=3) +
  geom_errorbar(mapping = aes(x=TR.N, y=Prevalence, ymin=lower.ci, ymax=upper.ci, color=TR), size=1, width=ci.width) +
  geom_text(aes(x=TR.N, y=Prevalence, label=Prevalence, color=TR),hjust=hjust, vjust=vjust, size=3,  colour="#666666",face="bold") +
  #facet_wrap(~ Location, nrow = 1) +
  facet_grid(~ Location, scale="free") +
  scale_y_continuous(limits = ylim)+ 
  theme_light() +
  labs(x="Subgroup", y="E. coli prevalence", color="Treatment")+
  theme(axis.text.x = element_text(color="#666666", face="bold"),   #angle = 45, hjust = 1),
        axis.text.y = element_text(color="#666666", face="bold"),
        axis.title.y = element_text(color="#666666", face="bold"),
        axis.title.x=element_blank(),
        strip.text.x = element_text(color="#666666", face="bold"),
        legend.position="none") 
  
end.plot<-   ggplot(data = prev_end) + 
  geom_point(mapping = aes(x=TR.N, y=Prevalence, color=TR), size=3) +
  geom_errorbar(mapping = aes(x=TR.N, y=Prevalence, ymin=lower.ci, ymax=upper.ci, color=TR), size=1, width=ci.width) +
  geom_text(aes(x=TR.N, y=Prevalence, label=Prevalence, color=TR),hjust=hjust, vjust=vjust, size=3,  colour="#666666",face="bold") +
  #facet_wrap(~ Location, nrow = 1) +
  facet_grid(~ Location, scale="free") +
  scale_y_continuous(limits = ylim)+ 
  theme_light() +
  labs(x="Subgroup", y="E. coli prevalence", color="Treatment")+
  theme(axis.text.x = element_text(color="#666666", face="bold"),   #angle = 45, hjust = 1),
        axis.text.y = element_text(color="#666666", face="bold"),
        axis.title.y = element_text(color="#666666", face="bold"),
        axis.title.x=element_blank(),
        strip.text.x = element_text(color="#666666", face="bold"),
        legend.position="none") 
  
#Print plots together 
vplayout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
grid.newpage()
pushViewport(viewport(layout=grid.layout(6,6)))
print(wb.plot,vp=vplayout(1:2,1:6))
print(mid.plot,vp=vplayout(3:4,1:4))
print(end.plot,vp=vplayout(5:6,1:5))  
  
  #scale_y_discrete(labels = function(x) round(as.numeric(x), digits=2)) +
  #theme(axis.line=element_blank(),axis.text.x=element_blank(),
  #        axis.ticks=element_blank(),
  #        axis.title.x=element_blank()) +

#p + theme(axis.line=element_blank(),axis.text.x=element_blank(),
#          axis.text.y=element_blank(),axis.ticks=element_blank(),
#          axis.title.x=element_blank(),
#          axis.title.y=element_blank(),legend.position="none",
#          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#          panel.grid.minor=element_blank(),plot.background=element_blank())


ggplot(data = prev_wb) + 
  geom_point(mapping = aes(x=subgroup, y=psi, color=TR), size=3) +
  geom_errorbar(mapping = aes(x=subgroup, y=psi, ymin=CI1, ymax=CI2, color=variable), size=1) +
  geom_hline(yintercept=0) +
  ggtitle("WASH Benefits-Bangladesh subgroup analysis") +
  labs(x="Subgroup",y="ATE of nutritional intervention on LAZ", color="Variable") + 
  theme(plot.title=element_text(""), 
        axis.text.x = element_text(color="#666666", face="bold",angle = 45, hjust = 1)) +
  theme(plot.title = element_text(color="#666666", face="bold", size=22, hjust=0)) +
  theme(axis.title = element_text(color="#666666", face="bold", size=12)) +
  theme_bw() +
  coord_flip()



#grid.arrange(ap1,ap2,ap3,ap4,ap5,ap6,ap7,ap8,ap9,ap10,ap11,ap12, nrow=3)


#Raw prevalences -show Control and Nutrition seperately


#Prevalence ratios- Control+Nutrition will be the reference. 


p <- ggplot(data = mtcars, aes(x = hp)) + 
  geom_dotplot(binwidth = 1) + 
  geom_density() + 
  facet_grid(. ~ cyl)

mylabels <- data.frame(cyl = c(4, 6, 8), 
                       label = c("first label", "seond label different", "and another"))

p + geom_text(x = 200, y = 0.75, aes(label = label), data = mylabels)

### compare that to this way with annotate

p + annotate("text", x = 200, y = 0.75, label = "same label everywhere")











# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




test <- structure(list(characteristic = structure(c(1L, 2L, 3L, 1L, 2L
), .Label = c("Factor1", "Factor2", "Factor3"), class = "factor"), 
    es = c(1.2, 1.4, 1.6, 1.3, 1.5), ci_low = c(1.1, 1.3, 1.5, 
    1.2, 1.4), ci_upp = c(1.3, 1.5, 1.7, 1.4, 1.6), label = structure(c(1L, 
    3L, 5L, 2L, 4L), .Label = c("1.2 (1.1, 1.3)", "1.3 (1.2, 1.4)", 
    "1.4 (1.3, 1.5)", "1.5 (1.4, 1.6)", "1.6 (1.5, 1.7)"), class = "factor"), 
    set = structure(c(1L, 1L, 1L, 2L, 2L), .Label = c("H", "S"
    ), class = "factor")), .Names = c("characteristic", "es", 
"ci_low", "ci_upp", "label", "set"), class = "data.frame", row.names = c(NA, 
-5L))




p <- ggplot(test, aes(y = characteristic, x = es, xmin = ci_low, xmax = ci_upp)) + 
  geom_point() +   
  geom_errorbarh(height = 0) +
  geom_text(aes(label = label, x = 2, y = characteristic)) + 
  scale_x_continuous(limits = c(1, 2.2), breaks = c(1, 1.2, 1.4, 1.6, 1.8),
    labels=c("1.0", "1.2", "1.4", "1.6", "1.8")) +
  facet_grid(set ~ ., scales = "free", space = "free") +
  theme_bw() + 
  theme(strip.text.y = element_text(angle = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
p

grid.text(expression(paste("ES " %+-% " ci")), x = 0.78,   y = .92,
   gp = gpar(fontsize = 18))



