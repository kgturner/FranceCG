#France data figures

#see FrSkdata_format.R for data formatting, and Fr_lmer.R for modeling descriptions

#read
FrdatSK<- read.table("FrTraitClimDat_SK.txt", header=T, sep="\t",quote='"', row.names=1)
Frdatsk.l<- read.table("FrTraitClimDat_SK_long.txt", header=T, sep="\t",quote='"', row.names=1)
frend<- read.table("FrEnd.txt", header=T, sep="\t",quote='"', row.names=1)

# library(lme4)
# library(lsmeans)
library(ggplot2)
library(plyr)

#for multiplot function, see bottom

#crown.log and RoseAh.log
grdat_cr <- FrdatSK[, c(1:7,53:55, 48,45)]
levels(grdat_cr$Origin)[levels(grdat_cr$Origin)=="inv"] <- "Invasive C. diffusa"
levels(grdat_cr$Origin)[levels(grdat_cr$Origin)=="nat"] <- "Native C. diffusa"
levels(grdat_cr$Origin)[levels(grdat_cr$Origin)=="sk"] <- "Native C. stoebe"
#change order? but then have to change colors...
# grdat$Origin <- factor(grdat$Origin, c("Native C. diffusa", "Invasive C. diffusa", "Native C. stoebe"))
colnames(grdat_cr)[4] <- "Treatment"

#crown.log box
pCrown <- ggplot(grdat_cr,aes(Origin, Crown.log, fill=Origin))+
  geom_boxplot()+
  xlab("Origin")+ylab("Root crown diameter at harvest [mm](log))")+ 
  theme_bw() #+
#   theme(legend.justification=c(1,0), legend.position=c(1,1),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))

# #legend position(left/right,top/bottom)
# p1 <- p1  + annotate('point',x = "Control", y = 5, pch=16, color="red",parse=T, size=4)+
#   annotate('point',x = "Nutr. Stress", y = 5, pch=16, color="red",parse=T, size=4)+
#   annotate('point',x = "Herbivory", y = 5, pch=8, color="red",parse=T, size=4)+
#   annotate(geom="text", x="Early Control", y=12.5, label="(a)",fontface="bold", size=5)+
#   theme(axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))
pCrown

#crown.log interaction
#for plots of pop means
grd_c <- ddply(grdat_cr, .(Pop, Origin,  PC1), summarize, popCount=length(Pop), popCrown=mean(Crown.log,na.rm = TRUE), popRose=mean(RoseAh.log, na.rm=TRUE))

pCrown.2 <- qplot(data=grd_c,  PC1,popCrown, color=Origin)+geom_smooth(method=glm, se=TRUE)
pCrown.2

#roseAh.log box
pRose <- ggplot(grdat_cr,aes(Treatment, RoseAh.log, fill=Origin))+
  geom_boxplot()+
  xlab("Treatment")+ylab("Rosette diameter at harvest [cm2](log)")+ 
  theme_bw() +
  theme(legend.justification=c(1,1), legend.position=c(1,1),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pRose

#roseAh.log interaction
#for plots of pop means
grd_r <- ddply(grdat_cr, .(Pop, Origin, Treatment, PC1), summarize, popCount=length(Pop), popCrown=mean(Crown.log,na.rm = TRUE), popRose=mean(RoseAh.log, na.rm=TRUE))

# pRose.2 <- qplot(data=grd_r, PC1, popRose, shape=Treatment, color=Origin, facets=grd_r$Trt)+geom_smooth(method=glm, se=TRUE)
# pRose.2

pRose.3 <- ggplot(grd_r,aes(PC1, popRose, color=Origin))+ 
  geom_point(aes(shape=Treatment))+

  geom_smooth(aes(linetype=Treatment),method=glm, se=TRUE)+
  
  xlab("PC1")+
  ylab("Rosette diameter at harvest [cm2](log)")+ 
  #title("Performance in drought vs. control treatments")+
#   theme_bw()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pRose.3
# p1 <- p1 +  
#   #   annotate('point',x = 1.94, y = 7, pch=8, color="red",parse=T,size=3) +
#   #   annotate('point',x = 2.06, y = 7, pch=8, color="red",parse=T,size=3) +
#   #   annotate(geom="text", x=2, y=7.3, label="Origin", size=5) +
#   annotate(geom="text", x=2, y=7.3, label="Origin*Control mass", size=5) +
#   annotate('point',x = 2, y = 7, pch=8, color="red",parse=T,size=3)+
#   annotate(geom="text", x=1.25, y=1, label="(a)",fontface="bold", size=5)+
#   theme(axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))
# p1





####Mass, bolt, harvest, wilt, yellow, death.date####
grdat_d <- frend[, c(1:7,8,10,12,15,21,24,31,33:35)]
levels(grdat_d$Origin)[levels(grdat_d$Origin)=="inv"] <- "Invasive C. diffusa"
levels(grdat_d$Origin)[levels(grdat_d$Origin)=="nat"] <- "Native C. diffusa"
levels(grdat_d$Origin)[levels(grdat_d$Origin)=="sk"] <- "Native C. stoebe"
#change order? but then have to change colors...
# grdat$Origin <- factor(grdat$Origin, c("Native C. diffusa", "Invasive C. diffusa", "Native C. stoebe"))

#for plots of pop means
grd2 <- ddply(grdat_d, .(Pop, Origin,  PC1), summarize, popCount=length(Pop), 
               popYellow=mean(Yellow, na.rm=TRUE),
              popbolt=mean(bolt.bin,na.rm = TRUE), popMass=mean(Mass.log, na.rm=TRUE))

#for plots of pop means with Trt
grd2trt <- ddply(grdat_d, .(Pop, Origin, Trt, PC1), summarize, popCount=length(Pop), 
              popWilt=mean(Wilt,na.rm = TRUE), 
              popDeath=mean(Death.date,na.rm = TRUE), popHarvest=mean(Harvest.date, na.rm=TRUE))
colnames(grd2trt)[4] <- "Treatment"
# Mass.log
pMass <- ggplot(grdat_d,aes(Origin, Mass.log, fill=Origin))+
  geom_boxplot()+
  xlab("Origin")+ylab("Shoot mass at harvest [g](log))")+ 
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(1,0),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pMass


pMass.2 <- qplot(data=grd2, PC1, popMass,  color=Origin)+geom_smooth(method=glm, se=TRUE)
pMass.2

###dates
#harvest
pHarvest <- ggplot(grdat_d,aes(Origin, Harvest.date, fill=Origin))+
  geom_boxplot()+
  xlab("Origin")+ylab("Harvest date (based on bolting) [g](log))")+ 
  theme_bw() +
  theme(legend.justification=c(1,1), legend.position=c(1,1),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pHarvest

pHarvest.2 <- qplot(data=grd2trt, PC1, popHarvest, shape=Trt,  color=Origin)+geom_smooth(method=glm, se=TRUE)
pHarvest.2

#bolt.bin needs mosaic plot!



####DEMO####
####export####
#pdf("KTurnerFig2.pdf", useDingbats=FALSE, width=13.38)
# # png("STsizebox_color.png",width=800, height = 600, pointsize = 16)
# postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)
# #plot....
# # multiplot(p1,p2, cols=2) #size plots only
# multiplot(p1,p2,p3, cols=3) #all st plots, code for p3 LH plot below
# dev.off()

####boxplot####
# p1 <- ggplot(grdat,aes(Trt, RootMass.g, fill=Origin))+
#   geom_boxplot()+
#   xlab("Treatment")+ylab("Root mass [g]")+ 
#   theme_bw()+
#   theme(legend.justification=c(1,1), legend.position=c(1,1),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))
# 
# #legend position(left/right,top/bottom)
# p1 <- p1  + annotate('point',x = "Control", y = 5, pch=16, color="red",parse=T, size=4)+
#   annotate('point',x = "Nutr. Stress", y = 5, pch=16, color="red",parse=T, size=4)+
#   annotate('point',x = "Herbivory", y = 5, pch=8, color="red",parse=T, size=4)+
#   annotate(geom="text", x="Early Control", y=12.5, label="(a)",fontface="bold", size=5)+
#   theme(axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))
# # p1
# 
# p2 <- ggplot(grdat, aes(Trt, LfCountH, fill=Origin))+geom_boxplot()+xlab("Treatment")+
#   ylab("Number of basal leaves")+ theme_bw()+theme(legend.position="none")
# 
# 
# p2 <- p2 +  annotate('point',x = "Control", y = 36, pch=8, color="red",parse=T, size=4)+
#   annotate('point',x = "Control", y = 38, pch=8, color="red",parse=T, size=4)+
#   annotate('point',x = "Control", y = 40, pch=8, color="red",parse=T, size=4)+
#   
#   annotate('point',x = "Herbivory", y = 40, pch=8, color="red",parse=T, size=4)+
#   
#   annotate(geom="text", x="Early Control", y=80, label="(b)",fontface="bold", size=5)+
#   annotate('point', x="Early Control", y=38, pch=8, color="red", parse=T, size=4)+
#   annotate('point', x="Early Control", y=38, pch=0, color="red", parse=T, size=6)+
#   
#   theme(axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))
# # p2

####mosaic plot####
#for p3
# grBatH2 <- ddply(grdatB, .(Trt, Origin), summarize, totcount = length(BoltedatH))
# grBatH2$xmax <- cumsum(grBatH2$totcount)
# grBatH2$xmin <- grBatH2$xmax-grBatH2$totcount
# grBatH3 <- ddply(grdatB, .(Trt, Origin, BoltedatH), summarize, count = length(BoltedatH))
# grBatH <- merge(grBatH2,grBatH3, all.y=TRUE)
# grBatH$Trt <- factor(grBatH$Trt, c("cont","nut def","cut"))
# grBatH$Treatment <- paste(grBatH$Trt, grBatH$Origin, grBatH$BoltedatH)
# 
# #percentages
# grBatHn <- grBatH[grBatH$BoltedatH=="n",]
# grBatHn<- ddply(grBatHn, .(Treatment), transform, ymax = cumsum(count/totcount*100))
# grBatHn <- ddply(grBatHn, .(Treatment), transform,
#                  ymin = ymax-(count/totcount*100))
# 
# grBatHy <- grBatH[grBatH$BoltedatH=="y",]
# grBatHy<- ddply(grBatHy, .(Treatment), transform, ymax = 100)
# grBatHy <- ddply(grBatHy, .(Treatment), transform,
#                  ymin = ymax-cumsum(count/totcount*100))
# grBatH1 <- merge(grBatHn, grBatHy, all=TRUE)
# #ggplot(grBatH1, aes(ymin = ymin, ymax = ymax, xmin=xmin, xmax=xmax,fill=Treatment))+ geom_rect(colour = I("grey"))+ scale_x_continuous(breaks=seq(16,80,32),labels=c("Control", "Herbivory", "Nutrient"))
# 
# #labels and tidying
# levels(grBatH1$Origin)[levels(grBatH1$Origin)=="inv"] <- "Invasive"
# levels(grBatH1$Origin)[levels(grBatH1$Origin)=="nat"] <- "Native"
# # grBatH1[grBatH1$xmax==100,]$xmax <- 96
# levels(grBatH1$BoltedatH)[levels(grBatH1$BoltedatH)=="n"] <- "Not Bolted"
# levels(grBatH1$BoltedatH)[levels(grBatH1$BoltedatH)=="y"] <- "Bolted"
# 
# colorset <- c("white","white","white","white","white","white","#F8766D","#00BFC4", "#00BFC4", "#F8766D","#00BFC4","#F8766D")
# cscale = scale_fill_manual(values=colorset)
# 
# grBatHStd <- grBatH1
# grBatHStd$xmin <- c(0,0,20,20,80,100,100,40,40,60,60)
# grBatHStd$xmax <- grBatHStd$xmin + 20
# #reverse stacking, not bolted comes out as white?
# grBatHStd$RevStackymax  <-  grBatHStd$ymax - grBatHStd$ymin
# grBatHStd[grBatHStd$BoltedatH=="Not Bolted",]$RevStackymax  <-  100
# grBatHStd$RevStackymin <- grBatHStd$RevStackymax-grBatHStd$ymax
# grBatHStd[grBatHStd$RevStackymin<0,]$RevStackymin <- 0

#p3 <- ggplot(grBatHStd, aes(ymin = RevStackymin, ymax = RevStackymax, xmin=xmin, xmax=xmax, fill=factor(col)))+
# geom_rect(colour = I("white"))+
#   scale_x_continuous(breaks=c(20,60,100),labels=c("Control", "Herbivory", "Nutr. Stress"), name="Treatment") +
#   scale_y_continuous(name="Percent Bolted at Harvest") + theme_bw()+cscale
# # p3
# annotate 
# p3 <- p3 + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
#   #annotate(geom="text", x=(grBatHStd$xmax-grBatHStd$xmin)/2 + grBatHStd$xmin, y=105, label=grBatHStd$Origin, size=4) +
#   #annotate(geom="text", x=(grBatHStd$xmax-grBatHStd$xmin)/2 + grBatHStd$xmin, y=grBatHStd$ymin+2, label=grBatHStd$BoltedatH, size=4)+ 
#   theme(legend.position="none", axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))+ 
#   annotate('point',x = 20, y = 50, pch=8, color="red",parse=T, size=4)+
#   annotate('point',x = 20, y = 54, pch=8, color="red",parse=T, size=4)+
#   annotate('point',x = 20, y = 50, pch=0, color="red",parse=T, size=6)+
#   annotate('point',x = 20, y = 54, pch=0, color="red",parse=T, size=6)+
#   
#   annotate('point',x = 60, y = 50, pch=8, color="red",parse=T,size=4)+
#   annotate('point',x = 60, y = 54, pch=8, color="red",parse=T,size=4)+
#   annotate('point',x = 60, y = 58, pch=8, color="red",parse=T,size=4)+
#   annotate('point',x = 60, y = 50, pch=0, color="red",parse=T,size=6)+
#   annotate('point',x = 60, y = 54, pch=0, color="red",parse=T,size=6)+
#   annotate('point',x = 60, y = 58, pch=0, color="red",parse=T,size=6)+
#   
#   annotate(geom="text", x=2.5, y=98, label="(c)",fontface="bold", size=5)

####interaction plot####
# p1 <- ggplot(stdrwiltTO,aes(CtrlPopShoot, popWilt, color=Origin))+ geom_point()+
#   geom_smooth(method=glm, se=FALSE)+
#   xlab("Population mean shoot mass [g] in control treatment")+
#   ylab("Population mean days to wilt in drought treatment")+ 
#   #title("Performance in drought vs. control treatments")+
#   theme_bw()+
#   theme(legend.justification=c(1,1), legend.position=c(1,1),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))
# 
# p1 <- p1 +  
#   #   annotate('point',x = 1.94, y = 7, pch=8, color="red",parse=T,size=3) +
#   #   annotate('point',x = 2.06, y = 7, pch=8, color="red",parse=T,size=3) +
#   #   annotate(geom="text", x=2, y=7.3, label="Origin", size=5) +
#   annotate(geom="text", x=2, y=7.3, label="Origin*Control mass", size=5) +
#   annotate('point',x = 2, y = 7, pch=8, color="red",parse=T,size=3)+
#   annotate(geom="text", x=1.25, y=1, label="(a)",fontface="bold", size=5)+
#   theme(axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))
# p1



##########
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
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