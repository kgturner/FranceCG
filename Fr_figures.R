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

#####crown.log and RoseAh.log####
grdat_cr <- FrdatSK[, c(1:7,22, 25, 53:55, 48,45)]
levels(grdat_cr$Origin)[levels(grdat_cr$Origin)=="inv"] <- "Invasive C. diffusa"
levels(grdat_cr$Origin)[levels(grdat_cr$Origin)=="nat"] <- "Native C. diffusa"
levels(grdat_cr$Origin)[levels(grdat_cr$Origin)=="sk"] <- "Native C. stoebe"
#change order? but then have to change colors...
# grdat$Origin <- factor(grdat$Origin, c("Native C. diffusa", "Invasive C. diffusa", "Native C. stoebe"))
colnames(grdat_cr)[4] <- "Treatment"

####crown.log box#####
pCrown <- ggplot(grdat_cr,aes(Origin, Crown.log, fill=Origin))+
  geom_boxplot()+
  xlab("Origin")+ylab("Root crown diameter at harvest [mm](log)")+ 
  theme_bw() #+
#   theme(legend.justification=c(1,0), legend.position=c(1,1),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))

pCrown

####crown.log interaction pop mean####
#for plots of pop means
grd_c <- ddply(grdat_cr, .(Pop, Origin,  PC1), summarize, popCount=length(Pop), 
               popCrown=mean(Crown.log,na.rm = TRUE), popRose=mean(RoseAh.log, na.rm=TRUE),
               popCrownDiam=mean(CrownDiam.mm, na.rm=TRUE), popRose.Area=mean(Rose.AreaH.m2, na.rm=TRUE))

# pCrown.2 <- qplot(data=grd_c,  PC1,popCrown, color=Origin)+geom_smooth(method=glm, se=TRUE)
# pCrown.2

pCrown.2 <- ggplot(grd_c,aes(PC1, popCrown,color=Origin))+geom_point(aes(shape=Origin, color=Origin), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("PC1")+ylab("Root crown diameter at harvest [mm](log)")+ 
  
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(1,0),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pCrown.2
# position=position_jitter(width=0.25,height=0.25)

####crown.log int data####
pCrown.3 <- ggplot(grdat_cr,aes(PC1, Crown.log,color=Origin))+geom_point(position=position_jitter(width=1,height=.5)) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("PC1")+ylab("Root crown diameter at harvest [mm](log))")+ 
  theme_bw() +
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pCrown.3

####CrownDiam int####
pCrown.4 <- ggplot(grd_c,aes(PC1, popCrownDiam,color=Origin))+geom_point(aes(shape=Origin, color=Origin), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("PC1")+ylab("Root crown diameter at harvest [mm]")+
  annotate(geom="text", x=-5, y=35, label="(a)",fontface="bold", size=5)+
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(1,0),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pCrown.4
####roseAh.log box####
pRose <- ggplot(grdat_cr,aes(Treatment, RoseAh.log, fill=Origin))+
  geom_boxplot()+
  xlab("Treatment")+ylab("Rosette diameter at harvest [cm2](log)")+ 
  theme_bw() +
  theme(legend.justification=c(1,1), legend.position=c(1,1),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pRose

####roseAh.log interaction pop mean####
#for plots of pop means with trt
grd_r <- ddply(grdat_cr, .(Pop, Origin, Treatment, PC1), summarize, popCount=length(Pop), popCrown=mean(Crown.log,na.rm = TRUE), popRose=mean(RoseAh.log, na.rm=TRUE))

#trts overplotted
# pRose.2 <- ggplot(grd_r,aes(PC1, popRose, color=Origin))+ 
#   geom_point(aes(shape=Treatment))+
# 
#   geom_smooth(aes(linetype=Treatment),method=glm, se=TRUE)+
#   
#   xlab("PC1")+
#   ylab("Rosette diameter at harvest [cm2](log)")+ 
#   #title("Performance in drought vs. control treatments")+
# #   theme_bw()+
#   theme(legend.justification=c(0,0), legend.position=c(0,0),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))
# pRose.2

#trts faceted
pRose.2 <- ggplot(grd_r,aes(PC1, popRose, color=Origin))+ facet_grid(. ~ Treatment)+
  geom_point()+

  geom_smooth(method=glm, se=TRUE)+
  
  xlab("PC1")+
  ylab("Rosette diameter at harvest [cm2](log)")+ 
  #title("Performance in drought vs. control treatments")+
  theme_bw()+
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pRose.2

####RoseAh.log w/ int data####
pRose.3 <- ggplot(grdat_cr,aes(PC1, RoseAh.log,color=Origin))+geom_point(position=position_jitter(width=1,height=.5)) + facet_grid(. ~ Treatment)+
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("PC1")+ylab("Rosette diameter at harvest [cm2](log)")+ 
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(1,0),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pRose.3

####Rose.Area int####
# grd_c <- ddply(grdat_cr, .(Pop, Origin,  PC1), summarize, popCount=length(Pop), 
#                popCrown=mean(Crown.log,na.rm = TRUE), popRose=mean(RoseAh.log, na.rm=TRUE),
#                popCrownDiam=mean(CrownDiam.mm, na.rm=TRUE), popRose.Area=mean(Rose.AreaH.m2, na.rm=TRUE))

pRose.4 <- ggplot(grd_c,aes(PC1, popRose.Area, color=Origin))+ #facet_grid(. ~ Treatment)+
  geom_point(aes(shape=Origin, color=Origin), size=3)+
  geom_smooth(method=glm, se=TRUE)+
  xlab("PC1")+ylab("Rosette area at harvest [m2]")+ 
  #title("Performance in drought vs. control treatments")+
  annotate(geom="text", x=-5, y=0.38, label="(b)",fontface="bold", size=5)+
  theme_bw()+theme(legend.position="none")
#   theme(legend.justification=c(0,0), legend.position=c(0,0),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))
pRose.4

####Mass, bolt, harvest, wilt, yellow, death.date####
grdat_d <- frend[, c(1:7,8,10,12,13,15,17,21,24,31,33:35)]
levels(grdat_d$Origin)[levels(grdat_d$Origin)=="inv"] <- "Invasive C. diffusa"
levels(grdat_d$Origin)[levels(grdat_d$Origin)=="nat"] <- "Native C. diffusa"
levels(grdat_d$Origin)[levels(grdat_d$Origin)=="sk"] <- "Native C. stoebe"
#change order? but then have to change colors...
# grdat$Origin <- factor(grdat$Origin, c("Native C. diffusa", "Invasive C. diffusa", "Native C. stoebe"))

#for plots of pop means
grd2 <- ddply(grdat_d, .(Pop, Origin,  PC1), summarize, popCount=length(Pop), 
               popShootMass=mean(Shoot.mass.gH, na.rm=TRUE),
              popbolt=mean(bolt.bin,na.rm = TRUE), popMass=mean(Mass.log, na.rm=TRUE))

#for plots of pop means with Trt
grd2trt <- ddply(grdat_d, .(Pop, Origin, Trt, PC1), summarize, popCount=length(Pop), 
              popWilt=mean(Wilt,na.rm = TRUE), 
              popDeath=mean(Death.date,na.rm = TRUE), popHarvest=mean(Harvest.date, na.rm=TRUE))
colnames(grd2trt)[3] <- "Treatment"
#### Mass.log box####
pMass <- ggplot(grdat_d,aes(Origin, Mass.log, fill=Origin))+ #facet_grid(. ~ Treatment)+
  geom_boxplot()+
  xlab("Origin")+ylab("Shoot mass at harvest [g](log))")+ 
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(1,0),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pMass

####Mass.log int pop means####
# pMass.2 <- qplot(data=grd2, PC1, popMass,  color=Origin)+geom_smooth(method=glm, se=TRUE)
# pMass.2

pMass.2<- ggplot(grd2,aes(PC1, popMass,color=Origin))+geom_point() + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("PC1")+ylab("Shoot mass at harvest [g](log)")+ 
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(1,0),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pMass.2
# position=position_jitter(width=1,height=.5)

####Shoot mass box####
pShoot <- ggplot(grdat_d,aes(Origin, Shoot.mass.gH, fill=Origin))+ #facet_grid(. ~ Treatment)+
  geom_boxplot()+
  xlab("Origin")+ylab("Shoot mass at harvest [g]")+ 
  scale_x_discrete(breaks=c("Invasive C. diffusa","Native C. diffusa", "Native C. stoebe"), labels=c("Invasive","Native","C. stoebe"))+
  annotate(geom="text", x=0.7, y=260, label="(a)",fontface="bold", size=5)+
  theme_bw() +  theme(legend.position="none")
#   theme(legend.justification=c(1,1), legend.position=c(1,1),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))
pShoot


####Shoot mass int pop means (not log)####
pShoot.2<- ggplot(grd2,aes(PC1, popShootMass,color=Origin))+geom_point(aes(shape=Origin, color=Origin), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  coord_cartesian(ylim = c(0, 120)) +
  xlab("PC1")+ylab("Shoot mass at harvest [g]")+
  annotate(geom="text", x=-5.1, y=110, label="(b)",fontface="bold", size=5)+
  theme_bw() +theme(legend.position="none")
#   theme(legend.justification=c(1,0), legend.position=c(1,0),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))
pShoot.2

####Mass.log int data####
pMass.3 <- ggplot(grdat_d,aes(PC1, Mass.log,color=Origin))+geom_point(position=position_jitter(width=1,height=.5)) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
#   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("PC1")+ylab("Shoot mass at harvest [g](log))")+ 
  theme_bw() +
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pMass.3

####harvest box####
pHarvest <- ggplot(grdat_d,aes(Origin, Harvest.date, fill=Origin))+
  geom_boxplot()+
  xlab("Origin")+ylab("Harvest date (based on bolting) [g](log))")+ 
  theme_bw() +
  theme(legend.justification=c(1,1), legend.position=c(1,1),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pHarvest

####harvest int pop means####
# pHarvest.2 <- qplot(data=grd2trt, PC1, popHarvest, shape=Treatment,  color=Origin)+geom_smooth(method=glm, se=TRUE)
# pHarvest.2
# pHarvest.2 <- qplot(data=grd2trt, PC1, popHarvest,  color=Origin)+geom_smooth(method=glm, se=TRUE)+ facet_grid(. ~ Treatment)
# pHarvest.2

ann_text <- data.frame(Origin=factor("Invasive C. diffusa", levels=c("Invasive C. diffusa", "Native C. diffusa",   "Native C. stoebe")), popHarvest=125, lab="(d)", PC1=-4.5,Trt="control" )

pHarvest.2<- ggplot(grd2trt,aes(PC1, popHarvest,color=Origin))+geom_point(aes(shape=Origin, color=Origin), size=3) + 
  facet_grid(. ~ Trt)+
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("PC1")+ylab("Population mean harvest date")+ 
  geom_text(data = ann_text,label = "(d)", fontface="bold", size=5, color="black")+
  theme_bw() + theme(legend.position="none")
#   theme(legend.justification=c(1,0), legend.position=c(1,0),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))
pHarvest.2

####harvest int data####
pHarvest.3 <- ggplot(grdat_d,aes(PC1, Harvest.date,color=Origin))+geom_point(position=position_jitter(width=1,height=.5))+ facet_grid(. ~ Trt)+
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #coord_cartesian(ylim = c(0, 1.02)) +
  xlab("PC1")+ylab("Proportion bolted at harvest")+ 
  theme_bw() +
  theme(legend.justification=c(1,1), legend.position=c(1,1),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pHarvest.3
summary(subset(grdat_d, Harvest.date<80))
summary(subset(grdat_d, Harvest.date>110))

#####bolt.bin mosaic with treatment####
grdat_B <- subset(grdat_d, !is.na(BoltedatH))
grdat_B1 <- ddply(grdat_B, .(Trt, Origin), summarize, totcount = length(BoltedatH))
grdat_B1$xmax <- cumsum(grdat_B1$totcount) # to set width of bars, if you want them represent sample size
grdat_B1$xmin <- grdat_B1$xmax-grdat_B1$totcount
grdat_B3 <- ddply(grdat_B, .(Trt, Origin, BoltedatH), summarize, count = length(BoltedatH))
grBatH <- merge(grdat_B1,grdat_B3, all.y=TRUE)
# grBatH$Trt <- factor(grBatH$Trt, c("cont","nut def","cut")) #set order
grBatH$Treatment <- paste(grBatH$Trt, grBatH$Origin, grBatH$BoltedatH)
# 
# #percentages - to set heights of bars
grBatHn <- grBatH[grBatH$BoltedatH=="No",]
grBatHn<- ddply(grBatHn, .(Treatment), transform, ymax = cumsum(count/totcount*100))
grBatHn <- ddply(grBatHn, .(Treatment), transform,
                 ymin = ymax-(count/totcount*100))

grBatHy <- grBatH[grBatH$BoltedatH=="Yes",]
grBatHy<- ddply(grBatHy, .(Treatment), transform, ymax = 100)
grBatHy <- ddply(grBatHy, .(Treatment), transform,
                 ymin = ymax-cumsum(count/totcount*100))
grBatH1 <- merge(grBatHn, grBatHy, all=TRUE)

#labels and tidying
levels(grBatH1$Origin)[levels(grBatH1$Origin)=="inv"] <- "Invasive C. diffusa"
levels(grBatH1$Origin)[levels(grBatH1$Origin)=="nat"] <- "Native C. diffusa"
levels(grBatH1$Origin)[levels(grBatH1$Origin)=="sk"] <- "Native C. stoebe"

levels(grBatH1$BoltedatH)[levels(grBatH1$BoltedatH)=="No"] <- "Not Bolted"
levels(grBatH1$BoltedatH)[levels(grBatH1$BoltedatH)=="Yes"] <- "Bolted"

col <-  with( grBatH1, interaction(Origin, Trt, BoltedatH))
colorset <- c("white","white","white","white","white","white","#F8766D", "#00BA38", "#619CFF","#F8766D", "#00BA38", "#619CFF")
cscale = scale_fill_manual(values=colorset)
# 
#evenly spaced columns - max xmax at 120
grBatHStd <- grBatH1
grBatHStd$xmin <- c(0,0,20,20,40,40,60,60,80,80,100,100)
grBatHStd$xmax <- grBatHStd$xmin + 20
#reverse stacking, not bolted comes out as white
grBatHStd$RevStackymax  <-  grBatHStd$ymax - grBatHStd$ymin
grBatHStd[grBatHStd$BoltedatH=="Not Bolted",]$RevStackymax  <-  100
grBatHStd$RevStackymin <- grBatHStd$RevStackymax-grBatHStd$ymax
grBatHStd[grBatHStd$RevStackymin<0,]$RevStackymin <- 0

pBolt <- ggplot(grBatHStd, aes(ymin = RevStackymin, ymax = RevStackymax, xmin=xmin, xmax=xmax, fill=factor(col))) +
geom_rect(colour = I("white"))+
  scale_x_continuous(breaks=c(30,90),labels=c("Control", "Drought"), name="Treatment") +
  scale_y_continuous(name="Percent Bolted at Harvest") + theme_bw()+cscale
pBolt
# annotate 
pBolt <- pBolt + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
  #annotate(geom="text", x=(grBatHStd$xmax-grBatHStd$xmin)/2 + grBatHStd$xmin, y=105, label=grBatHStd$Origin, size=4) +
  #annotate(geom="text", x=(grBatHStd$xmax-grBatHStd$xmin)/2 + grBatHStd$xmin, y=grBatHStd$ymin+2, label=grBatHStd$BoltedatH, size=4)+ 
  theme(legend.position="none", axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
        axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 )) #+ 
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

####bolt.bin mosaic without trt####
grdat_B <- subset(grdat_d, !is.na(BoltedatH))
grdat_B1 <- ddply(grdat_B, .( Origin), summarize, totcount = length(BoltedatH))
grdat_B1$xmax <- cumsum(grdat_B1$totcount) # to set width of bars, if you want them represent sample size
grdat_B1$xmin <- grdat_B1$xmax-grdat_B1$totcount
grdat_B3 <- ddply(grdat_B, .( Origin, BoltedatH), summarize, count = length(BoltedatH))
grBatH <- merge(grdat_B1,grdat_B3, all.y=TRUE)
# grBatH$Trt <- factor(grBatH$Trt, c("cont","nut def","cut")) #set order
grBatH$group <- paste(grBatH$Origin, grBatH$BoltedatH)
# 
# #percentages - to set heights of bars
grBatHn <- grBatH[grBatH$BoltedatH=="No",]
grBatHn<- ddply(grBatHn, .(group), transform, ymax = cumsum(count/totcount*100))
grBatHn <- ddply(grBatHn, .(group), transform,
                 ymin = ymax-(count/totcount*100))

grBatHy <- grBatH[grBatH$BoltedatH=="Yes",]
grBatHy<- ddply(grBatHy, .(group), transform, ymax = 100)
grBatHy <- ddply(grBatHy, .(group), transform,
                 ymin = ymax-cumsum(count/totcount*100))
grBatH1 <- merge(grBatHn, grBatHy, all=TRUE)

#labels and tidying
levels(grBatH1$Origin)[levels(grBatH1$Origin)=="inv"] <- "Invasive C. diffusa"
levels(grBatH1$Origin)[levels(grBatH1$Origin)=="nat"] <- "Native C. diffusa"
levels(grBatH1$Origin)[levels(grBatH1$Origin)=="sk"] <- "Native C. stoebe"

levels(grBatH1$BoltedatH)[levels(grBatH1$BoltedatH)=="No"] <- "Not Bolted"
levels(grBatH1$BoltedatH)[levels(grBatH1$BoltedatH)=="Yes"] <- "Bolted"

col <-  with( grBatH1, interaction(Origin, BoltedatH))
colorset <- c("white","white","white","#F8766D", "#00BA38", "#619CFF")
cscale = scale_fill_manual(values=colorset)
# 
#evenly spaced columns - max xmax at 60
grBatHStd <- grBatH1
grBatHStd$xmin <- c(0,0,20,20,40,40)
grBatHStd$xmax <- grBatHStd$xmin + 20
#reverse stacking, not bolted comes out as white
grBatHStd$RevStackymax  <-  grBatHStd$ymax - grBatHStd$ymin
grBatHStd[grBatHStd$BoltedatH=="Not Bolted",]$RevStackymax  <-  100
grBatHStd$RevStackymin <- grBatHStd$RevStackymax-grBatHStd$ymax
grBatHStd[grBatHStd$RevStackymin<0,]$RevStackymin <- 0

pBolt <- ggplot(grBatHStd, aes(ymin = RevStackymin, ymax = RevStackymax, xmin=xmin, xmax=xmax, fill=factor(col))) +
  geom_rect(colour = I("white"))+
  scale_x_continuous(breaks=c(10,30,50),labels=c("Invasive", "Native", "C. stoebe"), name="Origin") +
  scale_y_continuous(name="Bolting Probability (%)") + theme_bw()+cscale
# pBolt
# annotate 
pBolt <- pBolt + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
  annotate(geom="text", x=0.5, y=95, label="(c)",fontface="bold", size=5)+
  theme(legend.position="none")
#         , axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 )) #+

#annotate(geom="text", x=(grBatHStd$xmax-grBatHStd$xmin)/2 + grBatHStd$xmin, y=105, label=grBatHStd$Origin, size=4) +
#annotate(geom="text", x=(grBatHStd$xmax-grBatHStd$xmin)/2 + grBatHStd$xmin, y=grBatHStd$ymin+2, label=grBatHStd$BoltedatH, size=4)+ 
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
pBolt






# origincol <- c("#F8766D", "#00BA38", "#619CFF")
# 
# legend("top", c("Invasive C. diffusa","Native C. diffusa", "Native C. stoebe"), 
#        pch=c(16,17,15), fill=origincol,  bg="white", title = "Sampled populations", cex=1)
####bolt.bin, scatterplot popmean include PC1####
#sk included in plot 
moddata <- ddply(frend, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popbolt=mean(bolt.bin, na.rm=TRUE))
levels(moddata$Origin)[levels(moddata$Origin)=="inv"] <- "Invasive C. diffusa"
levels(moddata$Origin)[levels(moddata$Origin)=="nat"] <- "Native C. diffusa"
levels(moddata$Origin)[levels(moddata$Origin)=="sk"] <- "Native C. stoebe"

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,PC1, popbolt, color = Origin, 
#       xlab="PC1", 
#       ylab="Population mean Mass.log", main="", ymax=1, ymin=0) +geom_smooth(method=glm, se=TRUE)

pBolt.3 <- ggplot(moddata,aes(PC1, popbolt,color=Origin))+geom_point(aes(shape=Origin, color=Origin), size=3)+
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  coord_cartesian(ylim = c(0, 1.02)) +
#   scale_color_discrete(breaks=c("Invasive C. diffusa","Native C. diffusa", "Native C. stoebe"), labels=c("Invasive","Native","C. stoebe"))+
#   scale_shape_discrete(breaks=c("Invasive C. diffusa","Native C. diffusa", "Native C. stoebe"), labels=c("Invasive","Native","C. stoebe"))+
  xlab("PC1")+ylab("Bolting probability")+
  annotate(geom="text", x=-5.1, y=0.95, label="(d)",fontface="bold", size=5)+
  theme_bw() + theme(legend.position="none")
#   theme(legend.justification=c(1,1), legend.position=c(1,1),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))
pBolt.3

####lfc scatterplot####
grdat_l <- subset(Frdatsk.l, subset=lfc<200&m.date<80,select=c(1:7,22,23,26,28))
levels(grdat_l$Origin)[levels(grdat_l$Origin)=="inv"] <- "Invasive C. diffusa"
levels(grdat_l$Origin)[levels(grdat_l$Origin)=="nat"] <- "Native C. diffusa"
levels(grdat_l$Origin)[levels(grdat_l$Origin)=="sk"] <- "Native C. stoebe"


#for plots of pop means with Trt
# grd_l <- ddply(grdat_l, .(Pop, Origin, Trt, PC1,m.date, time), summarize, popCount=length(Pop), 
#                  poplfc=mean(lfc,na.rm = TRUE)) #avg per m.date
grd_l2 <- ddply(grdat_l, .(Pop, Origin, Trt, PC1,time), summarize, popCount=length(Pop), 
               poplfc=mean(lfc,na.rm = TRUE)) #avg per timepoint
# colnames(grd2trt)[3] <- "Treatment"

####lfc int data####
pLfc <- ggplot(grdat_l,aes(PC1, lfc,color=Origin))+geom_point(position=position_jitter(width=1,height=.5))+ 
  facet_grid(time ~ Trt, scales="free_y")+
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #coord_cartesian(ylim = c(0, 1.02)) +
  xlab("PC1")+ylab("Leaf number")+ 
  theme_bw() +
  theme(legend.justification=c(1,1), legend.position=c(1,1),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pLfc

####lfc int pop means####
#time goes right
pLfc.2 <- ggplot(grd_l2,aes(PC1, poplfc,color=Origin))+geom_point()+ 
  facet_grid(Trt ~ time,scales="free_y")+
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #coord_cartesian(ylim = c(0, 1.02)) +
  xlab("PC1")+ylab("Population mean leaf number")+ 
  theme_bw() #+
#   theme(legend.justification=c(0,0), legend.position=c(0,0),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))
pLfc.2
# position=position_jitter(width=1,height=.5)
# scales="free", space="free"

# #time goes down
# pLfc.2 <- ggplot(grd_l2,aes(PC1, poplfc,color=Origin))+geom_point()+ 
#   facet_grid(time ~ Trt,scales="free_y")+
#   geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
#   #coord_cartesian(ylim = c(0, 1.02)) +
#   xlab("PC1")+ylab("Population mean leaf number")+ 
#   theme_bw() #+
# #   theme(legend.justification=c(0,0), legend.position=c(0,0),
# #         legend.title = element_text(size=14, face="bold"),
# #         legend.text = element_text(size = 13))
# pLfc.2

####lfw box####
grdat_l <- subset(Frdatsk.l, subset=lfc<200&m.date<80,select=c(1:7,22:28))
levels(grdat_l$Origin)[levels(grdat_l$Origin)=="inv"] <- "Invasive"
levels(grdat_l$Origin)[levels(grdat_l$Origin)=="nat"] <- "Native"
levels(grdat_l$Origin)[levels(grdat_l$Origin)=="sk"] <- "C. stoebe"

ann_text <- data.frame(Origin=factor("Invasive", levels=c("Invasive","Native", "C. stoebe")), lfw=8.8, lab="(c)", time=1 )

plfwMont <- ggplot(subset(grdat_l, lfw<10),aes(Origin, lfw, fill=Origin))+
  geom_boxplot()+
  facet_grid(. ~ time,scales="free_y")+
  xlab("Origin")+ylab("Width of longest leaf [cm]")+
  geom_text(data = ann_text,label = "(c)", fontface="bold", size=5)+
  theme_bw() +
  theme(legend.position="none")
#   theme(legend.justification=c(1,0), legend.position=c(1,1),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))

plfwMont

####make sngl figs####
png("FrCrown_popmean.png",width=800, height = 600, pointsize = 16)
pCrown.2
dev.off()

png("FrRose_popmean.png",width=800, height = 600, pointsize = 16)
pRose.2
dev.off()

png("FrMass_popmean.png",width=800, height = 600, pointsize = 16)
pMass.2
dev.off()

png("FrHarvest.date_popmean.png",width=800, height = 600, pointsize = 16)
pHarvest.2
dev.off()

png("FrBolt_scatterplot_popmean.png",width=800, height = 600, pointsize = 16)
pBolt.3
dev.off()

png("Frlfc_tmpt_popmean.png",width=800, height = 600, pointsize = 16)
pLfc.2
dev.off()

####make multi figs####
origincol <- c("#F8766D", "#00BA38", "#619CFF")

pdf("KTurnerFig3.pdf", useDingbats=FALSE, width=6.65, height=9, pointsize = 12) #3.149, 4.4 or 6.65
multiplot(pShoot, pBolt, pShoot.2, pBolt.3, cols=2)
# legend("top", c("Invasive C. diffusa","Native C. diffusa", "Native C. stoebe"), 
#        pch=c(16,17,15), fill=origincol,  bg="white", title = "Sampled populations", cex=1)
dev.off()

png("Fr_Supp_DKtraits.png",width=800, height = 800, pointsize = 12)
multiplot(pCrown.4 ,plfwMont , pRose.4, pHarvest.2, cols=2)
dev.off()

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