#France MF figures

library(ggplot2)
library(plyr)

#read
Mf <- read.table("Fr_Mf_data.txt", header=T, sep="\t",quote='"', row.names=1)
Mf.l <- read.table("Fr_Mf_data_long.txt", header=T, sep="\t",quote='"', row.names=1)


####Mass, bolt, mass, etc. ####
grdat_mf <- Mf[, c(1:14, 52, 53, 21,23,24, 33:37,41:51)]
levels(grdat_mf$Origin)[levels(grdat_mf$Origin)=="inv"] <- "Invasive C. diffusa"
levels(grdat_mf$Origin)[levels(grdat_mf$Origin)=="nat"] <- "Native C. diffusa"
levels(grdat_mf$Origin)[levels(grdat_mf$Origin)=="sk"] <- "Native C. stoebe"
#change order? but then have to change colors...
# grdat$Origin <- factor(grdat$Origin, c("Native C. diffusa", "Invasive C. diffusa", "Native C. stoebe"))

#for plots of pop means (not comprehensive)
grdmf <- ddply(grdat_mf, .(Pop, Origin,  PC1, PC2), summarize, popCount=length(Pop), 
              popShootMass=mean(ShootMass.g, na.rm=TRUE),popCrownDiam=mean(CrownDiam.mm, na.rm=TRUE),
              popbolt=mean(bolt.bin,na.rm = TRUE), popMass=mean(Mass.log, na.rm=TRUE))

# #for plots of pop means with Trt
# grd2trt <- ddply(grdat_d, .(Pop, Origin, Trt, PC1), summarize, popCount=length(Pop), 
#                  popWilt=mean(Wilt,na.rm = TRUE), 
#                  popDeath=mean(Death.date,na.rm = TRUE), popHarvest=mean(Harvest.date, na.rm=TRUE))
# colnames(grd2trt)[3] <- "Treatment"
#
####Mass.log int pop means####
# pMass.2 <- qplot(data=grd2, PC1, popMass,  color=Origin)+geom_smooth(method=glm, se=TRUE)
# pMass.2

pMf_Mass.2<- ggplot(grdmf,aes(PC2, popMass,color=Origin))+geom_point() + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("PC1")+ylab("Shoot mass at harvest [g](log)")+ 
  theme_bw() +
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pMf_Mass.2
# position=position_jitter(width=1,height=.5)

####ShootMass.g int pop means (not log)####
pMf_Shoot.2<- ggplot(grdmf,aes(PC2, popShootMass,color=Origin))+geom_point() + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("PC2")+ylab("Shoot mass at harvest [g]")+ 
  annotate(geom="text", x=-6, y=5.2, label="(a)",fontface="bold", size=5)+
  theme_bw() +
  theme(legend.justification=c(0,0), legend.position=c(0,0),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
pMf_Shoot.2

####bolt.bin, scatterplot popmean include PC1####
#sk included in plot 
moddata <- ddply(Mf, .(Pop, Origin, PC2), summarize, popCount=length(Pop), popbolt=mean(bolt.bin, na.rm=TRUE))
levels(moddata$Origin)[levels(moddata$Origin)=="inv"] <- "Invasive C. diffusa"
levels(moddata$Origin)[levels(moddata$Origin)=="nat"] <- "Native C. diffusa"
levels(moddata$Origin)[levels(moddata$Origin)=="sk"] <- "Native C. stoebe"

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,PC1, popbolt, color = Origin, 
#       xlab="PC1", 
#       ylab="Population mean Mass.log", main="", ymax=1, ymin=0) +geom_smooth(method=glm, se=TRUE)

pMf_Bolt.3 <- ggplot(moddata,aes(PC2, popbolt,color=Origin))+geom_point()+
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  coord_cartesian(ylim = c(-0.02, 1.02)) +
  xlab("PC2")+ylab("Bolting probability")+ 
  theme_bw() + theme(legend.position="none")
#   theme(legend.justification=c(1,1), legend.position=c(1,1),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))
pMf_Bolt.3

####bolt.bin mosaic without trt####
grdat_B <- subset(grdat_mf, !is.na(BoltedatH))
grdat_B1 <- ddply(grdat_B, .( Origin), summarize, totcount = length(BoltedatH))
grdat_B1$xmax <- cumsum(grdat_B1$totcount) # to set width of bars, if you want them represent sample size
grdat_B1$xmin <- grdat_B1$xmax-grdat_B1$totcount
grdat_B3 <- ddply(grdat_B, .( Origin, BoltedatH), summarize, count = length(BoltedatH))
grBatH <- merge(grdat_B1,grdat_B3, all.y=TRUE)
# grBatH$Trt <- factor(grBatH$Trt, c("cont","nut def","cut")) #set order
grBatH$group <- paste(grBatH$Origin, grBatH$BoltedatH)
# 
# #percentages - to set heights of bars
grBatHn <- grBatH[grBatH$BoltedatH=="n",]
grBatHn<- ddply(grBatHn, .(group), transform, ymax = cumsum(count/totcount*100))
grBatHn <- ddply(grBatHn, .(group), transform,
                 ymin = ymax-(count/totcount*100))

grBatHy <- grBatH[grBatH$BoltedatH=="y",]
grBatHy<- ddply(grBatHy, .(group), transform, ymax = 100)
grBatHy <- ddply(grBatHy, .(group), transform,
                 ymin = ymax-cumsum(count/totcount*100))
grBatH1 <- merge(grBatHn, grBatHy, all=TRUE)

#labels and tidying
levels(grBatH1$BoltedatH)[levels(grBatH1$BoltedatH)=="n"] <- "Not Bolted"
levels(grBatH1$BoltedatH)[levels(grBatH1$BoltedatH)=="y"] <- "Bolted"

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
  coord_cartesian(ylim = c(0, 100)) +
  scale_x_continuous(breaks=c(10,30,50),labels=c("Invasive", "Native", "C. stoebe"), name="Origin") +
  scale_y_continuous(name="Bolting Probability (%)") + theme_bw()+cscale
# pBolt
# annotate 
pBolt <- pBolt + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
  annotate(geom="text", x=0.5, y=95, label="(f)",fontface="bold", size=5)+
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

####crown box#####
pCrown <- ggplot(subset(grdat_mf, CrownDiam.mm<2),aes(Origin, CrownDiam.mm, fill=Origin))+
  geom_boxplot()+
  xlab("Origin")+ylab("Root crown diameter at harvest [mm]")+ 
  annotate(geom="text", x=0.75, y=1.8, label="(b)",fontface="bold", size=5)+
  theme_bw() +
  theme(legend.position="none")
#   theme(legend.justification=c(1,0), legend.position=c(1,1),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))

pCrown

####bolt date box####
pBoltD <- ggplot(grdat_mf,aes(Origin, BoltDay+4, fill=Origin))+
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 100)) +
  xlab("Origin")+ylab("Bolt Date")+ 
  annotate(geom="text", x=0.75, y=95, label="(e)",fontface="bold", size=5)+
  theme_bw() +
  theme(legend.position="none")
#   theme(legend.justification=c(1,0), legend.position=c(1,1),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))

pBoltD

####lfw box####
grdat_mf.l <- Mf.l[, c(1:14, 18:24, 26:32)]
levels(grdat_mf.l$Origin)[levels(grdat_mf.l$Origin)=="inv"] <- "Invasive"
levels(grdat_mf.l$Origin)[levels(grdat_mf.l$Origin)=="nat"] <- "Native"
levels(grdat_mf.l$Origin)[levels(grdat_mf.l$Origin)=="sk"] <- "C. stoebe"

ann_text <- data.frame(Origin=factor("Invasive", levels=c("Invasive","Native", "C. stoebe")), lfw=9.8, lab="(d)", time=1 )

plfw <- ggplot(subset(grdat_mf.l, lfw<10),aes(Origin, lfw, fill=Origin))+
  geom_boxplot()+
  facet_grid(. ~ time,scales="free_y")+
  xlab("Origin")+ylab("Width of longest leaf [cm]")+
  geom_text(data = ann_text,label = "(d)", fontface="bold", size=5)+
  theme_bw() +
  theme(legend.position="none")
#   theme(legend.justification=c(1,0), legend.position=c(1,1),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))

plfw

####lfl int####
grdmf.l <- ddply(grdat_mf.l, .(Pop, Origin,  PC1, PC2, time), summarize, popCount=length(Pop), 
               poplfl=mean(lfl, na.rm=TRUE),poplfw=mean(lfw, na.rm=TRUE),
               poplfc=mean(lfc,na.rm = TRUE))

plfl.2<- ggplot(grdmf.l,aes(PC2, poplfl,color=Origin))+geom_point() + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  facet_grid(. ~ time,scales="free_y")+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("PC2")+ylab("Length of longest leaf [g]")+ 
  theme_bw() +
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13))
plfl.2


####lfl box####
ann_text <- data.frame(Origin=factor("Invasive", levels=c("Invasive","Native", "C. stoebe")), lfl=42, lab="(c)", time=1 )

plfl <- ggplot(subset(grdat_mf.l, lfl<50),aes(Origin, lfl, fill=Origin))+
  geom_boxplot()+
  facet_grid(. ~ time,scales="free_y")+
  xlab("Origin")+ylab("Length of longest leaf [cm]")+ 
  geom_text(data = ann_text,label = "(c)", fontface="bold", size=5)+
  theme_bw() +
  theme(legend.position="none")
#   theme(legend.justification=c(1,0), legend.position=c(1,1),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))

plfl


####make multi figs####
png("Fr_Mf_Supp_DKtraits.png",width=800, height = 1200, pointsize = 12)
multiplot(pMf_Shoot.2,plfl ,pBoltD , pCrown ,plfw , pBolt, cols=2)
dev.off()

png("Fr_Mf_Supp_DKSKtraits_1.png",width=800, height = 1200, pointsize = 12)
multiplot(pMf_Shoot.2,plfl ,pBoltD , pCrown ,plfw , pBolt, cols=2)
dev.off()

png("Fr_Mf_Supp_DKSKtraits_2.png",width=800, height = 1200, pointsize = 12)
multiplot(pMf_Shoot.2,plfl ,pBoltD , pCrown ,plfw , pBolt, cols=2)
dev.off()

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