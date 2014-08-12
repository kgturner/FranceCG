#France MF figures

library(ggplot2)
library(plyr)

#read
Mf <- read.table("Fr_Mf_data.txt", header=T, sep="\t",quote='"', row.names=1)
Mf.l <- read.table("Fr_Mf_data_long.txt", header=T, sep="\t",quote='"', row.names=1)


####Mass, bolt, mass, etc. ####
grdat_mf <- Mf[, c(1:14, 52, 53, 23,24, 33:37,41:51)]
levels(grdat_mf$Origin)[levels(grdat_mf$Origin)=="inv"] <- "Invasive C. diffusa"
levels(grdat_mf$Origin)[levels(grdat_mf$Origin)=="nat"] <- "Native C. diffusa"
levels(grdat_mf$Origin)[levels(grdat_mf$Origin)=="sk"] <- "Native C. stoebe"
#change order? but then have to change colors...
# grdat$Origin <- factor(grdat$Origin, c("Native C. diffusa", "Invasive C. diffusa", "Native C. stoebe"))

#for plots of pop means (not comprehensive)
grdmf <- ddply(grdat_mf, .(Pop, Origin,  PC1, PC2), summarize, popCount=length(Pop), 
              popShootMass=mean(ShootMass.g, na.rm=TRUE),
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
  theme_bw() +
  theme(legend.justification=c(0,1), legend.position=c(0,1),
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

####make multi figs####
png("Fr_Mf_mass_bolt_popmean.png",width=1000, height = 600, pointsize = 16)
multiplot(pMf_Shoot.2, pMf_Bolt.3, cols=2)
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