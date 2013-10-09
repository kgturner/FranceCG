#Fr plots
library(ggplot2)

# pdf("KTurnerFig2.pdf", useDingbats=FALSE, width=13.38)
# # png("STsizebox_color.png",width=800, height = 600, pointsize = 16)
# postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)
# #....
# multiplot(p1,p2,p3, cols=3) #code below
# dev.off()



#maxlfwidth2
modeldata <- FrdatSK[!is.na(FrdatSK$MaxLfWdth2),]
# modeldata<-df[!is.na(df[[trait]]),]
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
modeldata$Origin <- factor(modeldata$Origin, levels=c("nat", "inv","sk"))

p1 <- ggplot(modeldata,aes(Trt, MaxLfWdth2, fill=Origin))+
  geom_boxplot()+xlab("Stress Treatment")+
  ylab("leaf width time point 2")+ 
  theme(legend.justification=c(1,1), legend.position=c(1,1))
p1

p2 <- ggplot(modeldata,aes(Latitude, MaxLfWdth2, color=Origin))+
  geom_point()+xlab("latitude")+ geom_smooth(method=glm, se=FALSE)+
  ylab("leaf width time point 2")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))
p2
png("Fr_lfwdth2_lat.png",width=800, height = 600, pointsize = 16)
multiplot(p1, p2, cols=2)
dev.off()

#lflegnthH
modeldata <- FrdatSK[!is.na(FrdatSK$LfLgth),]
# modeldata<-df[!is.na(df[[trait]]),]
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
modeldata$Origin <- factor(modeldata$Origin, levels=c("nat", "inv","sk"))

p1 <- ggplot(modeldata,aes(Trt, LfLgth, fill=Origin))+
  geom_boxplot()+xlab("Stress Treatment")+
  ylab("max lf length at harvest")+ 
  theme(legend.justification=c(1,1), legend.position=c(1,1))
p1

p2 <- ggplot(modeldata,aes(Latitude, LfLgth, color=Origin))+
  geom_point()+xlab("latitude")+ geom_smooth(method=glm, se=FALSE)+
  ylab("max lf length at harvest")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))

png("Fr_lflghtH_lat.png",width=800, height = 600, pointsize = 16)
multiplot(p1, p2, cols=2)
dev.off()

#maxboltHtH
modeldata <- FrdatSK[!is.na(FrdatSK$MaxBoltHtH),]
# modeldata<-df[!is.na(df[[trait]]),]
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
modeldata$Origin <- factor(modeldata$Origin, levels=c("nat", "inv","sk"))

p1 <- ggplot(modeldata,aes(Trt, MaxBoltHtH, fill=Origin))+
  geom_boxplot()+xlab("Stress Treatment")+
  ylab("max bolt height at harvest")+ 
  theme(legend.justification=c(1,1), legend.position=c(1,1))
p1

p2 <- ggplot(modeldata,aes(PC2, MaxBoltHtH, color=Origin))+
  geom_point()+xlab("PC2 of climate")+ geom_smooth(method=glm, se=FALSE)+
  ylab("max bolt height at harvest")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))
p2

png("Fr_boltHtH_PC2.png",width=800, height = 600, pointsize = 16)
multiplot(p1, p2, cols=2)
dev.off()


#LfCount1.sq
#check normality
modeldata <- FrdatSK[!is.na(FrdatSK$LfCount1.sq),]
# modeldata<-df[!is.na(df[[trait]]),]
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
modeldata$Origin <- factor(modeldata$Origin, levels=c("nat", "inv","sk"))

p1 <- ggplot(modeldata,aes(Trt, LfCount1.sq, fill=Origin))+
  geom_boxplot()+xlab("Stress Treatment")+
  ylab("transformed lf count, time point 1")+ 
  theme(legend.justification=c(1,1), legend.position=c(1,1))
p1

p2 <- ggplot(modeldata,aes(bio19, LfCount1.sq, color=Origin))+
  geom_point()+xlab("Precipitation of Coldest Quarter")+ geom_smooth(method=glm, se=FALSE)+
  ylab("transformed lf count, time point 1")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))
p2

png("Fr_LfCount1sq_bio19.png", width=800, height=600, pointsize = 16)
multiplot(p1, p2, cols=2)
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