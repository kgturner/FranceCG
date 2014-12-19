#France fitness proxy
#data from greenhouse crosses for maternal effects common garden
#Turner et al., New Phyt, 2014

#Control and drought, REML, using lme4
#mixed effect models 
library(lme4.0)
# library(lsmeans)
library(ggplot2)
library(plyr)

library(devtools)
# install_github("dgrtwo/broom")
library(broom)

#read
seed<- read.table("GhouseCrosses.txt", header=T, sep="\t",quote='"')
head(seed)
seed$crossID <- as.factor(paste0(seed$pop, '_', seed$KNnum))
seed$Origin <- "inv"
seed[seed$pop%in%c("BG001","RU008","GR002","TR001"),]$Origin <- "nat"
seed$Origin <- as.factor(seed$Origin)
seed$Mom <- as.factor(paste0(seed$matmom, '_', seed$matindiv))
colnames(seed)[1] <- "Pop"
seed$Pop <- as.factor(seed$Pop)

####seed number####
modeldata <- seed
modeldata$seednum.log <- log(modeldata$seednum)
modeldata<-modeldata[!is.na(modeldata$seednum.log),]

modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)

model1<-lmer(seednum.log  ~ Origin +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(seednum.log  ~ Origin +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(seednum.log  ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.0115,1)

modelg <- glm(seednum ~ Origin, family=poisson,data=modeldata)
anova(modelg, test="LRT") 
modelg1 <- glm(seednum.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg1, test="LRT") 
qchisq(0.8596,1,lower=FALSE)#chisq value

# CI.LS.gaussian.log(modelint)
# CI.LS.gaussian.log(model1)
# 
# qplot(data=modeldata,PC1, Crown.log, color = Origin)+geom_point(position="jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popCrown.log=mean(Crown.log, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,PC1, popCrown.log, color = Origin, 
#       xlab="PC1", 
#       ylab="Population mean Crown.log", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# 

####seed wt####
modeldata <- seed
# modeldata$avg.seed.wt <- log(modeldata$seednum)
modeldata<-modeldata[!is.na(modeldata$avg.seed.wt),]

modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)

model1<-lmer(avg.seed.wt  ~ Origin +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(avg.seed.wt ~ Origin +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(avg.seed.wt ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.2991,1)

modelg1 <- glm(avg.seed.wt ~ Origin, family=gaussian,data=modeldata)
anova(modelg1, test="LRT") 
qchisq(0.8596,1,lower=FALSE)#chisq value
