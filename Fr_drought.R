#France drought, DK only
# for traits with sig trt effect?
#Do I test all the covariates? 

#REML, using lme4
#mixed effect models 
library(plyr)
library(lme4)
library(lsmeans)
library(ggplot2)


#with SK
#for long data formating, see FrSKdata_format.R
#read
Frdatsk.l<- read.table("FrTraitClimDat_SK_long.txt", header=T, sep="\t",quote='"', row.names=1)
#read
FrdatSK<- read.table("FrTraitClimDat_SK.txt", header=T, sep="\t",quote='"', row.names=1)
#read
frend<- read.table("FrEnd.txt", header=T, sep="\t",quote='"', row.names=1)

#for DK only include:
subset(frend, Origin%in%c("inv", "nat"))

#for drought only include:
subset(frend, Trt=="drought")

#pop control means to look at stress treatments
se <- function(x) sqrt(var(x)/length(x))
ctrlmeans<- ddply(subset(frend, Trt=="control"&!is.na(Mass.log)), .(Pop, Origin), summarize, 
                  CtrlPopCount=length(Pop), CtrlPopMass.log=mean(Mass.log), CtrlPopMass.logSE=se(Mass.log),
                  CtrlPopShoot=mean(Shoot.mass.gH), CtrlPopShootSE=se(Shoot.mass.gH))
ctrlmeans
# #explicit trade-off using shootmass
# modeldata <- merge(modeldata, ctrlmeans, all.x=TRUE)
# modeldata <- modeldata[!is.na(modeldata$CtrlPopMass.log),]

summary(subset(frend, Trt%in%"drought"))
summary(subset(FrdatSK, Trt%in%"drought"))
summary(subset(Frdatsk.l, Trt%in%"drought"))

###################Origin*CtrlPopMass.log############

modeldata <- subset(frend, Trt%in%"drought"&Origin%in%c("inv", "nat"))
modeldata <- merge(modeldata, ctrlmeans, all.x=TRUE)
modeldata <- droplevels(modeldata)
frGLR.frend_DKdr <- lapply(names(modeldata)[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,modeldata, covariate="CtrlPopMass.log"))#apply func to all gaussian traits
frPLR.frend_DKdr <- lapply(names(modeldata)[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,modeldata, covariate="CtrlPopMass.log", family=poisson))#apply func to all poisson traits
modeldata$Ctrl.l <- modeldata$CtrlPopMass.log/100
frBLR.frend_DKdr <- lapply(names(modeldata)[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,modeldata, covariate="Ctrl.1", family=binomial)) #apply to binomial trait

modeldata <- subset(FrdatSK, Trt%in%"drought"&Origin%in%c("inv", "nat"))
modeldata <- merge(modeldata, ctrlmeans, all.x=TRUE)
modeldata <- droplevels(modeldata)
frGLR.wide_DKdr <- lapply(names(modeldata)[c(49, 53,52)],function(n) CGtrait.LR_snglcov_int(n,modeldata, covariate="CtrlPopMass.log"))#apply func to all gaussian traits

modeldata <- subset(Frdatsk.l, Trt%in%"drought"&Origin%in%c("inv", "nat"))
modeldata <- merge(modeldata, ctrlmeans, all.x=TRUE)
modeldata <- droplevels(modeldata)
frGLR.long_DKdr <- lapply(names(modeldata)[c(28,29,31)],function(n) CGtrait.LR_snglcov_int_mdate(n,modeldata, covariate="CtrlPopMass.log"))#apply func to all gaussian traits
frPLR.long_DKdr <- CGtrait.LR_snglcov_int_mdate("lfc",modeldata, covariate="CtrlPopMass.log", family=poisson)#apply func to all poisson traits

CGtrait_sigaov_func_Fr(frGLR.frend_DKdr, selectaov=1:4, cutoff=0.05)
CGtrait_sigaov_func_Fr(frPLR.frend_DKdr, selectaov=1:4, cutoff=0.05)
CGtrait_sigaov_func_Fr(frBLR.frend_DKdr, selectaov=1:4, cutoff=0.05)

CGtrait_sigaov_func_Fr(frGLR.wide_DKdr, selectaov=1:4, cutoff=0.05)

CGtrait_sigaov_func_Fr(frGLR.long_DKdr, selectaov=1:4, cutoff=0.05)
frPLR.long_DKdr
######
###Harvest.date
modeldata<-frend[!is.na(frend$Harvest.date),]
modeldata <- subset(modeldata, Trt%in%"drought"&Origin%in%c("inv", "nat"))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, ctrlmeans, all.x=TRUE)

model1<-lmer(Harvest.date ~ Origin * CtrlPopMass.log+ PC1 + (1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date ~ Origin * CtrlPopMass.log+ PC1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date ~ Origin * CtrlPopMass.log+ PC1 +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.4076,1)

modelint <- lmer(Harvest.date ~ Origin + CtrlPopMass.log+ PC1 + (1|Pop/Mom), family=poisson,data=modeldata)
anova(modelint, model1)

modelcov <- lmer(Harvest.date ~ Origin + CtrlPopMass.log + (1|Pop/Mom), family=poisson,data=modeldata)
anova(modelcov, modelint)

modelctrl<- lmer(Harvest.date ~ Origin + (1|Pop/Mom), family=poisson,data=modeldata)
anova(modelctrl, modelcov)

modelO<- lmer(Harvest.date ~ CtrlPopMass.log + (1|Pop/Mom), family=poisson,data=modeldata)
anova(modelO, modelcov)

# modelg <- glm(Harvest ~ Origin*CtrlPopShoot*PC1, family=poisson,data=modeldata)
# modelg1 <- glm(Harvest ~ Origin*CtrlPopShoot+PC1, family=poisson,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.8596,1,lower=FALSE)#chisq value
# 
# modelg3<- glm(Harvest ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# qchisq(0.05969,1,lower=FALSE)#chisq value
# dispersiontest(modelg3)
# modelg2<- glm(Harvest ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
# anova(modelg2,modelg3, test="LRT")
# qchisq(0.03689,1,lower=FALSE)#chisq value
# 
# modelg4 <- glm(Harvest ~Origin, family=poisson, data=modeldata)
# anova(modelg4, modelg2, test="LRT")
# qchisq(1.488e-05,1,lower=FALSE)#chisq value
# modelg5 <- glm(Harvest~CtrlPopShoot, family=poisson, data=modeldata)
# anova(modelg5, modelg2, test="LRT")

qplot(data=modeldata,CtrlPopMass.log, Harvest.date, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(Pop, Origin, PC1, CtrlPopMass.log, CtrlPopShoot), summarize, popCount=length(Pop), popHarvest=mean(Harvest.date))

# png("STdr_deathtradeoff_color.png",width=800, height = 600, pointsize = 16)
qplot(data=moddata,CtrlPopMass.log, popHarvest, color = Origin, 
      xlab="Population mean log shoot mass in control treatment", 
      ylab="Population mean days to Harvest in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)
# qplot(data=moddata,CtrlPopShoot, popHarvest, color = Origin, 
#       xlab="Population mean shoot mass in control treatment", 
#       ylab="Population mean days to Harvest in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)


# dev.off()

modelg3
summary(modelg3)
anova(modelg3, test="LRT")

CI.LS.poisson(modelg3, conf=95)

############
###Wilt
modeldata<-frend[!is.na(frend$Wilt),]
modeldata <- subset(modeldata, Trt%in%"drought"&Origin%in%c("inv", "nat"))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, ctrlmeans, all.x=TRUE)

model1<-lmer(Wilt ~ Origin * CtrlPopMass.log+ PC1+ (1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt ~ Origin * CtrlPopMass.log+ PC1+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt ~ Origin * CtrlPopMass.log+ PC1+(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.4076,1)

# modelint <- lmer(Wilt ~ Origin + CtrlPopMass.log+ PC1+ (1|Pop/Mom), family=poisson,data=modeldata)
# anova(modelint, model1)
# 
# modelcov <- lmer(Wilt ~ Origin + CtrlPopMass.log + (1|Pop/Mom), family=poisson,data=modeldata)
# anova(modelcov, modelint)
# 
# modelctrl<- lmer(Wilt ~ Origin + (1|Pop/Mom), family=poisson,data=modeldata)
# anova(modelctrl, modelcov)
# 
# modelO<- lmer(Wilt ~ CtrlPopMass.log + (1|Pop/Mom), family=poisson,data=modeldata)
# anova(modelO, modelcov)

modelg <- glm(Wilt ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(Wilt ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.8596,1,lower=FALSE)#chisq value

modelg3<- glm(Wilt ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.05969,1,lower=FALSE)#chisq value
dispersiontest(modelg3)
modelg2<- glm(Wilt ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2,modelg3, test="LRT")
qchisq(0.03689,1,lower=FALSE)#chisq value

modelg4 <- glm(Wilt ~Origin, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
qchisq(1.488e-05,1,lower=FALSE)#chisq value
modelg5 <- glm(Wilt~CtrlPopShoot, family=poisson, data=modeldata)
anova(modelg5, modelg2, test="LRT")

qplot(data=modeldata,CtrlPopMass.log, Wilt, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(Pop, Origin, Latitude, CtrlPopMass.log, CtrlPopShoot), summarize, popCount=length(Pop), popWilt=mean(Wilt))

# png("STdr_deathtradeoff_color.png",width=800, height = 600, pointsize = 16)
qplot(data=moddata,CtrlPopMass.log, popWilt, color = Origin, 
      xlab="Population mean log shoot mass in control treatment", 
      ylab="Population mean days to Wilt in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)
# qplot(data=moddata,CtrlPopShoot, popHarvest, color = Origin, 
#       xlab="Population mean shoot mass in control treatment", 
#       ylab="Population mean days to Harvest in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)


# dev.off()

xtabs(Wilt~Origin+CtrlPopMass.log, modeldata)
summary(modeldata$Origin)
summary(modeldata$Pop)

modelg3
summary(modelg3)
anova(modelg3, test="LRT")

CI.LS.poisson(modelg3, conf=95)



###Bolt.date####################
modeldata<-frend[!is.na(frend$Bolt.date),]
modeldata <- subset(modeldata, Trt%in%"drought"&Origin%in%c("inv", "nat"))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, ctrlmeans, all.x=TRUE)

model1<-lmer(Bolt.date ~ Origin * CtrlPopMass.log+PC1+  (1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date ~ Origin * CtrlPopMass.log+PC1+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date ~ Origin * CtrlPopMass.log+PC1+(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(3e-04,1)

modelg <- glm(Bolt.date ~ Origin*CtrlPopMass.log*PC1, family=poisson,data=modeldata)
modelg1 <- glm(Bolt.date ~ Origin*CtrlPopMass.log+PC1, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.8596,1,lower=FALSE)#chisq value

modelg3<- glm(Bolt.date ~ Origin*CtrlPopMass.log, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.05969,1,lower=FALSE)#chisq value
# dispersiontest(modelg3)
modelg2<- glm(Bolt.date ~Origin +CtrlPopMass.log, family=poisson,data=modeldata)
anova(modelg2,modelg3, test="LRT")
# qchisq(0.03689,1,lower=FALSE)#chisq value

modelg4 <- glm(Bolt.date ~Origin, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
# qchisq(1.488e-05,1,lower=FALSE)#chisq value
modelg5 <- glm(Bolt.date~CtrlPopMass.log, family=poisson, data=modeldata)
anova(modelg5, modelg2, test="LRT")

qplot(data=modeldata,CtrlPopMass.log, Bolt.date, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(Pop, Origin, PC1, CtrlPopMass.log, CtrlPopShoot), summarize, popCount=length(Pop), popBolt=mean(Bolt.date))

# png("STdr_deathtradeoff_color.png",width=800, height = 600, pointsize = 16)
qplot(data=moddata,CtrlPopMass.log, popBolt, color = Origin, 
      xlab="Population mean log shoot mass in control treatment", 
      ylab="Population mean days to Bolt in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)
# qplot(data=moddata,CtrlPopShoot, popBolt, color = Origin, 
#       xlab="Population mean shoot mass in control treatment", 
#       ylab="Population mean days to Bolt in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)


# dev.off()

modelg3
summary(modelg3)
anova(modelg3, test="LRT")

CI.LS.poisson(modelg3, conf=95)

##############
###lfc
modeldata<-Frdatsk.l[!is.na(Frdatsk.l$lfc),]
modeldata <- subset(modeldata, Trt%in%"drought"&Origin%in%c("inv", "nat"))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, ctrlmeans, all.x=TRUE)

model1<-lmer(lfc ~ Origin * CtrlPopMass.log+ Latitude +m.date+ (1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(lfc ~ Origin * CtrlPopMass.log+ Latitude +m.date+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc ~ Origin * CtrlPopMass.log+ Latitude +m.date+(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.4076,1)

modelint <- lmer(lfc ~ Origin + CtrlPopMass.log+ Latitude +m.date+ (1|Pop/Mom), family=poisson,data=modeldata)
anova(modelint, model1)

modelcov <- lmer(lfc ~ Origin + CtrlPopMass.log +m.date+ (1|Pop/Mom), family=poisson,data=modeldata)
anova(modelcov, modelint)

modelctrl<- lmer(lfc ~ Origin +m.date+ (1|Pop/Mom), family=poisson,data=modeldata)
anova(modelctrl, modelcov)

modelO<- lmer(lfc ~ CtrlPopMass.log +m.date+ (1|Pop/Mom), family=poisson,data=modeldata)
anova(modelO, modelcov)

# modelg <- glm(Harvest ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
# modelg1 <- glm(Harvest ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.8596,1,lower=FALSE)#chisq value
# 
# modelg3<- glm(Harvest ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# qchisq(0.05969,1,lower=FALSE)#chisq value
# dispersiontest(modelg3)
# modelg2<- glm(Harvest ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
# anova(modelg2,modelg3, test="LRT")
# qchisq(0.03689,1,lower=FALSE)#chisq value
# 
# modelg4 <- glm(Harvest ~Origin, family=poisson, data=modeldata)
# anova(modelg4, modelg2, test="LRT")
# qchisq(1.488e-05,1,lower=FALSE)#chisq value
# modelg5 <- glm(Harvest~CtrlPopShoot, family=poisson, data=modeldata)
# anova(modelg5, modelg2, test="LRT")

qplot(data=modeldata,CtrlPopMass.log, lfc, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(Pop, Origin, Latitude, CtrlPopMass.log, CtrlPopShoot), summarize, popCount=length(Pop), poplfc=mean(lfc))

# png("STdr_deathtradeoff_color.png",width=800, height = 600, pointsize = 16)
qplot(data=moddata,CtrlPopMass.log, poplfc, color = Origin, 
      xlab="Population mean log shoot mass in control treatment", 
      ylab="Population mean leaf count in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)
# qplot(data=moddata,CtrlPopShoot, popHarvest, color = Origin, 
#       xlab="Population mean shoot mass in control treatment", 
#       ylab="Population mean days to Harvest in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)


# dev.off()

modelg3
summary(modelg3)
anova(modelg3, test="LRT")

CI.LS.poisson(modelg3, conf=95)

###Mass.log####################
modeldata<-frend[!is.na(frend$Mass.log),]
modeldata <- subset(modeldata, Trt%in%"drought"&Origin%in%c("inv", "nat"))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, ctrlmeans, all.x=TRUE)

model1<-lmer(Mass.log ~ Origin * CtrlPopMass.log+ Latitude + (1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log ~ Origin * CtrlPopMass.log+ Latitude +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log ~ Origin * CtrlPopMass.log+ Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.4076,1)

modelint <- lmer(Mass.log ~ Origin + CtrlPopMass.log+ Latitude + (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelint, model1)

modelcov <- lmer(Mass.log ~ Origin + CtrlPopMass.log + (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelcov, modelint)

modelctrl<- lmer(Mass.log ~ Origin + Latitude + (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelctrl, modelint)

modelO<- lmer(Mass.log ~ CtrlPopMass.log +Latitude+ (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelO, modelint)

qplot(data=modeldata,CtrlPopMass.log, Mass.log, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(Pop, Origin, Latitude, CtrlPopMass.log, CtrlPopShoot), summarize, popCount=length(Pop), popMass.log=mean(Mass.log))

# png("STdr_deathtradeoff_color.png",width=800, height = 600, pointsize = 16)
qplot(data=moddata,CtrlPopMass.log, popMass.log, color = Origin, 
      xlab="Population mean log shoot mass in control treatment", 
      ylab="Population mean log shoot mass in drought treatment", main="Performance in drought vs. control treatments") +
  geom_smooth(method=glm, se=TRUE)
# qplot(data=moddata,CtrlPopShoot, popBolt, color = Origin, 
#       xlab="Population mean shoot mass in control treatment", 
#       ylab="Population mean days to Bolt in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)


# dev.off()

###RoseAh.log####################
modeldata<-FrdatSK[!is.na(FrdatSK$RoseAh.log),]
modeldata <- subset(modeldata, Trt%in%"drought"&Origin%in%c("inv", "nat"))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, ctrlmeans, all.x=TRUE)

model1<-lmer(RoseAh.log ~ Origin * CtrlPopMass.log+ Latitude + (1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(RoseAh.log ~ Origin * CtrlPopMass.log+ Latitude +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RoseAh.log ~ Origin * CtrlPopMass.log+ Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.4076,1)

modelint <- lmer(RoseAh.log ~ Origin + CtrlPopMass.log+ Latitude + (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelint, model1)

modelcov <- lmer(RoseAh.log ~ Origin + CtrlPopMass.log + (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelcov, modelint)

modelctrl<- lmer(RoseAh.log ~ Origin  + (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelctrl, modelcov)

modelO<- lmer(RoseAh.log ~ CtrlPopMass.log + (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelO, modelcov)

qplot(data=modeldata,CtrlPopMass.log, RoseAh.log, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(Pop, Origin, Latitude, CtrlPopMass.log, CtrlPopShoot), summarize, popCount=length(Pop), popRoseAh.log=mean(RoseAh.log))

# png("STdr_deathtradeoff_color.png",width=800, height = 600, pointsize = 16)
qplot(data=moddata,CtrlPopMass.log, popRoseAh.log, color = Origin, 
      xlab="Population mean log shoot mass in control treatment", 
      ylab="Population mean log shoot mass in drought treatment", main="Performance in drought vs. control treatments") +
  geom_smooth(method=glm, se=TRUE)
# qplot(data=moddata,CtrlPopShoot, popBolt, color = Origin, 
#       xlab="Population mean shoot mass in control treatment", 
#       ylab="Population mean days to Bolt in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)


# dev.off()

#######
#bolt.bin
modeldata<-frend[!is.na(frend$bolt.bin),]
modeldata <- subset(modeldata, Trt%in%"drought"&Origin%in%c("inv", "nat"))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, ctrlmeans, all.x=TRUE)

model1<-lmer(bolt.bin ~ Origin * CtrlPopMass.log+PC1 + (1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin ~ Origin * CtrlPopMass.log+ PC1+(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin ~ Origin * CtrlPopMass.log+ PC1+(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.4076,1)

modelint <- lmer(bolt.bin ~ Origin + CtrlPopMass.log+ PC1+ (1|Pop/Mom), family=binomial,data=modeldata)
anova(modelint, model1)

modelcov <- lmer(bolt.bin ~ Origin + CtrlPopMass.log + (1|Pop/Mom), family=binomial,data=modeldata)
anova(modelcov, modelint)

modelctrl<- lmer(bolt.bin ~ Origin + (1|Pop/Mom), family=binomial,data=modeldata)
anova(modelctrl, modelcov)

modelO<- lmer(bolt.bin ~ CtrlPopMass.log + (1|Pop/Mom), family=binomial,data=modeldata)
anova(modelO, modelcov)

# modelg <- glm(Harvest ~ Origin*CtrlPopShoot*Latitude, family=binomial,data=modeldata)
# modelg1 <- glm(Harvest ~ Origin*CtrlPopShoot+Latitude, family=binomial,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.8596,1,lower=FALSE)#chisq value
# 
# modelg3<- glm(Harvest ~ Origin*CtrlPopShoot, family=binomial,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# qchisq(0.05969,1,lower=FALSE)#chisq value
# dispersiontest(modelg3)
# modelg2<- glm(Harvest ~Origin +CtrlPopShoot, family=binomial,data=modeldata)
# anova(modelg2,modelg3, test="LRT")
# qchisq(0.03689,1,lower=FALSE)#chisq value
# 
# modelg4 <- glm(Harvest ~Origin, family=binomial, data=modeldata)
# anova(modelg4, modelg2, test="LRT")
# qchisq(1.488e-05,1,lower=FALSE)#chisq value
# modelg5 <- glm(Harvest~CtrlPopShoot, family=binomial, data=modeldata)
# anova(modelg5, modelg2, test="LRT")

qplot(data=modeldata,CtrlPopMass.log, bolt.bin, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(Pop, Origin, Latitude, CtrlPopMass.log, CtrlPopShoot), summarize, popCount=length(Pop), popbolt.bin=mean(bolt.bin))

# png("STdr_deathtradeoff_color.png",width=800, height = 600, pointsize = 16)
qplot(data=moddata,CtrlPopMass.log, popbolt.bin, color = Origin, 
      xlab="Population mean log shoot mass in control treatment", 
      ylab="Population mean bolt status at harvest in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)
