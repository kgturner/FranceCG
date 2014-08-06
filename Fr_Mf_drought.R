##France/Maternal effects comparison_modeling
#for drought

#REML, using lme4
#mixed effect models 
library(plyr)
library(lme4)
library(lsmeans)
library(ggplot2)

# #necessary...?
library(AER)
# dispersiontest(modelg1)

#read
Mf <- read.table("Fr_Mf_data.txt", header=T, sep="\t",quote='"', row.names=1)
Mf.l <- read.table("Fr_Mf_data_long.txt", header=T, sep="\t",quote='"', row.names=1)

#pop control means to look at stress treatments
se <- function(x) sqrt(var(x)/length(x))
Mf.ctrlmeans<- ddply(subset(Mf, Trt%in%"control"&!is.na(Mass.log)), .(Pop, Origin), summarize, 
                  CtrlPopCount=length(Pop), CtrlPopMass.log=mean(Mass.log), CtrlPopMass.logSE=se(Mass.log),
                  CtrlPopShoot=mean(ShootMass.g), CtrlPopShootSE=se(ShootMass.g))
Mf.ctrlmeans

####recreate new phyt paper####
####with ctrl pop means from balanced subset, have significant effect.
####with means from unbalanced full dataset, effect goes away.
#pop control means to look at stress treatments
mfco.dk1<-read.table("MatFxBonusCtrl.txt", header=T, sep="\t", quote='"', row.names=1) #largest balanced control
mfmom.dk<-read.table("MatFxMom.dk.txt", header=T, sep="\t", quote='"', row.names=1) 
#merge mf ctrl and mom df
str(mfmom.dk)
str(mfco.dk1)
totmf <- merge(mfmom.dk,mfco.dk1, all.y=TRUE )

se <- function(x) sqrt(var(x)/length(x)) #chokes on NAs
comeans<- ddply(totmf, .(PopID, Origin, Latitude), summarize, CtrlPopCount=length(PopID), CtrlPopLf=mean(LfCountH, na.rm=TRUE), CtrlPopLfSE=se(LfCountH),
                CtrlPopShoot=mean(ShootMass.g, na.rm=TRUE), CtrlPopShootSE=se(ShootMass.g))

mfd.dk<-read.table("MatFxDrought.dk.txt", header=T, sep="\t", quote='"', row.names=1) #drought, dk only
head(mfd.dk)

#merge mfcu and mom df
str(mfmom.dk)
str(mfd.dk)
totmfd <- merge(mfmom.dk,mfd.dk, all.y=TRUE )

###drought, tot wilt
modeldata<-totmfd[!is.na(totmfd$TotWiltDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(TotWiltDay ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(TotWiltDay ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(TotWiltDay ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(TotWiltDay ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)

modelg <- glm(TotWiltDay ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(TotWiltDay ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(pval,1,lower=FALSE)#chisq value
modelg3<- glm(TotWiltDay ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
dispersiontest(modelg3)
modelg2<- glm(TotWiltDay ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2,modelg3, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg4 <- glm(TotWiltDay ~Origin, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
anova(modelg4)

summary(modelg3)

qplot(data=modeldata,CtrlPopShoot, TotWiltDay, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popTotWiltDay=mean(TotWiltDay))

png("MF_performance_totwilt_shoot.png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,CtrlPopShoot, popTotWiltDay, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean days to TotWiltDay in drought treatment",
      main="Performance in drought vs. control treatments") +geom_smooth(method=glm,se=TRUE)
dev.off()

####CtrlPOpMass.log####
####death.date####
modeldata<-subset(Mf,Origin%in%c("inv", "nat")&Trt%in%"drought"&!is.na(Mf$DeathDay))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, Mf.ctrlmeans, all.x=TRUE)

modelOr <- lmer(DeathDay ~ Origin * CtrlPopMass.log+ PC1 + (Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(DeathDay ~ Origin * CtrlPopMass.log+ PC1+ (1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(DeathDay ~ Origin * CtrlPopMass.log+ PC1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(DeathDay ~ Origin * CtrlPopMass.log+ PC1 +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.4076,1)

# modelint <- lmer(Death.date ~ Origin + CtrlPopMass.log+ PC1 + (1|Pop/Mom), family=poisson,data=modeldata)
# anova(modelint, model1)

modelg <- glm(DeathDay ~ Origin * CtrlPopMass.log +PC1, family=poisson,data=modeldata)
modelg1 <- glm(DeathDay ~ Origin + CtrlPopMass.log +PC1, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT")

modelg2 <- glm(DeathDay ~ Origin + CtrlPopMass.log, family=poisson,data=modeldata)
anova(modelg2, modelg1, test="LRT")

modelg3 <- glm(DeathDay ~ Origin, family=poisson,data=modeldata)

####WiltDay####
modeldata<-subset(Mf,Origin%in%c("inv", "nat")&Trt%in%"drought"&!is.na(Mf$WiltDay))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, Mf.ctrlmeans, all.x=TRUE)

modelOr <- lmer(WiltDay ~ Origin * CtrlPopMass.log+ PC1 + (Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(WiltDay ~ Origin * CtrlPopMass.log+ PC1+ (1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(WiltDay ~ Origin * CtrlPopMass.log+ PC1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(WiltDay ~ Origin * CtrlPopMass.log+ PC1 +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.4076,1)

# modelint <- lmer(WiltDay ~ Origin + CtrlPopMass.log+ PC1 + (1|Pop/Mom), family=poisson,data=modeldata)
# anova(modelint, model1)

modelg <- glm(WiltDay ~ Origin * CtrlPopMass.log +PC1, family=poisson,data=modeldata)
modelg1 <- glm(WiltDay ~ Origin + CtrlPopMass.log +PC1, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT")

modelg2 <- glm(WiltDay ~ Origin + CtrlPopMass.log, family=poisson,data=modeldata)
anova(modelg2, modelg1, test="LRT")

modelg3 <- glm(WiltDay ~ Origin, family=poisson,data=modeldata)
anova(modelg3, modelg2, test="LRT")

####TotWiltDay####
modeldata<-subset(Mf,Origin%in%c("inv", "nat")&Trt%in%"drought"&!is.na(Mf$TotWiltDay))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, Mf.ctrlmeans, all.x=TRUE)

modelOr <- lmer(TotWiltDay ~ Origin * CtrlPopMass.log+ PC1 + (Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(TotWiltDay ~ Origin * CtrlPopMass.log+ PC1+ (1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(TotWiltDay ~ Origin + CtrlPopMass.log+ PC1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(TotWiltDay ~ Origin + CtrlPopMass.log+ PC1 +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.4076,1)

# modelint <- lmer(TotWiltDay ~ Origin + CtrlPopMass.log+ PC1 + (1|Pop/Mom), family=poisson,data=modeldata)
# anova(modelint, model1)

modelg <- glm(TotWiltDay ~ Origin * CtrlPopMass.log +PC1, family=poisson,data=modeldata)
modelg1 <- glm(TotWiltDay ~ Origin + CtrlPopMass.log +PC1, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT")

modelg2 <- glm(TotWiltDay ~ Origin + CtrlPopMass.log, family=poisson,data=modeldata)
anova(modelg2, modelg1, test="LRT")

modelg3 <- glm(TotWiltDay ~ Origin, family=poisson,data=modeldata)

####ctrlshootmass(not log)####
####TotWiltDay####
modeldata<-subset(Mf,Origin%in%c("inv", "nat")&Trt%in%"drought"&!is.na(Mf$TotWiltDay))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, Mf.ctrlmeans, all.x=TRUE)
modeldata <- subset(modeldata, select=c(Origin, Pop, Mom, CtrlPopShoot, CtrlPopMass.log, PC1, TotWiltDay, blank))

modelOr <- lmer(TotWiltDay ~ Origin * CtrlPopShoot+ PC1 + (Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(TotWiltDay ~ Origin * CtrlPopShoot+ PC1+ (1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(TotWiltDay ~ Origin * CtrlPopShoot+ PC1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(TotWiltDay ~ Origin * CtrlPopShoot+ PC1 +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.4076,1)

# modelint <- lmer(TotWiltDay ~ Origin + CtrlPopMass.log+ PC1 + (1|Pop/Mom), family=poisson,data=modeldata)
# anova(modelint, model1)

modelg <- glm(TotWiltDay ~ Origin * CtrlPopShoot +PC1, family=poisson,data=modeldata)
modelg1 <- glm(TotWiltDay ~ Origin + CtrlPopShoot +PC1, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT")

modelg2 <- glm(TotWiltDay ~ Origin + CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2, modelg1, test="LRT")

modelg3 <- glm(TotWiltDay ~ Origin, family=poisson,data=modeldata)

modelg4 <- glm(TotWiltDay ~ Origin * CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg4, modelg2, test="LRT")

summary(modelg4)

#######using comeans, and PC2####
####TotWiltDay####
modeldata<-subset(Mf,Origin%in%c("inv", "nat")&Trt%in%"drought"&!is.na(Mf$TotWiltDay))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- subset(modeldata, select=c(Origin, Pop, Mom, CtrlPopShoot, PC2, TotWiltDay, blank))

modelOr <- lmer(TotWiltDay ~ Origin * CtrlPopShoot+ PC2 + (Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(TotWiltDay ~ Origin * CtrlPopShoot+ PC2+ (1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(TotWiltDay ~ Origin * CtrlPopShoot+ PC2 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(TotWiltDay ~ Origin * CtrlPopShoot+ PC2 +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.4076,1)

# modelint <- lmer(TotWiltDay ~ Origin + CtrlPopMass.log+ PC1 + (1|Pop/Mom), family=poisson,data=modeldata)
# anova(modelint, model1)

modelg <- glm(TotWiltDay ~ Origin * CtrlPopShoot +PC2, family=poisson,data=modeldata)
modelg1 <- glm(TotWiltDay ~ Origin + CtrlPopShoot +PC2, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT")

modelg2 <- glm(TotWiltDay ~ Origin * CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2, modelg, test="LRT")

modelg3 <- glm(TotWiltDay ~ Origin + CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3, modelg1)

modelg4 <- glm(TotWiltDay ~ Origin * CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg4, modelg3, test="LRT")

summary(modelg4)

####WiltDay####
modeldata<-subset(Mf,Origin%in%c("inv", "nat")&Trt%in%"drought"&!is.na(Mf$WiltDay))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- subset(modeldata, select=c(Origin, Pop, Mom, CtrlPopShoot, PC2, WiltDay, blank))

modelOr <- lmer(WiltDay ~ Origin * CtrlPopShoot+ PC2 + (Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(WiltDay ~ Origin * CtrlPopShoot+ PC2+ (1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(WiltDay ~ Origin * CtrlPopShoot+ PC2 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(WiltDay ~ Origin * CtrlPopShoot+ PC2 +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.4076,1)

# modelint <- lmer(WiltDay ~ Origin + CtrlPopMass.log+ PC1 + (1|Pop/Mom), family=poisson,data=modeldata)
# anova(modelint, model1)

modelg <- glm(WiltDay ~ Origin * CtrlPopShoot +PC2, family=poisson,data=modeldata)
modelg1 <- glm(WiltDay ~ Origin + CtrlPopShoot +PC2, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT")

modelg2 <- glm(WiltDay ~ Origin * CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2, modelg, test="LRT")

modelg3 <- glm(WiltDay ~ Origin + CtrlPopShoot, family=poisson,data=modeldata)

modelg4 <- glm(WiltDay ~ Origin * CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg4, modelg3, test="LRT")

summary(modelg4)

####DeathDay####
modeldata<-subset(Mf,Origin%in%c("inv", "nat")&Trt%in%"drought"&!is.na(Mf$DeathDay))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- subset(modeldata, select=c(Origin, Pop, Mom, CtrlPopShoot, PC2, TDeathDay, blank))

modelOr <- lmer(DeathDay ~ Origin * CtrlPopShoot+ PC2 + (Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(DeathDay ~ Origin * CtrlPopShoot+ PC2+ (1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(DeathDay ~ Origin * CtrlPopShoot+ PC2 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(DeathDay ~ Origin * CtrlPopShoot+ PC2 +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.4076,1)

# modelint <- lmer(TotWiltDay ~ Origin + CtrlPopMass.log+ PC1 + (1|Pop/Mom), family=poisson,data=modeldata)
# anova(modelint, model1)

modelg <- glm(DeathDay ~ Origin * CtrlPopShoot +PC2, family=poisson,data=modeldata)
modelg1 <- glm(DeathDay ~ Origin + CtrlPopShoot +PC2, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT")

modelg2 <- glm(DeathDay ~ Origin * CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2, modelg, test="LRT")

modelg3 <- glm(DeathDay ~ Origin + CtrlPopShoot, family=poisson,data=modeldata)

modelg4 <- glm(DeathDay ~ Origin * CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg4, modelg3, test="LRT")

summary(modelg4)
