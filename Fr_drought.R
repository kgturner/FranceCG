#France drought, 
# for traits with sig trt effect?
#Do I test all the covariates? 

#REML, using lme4
#mixed effect models 
library(lme4)
library(lsmeans)
library(ggplot2)
library(plyr)

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


######
###Harvest.date
modeldata<-frend[!is.na(frend$Harvest.date),]
modeldata <- subset(modeldata, Trt%in%"drought")
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, ctrlmeans, all.x=TRUE)

model1<-lmer(Harvest.date ~ Origin * CtrlPopMass.log+ Latitude + (1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date ~ Origin * CtrlPopMass.log+ Latitude +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date ~ Origin * CtrlPopMass.log+ Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.4076,1)

modelg <- glm(Harvest ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(Harvest ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.8596,1,lower=FALSE)#chisq value

modelg3<- glm(Harvest ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.05969,1,lower=FALSE)#chisq value
dispersiontest(modelg3)
modelg2<- glm(Harvest ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2,modelg3, test="LRT")
qchisq(0.03689,1,lower=FALSE)#chisq value

modelg4 <- glm(Harvest ~Origin, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
qchisq(1.488e-05,1,lower=FALSE)#chisq value
modelg5 <- glm(Harvest~CtrlPopShoot, family=poisson, data=modeldata)
anova(modelg5, modelg2, test="LRT")

qplot(data=modeldata,CtrlPopMass.log, Harvest.date, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(Pop, Origin, Latitude, CtrlPopMass.log, CtrlPopShoot), summarize, popCount=length(Pop), popHarvest=mean(Harvest.date))

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

###Bolt.date####################
modeldata<-frend[!is.na(frend$Bolt.date),]
modeldata <- subset(modeldata, Trt%in%"drought")
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, ctrlmeans, all.x=TRUE)

model1<-lmer(Bolt.date ~ Origin * CtrlPopMass.log+ Latitude + (1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date ~ Origin * CtrlPopMass.log+ Latitude +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date ~ Origin * CtrlPopMass.log+ Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.4076,1)

modelg <- glm(Bolt ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(Bolt ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.8596,1,lower=FALSE)#chisq value

modelg3<- glm(Bolt ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.05969,1,lower=FALSE)#chisq value
dispersiontest(modelg3)
modelg2<- glm(Bolt ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2,modelg3, test="LRT")
qchisq(0.03689,1,lower=FALSE)#chisq value

modelg4 <- glm(Bolt ~Origin, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
qchisq(1.488e-05,1,lower=FALSE)#chisq value
modelg5 <- glm(Bolt~CtrlPopShoot, family=poisson, data=modeldata)
anova(modelg5, modelg2, test="LRT")

qplot(data=modeldata,CtrlPopMass.log, Bolt.date, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(Pop, Origin, Latitude, CtrlPopMass.log, CtrlPopShoot), summarize, popCount=length(Pop), popBolt=mean(Bolt.date))

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

###Mass.log####################
modeldata<-frend[!is.na(frend$Mass.log),]
modeldata <- subset(modeldata, Trt%in%"drought")
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, ctrlmeans, all.x=TRUE)

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
