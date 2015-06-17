#France data 2/21/2014
#checking normality
#using sngl time pt measures (Shootmass), and time series (lfw)

#tested here with latitude trait~origin+latitude

#Control and drought, REML, using lme4
#mixed effect models 
library(lme4)
library(lsmeans)
library(ggplot2)
library(plyr)

##############DK only################
#read
frdat<- read.table("FrTraitClimDat.txt", header=T, sep="\t",quote='"', row.names=1)

[[27]]
[1] "MaxBoltHtH"

modeldata<-frdat[!is.na(frdat$MaxBoltHtH),]

par(mfrow=c(1,2))
modelg3 <- glm(MaxBoltHtH ~ Origin+Latitude, family=gaussian,data=modeldata)
qqnorm(resid(modelg3), main="Q-Q plot for residuals")
qqline(resid(modelg3))

modeldata$BoltHt.log <- log(modeldata$MaxBoltHtH)
modelg3log<- glm(BoltHt.log ~ Origin+Latitude, family=gaussian,data=modeldata)
qqnorm(resid(modelg3log), main="Q-Q plot for residuals")
qqline(resid(modelg3log))
#transform looks mildly better

[[29]]
[1] "Min.Rose.diamH" / 
  [[37]]
[1] "MinRoseDh.log"

#necessary?

[[30]]
[1] "Rose.AreaH.m2" / 
  [[38]]
[1] "RoseAh.log"

modeldata<-frdat[!is.na(frdat$Rose.AreaH.m2),]

par(mfrow=c(1,2))
modelg3 <- glm(Rose.AreaH.m2 ~ Origin+Latitude, family=gaussian,data=modeldata)
qqnorm(resid(modelg3), main="Q-Q plot for residuals")
qqline(resid(modelg3))

# modeldata$BoltHt.log <- log(modeldata$Rose.AreaH.m2)
modelg3log<- glm(RoseAh.log ~ Origin+Latitude, family=gaussian,data=modeldata)
qqnorm(resid(modelg3log), main="Q-Q plot for residuals")
qqline(resid(modelg3log))
#transform looks much better!

[[33]]
[1] "CrownDiam.mm" / 
  [[41]]
[1] "Crown.log"

modeldata<-frdat[!is.na(frdat$CrownDiam.mm),]

par(mfrow=c(1,2))
modelg3 <- glm(CrownDiam.mm ~ Origin+Latitude, family=gaussian,data=modeldata)
qqnorm(resid(modelg3), main="Q-Q plot for residuals")
qqline(resid(modelg3))

# modeldata$BoltHt.log <- log(modeldata$CrownDiam.mm)
modelg3log<- glm(Crown.log ~ Origin+Latitude, family=gaussian,data=modeldata)
qqnorm(resid(modelg3log), main="Q-Q plot for residuals")
qqline(resid(modelg3log))
#transform looks better!

[[34]]
[1] "Shoot.mass.gH" / 
  [[42]]
[1] "Mass.log"

modeldata<-frdat[!is.na(frdat$Shoot.mass.gH),]

par(mfrow=c(1,2))
modelg3 <- glm(Shoot.mass.gH ~ Origin+Latitude, family=gaussian,data=modeldata)
qqnorm(resid(modelg3), main="Q-Q plot for residuals")
qqline(resid(modelg3))

# modeldata$BoltHt.log <- log(modeldata$Shoot.mass.gH)
modelg3log<- glm(Mass.log ~ Origin+Latitude, family=gaussian,data=modeldata)
qqnorm(resid(modelg3log), main="Q-Q plot for residuals")
qqline(resid(modelg3log))
#transform looks much better!


###DK only long###
#include m.date
#read
frdat.l<- read.table("FrTraitClimDat_DKonly_long.txt", header=T, sep="\t",quote='"', row.names=1)

[[30]]
[1] "lfl"

modeldata<-frdat.l[!is.na(frdat.l$lfl),]

par(mfrow=c(1,2))
modelg3 <- glm(lfl~ Origin+Latitude+m.date, family=gaussian,data=modeldata)
qqnorm(resid(modelg3), main="Q-Q plot for residuals")
qqline(resid(modelg3))

modeldata$lfl.log <- log(modeldata$lfl)
modelg3log<- glm(lfl.log ~ Origin+Latitude+m.date, family=gaussian,data=modeldata)
qqnorm(resid(modelg3log), main="Q-Q plot for residuals")
qqline(resid(modelg3log))
#transform looks worse! DO NOT TRANSFORM

[[31]]
[1] "lfw"

modeldata<-frdat.l[!is.na(frdat.l$lfw),]

par(mfrow=c(1,2))
modelg3 <- glm(lfw~ Origin+Latitude+m.date, family=gaussian,data=modeldata)
qqnorm(resid(modelg3), main="Q-Q plot for residuals")
qqline(resid(modelg3))

modeldata$lfw.log <- log(modeldata$lfw)
modelg3log<- glm(lfw.log ~ Origin+Latitude+m.date, family=gaussian,data=modeldata)
qqnorm(resid(modelg3log), main="Q-Q plot for residuals")
qqline(resid(modelg3log))
#transform looks worse! DO NOT TRANSFORM


[[33]]
[1] "rd"

modeldata<-frdat.l[!is.na(frdat.l$rd),]

par(mfrow=c(1,2))
modelg3 <- glm(rd~ Origin+Latitude+m.date, family=gaussian,data=modeldata)
qqnorm(resid(modelg3), main="Q-Q plot for residuals")
qqline(resid(modelg3))

modeldata$rd.log <- log(modeldata$rd)
modelg3log<- glm(rd.log ~ Origin+Latitude+m.date, family=gaussian,data=modeldata)
qqnorm(resid(modelg3log), main="Q-Q plot for residuals")
qqline(resid(modelg3log))
#transform looks worse! DO NOT TRANSFORM

################################################
#########DK+SK data##############

#read
FrdatSK<- read.table("FrTraitClimDat_SK.txt", header=T, sep="\t",quote='"', row.names=1)

[[19]]
[1] "MaxBoltHtH"

modeldata<-FrdatSK[!is.na(FrdatSK$MaxBoltHtH),]

par(mfrow=c(1,2))
modelg3 <- glm(MaxBoltHtH ~ Origin+Latitude, family=gaussian,data=modeldata)
qqnorm(resid(modelg3), main="Q-Q plot for residuals")
qqline(resid(modelg3))

modeldata$BoltHt.log <- log(modeldata$MaxBoltHtH)
modelg3log<- glm(BoltHt.log ~ Origin+Latitude, family=gaussian,data=modeldata)
qqnorm(resid(modelg3log), main="Q-Q plot for residuals")
qqline(resid(modelg3log))
#transform looks mildly better

[[21]]
[1] "Min.Rose.diamH" / 
  [[48]]
[1] "MinRoseDh.log"

#necessary?

[[22]]
[1] "Rose.AreaH.m2" / 
  [[49]]
[1] "RoseAh.log"

modeldata<-FrdatSK[!is.na(FrdatSK$Rose.AreaH.m2),]

par(mfrow=c(1,2))
modelg3 <- glm(Rose.AreaH.m2 ~ Origin+Latitude, family=gaussian,data=modeldata)
qqnorm(resid(modelg3), main="Q-Q plot for residuals")
qqline(resid(modelg3))

# modeldata$BoltHt.log <- log(modeldata$Rose.AreaH.m2)
modelg3log<- glm(RoseAh.log ~ Origin+Latitude, family=gaussian,data=modeldata)
qqnorm(resid(modelg3log), main="Q-Q plot for residuals")
qqline(resid(modelg3log))
#transform looks much better!

[[25]]
[1] "CrownDiam.mm" / 
  [[52]]
[1] "Crown.log"

modeldata<-FrdatSK[!is.na(FrdatSK$CrownDiam.mm),]

par(mfrow=c(1,2))
modelg3 <- glm(CrownDiam.mm ~ Origin+Latitude, family=gaussian,data=modeldata)
qqnorm(resid(modelg3), main="Q-Q plot for residuals")
qqline(resid(modelg3))

# modeldata$BoltHt.log <- log(modeldata$CrownDiam.mm)
modelg3log<- glm(Crown.log ~ Origin+Latitude, family=gaussian,data=modeldata)
qqnorm(resid(modelg3log), main="Q-Q plot for residuals")
qqline(resid(modelg3log))
#transform looks better!

[[26]]
[1] "Shoot.mass.gH" / 
  [[53]]
[1] "Mass.log"

modeldata<-FrdatSK[!is.na(FrdatSK$Shoot.mass.gH),]

par(mfrow=c(1,2))
modelg3 <- glm(Shoot.mass.gH ~ Origin+Latitude, family=gaussian,data=modeldata)
qqnorm(resid(modelg3), main="Q-Q plot for residuals")
qqline(resid(modelg3))

# modeldata$BoltHt.log <- log(modeldata$Shoot.mass.gH)
modelg3log<- glm(Mass.log ~ Origin+Latitude, family=gaussian,data=modeldata)
qqnorm(resid(modelg3log), main="Q-Q plot for residuals")
qqline(resid(modelg3log))
#transform looks much better!


###DK only long###
#include m.date
#read
Frdatsk.l<- read.table("FrTraitClimDat_SK_long.txt", header=T, sep="\t",quote='"', row.names=1)

[[28]]
[1] "lfl"

modeldata<-Frdatsk.l[!is.na(Frdatsk.l$lfl),]

par(mfrow=c(1,2))
modelg3 <- glm(lfl~ Origin+Latitude+m.date, family=gaussian,data=modeldata)
qqnorm(resid(modelg3), main="Q-Q plot for residuals")
qqline(resid(modelg3))

modeldata$lfl.log <- log(modeldata$lfl)
modelg3log<- glm(lfl.log ~ Origin+Latitude+m.date, family=gaussian,data=modeldata)
qqnorm(resid(modelg3log), main="Q-Q plot for residuals")
qqline(resid(modelg3log))
#transform looks worse! DO NOT TRANSFORM

[[29]]
[1] "lfw"

modeldata<-Frdatsk.l[!is.na(Frdatsk.l$lfw),]

par(mfrow=c(1,2))
modelg3 <- glm(lfw~ Origin+Latitude+m.date, family=gaussian,data=modeldata)
qqnorm(resid(modelg3), main="Q-Q plot for residuals")
qqline(resid(modelg3))

modeldata$lfw.log <- log(modeldata$lfw)
modelg3log<- glm(lfw.log ~ Origin+Latitude+m.date, family=gaussian,data=modeldata)
qqnorm(resid(modelg3log), main="Q-Q plot for residuals")
qqline(resid(modelg3log))
#transform looks worse! DO NOT TRANSFORM


[[31]]
[1] "rd"

modeldata<-Frdatsk.l[!is.na(Frdatsk.l$rd),]

par(mfrow=c(1,2))
modelg3 <- glm(rd~ Origin+Latitude+m.date, family=gaussian,data=modeldata)
qqnorm(resid(modelg3), main="Q-Q plot for residuals")
qqline(resid(modelg3))

modeldata$rd.log <- log(modeldata$rd)
modelg3log<- glm(rd.log ~ Origin+Latitude+m.date, family=gaussian,data=modeldata)
qqnorm(resid(modelg3log), main="Q-Q plot for residuals")
qqline(resid(modelg3log))
#transform looks worse! DO NOT TRANSFORM
