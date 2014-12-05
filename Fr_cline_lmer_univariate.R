#Fr univariate lmer models, DK only for cline ms 12/3/2014

#for dataframe construction, see Frdeath_format.R

#for modeling function, se CGtrait_lmer_func_Fr.R
#for confidence interval function, see lmerMeansCIfunc_Fr.R

#Control and drought, REML, using lme4
#mixed effect models 
library(lme4.0)
# library(lsmeans)
library(ggplot2)
library(plyr)

# library(devtools)
# install_github("dgrtwo/broom")
# library(broom)

####gaussian models####
#read
Frdatcline<- read.table("FrTraitClimDat_cline.txt", header=T, sep="\t",quote='"', row.names=1)

###Crown.log####
modeldata <- Frdatcline
modeldata$Crown.log <- log(modeldata$CrownDiam.mm)
modeldata<-modeldata[!is.na(modeldata$Crown.log),]

modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)

#PC1
modelOr <- lmer(Crown.log  ~ Origin * PC1 +Trt+(Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(Crown.log  ~ Origin * PC1 +Trt+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Crown.log  ~ Origin * PC1 +Trt+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Crown.log  ~ Origin * PC1 +Trt+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.2993,1)

modelT <- lmer(Crown.log  ~ Origin * PC1 +(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelT, model1)

modelint <- lmer(Crown.log ~ Origin +  PC1+ (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelint, modelT)

modelT
modelg <- glm(Crown.log ~ Origin*PC1, family=gaussian,data=modeldata)
summary(modelg)

CI.LS.gaussian.log(modelint)
CI.LS.gaussian.log(model1)

qplot(data=modeldata,PC1, Crown.log, color = Origin)+geom_point(position="jitter")
 
moddata <- ddply(modeldata, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popCrown.log=mean(Crown.log, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC1, popCrown.log, color = Origin, 
      xlab="PC1", 
      ylab="Population mean Crown.log", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

# modelg <- glm(Crown.log ~ Origin*PC1, family=poisson,data=modeldata)
# modelg1 <- glm(Wilt ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.8596,1,lower=FALSE)#chisq value
####Crown~Origin*trt####
modelOr <- lmer(Crown.log  ~ Origin * Trt +PC1+(Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(Crown.log  ~ Origin * Trt +PC1+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Crown.log  ~ Origin * Trt +PC1+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Crown.log  ~ Origin * Trt +PC1+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(12.75,1)

modelP <- lmer(Crown.log  ~ Origin * Trt +(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelP, model1)

modelint <- lmer(Crown.log ~ Origin +  Trt+PC1+ (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelint, model1)

modelcov <- lmer(Crown.log  ~ Origin +PC1+(1|Pop/Mom), family=gaussian,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Crown.log ~ PC1+(1|Pop/Mom), family=gaussian,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

# modelOC <- lmer(Crown.log  ~ Trt +(1|Pop/Mom), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov

modelcov
modelg <- glm(Crown.log ~ Origin+PC1, family=gaussian,data=modeldata)
summary(modelg)

####RoseAh.log~Origin*Pc1+Trt####
modeldata <- Frdatcline
modeldata$RoseAh.log <- log(modeldata$Rose.AreaH.m2)
modeldata<-modeldata[!is.na(modeldata$RoseAh.log),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)

#PC1
modelOr <- lmer(RoseAh.log  ~ Origin * PC1 +Trt +(Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(RoseAh.log  ~ Origin * PC1 +Trt +(1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(RoseAh.log  ~ Origin * PC1 +Trt +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RoseAh.log  ~ Origin * PC1 +Trt +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(6.0966,1)

modelT <- lmer(RoseAh.log  ~ Origin * PC1 +(1|Pop), family=gaussian,data=modeldata)
trtAov <- anova(model2, modelT)
trtAov

modelint<-lmer(RoseAh.log  ~ Origin +PC1 +Trt +(1|Pop), family=gaussian,data=modeldata)
intAov <- anova(model2, modelint)
intAov

model2
modelg <- glm(RoseAh.log ~ Origin*PC1+Trt, family=gaussian,data=modeldata)
summary(modelg)


# #for lsmeans, control only: 
# modeldata <- droplevels(subset(FrdatSK, Origin%in%c("inv", "nat")&Trt%in%"control"))
# modeldata<-modeldata[!is.na(modeldata$RoseAh.log),]
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
# summary(modeldata$Origin)
# summary(modeldata$Pop)
# modelint<-lmer(RoseAh.log  ~ Origin +PC1  +(1|Pop), family=gaussian,data=modeldata)
# CI.LS.gaussian.log(modelint)
# 
# #for lsmeans, dr only: 
# modeldata <- droplevels(subset(FrdatSK, Origin%in%c("inv", "nat")&Trt%in%"drought"))
# modeldata<-modeldata[!is.na(modeldata$RoseAh.log),]
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
# summary(modeldata$Origin)
# summary(modeldata$Pop)
# modelint<-lmer(RoseAh.log  ~ Origin +PC1  +(1|Pop), family=gaussian,data=modeldata)
# CI.LS.gaussian.log(modelint)
####Rose~Origin*trt####
modelOr <- lmer(RoseAh.log  ~ Origin * Trt +PC1+(Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(RoseAh.log  ~ Origin * Trt +PC1+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(RoseAh.log  ~ Origin * Trt +PC1+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RoseAh.log  ~ Origin * Trt +PC1+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(58.071,1)

modelP <- lmer(RoseAh.log  ~ Origin * Trt +(1|Pop), family=gaussian,data=modeldata)
anova(modelP, model2)

modelint<-lmer(RoseAh.log  ~ Origin +Trt +(1|Pop), family=gaussian,data=modeldata)
intAov <- anova(modelP, modelint)
intAov

modelcov <- lmer(RoseAh.log  ~ Origin +(1|Pop), family=gaussian,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

# modelO<-lmer(RoseAh.log ~ (Origin|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov

modelOC <- lmer(RoseAh.log  ~ Trt +(1|Pop), family=gaussian,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

modelint

# # #try glm
# # modelg <- glm(RoseAh.log ~ Origin*Trt, family=gaussian,data=modeldata)
modelg1 <- glm(RoseAh.log ~ Origin+Trt, family=gaussian,data=modeldata)
# # anova(modelg1, modelg, test="LRT") 
# # qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# # 
# # modelg3<- glm(RoseAh.log ~ Origin, family=gaussian,data=modeldata)
# # anova(modelg3,modelg1, test="LRT")
# # qchisq(0.9672,1,lower=FALSE)#chisq value
# # anova(modelg3, test="LRT")
# # # modelg2<- glm(RoseAh.log ~ Trt, family=gaussian,data=modeldata)
# # # anova(modelg2,modelg1)
# # qchisq(0.5399,1,lower=FALSE)#chisq value
# # 
summary(modelg1)
# # modelg3
# # summary(modelg3)
# # 
# # lsmeans(modelg3, ~ Origin, conf=95)
# # 
# # # interaction.plot(response = modeldata$RoseAh.log, x.factor = modeldata$Trt, trace.factor = modeldata$Origin)
# # # plot(modeldata$Trt, modeldata$Origin)
# # qplot(data=modeldata, Trt, RoseAh.log, color=Origin, geom = "jitter")
# # 
# # moddata <- ddply(modeldata, .(Pop, Origin, Trt), summarize, popCount=length(Pop), popRoseAh.log=mean(RoseAh.log, na.rm=TRUE))
# # 
# # #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# # qplot(data=moddata,Trt, popRoseAh.log, color = Origin, 
# #       xlab="Trt", 
# #       ylab="Population mean RoseAh.log", main="") +geom_smooth(method=glm, se=TRUE)
# # # dev.off()




############end models (mostly poisson/binomial)#############################
#read
frendcline<- read.table("FrEnd_cline.txt", header=T, sep="\t",quote='"', row.names=1)

###wilt####
modeldata <- frendcline
modeldata<-modeldata[!is.na(modeldata$Wilt),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)
# 
#PC1
modelOr <- lmer(Wilt  ~ Origin * PC1 +Trt +(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(Wilt  ~ Origin * PC1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Wilt  ~ Origin * PC1 +Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * PC1 +Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(8.5324,1)

modelT <- lmer(Wilt  ~ Origin *PC1  +(1|Pop), family=poisson,data=modeldata)
anova(model2, modelT)

modelint<-lmer(Wilt  ~ Origin +PC1 +Trt +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov<-lmer(Wilt  ~ Origin +Trt +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Wilt  ~ Trt +(1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelcov, modelO)
originAov

modelT <- lmer(Wilt  ~ (1|Pop), family=poisson,data=modeldata)
trtAov <- anova(modelO, modelT)
trtAov

modelO

lsmeans(modelO, ~Trt, conf=95)
# $`Trt lsmeans`
#     Trt   lsmean         SE df asymp.LCL asymp.UCL
# control 3.811387 0.02010636 NA  3.771979  3.850795
# drought 3.702058 0.02407447 NA  3.654873  3.749243
intc<-3.811387#cont mean
Bdr<-3.702058#dr mean
pc<-exp(intc)
pdr<-exp(Bdr)
pc #45.21311
pdr #40.53063
####wilt~origin*trt####
modelOr <- lmer(Wilt  ~ Origin * Trt +PC1 +(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(Wilt  ~ Origin * Trt +PC1+(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Wilt  ~ Origin * Trt +PC1+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * Trt +PC1+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(8.5322,1)

modelP <- lmer(Wilt  ~ Origin * Trt +(1|Pop), family=poisson,data=modeldata)
anova(modelP, model2)

modelint<-lmer(Wilt  ~ Origin +Trt +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(modelP, modelint)
intAov

modelcov <- lmer(Wilt  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

# modelO<-lmer(Wilt ~ (1|Pop), family=poisson,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov

modelOC <- lmer(Wilt  ~ Trt +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

modelOC

###Bolt.date####################
modeldata <- droplevels(frendcline)
modeldata<-modeldata[!is.na(modeldata$Bolt.date),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

summary(modeldata$Origin)
summary(modeldata$Pop)
xtabs(formula=~Origin+Trt, data=modeldata)

#pc1
#model 1 throwing errors. glms tell me Trt not sig, so remove from early models, test after random effects
modelOr <- lmer(Bolt.date  ~ Origin * PC1 +(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(Bolt.date  ~ Origin * PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Bolt.date  ~ Origin * PC1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * PC1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(8.5531,1)

modelT <- lmer(Bolt.date  ~ Origin * PC1 +(Origin|Pop), family=poisson,data=modeldata)
modelT1 <- lmer(Bolt.date  ~ Origin * PC1+Trt +(Origin|Pop), family=poisson,data=modeldata)
anova(modelT, modelT1)

modelint<-lmer(Bolt.date  ~ Origin +PC1  +(Origin|Pop), family=poisson,data=modeldata)
anova(modelint, modelT)

# modelcov <- lmer(Bolt.date  ~ Origin  +(1|Pop/Mom), family=poisson,data=modeldata)
# anova(modelcov, modelint)
# 
# modeloc <- lmer(Bolt.date  ~ PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
# anova(modeloc, modelint)
# 
# modeloc
modelT

#try glm
modelg <- glm(Bolt.date ~ Origin*PC1+Trt, family=poisson,data=modeldata)
modelg1 <- glm(Bolt.date ~ Origin*PC1, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.7422,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(Bolt.date ~ Origin+PC1, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.9672,1,lower=FALSE)#chisq value

# modelg2<- glm(RoseAh.log ~ Trt, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
qchisq(0.5399,1,lower=FALSE)#chisq value

summary(modelg1)
# # modelg3
# # summary(modelg3)
CI.LS.poisson(modelint)

qplot(data=modeldata,PC1, Bolt.date, color = Origin)+geom_point(position="jitter")

#sk included in plot 
moddata <- ddply(frend, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popBolt.date=mean(Bolt.date, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC1, popBolt.date, color = Origin, 
      xlab="PC1", 
      ylab="Population mean Bolt.date", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

# 
####bolt.date~origin*trt####
modelOr <- lmer(Bolt.date  ~ Origin * Trt +PC1+(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(Bolt.date  ~ Origin * Trt +PC1+(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Bolt.date  ~ Origin * Trt +PC1+(Origin|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * Trt +PC1+(Origin|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,modelOr) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(8.5153,1)

modelP <- lmer(Bolt.date  ~ Origin * Trt +(Origin|Pop), family=poisson,data=modeldata)
anova(modelP, model2)

modelint<-lmer(Bolt.date  ~ Origin +Trt  +PC1+(Origin|Pop), family=poisson,data=modeldata)
anova(modelint, model2)

modelcov <- lmer(Bolt.date  ~ Origin +PC1 +(Origin|Pop), family=poisson,data=modeldata)
anova(modelcov, modelint)

modelo <- lmer(Bolt.date  ~ PC1 + (Origin|Pop), family=poisson,data=modeldata)
anova(modelo, modelcov)

modelo
modelcov

###Yellow####################
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$Yellow),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)

#pc1
modelOr <- lmer(Yellow  ~ Origin * PC1 +Trt+(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(Yellow  ~ Origin * PC1 +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Yellow  ~ Origin * PC1 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Yellow  ~ Origin * PC1 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(22.405,1)

modelT <- lmer(Yellow  ~ Origin * PC1 +(1|Pop), family=poisson,data=modeldata)
anova(modelT, model2)

modelint<-lmer(Yellow  ~ Origin +PC1 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(modelT, modelint)
intAov

modelcov <- lmer(Yellow  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Yellow ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

# modelOC <- lmer(Yellow  ~ PC1 +(1|Pop), family=poisson,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov

modelO

CI.LS.poisson(modelint)
summary(modeldata$Origin)
summary(modeldata$Pop)
####yellow~origin*trt####
modelOr <- lmer(Yellow  ~ Origin * Trt+PC1+(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(Yellow  ~ Origin * Trt+PC1+(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Yellow  ~ Origin * Trt +PC1+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Yellow  ~ Origin * Trt +PC1+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(26.226,1)

modelP <- lmer(Yellow  ~ Origin * Trt +(1|Pop), family=poisson,data=modeldata)
anova(modelP, model2)

modelint<-lmer(Yellow  ~ Origin +Trt +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(modelP, modelint)
intAov

modelcov <- lmer(Yellow  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Yellow ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Yellow  ~ Trt +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

#####Mass.log#####
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$Mass.log),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)

#check pop sig only
#PC1
modelOr <- lmer(Mass.log  ~ Origin * PC1 + Trt+ (Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(Mass.log  ~ Origin * PC1 + Trt+ (1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Mass.log  ~ Origin * PC1 +Trt+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * PC1 +Trt+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.2993,1)

modelT <- lmer(Mass.log  ~ Origin * PC1 + (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelT, model1)

modelint<- lmer(Mass.log  ~ Origin + PC1 + (1|Pop/Mom), family=gaussian,data=modeldata)

modelT
modelg <- glm(Mass.log ~ Origin*PC1, family=gaussian,data=modeldata)
summary(modelg)


CI.LS.gaussian.log(modelint)
modelT.rg1 = ref.grid(modelT)


qplot(data=modeldata,PC1, Mass.log, color = Origin)+geom_point(position="jitter")

#sk included in plot 
moddata <- ddply(frend, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popMass.log=mean(Mass.log, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC1, popMass.log, color = Origin, 
      xlab="PC1", 
      ylab="Population mean Mass.log", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()
####mass~origin*trt####
modelOr <- lmer(Mass.log  ~ Origin *Trt+PC1+ (Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(Mass.log  ~ Origin *Trt+PC1+ (1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Mass.log  ~ Origin * Trt +PC1+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * Trt +PC1+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(49.061,1)

modelP <- lmer(Mass.log  ~ Origin *Trt+ (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelP, model1)

modelint<- lmer(Mass.log  ~ Origin + Trt +PC1+ (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelint, model1)

modelcov <- lmer(Mass.log  ~ Origin + PC1+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelcov, modelint)

modelO <- lmer(Mass.log  ~ PC1 +(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelO, modelcov)

modelcov

######bolt.bin####
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$bolt.bin),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)

#PC1
modelOr <- lmer(bolt.bin  ~ Origin * PC1 +Trt+(Origin|Pop/Mom), family=binomial,data=modeldata)
model1<-lmer(bolt.bin  ~ Origin * PC1 +Trt+(1|Pop/Mom), family=binomial,data=modeldata)
anova(model1, modelOr)
model2<-lmer(bolt.bin  ~ Origin * PC1 +Trt+(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * PC1 +Trt+(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.9127 ,1)

modelT <- lmer(bolt.bin  ~ Origin * PC1 +(1|Pop/Mom), family=binomial,data=modeldata)
anova(modelT, model1)

modelint<-lmer(bolt.bin  ~ Origin +PC1  +(1|Pop/Mom), family=binomial,data=modeldata)
anova(modelint, modelT)

modelT

CI.LS.binomial(modelint)

qplot(data=modeldata,PC1, bolt.bin, color = Origin)+geom_point(position="jitter")

#sk included in plot 
moddata <- ddply(frend, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popbolt.bin=mean(bolt.bin, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC1, popbolt.bin, color = Origin, 
      xlab="PC1", 
      ylab="Population mean bolt.bin", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()
####bolt.bin~origin*trt####
modelOr <- lmer(bolt.bin  ~ Origin * Trt+PC1+(Origin|Pop/Mom), family=binomial,data=modeldata)
model1<-lmer(bolt.bin  ~ Origin *Trt+PC1+(1|Pop/Mom), family=binomial,data=modeldata)
anova(model1, modelOr)
model2<-lmer(bolt.bin  ~ Origin * Trt+PC1 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * Trt +PC1+(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(28.398,1)

modelP <- lmer(bolt.bin  ~ Origin *Trt+(1|Pop/Mom), family=binomial,data=modeldata)
anova(modelP, model1)

modelint<-lmer(bolt.bin  ~ Origin +Trt +PC1 +(1|Pop/Mom), family=binomial,data=modeldata)
anova(modelint, model1)

modelcov <- lmer(bolt.bin  ~ Origin +PC1+(1|Pop/Mom), family=binomial,data=modeldata)
anova(modelcov, modelint)

modelO <- lmer(bolt.bin  ~ PC1 +(1|Pop/Mom), family=binomial,data=modeldata)
anova(modelO, modelcov)

modelcov

######end.bin####
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$end.bin),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#PC1
modelOr <- lmer(end.bin  ~ Origin * PC1 +Trt+(Origin|Pop/Mom), family=binomial,data=modeldata)
model1<-lmer(end.bin  ~ Origin * PC1 +Trt+(1|Pop/Mom), family=binomial,data=modeldata)
anova(model1, modelOr)
model2<-lmer(end.bin  ~ Origin * PC1 +Trt+(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(end.bin  ~ Origin * PC1 +Trt+(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.554,1)

# modelint<-lmer(end.bin  ~ Origin +PC1 +(1|Pop), family=binomial,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(end.bin  ~ Origin +(1|Pop), family=binomial,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(end.bin ~ (1|Pop), family=binomial,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(end.bin  ~ PC1 +(1|Pop), family=binomial,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov

#try glm
modelg <- glm(end.bin ~ Origin*PC1+Trt, family=binomial,data=modeldata)
modelgT <- glm(end.bin ~ Origin*PC1, family=binomial,data=modeldata)
anova(modelgT, modelg, test="LRT")#put in pval to get chisq value HOWEVER, for non-gaussian models, Deviance = chisq
qchisq(0.4327, 1, lower=FALSE)

modelg1 <- glm(end.bin ~ Origin+PC1, family=binomial,data=modeldata)
anova(modelg1, modelgT, test="LRT") 
# qchisq(0.09753,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(end.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(end.bin ~ PC1, family=binomial,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# # qchisq(5.0702,1,lower=FALSE)#chisq value


modelg
summary(modelg)

CI.LS.binomial(modelg1)

#overdispersion
deviance(modelg) 
summary(modelg)$dispersion 
dfr <- df.residual(modelg)
deviance(modelg)/dfr 
d_2 <- sum(residuals(modelg,"pearson")^2) 
(disp2 <- d_2/dfr)  
pchisq(d_2,df=dfr,lower.tail=FALSE) 

# # # 
# # interaction.plot(response = modeldata$end.bin, x.factor = modeldata$PC1, trace.factor = modeldata$Origin)
# # plot(modeldata$PC1, modeldata$Origin)
# qplot(data=modeldata, PC1, end.bin, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popend.bin=mean(end.bin, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,PC1, popend.bin, color = Origin, 
#       xlab="PC1", 
#       ylab="Population mean end.bin", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
####end.bin~origin*trt####
modelOr <- lmer(end.bin  ~ Origin * Trt+PC1+(Origin|Pop/Mom), family=binomial,data=modeldata)
model1<-lmer(end.bin  ~ Origin *Trt+PC1+(1|Pop/Mom), family=binomial,data=modeldata)
anova(model1, modelOr)
model2<-lmer(end.bin  ~ Origin * Trt+PC1 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(end.bin  ~ Origin * Trt +PC1+(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(30.13,1)
#try glm
modelgP <- glm(end.bin ~ Origin*Trt, family=binomial,data=modeldata)
modelg <- glm(end.bin ~ Origin*Trt+PC1, family=binomial,data=modeldata)
anova(modelgP, modelg, test="LRT")

modelg1 <- glm(end.bin ~ Origin+Trt, family=binomial,data=modeldata)
anova(modelg1, modelgP, test="LRT") 
# qchisq(0.4444,1,lower=FALSE)#put in pval to get chisq value, necessary for gaussian only

modelg3<- glm(end.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(end.bin ~ Trt, family=binomial,data=modeldata)
# anova(modelg2,modelg1, test="LRT")

#no other variable seems to have sig effect on ending... def not trt
anova(modelg2, test="LRT")
modelgr<- glm(end.bin ~ Longitude, family=binomial,data=modeldata)
anova(modelgr, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

########sla.log#####
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$sla.log),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#PC1
modelOr <- lmer(sla.log  ~ Origin * PC1 +Trt +(Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(sla.log  ~ Origin * PC1 +Trt +(1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(sla.log  ~ Origin * PC1 +Trt+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(sla.log  ~ Origin * PC1 +Trt+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.2984,1)
# 
#try glm
modelg <- glm(sla.log ~ Origin*PC1+Trt, family=gaussian,data=modeldata)
modelgT <- glm(sla.log ~ Origin*PC1, family=gaussian,data=modeldata)
anova(modelgT, modelg, test="LRT")
qchisq(0.9338,1,lower=FALSE)#put in pval to get chisq value

modelg1 <- glm(sla.log ~ Origin+PC1, family=gaussian,data=modeldata)
anova(modelg1, modelgT, test="LRT") 
qchisq(0.3571,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.1803,1,lower=FALSE)#chisq value

anova(modelg3, test="LRT")
# modelg2<- glm(sla.log ~ PC1, family=gaussian,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
qchisq(0.2667,1,lower=FALSE)#chisq value

summary(modelg3)
####sla.log~origin*trt####
modelOr <- lmer(sla.log  ~ Origin *Trt+PC1 +(Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(sla.log  ~ Origin *Trt+PC1 +(1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(sla.log  ~ Origin * Trt+PC1 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(sla.log  ~ Origin * Trt +PC1+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(1.3008,1)
#try glm
modelgP <- glm(sla.log ~ Origin*Trt+PC1, family=gaussian,data=modeldata)
modelg <- glm(sla.log ~ Origin*Trt, family=gaussian,data=modeldata)
anova(modelg, modelgP, test="LRT")
qchisq(0.1694,1,lower=FALSE)

modelg1 <- glm(sla.log ~ Origin+Trt, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.3705,1,lower=FALSE)#put in pval to get chisq value, necessary for gaussian!

modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.8396,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(sla.log ~ Trt, family=gaussian,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
qchisq(0.2667,1,lower=FALSE)#chisq value

###removed####
###Harvest.date####
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$Harvest.date),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)
# 
#PC1
modelOr <- lmer(Harvest.date  ~ Origin * PC1 +Trt +(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(Harvest.date  ~ Origin * PC1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Harvest.date  ~ Origin * PC1 +Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * PC1 +Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(272.19,1)

# 
modelT <- lmer(Harvest.date  ~ Origin * PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
trtAov <- anova(modelT, model1)
trtAov

modelint<-lmer(Harvest.date  ~ Origin +PC1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(modelint, model1)
intAov

model1

# #lsmeans w/ ctrl only
# modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")&Trt%in%"control"))
# modeldata<-modeldata[!is.na(modeldata$Harvest.date),]
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
# modelint<-lmer(Harvest.date  ~ Origin +PC1  +(1|Pop/Mom), family=poisson,data=modeldata)
# CI.LS.poisson(modelint)
# summary(modeldata$Origin)
# str(modeldata$Pop)
# 
# #lsmeans w/ dr only
# modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")&Trt%in%"drought"))
# modeldata<-modeldata[!is.na(modeldata$Harvest.date),]
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
# modelint<-lmer(Harvest.date  ~ Origin +PC1  +(1|Pop/Mom), family=poisson,data=modeldata)
# CI.LS.poisson(modelint)
# summary(modeldata$Origin)
# summary(modeldata$Pop)
####harvest~origin*trt####
modelOr <- lmer(Harvest.date  ~ Origin *Trt +PC1+(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(Harvest.date  ~ Origin *Trt +PC1+(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Harvest.date  ~ Origin * Trt +PC1+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * Trt +PC1+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(232.07,1)

modelP <- lmer(Harvest.date  ~ Origin *Trt +(1|Pop/Mom), family=poisson,data=modeldata)
anova(modelP,model1)

modelint<-lmer(Harvest.date  ~ Origin +Trt +PC1+(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(modelint, model1)
intAov

modelcov <- lmer(Harvest.date  ~ Origin +PC1+(1|Pop/Mom), family=poisson,data=modeldata)
anova(modelcov, modelint)

modelO <- lmer(Harvest.date  ~ Trt+PC1+ (1|Pop/Mom), family=poisson,data=modeldata)
anova(modelO, modelint)

modelint

###Death.date####
modeldata <- frendcline
modeldata<-modeldata[!is.na(modeldata$Death.date),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)
# 
#PC1
modelOr <- lmer(Death.date  ~ Origin * PC1 +Trt +(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(Death.date  ~ Origin * PC1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Death.date  ~ Origin * PC1 +Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death.date  ~ Origin * PC1 +Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.5618,1)

modelint<-lmer(Death.date  ~ Origin +PC1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov

modelcov<-lmer(Death.date  ~ Origin +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov
# 
modelO<-lmer(Death.date  ~ Trt +(1|Pop/Mom), family=poisson,data=modeldata)
originAov <- anova(modelcov, modelO)
originAov
# 
modelT <- lmer(Death.date  ~ (1|Pop/Mom), family=poisson,data=modeldata)
trtAov <- anova(modelO, modelT)
trtAov

modelO

lsmeans(modelO, ~Trt, conf=95)
# $`Trt lsmeans`
#     Trt   lsmean        SE df asymp.LCL asymp.UCL
# control 3.457070 0.0468643 NA  3.365217  3.548922
# drought 3.279163 0.0714766 NA  3.139072  3.419255

intc<-3.457070#cont mean
Bdr<-3.279163#dr mean
pc<-exp(intc)
pdr<-exp(Bdr)
pc #31.72389
pdr #26.55354
####death.date~origin*trt####
modelOr <- lmer(Death.date  ~ Origin *Trt +PC1 +(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(Death.date  ~ Origin *Trt +PC1+(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Death.date  ~ Origin * Trt +PC1+(Origin|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death.date  ~ Origin * Trt +PC1+(Origin|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,modelOr) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(6.8593,1)

modelP <- lmer(Death.date  ~ Origin *Trt  +(Origin|Pop/Mom), family=poisson,data=modeldata)
anova(modelP, modelOr)

modelint<-lmer(Death.date  ~ Origin +Trt +(Origin|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(modelP, modelint)
intAov

# modelcov <- lmer(Death.date  ~ Origin +(Origin|Pop/Mom), family=poisson,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(Death.date ~ (Origin|Pop/Mom), family=poisson,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(Death.date  ~ Trt +(1|Pop/Mom), family=poisson,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov

modelP

CI.LS.poisson(modelint)
xtabs(~Origin+Trt, modeldata)

qplot(data=modeldata, Trt, Death.date, color=Origin, geom = "jitter") #
interaction.plot(response = modeldata$Death.date, x.factor = modeldata$Trt, trace.factor = modeldata$Origin)

moddata <- ddply(modeldata, .(Pop, Origin, Trt), summarize, popCount=length(Pop), popDeath.date=mean(Death.date, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,Trt, popDeath.date, color = Origin, 
      xlab="Treatment", 
      ylab="Population mean Death.date", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

