#Fr_lmer_SK
#mixed-effects models of univariate traits
#lots of climate variables! Including climate PCA
#Oct 2013
#see FrSkdata_format.R for data formatting, and Fr_lmer.R for modeling descriptions

###tests2 tests2 tests2###


#read
FrdatSK<- read.table("FrTraitClimDat_SK.txt", header=T, sep="\t",quote='"', row.names=1)

#Control and drought, REML, using lme4
#mixed effect models 
library(lme4)
library(lsmeans)
library(ggplot2)
library(plyr)

# ####Latitude####
# #maxlfwidth2
# modeldata <- FrdatSK[!is.na(FrdatSK$MaxLfWdth2),]
# # modeldata<-df[!is.na(df[[trait]]),]
# # modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# # modeldata$Mom<-as.factor(modeldata$Mom)
# modeldata$Origin <- factor(modeldata$Origin, levels=c("nat", "inv","sk"))
# 
# p1 <- ggplot(modeldata,aes(Trt, MaxLfWdth2, fill=Origin))+
#   geom_boxplot()+xlab("Stress Treatment")+
#   ylab("lfwidth2")+ 
#   theme(legend.justification=c(1,1), legend.position=c(1,1))
# p1
# 
# p2 <- ggplot(modeldata,aes(Latitude, MaxLfWdth2, color=Origin))+
#   geom_point()+xlab("latitude")+ geom_smooth(method=glm, se=FALSE)+
#   ylab("lfwidth2")+ 
#   theme(legend.justification=c(0,1), legend.position=c(0,1))
# 
# multiplot(p1, p2, cols=2)
# 
# #lflegnthH
# modeldata <- FrdatSK[!is.na(FrdatSK$LfLgth),]
# # modeldata<-df[!is.na(df[[trait]]),]
# # modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# # modeldata$Mom<-as.factor(modeldata$Mom)
# modeldata$Origin <- factor(modeldata$Origin, levels=c("nat", "inv","sk"))
# 
# p1 <- ggplot(modeldata,aes(Trt, LfLgth, fill=Origin))+
#   geom_boxplot()+xlab("Stress Treatment")+
#   ylab("max lf length at harvest")+ 
#   theme(legend.justification=c(1,1), legend.position=c(1,1))
# p1
# 
# p2 <- ggplot(modeldata,aes(Latitude, LfLgth, color=Origin))+
#   geom_point()+xlab("latitude")+ geom_smooth(method=glm, se=FALSE)+
#   ylab("max lf length at harvest")+ 
#   theme(legend.justification=c(0,1), legend.position=c(0,1))
# 
# multiplot(p1, p2, cols=2)
# 
# ####PC2####
# #maxBoltHtH
# modeldata <- FrdatSK[!is.na(FrdatSK$MaxBoltHtH),]
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
# modeldata$Origin <- factor(modeldata$Origin, levels=c("nat", "inv","sk"))
# 
# p1 <- ggplot(modeldata,aes(Trt, MaxBoltHtH, fill=Origin))+
#   geom_boxplot()+xlab("Stress Treatment")+
#   ylab("MaxBoltHtH")+ 
#   theme(legend.justification=c(1,1), legend.position=c(1,1))
# p1
# 
# p2 <- ggplot(modeldata,aes(PC2, MaxBoltHtH, color=Origin))+
#   geom_point()+xlab("PC2")+ geom_smooth(method=glm, se=FALSE)+
#   ylab("MaxBoltHtH")+ 
#   theme(legend.justification=c(0,1), legend.position=c(0,1))
# p2
# multiplot(p1, p2, cols=2)
# 
# ####bio19####
# #LfCount1.sq
# modeldata <- FrdatSK[!is.na(FrdatSK$LfCount1.sq),]
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
# modeldata$Origin <- factor(modeldata$Origin, levels=c("nat", "inv","sk"))
# 
# p1 <- ggplot(modeldata,aes(Trt, LfCount1.sq, fill=Origin))+
#   geom_boxplot()+xlab("Stress Treatment")+
#   ylab("LfCount1.sq")+ 
#   theme(legend.justification=c(1,1), legend.position=c(1,1))
# p1
# 
# p2 <- ggplot(modeldata,aes(bio19, LfCount1.sq, color=Origin))+
#   geom_point()+xlab("bio19")+ geom_smooth(method=glm, se=FALSE)+
#   ylab("LfCount1.sq")+ 
#   theme(legend.justification=c(0,1), legend.position=c(0,1))
# p2
# multiplot(p1, p2, cols=2)

###########sngl cov with interaction########################
#so for each cov and distribution

#PC1
frGLR.PC1_SKint <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="PC1"))#apply func to all gaussian traits
frPLR.PC1_SKint <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="PC1", family=poisson))#apply func to all poisson traits
boltLR.PC1_SKint <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=FrdatSK,covariate="PC1",family=binomial) #apply to single binomial trait

#PC2
frGLR.PC2_SKint <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="PC2"))#apply func to all gaussian traits
frPLR.PC2_SKint <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="PC2", family=poisson))#apply func to all poisson traits
boltLR.PC2_SKint <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=FrdatSK,covariate="PC2",family=binomial) #apply to single binomial trait

#PC3
frGLR.PC3_SKint <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="PC3"))#apply func to all gaussian traits
frPLR.PC3_SKint <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="PC3", family=poisson))#apply func to all poisson traits
boltLR.PC3_SKint <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=FrdatSK,covariate="PC3",family=binomial) #apply to single binomial trait

#bio11
frGLR.bio11_SKint <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="bio11"))#apply func to all gaussian traits
frPLR.bio11_SKint <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="bio11", family=poisson))#apply func to all poisson traits
boltLR.bio11_SKint <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=FrdatSK,covariate="bio11",family=binomial) #apply to single binomial trait

#bio9
frGLR.bio9_SKint <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="bio9"))#apply func to all gaussian traits
frPLR.bio9_SKint <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="bio9", family=poisson))#apply func to all poisson traits
boltLR.bio9_SKint <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=FrdatSK,covariate="bio9",family=binomial) #apply to single binomial trait

#bio6
frGLR.bio6_SKint <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="bio6"))#apply func to all gaussian traits
frPLR.bio6_SKint <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="bio6", family=poisson))#apply func to all poisson traits
boltLR.bio6_SKint <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=FrdatSK,covariate="bio6",family=binomial) #apply to single binomial trait

#Latitude
frGLR.lat_SKint <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="Latitude"))#apply func to all gaussian traits
frPLR.lat_SKint <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="Latitude", family=poisson))#apply func to all poisson traits
boltLR.lat_SKint <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=FrdatSK,covariate="Latitude",family=binomial) #apply to single binomial trait

#Treatment
frGLR.Trt_SKint <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="Trt"))#apply func to all gaussian traits
frPLR.Trt_SKint <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="Trt", family=poisson))#apply func to all poisson traits
boltLR.Trt_SKint <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=FrdatSK,covariate="Trt",family=binomial) #apply to single binomial trait

#which anovas have sig covariate or origin?
snglcov_SKint <- c(frGLR.PC1_SKint,frGLR.PC2_SKint,frGLR.bio11_SKint,frGLR.bio9_SKint,frGLR.bio6_SKint,frGLR.Trt_SKint,
                  frPLR.PC1_SKint,frPLR.PC3_SKint,frPLR.Trt_SKint,
                    boltLR.PC1_SKint,boltLR.PC2_SKint,boltLR.PC3_SKint,boltLR.bio11_SKint,boltLR.bio9_SKint,boltLR.bio6_SKint,boltLR.lat_SKint,boltLR.Trt_SKint)
# frPLR.bio4_SK, frPLR.bio7_SK,frPLR.bio19_SK,frGLR.lat_SK,frPLR.lat_SK,boltLR.bio4_SK,boltLR.bio7_SK,
save(snglcov_SKint, file="FrDKSKaovlists.RData")
snglcov_SKint <- load(file="FrDKSKaovlists.RData")


CGtrait_sigaov_func_Fr(frGLR.PC1_SKint, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.PC1_SKint, selectaov=1:6)
boltLR.PC1_SKint

CGtrait_sigaov_func_Fr(frGLR.PC2_SKint, selectaov=1:6) #warnings
CGtrait_sigaov_func_Fr(frPLR.PC2_SKint, selectaov=1:6)
boltLR.PC2_SKint

CGtrait_sigaov_func_Fr(frGLR.PC3_SKint, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.PC3_SKint, selectaov=1:6)
boltLR.PC3_SKint

CGtrait_sigaov_func_Fr(frGLR.bio11_SKint, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.bio11_SKint, selectaov=1:6) #warnings
boltLR.bio11_SKint

CGtrait_sigaov_func_Fr(frGLR.bio9_SKint, selectaov=1:6)
# CGtrait_sigaov_func_Fr(frPLR.bio9_SKint, selectaov=1:6) #errors
boltLR.bio9_SKint

CGtrait_sigaov_func_Fr(frGLR.bio6_SKint, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.bio6_SKint, selectaov=1:6) #warnings
boltLR.bio6_SKint

CGtrait_sigaov_func_Fr(frGLR.lat_SKint, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.lat_SKint, selectaov=1:6) #warnings
boltLR.lat_SKint

CGtrait_sigaov_func_Fr(frGLR.Trt_SKint, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.Trt_SKint, selectaov=1:6)
boltLR.Trt_SKint


##########DK+SK single traits##########################
#focus on single timept measures, poisson model fails: 
#MaxBoltHtH(all), Bolt.date (bio11), poisson measures for bio9 (just bolt.date and harvest.date...)
#use transform for Rose area, crown, shoot mass

###MaxBoltHtH##############
# CGtrait.LR_snglcov_int(trait="MaxBoltHtH", df=frdat, covariate="PC1", family=gaussian)
# CGtrait.models_snglcov_int(trait="MaxBoltHtH", df=frdat, covariate="PC1", family=gaussian)

modeldata<-FrdatSK[!is.na(FrdatSK$MaxBoltHtH),]

modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
# 
#pc1
model1<-lmer(MaxBoltHtH  ~ Origin * PC1 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(MaxBoltHtH  ~ Origin * PC1 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(MaxBoltHtH  ~ Origin * PC1 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.1744,1)
# # qchisq(558.65,1,lower=FALSE)#chisq value
# # 
# # modelint<-lmer(MaxBoltHtH  ~ Origin +PC1 +(1|Pop), family=gaussian,data=modeldata)
# # intAov <- anova(model2, modelint)
# # intAov
# # 
# # modelcov <- lmer(MaxBoltHtH  ~ Origin +(1|Pop), family=gaussian,data=modeldata)
# # covAov <- anova(modelint, modelcov)
# # covAov
# # 
# # modelO<-lmer(MaxBoltHtH ~ (1|Pop), family=gaussian,data=modeldata)
# # originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# # originAov
# # 
# # modelOC <- lmer(MaxBoltHtH  ~ PC1 +(1|Pop), family=gaussian,data=modeldata)
# # ocAov <- anova(modelint, modelOC)
# # ocAov
# #try glm
# modelg <- glm(MaxBoltHtH ~ Origin*PC1, family=gaussian,data=modeldata)
# modelg1 <- glm(MaxBoltHtH ~ Origin+PC1, family=gaussian,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# 
# modelg3<- glm(MaxBoltHtH ~ Origin, family=gaussian,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
# anova(modelg3, test="LRT")
# # modelg2<- glm(MaxBoltHtH ~ PC1, family=gaussian,data=modeldata)
# # anova(modelg2,modelg1)
# qchisq(0.5399,1,lower=FALSE)#chisq value
# 
# 
# modelg3
# summary(modelg3)
# 
# lsmeans(modelg3, ~ Origin, conf=95)
# 
# # interaction.plot(response = modeldata$MaxBoltHtH, x.factor = modeldata$PC1, trace.factor = modeldata$Origin)
# # plot(modeldata$PC1, modeldata$Origin)
# qplot(data=modeldata, PC1, MaxBoltHtH, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popMaxBoltHtH=mean(MaxBoltHtH, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,PC1, popMaxBoltHtH, color = Origin, 
#       xlab="PC1", 
#       ylab="Population mean MaxBoltHtH", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()

###Bolt.date####################
modeldata<-FrdatSK[!is.na(FrdatSK$Bolt.date),]

modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#pc1
model1<-lmer(Bolt.date  ~ Origin * PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * PC1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * PC1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.4168,1)
# qchisq(558.65,1,lower=FALSE)#chisq value
# 
modelint<-lmer(Bolt.date  ~ Origin +PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov

modelcov <- lmer(Bolt.date  ~ Origin +(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Bolt.date ~ (1|Pop/Mom), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Bolt.date  ~ PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

# #try glm
# modelg <- glm(Bolt.date ~ Origin*PC1, family=poisson,data=modeldata)
# modelg1 <- glm(Bolt.date ~ Origin+PC1, family=poisson,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# 
# modelg3<- glm(Bolt.date ~ Origin, family=poisson,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# # qchisq(0.9672,1,lower=FALSE)#chisq value
# # anova(modelg3, test="LRT")
# modelg2<- glm(Bolt.date ~ PC1, family=poisson,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# # qchisq(0.5399,1,lower=FALSE)#chisq value
# 
# 
# # modelg
# # summary(modelg)
# # 
CI.LS.poisson(modelg1)
# # 
# # interaction.plot(response = modeldata$Bolt.date, x.factor = modeldata$PC1, trace.factor = modeldata$Origin)
# # plot(modeldata$PC1, modeldata$Origin)
# qplot(data=modeldata, PC1, Bolt.date, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popBolt.date=mean(Bolt.date, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,PC1, popBolt.date, color = Origin, 
#       xlab="PC1", 
#       ylab="Population mean Bolt.date", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# # 


#bio11
model1<-lmer(Bolt.date  ~ Origin * bio11 +(1|Pop/Mom), family=poisson,data=modeldata)
#false convergence
model2<-lmer(Bolt.date  ~ Origin * bio11 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * bio11 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.8851,1)
# qchisq(558.65,1,lower=FALSE)#chisq value
# 
# modelint<-lmer(Bolt.date  ~ Origin +bio11 +(1|Pop/Mom), family=poisson,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(Bolt.date  ~ Origin +(1|Pop/Mom), family=poisson,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(Bolt.date ~ (1|Pop/Mom), family=poisson,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(Bolt.date  ~ bio11 +(1|Pop/Mom), family=poisson,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov

#try glm
modelg <- glm(Bolt.date ~ Origin*bio11, family=poisson,data=modeldata)
modelg1 <- glm(Bolt.date ~ Origin+bio11, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(Bolt.date ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(Bolt.date ~ bio11, family=poisson,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(0.5399,1,lower=FALSE)#chisq value
# 
# 
# # modelg
# # summary(modelg)
# # 
CI.LS.poisson(modelg1)
# # 
# # interaction.plot(response = modeldata$Bolt.date, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# # plot(modeldata$bio11, modeldata$Origin)
# qplot(data=modeldata, bio11, Bolt.date, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, bio11), summarize, popCount=length(Pop), popBolt.date=mean(Bolt.date, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,bio11, popBolt.date, color = Origin, 
#       xlab="bio11", 
#       ylab="Population mean Bolt.date", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# # 

#bio9
# model1<-lmer(Bolt.date  ~ Origin * bio9 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * bio9 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * bio9 +(1|blank), family=poisson,data=modeldata) # Test population effect
#false convergence
# momAov <- anova(model2,model1) # mom is sig!
# momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.8581,1)
# qchisq(558.65,1,lower=FALSE)#chisq value
# 
modelint<-lmer(Bolt.date  ~ Origin +bio9 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(Bolt.date  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Bolt.date ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Bolt.date  ~ bio9 +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

# #try glm
# modelg <- glm(Bolt.date ~ Origin*bio9, family=poisson,data=modeldata)
# modelg1 <- glm(Bolt.date ~ Origin+bio9, family=poisson,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# 
# modelg3<- glm(Bolt.date ~ Origin, family=poisson,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# # qchisq(0.9672,1,lower=FALSE)#chisq value
# # anova(modelg3, test="LRT")
# modelg2<- glm(Bolt.date ~ bio9, family=poisson,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# # qchisq(5.0702,1,lower=FALSE)#chisq value
# 
# 
# # modelg
# # summary(modelg)
# # 
CI.LS.poisson(modelg1)
# # 
# # interaction.plot(response = modeldata$Bolt.date, x.factor = modeldata$bio9, trace.factor = modeldata$Origin)
# # plot(modeldata$bio9, modeldata$Origin)
# qplot(data=modeldata, bio9, Bolt.date, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, bio9), summarize, popCount=length(Pop), popBolt.date=mean(Bolt.date, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,bio9, popBolt.date, color = Origin, 
#       xlab="bio9", 
#       ylab="Population mean Bolt.date", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# # 

###RoseAh.log##############
# CGtrait.LR_snglcov_int(trait="RoseAh.log", df=frdat, covariate="PC1", family=gaussian)
# CGtrait.models_snglcov_int(trait="RoseAh.log", df=frdat, covariate="PC1", family=gaussian)

modeldata<-FrdatSK[!is.na(FrdatSK$RoseAh.log),]

modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
# 
#trt
model1<-lmer(RoseAh.log  ~ Origin * Trt +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(RoseAh.log  ~ Origin * Trt +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RoseAh.log  ~ Origin * Trt +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(64.593,1)
# # qchisq(558.65,1,lower=FALSE)#chisq value
# # 
modelint<-lmer(RoseAh.log  ~ Origin +Trt +(1|Pop), family=gaussian,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(RoseAh.log  ~ Origin +(1|Pop), family=gaussian,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(RoseAh.log ~ (1|Pop), family=gaussian,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(RoseAh.log  ~ Trt +(1|Pop), family=gaussian,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov
# #try glm
# modelg <- glm(RoseAh.log ~ Origin*Trt, family=gaussian,data=modeldata)
# modelg1 <- glm(RoseAh.log ~ Origin+Trt, family=gaussian,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# 
# modelg3<- glm(RoseAh.log ~ Origin, family=gaussian,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
# anova(modelg3, test="LRT")
# # modelg2<- glm(RoseAh.log ~ Trt, family=gaussian,data=modeldata)
# # anova(modelg2,modelg1)
# qchisq(0.5399,1,lower=FALSE)#chisq value
# 
# 
# modelg3
# summary(modelg3)
# 
# lsmeans(modelg3, ~ Origin, conf=95)
# 
# # interaction.plot(response = modeldata$RoseAh.log, x.factor = modeldata$Trt, trace.factor = modeldata$Origin)
# # plot(modeldata$Trt, modeldata$Origin)
# qplot(data=modeldata, Trt, RoseAh.log, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, Trt), summarize, popCount=length(Pop), popRoseAh.log=mean(RoseAh.log, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,Trt, popRoseAh.log, color = Origin, 
#       xlab="Trt", 
#       ylab="Population mean RoseAh.log", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()





######################early stuff below here##################3

# ##########does Trt matter? ###########################
# FrdatSK$Trt <- droplevels(FrdatSK$Trt)
# 
# #Trt
# frGLR.Trt_SK <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="Trt"))
# #apply func to all gaussian traits. cols 38:51, 53:54 are transformed variables
# frPLR.Trt_SK <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="Trt", family=poisson))#apply func to all poisson traits
# boltLR.Trt_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="Trt",family=binomial) #apply to single binomial trait
# 
# CGtrait_sigaov_func_Fr(frGLR.Trt_SK)
# CGtrait_sigaov_func_Fr(frPLR.Trt_SK)
# boltLR.Trt_SK
# 
# ###########################################
# #so for each cov and distribution
# 
# #PC1
# frGLR.PC1_SK <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC1"))#apply func to all gaussian traits
# frPLR.PC1_SK <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC1", family=poisson))#apply func to all poisson traits
# boltLR.PC1_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="PC1",family=binomial) #apply to single binomial trait
# 
# #PC2
# frGLR.PC2_SK <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC2"))#apply func to all gaussian traits
# frPLR.PC2_SK <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC2", family=poisson))#apply func to all poisson traits
# boltLR.PC2_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="PC2",family=binomial) #apply to single binomial trait
# 
# #PC3
# frGLR.PC3_SK <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC3"))#apply func to all gaussian traits
# frPLR.PC3_SK <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC3", family=poisson))#apply func to all poisson traits
# boltLR.PC3_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="PC3",family=binomial) #apply to single binomial trait
# 
# #bio11
# frGLR.bio11_SK <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio11"))#apply func to all gaussian traits
# frPLR.bio11_SK <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio11", family=poisson))#apply func to all poisson traits
# boltLR.bio11_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="bio11",family=binomial) #apply to single binomial trait
# 
# #bio9
# frGLR.bio9_SK <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio9"))#apply func to all gaussian traits
# frPLR.bio9_SK <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio9", family=poisson))#apply func to all poisson traits
# boltLR.bio9_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="bio9",family=binomial) #apply to single binomial trait
# 
# #bio6
# frGLR.bio6_SK <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio6"))#apply func to all gaussian traits
# frPLR.bio6_SK <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio6", family=poisson))#apply func to all poisson traits
# boltLR.bio6_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="bio6",family=binomial) #apply to single binomial trait
# 
# #Latitude
# frGLR.lat_SK <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="Latitude"))#apply func to all gaussian traits
# frPLR.lat_SK <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="Latitude", family=poisson))#apply func to all poisson traits
# boltLR.lat_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="Latitude",family=binomial) #apply to single binomial trait
# 
# #which anovas have sig covariate or origin?
# snglcov_SK <- c(frGLR.PC1_SK, frPLR.PC1_SK, boltLR.PC1_SK,frGLR.PC2_SK, frPLR.PC2_SK, boltLR.PC2_SK,frGLR.PC3_SK, frPLR.PC3_SK, boltLR.PC3_SK,
#             frGLR.bio4_SK, boltLR.bio4_SK,frGLR.bio7_SK,  boltLR.bio7_SK,frGLR.bio19_SK,  boltLR.bio19_SK,
#              frGLR.lat_SK, frPLR.lat_SK, boltLR.lat_SK, frGLR.Trt_SK, frPLR.Trt_SK, boltLR.Trt_SK)
# #frPLR.bio4_SK, frPLR.bio7_SK,frPLR.bio19_SK,
# save(snglcov_SK, file="Fr_aovlists_SK.RData")
# load(file="Fr_aovlists_SK.RData")
# 
# 
# CGtrait_sigaov_func_Fr(frGLR.PC1_SK)
# CGtrait_sigaov_func_Fr(frGLR.PC2_SK)
# CGtrait_sigaov_func_Fr(frGLR.PC3_SK)
# CGtrait_sigaov_func_Fr(frGLR.bio4_SK)
# CGtrait_sigaov_func_Fr(frGLR.bio7_SK)
# CGtrait_sigaov_func_Fr(frGLR.bio19_SK)
# CGtrait_sigaov_func_Fr(frGLR.lat_SK)
# 
# CGtrait_sigaov_func_Fr(frPLR.PC1_SK)
# CGtrait_sigaov_func_Fr(frPLR.PC2_SK)
# CGtrait_sigaov_func_Fr(frPLR.PC3_SK)
# # CGtrait_sigaov_func_Fr(frPLR.bio4_SK)
# # CGtrait_sigaov_func_Fr(frPLR.bio7_SK)
# # CGtrait_sigaov_func_Fr(frPLR.bio19_SK)
# # CGtrait_sigaov_func_Fr(frPLR.lat_SK)
# 
# boltLR.PC1_SK
# boltLR.PC2_SK
# boltLR.PC3_SK
# # boltLR.bio4_SK
# # boltLR.bio7_SK
# boltLR.bio19_SK
# boltLR.lat_SK
# 