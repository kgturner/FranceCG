#Fr end lmer models, DK only
#for dataframe construction, see Frdeath_format.R

#for modeling function, se CGtrait_lmer_func_Fr.R
#for confidence interval function, see lmerMeansCIfunc_Fr.R

#read
frend<- read.table("FrEnd.txt", header=T, sep="\t",quote='"', row.names=1)

#Control and drought, REML, using lme4
#mixed effect models 
library(lme4)
library(lsmeans)
library(ggplot2)
library(plyr)

#for DK only include:
subset(frend, Origin%in%c("inv", "nat"))

###########sngl cov with interaction########################
#so for each cov and distribution

#PC1
frGLR.PC1_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="PC1"))#apply func to all gaussian traits
frPLR.PC1_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="PC1", family=poisson))#apply func to all poisson traits
frBLR.PC1_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="PC1", family=binomial)) #apply to binomial trait

# #PC2
# frGLR.PC2_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="PC2"))#apply func to all gaussian traits
# frPLR.PC2_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="PC2", family=poisson))#apply func to all poisson traits
# frBLR.PC2_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="PC2", family=binomial)) #apply to binomial trait
# 
# #PC3
# frGLR.PC3_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="PC3"))#apply func to all gaussian traits
# frPLR.PC3_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="PC3", family=poisson))#apply func to all poisson traits
# frBLR.PC3_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="PC3", family=binomial)) #apply to binomial trait
# 
# #bio11
# #to fix false convergence
# frend$bio11.1 <- frend$bio11/100
# frGLR.bio11_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio11"))#apply func to all gaussian traits
# frPLR.bio11.1_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio11.1", family=poisson))#apply func to all poisson traits
# frBLR.bio11_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio11", family=binomial)) #apply to binomial trait
# 
# #bio6
# #to fix false convergence
# frend$bio6.1 <- frend$bio6/100
# frGLR.bio6_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio6"))#apply func to all gaussian traits
# frPLR.bio6.1_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio6.1", family=poisson))#apply func to all poisson traits
# frBLR.bio6_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio6", family=binomial)) #apply to binomial trait
# 
# #bio9
# #to fix false convergence
# frend$bio9.1 <- frend$bio9/100
# frGLR.bio9_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio9"))#apply func to all gaussian traits
# frPLR.bio9.1_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio9.1", family=poisson))#apply func to all poisson traits
# frBLR.bio9_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio9", family=binomial)) #apply to binomial trait
# 
# #lat
# frGLR.Latitude_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="Latitude"))#apply func to all gaussian traits
# frPLR.Latitude_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="Latitude", family=poisson))#apply func to all poisson traits
# frBLR.Latitude_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="Latitude", family=binomial)) #apply to binomial trait
# 
# #trt
# frGLR.Trt_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="Trt"))#apply func to all gaussian traits
# frPLR.Trt_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="Trt", family=poisson))#apply func to all poisson traits
# frBLR.Trt_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="Trt", family=binomial)) #apply to binomial trait

#which anovas have sig covariate or origin?
# snglcov_DKint <- c(frGLR.PC1_SKint,frGLR.PC2_SKint,frGLR.bio11_SKint,frGLR.bio9_SKint,frGLR.bio6_SKint,frGLR.Trt_SKint,
#                    frPLR.PC1_SKint,frPLR.PC3_SKint,frPLR.Trt_SKint,
#                    boltLR.PC1_SKint,boltLR.PC2_SKint,boltLR.PC3_SKint,boltLR.bio11_SKint,boltLR.bio9_SKint,boltLR.bio6_SKint,boltLR.lat_SKint,boltLR.Trt_SKint)
# # frPLR.bio4_SK, frPLR.bio7_SK,frPLR.bio19_SK,frGLR.lat_SK,frPLR.lat_SK,boltLR.bio4_SK,boltLR.bio7_SK,
# save(snglcov_SKint, file="FrDKSKaovlists.RData")
# snglcov_SKint <- load(file="FrDKSKaovlists.RData")


CGtrait_sigaov_func_Fr(frGLR.PC1_DKend, selectaov=1:6, cutoff=0.05)
CGtrait_sigaov_func_Fr(frPLR.PC1_DKend, selectaov=1:6, cutoff=0.05)
CGtrait_sigaov_func_Fr(frBLR.PC1_DKend, selectaov=1:6, cutoff=0.05)

# CGtrait_sigaov_func_Fr(frGLR.PC2_DKend, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frPLR.PC2_DKend, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frBLR.PC2_DKend, selectaov=1:6, cutoff=0.05)
# 
# CGtrait_sigaov_func_Fr(frGLR.PC3_DKend, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frPLR.PC3_DKend, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frBLR.PC3_DKend, selectaov=1:6, cutoff=0.05)
# 
# CGtrait_sigaov_func_Fr(frGLR.bio11_DKend, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frPLR.bio11.1_DKend, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frBLR.bio11_DKend, selectaov=1:6, cutoff=0.05)
# 
# CGtrait_sigaov_func_Fr(frGLR.bio6_DKend, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frPLR.bio6.1_DKend, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frBLR.bio6_DKend, selectaov=1:6, cutoff=0.05)
# 
# CGtrait_sigaov_func_Fr(frGLR.bio9_DKend, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frPLR.bio9.1_DKend, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frBLR.bio9_DKend, selectaov=1:6, cutoff=0.05)
# 
# CGtrait_sigaov_func_Fr(frGLR.Latitude_DKend, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frPLR.Latitude_DKend, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frBLR.Latitude_DKend, selectaov=1:6, cutoff=0.05)
# 
# CGtrait_sigaov_func_Fr(frGLR.Trt_DKend, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frPLR.Trt_DKend, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frBLR.Trt_DKend, selectaov=1:6, cutoff=0.05)

############final models#############################

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
modelOr <- lmer(Harvest.date  ~ Origin *Trt +(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(Harvest.date  ~ Origin *Trt +(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Harvest.date  ~ Origin * Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(232.07,1)

modelint<-lmer(Harvest.date  ~ Origin +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(modelint, model1)
intAov

modelcov <- lmer(Harvest.date  ~ Origin +(1|Pop/Mom), family=poisson,data=modeldata)
anova(modelcov, modelint)

modelO <- lmer(Harvest.date  ~ Trt+ (1|Pop/Mom), family=poisson,data=modeldata)
anova(modelO, modelint)

modelO

###wilt####
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
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
1-pchisq(6.56,1)

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
modelOr <- lmer(Wilt  ~ Origin * Trt +(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(Wilt  ~ Origin * Trt +(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Wilt  ~ Origin * Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(8.5824,1)

modelint<-lmer(Wilt  ~ Origin +Trt +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
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

###Death.date####
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
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
modelOr <- lmer(Death.date  ~ Origin *Trt +(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(Death.date  ~ Origin *Trt +(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Death.date  ~ Origin * Trt +(Origin|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death.date  ~ Origin * Trt +(Origin|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,modelOr) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(8.247,1)

modelint<-lmer(Death.date  ~ Origin +Trt +(Origin|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(modelOr, modelint)
intAov

modelcov <- lmer(Death.date  ~ Origin +(Origin|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Death.date ~ (Origin|Pop/Mom), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Death.date  ~ Trt +(1|Pop/Mom), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

modelOr

CI.LS.poisson(modelint)
xtabs(~Origin+Trt, modeldata)

###Bolt.date####################
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$Bolt.date),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

summary(modeldata$Origin)
summary(modeldata$Pop)

# check pop sig: all
#pc1
modelOr <- lmer(Bolt.date  ~ Origin * PC1 +Trt+(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(Bolt.date  ~ Origin * PC1 +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Bolt.date  ~ Origin * PC1 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * PC1 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(0.008,1)

modelT <- lmer(Bolt.date  ~ Origin * PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
anova(modelT, model1)

modelint<-lmer(Bolt.date  ~ Origin +PC1  +(1|Pop/Mom), family=poisson,data=modeldata)
anova(modelint, modelT)

modelcov <- lmer(Bolt.date  ~ Origin  +(1|Pop/Mom), family=poisson,data=modeldata)
anova(modelcov, modelint)

modeloc <- lmer(Bolt.date  ~ PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
anova(modeloc, modelint)

modeloc

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
modelOr <- lmer(Bolt.date  ~ Origin * Trt +(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(Bolt.date  ~ Origin * Trt +(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Bolt.date  ~ Origin * Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(13.285,1)

modelint<-lmer(Bolt.date  ~ Origin +Trt  +(1|Pop/Mom), family=poisson,data=modeldata)
anova(modelint, model1)

modelcov <- lmer(Bolt.date  ~ Origin  +(1|Pop/Mom), family=poisson,data=modeldata)
anova(modelcov, modelint)

modelo <- lmer(Bolt.date  ~ (1|Pop/Mom), family=poisson,data=modeldata)
anova(modelo, modelcov)

modelo

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
modelOr <- lmer(Yellow  ~ Origin * Trt+(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(Yellow  ~ Origin * Trt+(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Yellow  ~ Origin * Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Yellow  ~ Origin * Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(26.226,1)

modelint<-lmer(Yellow  ~ Origin +Trt +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
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
modelOr <- lmer(Mass.log  ~ Origin *Trt+ (Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(Mass.log  ~ Origin *Trt+ (1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Mass.log  ~ Origin * Trt +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * Trt +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(49.061,1)

modelint<- lmer(Mass.log  ~ Origin + Trt + (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelint, model1)

modelcov <- lmer(Mass.log  ~ Origin + (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelcov, modelint)

modelO <- lmer(Mass.log  ~ (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelO, modelcov)

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
modelOr <- lmer(bolt.bin  ~ Origin * Trt+(Origin|Pop/Mom), family=binomial,data=modeldata)
model1<-lmer(bolt.bin  ~ Origin *Trt+(1|Pop/Mom), family=binomial,data=modeldata)
anova(model1, modelOr)
model2<-lmer(bolt.bin  ~ Origin * Trt +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * Trt +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(28.398,1)

modelint<-lmer(bolt.bin  ~ Origin +Trt  +(1|Pop/Mom), family=binomial,data=modeldata)
anova(modelint, model1)

modelcov <- lmer(bolt.bin  ~ Origin +(1|Pop/Mom), family=binomial,data=modeldata)
anova(modelcov, modelint)

modelO <- lmer(bolt.bin  ~ (1|Pop/Mom), family=binomial,data=modeldata)
anova(modelO, modelcov)


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
modelOr <- lmer(end.bin  ~ Origin * Trt+(Origin|Pop/Mom), family=binomial,data=modeldata)
model1<-lmer(end.bin  ~ Origin *Trt+(1|Pop/Mom), family=binomial,data=modeldata)
anova(model1, modelOr)
model2<-lmer(end.bin  ~ Origin * Trt +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(end.bin  ~ Origin * Trt +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(30.13,1)
#try glm
modelg <- glm(end.bin ~ Origin*Trt, family=binomial,data=modeldata)
modelg1 <- glm(end.bin ~ Origin+Trt, family=binomial,data=modeldata)
anova(modelg1, modelg, test="LRT") 
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
modelOr <- lmer(sla.log  ~ Origin *Trt +(Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(sla.log  ~ Origin *Trt +(1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(sla.log  ~ Origin * Trt +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(sla.log  ~ Origin * Trt +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(1.3008,1)
#try glm
modelg <- glm(sla.log ~ Origin*Trt, family=gaussian,data=modeldata)
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





##########preliminary models below here##########################
#focus on single timept measures, poisson model fails: 

###Death.date w/o trt####
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$Death.date),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#check pop sig only: all
#PC1
model1<-lmer(Death.date  ~ Origin * PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Death.date  ~ Origin * PC1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death.date  ~ Origin * PC1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.91,1)

#PC2
model1<-lmer(Death.date  ~ Origin * PC2 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Death.date  ~ Origin * PC2 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death.date  ~ Origin * PC2 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.9544,1)

#PC3
model1<-lmer(Death.date  ~ Origin * PC3 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Death.date  ~ Origin * PC3 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death.date  ~ Origin * PC3 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(9.088,1)

#bio11
model1<-lmer(Death.date  ~ Origin * bio11 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Death.date  ~ Origin * bio11 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death.date  ~ Origin * bio11 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(6.9473,1)

#bio6
model1<-lmer(Death.date  ~ Origin * bio6 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Death.date  ~ Origin * bio6 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death.date  ~ Origin * bio6 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(7.3568,1)

#bio9
#false convergence 
modeldata$bio9.1 <- modeldata$bio9/100
model1<-lmer(Death.date  ~ Origin * bio9.1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Death.date  ~ Origin * bio9.1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death.date  ~ Origin * bio9.1 +(1|blank), family=poisson,data=modeldata) # Test population effect
#singular convergence
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.16,1)

#lat
model1<-lmer(Death.date  ~ Origin * Latitude +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Death.date  ~ Origin * Latitude +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death.date  ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(7.7969,1)

#trt
model1<-lmer(Death.date  ~ Origin * Trt +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Death.date  ~ Origin * Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death.date  ~ Origin * Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(6.4511,1)

modelint<-lmer(Death.date  ~ Origin +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov

modelcov <- lmer(Death.date  ~ Origin +(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Death.date ~ (1|Pop/Mom), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Death.date  ~ Trt +(1|Pop/Mom), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

CI.LS.poisson(modelint)
xtabs(~Origin+Trt, modeldata)

###Bolt.date####################
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$Bolt.date),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

# check pop sig: all
#pc1
model1<-lmer(Bolt.date  ~ Origin * PC1 +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * PC1 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * PC1 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(0.008,1)

modelT <- lmer(Bolt.date  ~ Origin * PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
anova(modelT, model1)

modelint<-lmer(Bolt.date  ~ Origin +PC1  +(1|Pop/Mom), family=poisson,data=modeldata)
anova(modelint, modelT)

modelcov <- lmer(Bolt.date  ~ Origin  +(1|Pop/Mom), family=poisson,data=modeldata)
anova(modelcov, modelint)

modeloc <- lmer(Bolt.date  ~ PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
anova(modeloc, modelint)

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
#pc2
model1<-lmer(Bolt.date  ~ Origin * PC2 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * PC2 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * PC2 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.7078,1)

modelint<-lmer(Bolt.date  ~ Origin +PC2  +(1|Pop/Mom), family=poisson,data=modeldata)
anova(modelint, model1)
CI.LS.poisson(modelint)

qplot(data=modeldata,PC2, Bolt.date, color = Origin)+geom_point(position="jitter")

#sk included in plot 
moddata <- ddply(frend, .(Pop, Origin, PC2), summarize, popCount=length(Pop), popBolt.date=mean(Bolt.date, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC2, popBolt.date, color = Origin, 
      xlab="PC2", 
      ylab="Population mean Bolt.date", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

#pc3
model1<-lmer(Bolt.date  ~ Origin * PC3 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * PC3 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * PC3 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(13.374,1)

#bio6
modeldata$bio6.1 <- modeldata$bio6/100
model1<-lmer(Bolt.date  ~ Origin * bio6.1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * bio6.1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * bio6.1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.9416,1)
# 
#trt
model1<-lmer(Bolt.date  ~ Origin * Trt +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(13.285,1)
# 
#lat
model1<-lmer(Bolt.date  ~ Origin * Latitude +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * Latitude +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.689,1)

#bio11
#false convergence
modeldata$bio11.1 <- modeldata$bio11/100
model1<-lmer(Bolt.date  ~ Origin * bio11.1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * bio11.1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * bio11.1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.4165,1)

#bio9
#false convergence
modeldata$bio9.1 <- modeldata$bio9/100
model1<-lmer(Bolt.date  ~ Origin * bio9.1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * bio9.1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * bio9.1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.9494,1)

###Yellow####################
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$Yellow),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#full models
#pc1
model1<-lmer(Yellow  ~ Origin * PC1 +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
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

modelOC <- lmer(Yellow  ~ PC1 +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

CI.LS.poisson(modelint)
summary(modeldata$Origin)
summary(modeldata$Pop)

#pc2
model1<-lmer(Yellow  ~ Origin * PC2 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Yellow  ~ Origin * PC2 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Yellow  ~ Origin * PC2 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.5478,1)
#try glm
modelg <- glm(Yellow ~ Origin*PC2, family=poisson,data=modeldata)
modelg1 <- glm(Yellow ~ Origin+PC2, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

# modelg3<- glm(Yellow ~ Origin, family=poisson,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# # qchisq(0.9672,1,lower=FALSE)#chisq value
# anova(modelg3, test="LRT")
# modelg2<- glm(Yellow ~ PC2, family=poisson,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value
# # 
# # 
# # # modelg
# # # summary(modelg)
# # # 
# CI.LS.poisson(modelg1)
# # # 
# # # interaction.plot(response = modeldata$Yellow, x.factor = modeldata$PC1, trace.factor = modeldata$Origin)
# # # plot(modeldata$PC1, modeldata$Origin)
# # qplot(data=modeldata, PC1, Yellow, color=Origin, geom = "jitter")
# # 
# # moddata <- ddply(modeldata, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popYellow=mean(Yellow, na.rm=TRUE))
# # 
# # #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# # qplot(data=moddata,PC1, popYellow, color = Origin, 
# #       xlab="PC1", 
# #       ylab="Population mean Yellow", main="") +geom_smooth(method=glm, se=TRUE)
# # # dev.off()
# # # 
#pc3
model1<-lmer(Yellow  ~ Origin * PC3 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Yellow  ~ Origin * PC3 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Yellow  ~ Origin * PC3 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(22.81,1)

modelint<-lmer(Yellow  ~ Origin +PC3 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(Yellow  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Yellow ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Yellow  ~ PC3 +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

#bio6
model1<-lmer(Yellow  ~ Origin * bio6 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Yellow  ~ Origin * bio6 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Yellow  ~ Origin * bio6 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(26.255,1)

modelint<-lmer(Yellow  ~ Origin +bio6 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(Yellow  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Yellow ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Yellow  ~ bio6 +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

#trt
model1<-lmer(Yellow  ~ Origin * Trt +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Yellow  ~ Origin * Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Yellow  ~ Origin * Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(25.346,1)

modelint<-lmer(Yellow  ~ Origin +Trt +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
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

#lat
model1<-lmer(Yellow  ~ Origin * Latitude +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Yellow  ~ Origin * Latitude +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Yellow  ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(15.297,1)

modelint<-lmer(Yellow  ~ Origin +Latitude +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(Yellow  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelOC <- lmer(Yellow  ~ Latitude +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

#bio11
#false convergence
modeldata$bio11.1 <- modeldata$bio11/100
model1<-lmer(Yellow  ~ Origin * bio11.1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Yellow  ~ Origin * bio11.1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Yellow  ~ Origin * bio11.1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(25.926,1)

modelint<-lmer(Yellow  ~ Origin +bio11.1 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(Yellow  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Yellow ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Yellow  ~ bio11.1 +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

#bio9
#false convergence
modeldata$bio9.1 <- modeldata$bio9/100
model1<-lmer(Yellow  ~ Origin * bio9.1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Yellow  ~ Origin * bio9.1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Yellow  ~ Origin * bio9.1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(20.787,1)

modelint<-lmer(Yellow  ~ Origin +bio9.1 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(Yellow  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Yellow ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Yellow  ~ bio9.1 +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov
# 

#####Mass.log#####
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$Mass.log),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)

#check pop sig only
#PC1
model1<-lmer(Mass.log  ~ Origin * PC1 + Trt+ (1|Pop/Mom), family=gaussian,data=modeldata)
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

#PC2
model1<-lmer(Mass.log  ~ Origin * PC2 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log  ~ Origin * PC2 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * PC2 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(20.107,1)

modelint<-lmer(Mass.log  ~ Origin +PC2  +(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelint, model1)
CI.LS.gaussian.log(modelint)

qplot(data=modeldata,PC2, Mass.log, color = Origin)+geom_point(position="jitter")

#sk included in plot 
moddata <- ddply(frend, .(Pop, Origin, PC2), summarize, popCount=length(Pop), popMass.log=mean(Mass.log, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC2, popMass.log, color = Origin, 
      xlab="PC2", 
      ylab="Population mean Mass.log", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()


#PC3
model1<-lmer(Mass.log  ~ Origin * PC3 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log  ~ Origin * PC3 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * PC3 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(52.525,1)
# 
#bio11
model1<-lmer(Mass.log  ~ Origin * bio11 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log  ~ Origin * bio11 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * bio11 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(10.788,1)

#bio6
model1<-lmer(Mass.log  ~ Origin * bio6 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log  ~ Origin * bio6 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * bio6 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(16.699,1)
# 
#bio9
model1<-lmer(Mass.log  ~ Origin * bio9 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log  ~ Origin * bio9 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * bio9 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(17.861,1)
# 
#lat
model1<-lmer(Mass.log  ~ Origin * Latitude +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log  ~ Origin * Latitude +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.3682,1)
# 
#trt
model1<-lmer(Mass.log  ~ Origin * Trt +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log  ~ Origin * Trt +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * Trt +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(61.831,1)
#

############Harvest.date w/o trt#######
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$Harvest.date),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#check pop sig only
#PC1
model1<-lmer(Harvest.date  ~ Origin * PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * PC1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * PC1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(232.07,1)

qplot(data=modeldata,PC1, Harvest.date, color = Origin)+geom_point(position="jitter")

#sk included in plot 
moddata <- ddply(frend, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popHarvest.date=mean(Harvest.date, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC1, popHarvest.date, color = Origin, 
      xlab="PC1", 
      ylab="Population mean Harvest.date", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

#lsmeans w/ ctrl only
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")&Trt%in%"control"))
modeldata<-modeldata[!is.na(modeldata$Harvest.date),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modelint<-lmer(Harvest.date  ~ Origin +PC1  +(1|Pop/Mom), family=poisson,data=modeldata)
CI.LS.poisson(modelint)
summary(modeldata$Origin)
str(modeldata$Pop)

#lsmeans w/ dr only
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")&Trt%in%"drought"))
modeldata<-modeldata[!is.na(modeldata$Harvest.date),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modelint<-lmer(Harvest.date  ~ Origin +PC1  +(1|Pop/Mom), family=poisson,data=modeldata)
CI.LS.poisson(modelint)
summary(modeldata$Origin)
summary(modeldata$Pop)

#PC2
model1<-lmer(Harvest.date  ~ Origin * PC2 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * PC2 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * PC2 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(232.07,1)

qplot(data=modeldata,PC2, Harvest.date, color = Origin)+geom_point(position="jitter")

#sk included in plot 
moddata <- ddply(frend, .(Pop, Origin, PC2), summarize, popCount=length(Pop), popHarvest.date=mean(Harvest.date, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC2, popHarvest.date, color = Origin, 
      xlab="PC2", 
      ylab="Population mean Harvest.date", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

#lsmeans w/ ctrl only
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")&Trt%in%"control"))
modeldata<-modeldata[!is.na(modeldata$Harvest.date),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modelint<-lmer(Harvest.date  ~ Origin +PC2  +(1|Pop/Mom), family=poisson,data=modeldata)
CI.LS.poisson(modelint)

#lsmeans w/ dr only
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")&Trt%in%"drought"))
modeldata<-modeldata[!is.na(modeldata$Harvest.date),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modelint<-lmer(Harvest.date  ~ Origin +PC2  +(1|Pop/Mom), family=poisson,data=modeldata)
CI.LS.poisson(modelint)
summary(modeldata$Origin)
summary(modeldata$Pop)

# 
#PC3
model1<-lmer(Harvest.date  ~ Origin * PC3 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * PC3 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * PC3 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(232.07,1)
# 
#bio11
#false convergence
modeldata$bio11.1 <- modeldata$bio11/100
model1<-lmer(Harvest.date  ~ Origin * bio11.1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * bio11.1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * bio11.1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(232.07,1)
# 
#bio6
#false convergence
modeldata$bio6.1 <- modeldata$bio6/100
model1<-lmer(Harvest.date  ~ Origin * bio6.1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * bio6.1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * bio6.1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(232.07,1)
# 
#lat
model1<-lmer(Harvest.date  ~ Origin * Latitude +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * Latitude +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(232.07,1)
# # 
#trt
model1<-lmer(Harvest.date  ~ Origin * Trt +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(232.07,1)

#full model
#bio9
#false convergence
modeldata$bio9.1 <- modeldata$bio9/100
model1<-lmer(Harvest.date  ~ Origin * bio9.1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * bio9.1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * bio9.1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(232.07,1)

modelint<-lmer(Harvest.date  ~ Origin +bio9.1 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

# modelcov <- lmer(Harvest.date  ~ Origin +(1|Pop), family=poisson,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(Harvest.date ~ (1|Pop), family=poisson,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(Harvest.date  ~ bio9 +(1|Pop), family=poisson,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov
# 
# # #try glm
# # modelg <- glm(Harvest.date ~ Origin*bio9, family=poisson,data=modeldata)
# # modelg1 <- glm(Harvest.date ~ Origin+bio9, family=poisson,data=modeldata)
# # anova(modelg1, modelg, test="LRT") 
# # qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# # 
# # modelg3<- glm(Harvest.date ~ Origin, family=poisson,data=modeldata)
# # anova(modelg3,modelg1, test="LRT")
# # # qchisq(0.9672,1,lower=FALSE)#chisq value
# # # anova(modelg3, test="LRT")
# # modelg2<- glm(Harvest.date ~ bio9, family=poisson,data=modeldata)
# # anova(modelg2,modelg1, test="LRT")
# # # qchisq(5.0702,1,lower=FALSE)#chisq value
# # 
# # 
# # # modelg
# # # summary(modelg)
# # # 
# CI.LS.poisson(modelg1)
# # # 
# # # interaction.plot(response = modeldata$Harvest.date, x.factor = modeldata$bio9, trace.factor = modeldata$Origin)
# # # plot(modeldata$bio9, modeldata$Origin)
# # qplot(data=modeldata, bio9, Harvest.date, color=Origin, geom = "jitter")
# # 
# # moddata <- ddply(modeldata, .(Pop, Origin, bio9), summarize, popCount=length(Pop), popHarvest.date=mean(Harvest.date, na.rm=TRUE))
# # 
# # #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# # qplot(data=moddata,bio9, popHarvest.date, color = Origin, 
# #       xlab="bio9", 
# #       ylab="Population mean Harvest.date", main="") +geom_smooth(method=glm, se=TRUE)
# # # dev.off()
# # # 
# 

######bolt.bin####
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$bolt.bin),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)


#check pop sig only
#PC1
model1<-lmer(bolt.bin  ~ Origin * PC1 +Trt+(1|Pop/Mom), family=binomial,data=modeldata)
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

#PC2
model1<-lmer(bolt.bin  ~ Origin * PC2 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * PC2 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * PC2 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.54,1)

modelint<-lmer(bolt.bin  ~ Origin +PC2  +(1|Pop/Mom), family=binomial,data=modeldata)
anova(modelint, model1)
CI.LS.binomial(modelint)

qplot(data=modeldata,PC2, bolt.bin, color = Origin)+geom_point(position="jitter")

#sk included in plot 
moddata <- ddply(frend, .(Pop, Origin, PC2), summarize, popCount=length(Pop), popbolt.bin=mean(bolt.bin, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC2, popbolt.bin, color = Origin, 
      xlab="PC2", 
      ylab="Population mean bolt.bin", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()


#PC3
model1<-lmer(bolt.bin  ~ Origin * PC3 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * PC3 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * PC3 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(25.084,1)

#bio11
model1<-lmer(bolt.bin  ~ Origin * bio11 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * bio11 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * bio11 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(13.787,1)

#bio6
model1<-lmer(bolt.bin  ~ Origin * bio6 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * bio6 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * bio6 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(14.923,1)

#bio9
model1<-lmer(bolt.bin  ~ Origin * bio9 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * bio9 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * bio9 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.5642,1)

#Latitude
model1<-lmer(bolt.bin  ~ Origin * Latitude +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * Latitude +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * Latitude +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.6818,1)

#trt
model1<-lmer(bolt.bin  ~ Origin * Trt +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * Trt +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * Trt +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.6818,1)


######end.bin####
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$end.bin),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#all full models
#PC1
model1<-lmer(end.bin  ~ Origin * PC1 +Trt+(1|Pop/Mom), family=binomial,data=modeldata)
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

#PC2
model1<-lmer(end.bin  ~ Origin * PC2 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(end.bin  ~ Origin * PC2 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(end.bin  ~ Origin * PC2 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.8637,1)

#try glm
modelg <- glm(end.bin ~ Origin*PC2, family=binomial,data=modeldata)
modelg1 <- glm(end.bin ~ Origin+PC2, family=binomial,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(end.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(end.bin ~ PC2, family=binomial,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value
# 

#PC3
model1<-lmer(end.bin  ~ Origin * PC3 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(end.bin  ~ Origin * PC3 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(end.bin  ~ Origin * PC3 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(22.824,1)
# try glm
modelg <- glm(end.bin ~ Origin*PC3, family=binomial,data=modeldata)
modelg1 <- glm(end.bin ~ Origin+PC3, family=binomial,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(end.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(end.bin ~ PC3, family=binomial,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

#bio11
model1<-lmer(end.bin  ~ Origin * bio11 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(end.bin  ~ Origin * bio11 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(end.bin  ~ Origin * bio11 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(11.151,1)
#try glm
modelg <- glm(end.bin ~ Origin*bio11, family=binomial,data=modeldata)
modelg1 <- glm(end.bin ~ Origin+bio11, family=binomial,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(end.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(end.bin ~ bio11, family=binomial,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value
# # 
#bio6
model1<-lmer(end.bin  ~ Origin * bio6 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(end.bin  ~ Origin * bio6 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(end.bin  ~ Origin * bio6 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(14.923,1)
#try glm
modelg <- glm(end.bin ~ Origin*bio6, family=binomial,data=modeldata)
modelg1 <- glm(end.bin ~ Origin+bio6, family=binomial,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(end.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(end.bin ~ bio6, family=binomial,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value
# 
#bio9
modeldata$bio9.1 <- modeldata$bio9/100
model1<-lmer(end.bin  ~ Origin * bio9.1 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(end.bin  ~ Origin * bio9.1 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(end.bin  ~ Origin * bio9.1 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.8101,1)

#try glm
modelg <- glm(end.bin ~ Origin*bio9, family=binomial,data=modeldata)
modelg1 <- glm(end.bin ~ Origin+bio9, family=binomial,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(end.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(end.bin ~ bio9, family=binomial,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

#trt
model1<-lmer(end.bin  ~ Origin * Trt +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(end.bin  ~ Origin * Trt +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(end.bin  ~ Origin * Trt +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(30.13,1)
#try glm
modelg <- glm(end.bin ~ Origin*Trt, family=binomial,data=modeldata)
modelg1 <- glm(end.bin ~ Origin+Trt, family=binomial,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

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

# # 
#Latitude
model1<-lmer(end.bin  ~ Origin * Latitude +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(end.bin  ~ Origin * Latitude +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(end.bin  ~ Origin * Latitude +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.0479,1)
#try glm
modelg <- glm(end.bin ~ Origin*Latitude, family=binomial,data=modeldata)
modelg1 <- glm(end.bin ~ Origin+Latitude, family=binomial,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(end.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(end.bin ~ Latitude, family=binomial,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value


########sla.log#####
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$sla.log),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#full models
#PC1
model1<-lmer(sla.log  ~ Origin * PC1 +Trt +(1|Pop/Mom), family=gaussian,data=modeldata)
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
# # 
#PC2
model1<-lmer(sla.log  ~ Origin * PC2 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(sla.log  ~ Origin * PC2 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(sla.log  ~ Origin * PC2 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.2991,1)

#try glm
modelg <- glm(sla.log ~ Origin*PC2, family=gaussian,data=modeldata)
modelg1 <- glm(sla.log ~ Origin+PC2, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(sla.log ~ PC2, family=gaussian,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value
 
#PC3
model1<-lmer(sla.log  ~ Origin * PC3 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(sla.log  ~ Origin * PC3 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(sla.log  ~ Origin * PC3 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(1.2996,1)
#try glm
modelg <- glm(sla.log ~ Origin*PC3, family=gaussian,data=modeldata)
modelg1 <- glm(sla.log ~ Origin+PC3, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(sla.log ~ PC3, family=gaussian,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

# 
#bio11
model1<-lmer(sla.log  ~ Origin * bio11 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(sla.log  ~ Origin * bio11 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(sla.log  ~ Origin * bio11 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(6.2833,1)
#try glm
modelg <- glm(sla.log ~ Origin*bio11, family=gaussian,data=modeldata)
modelg1 <- glm(sla.log ~ Origin+bio11, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(sla.log ~ bio11, family=gaussian,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

#bio6
model1<-lmer(sla.log  ~ Origin * bio6 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(sla.log  ~ Origin * bio6 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(sla.log  ~ Origin * bio6 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(17.861,1)
#try glm
modelg <- glm(sla.log ~ Origin*bio6, family=gaussian,data=modeldata)
modelg1 <- glm(sla.log ~ Origin+bio6, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(sla.log ~ bio6, family=gaussian,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value
# 
#bio9
model1<-lmer(sla.log  ~ Origin * bio9 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(sla.log  ~ Origin * bio9 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(sla.log  ~ Origin * bio9 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.2969,1)

#try glm
modelg <- glm(sla.log ~ Origin*bio9, family=gaussian,data=modeldata)
modelg1 <- glm(sla.log ~ Origin+bio9, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(sla.log ~ bio9, family=gaussian,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

#lat
model1<-lmer(sla.log  ~ Origin * Latitude +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(sla.log  ~ Origin * Latitude +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(sla.log  ~ Origin * Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(17.861,1)
#try glm
modelg <- glm(sla.log ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(sla.log ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(sla.log ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

#trt
model1<-lmer(sla.log  ~ Origin + Trt +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(sla.log  ~ Origin + Trt +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(sla.log  ~ Origin + Trt +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(1.3008,1)
#try glm
modelg <- glm(sla.log ~ Origin*Trt, family=gaussian,data=modeldata)
modelg1 <- glm(sla.log ~ Origin+Trt, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(sla.log ~ Trt, family=gaussian,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value


####Wilt w/o trt####
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$Wilt),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#full models
#PC1
model1<-lmer(Wilt  ~ Origin * PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * PC1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * PC1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(6.4803,1)

modelint<-lmer(Wilt  ~ Origin +PC1 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(Wilt  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Wilt ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Wilt  ~ PC1 +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

#PC2
model1<-lmer(Wilt  ~ Origin * PC2 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * PC2 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * PC2 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.1629,1)

#try glm
modelg <- glm(Wilt ~ Origin*PC2, family=poisson,data=modeldata)
modelg1 <- glm(Wilt ~ Origin+PC2, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(Wilt ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(Wilt ~ PC2, family=poisson,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(0.5399,1,lower=FALSE)#chisq value

#lsmeans for ctrl only
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")&Trt%in%"control"))
modeldata<-modeldata[!is.na(modeldata$Wilt),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modelg1 <- glm(Wilt ~ Origin+PC2, family=poisson,data=modeldata)
CI.LS.poisson(modelg1)

#lsmeans for dr only
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")&Trt%in%"drought"))
modeldata<-modeldata[!is.na(modeldata$Wilt),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modelg1 <- glm(Wilt ~ Origin+PC2, family=poisson,data=modeldata)
CI.LS.poisson(modelg1)

#PC3
model1<-lmer(Wilt  ~ Origin * PC3 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * PC3 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * PC3 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.5574,1)

#try glm
modelg <- glm(Wilt ~ Origin*PC3, family=poisson,data=modeldata)
modelg1 <- glm(Wilt ~ Origin+PC3, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(Wilt ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(Wilt ~ PC3, family=poisson,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(0.5399,1,lower=FALSE)#chisq value

#bio11
#false convergence
modeldata$bio11.1 <- modeldata$bio11/100
model1<-lmer(Wilt  ~ Origin * bio11.1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * bio11.1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * bio11.1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(6.6767,1)

modelint<-lmer(Wilt  ~ Origin +bio11.1 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(Wilt  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Wilt ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Wilt  ~ bio11.1 +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

# 
# # modelg
# # summary(modelg)
# # 
CI.LS.poisson(modelint)
# # 
# # interaction.plot(response = modeldata$Wilt, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# # plot(modeldata$bio11, modeldata$Origin)
# qplot(data=modeldata, bio11, Wilt, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, bio11), summarize, popCount=length(Pop), popWilt=mean(Wilt, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,bio11, popWilt, color = Origin, 
#       xlab="bio11", 
#       ylab="Population mean Wilt", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# # 

#bio6
# #false convergence? 
# modeldata$bio6.1 <- modeldata$bio6/100
model1<-lmer(Wilt  ~ Origin * bio6 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * bio6 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * bio6 +(1|blank), family=poisson,data=modeldata) # Test population effect

momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(7.367,1)

modelint<-lmer(Wilt  ~ Origin +bio6 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(Wilt  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Wilt ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Wilt  ~ bio6 +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

#bio9
#false convergence
modeldata$bio9.1 <- modeldata$bio9/100
model1<-lmer(Wilt  ~ Origin * bio9.1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * bio9.1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * bio9.1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(6.147,1)
# 
modelint<-lmer(Wilt  ~ Origin +bio9.1 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(Wilt  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Wilt ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Wilt  ~ bio9.1 +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov
# 
# # modelg
# # summary(modelg)
# # 
CI.LS.poisson(modelg1)
# # 
# # interaction.plot(response = modeldata$Wilt, x.factor = modeldata$bio9, trace.factor = modeldata$Origin)
# # plot(modeldata$bio9, modeldata$Origin)
# qplot(data=modeldata, bio9, Wilt, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, bio9), summarize, popCount=length(Pop), popWilt=mean(Wilt, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,bio9, popWilt, color = Origin, 
#       xlab="bio9", 
#       ylab="Population mean Wilt", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# # 

#lat
model1<-lmer(Wilt  ~ Origin * Latitude +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * Latitude +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.0117,1)

modelint<-lmer(Wilt  ~ Origin +Latitude +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(Wilt  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Wilt ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Wilt  ~ Latitude +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

#trt
model1<-lmer(Wilt  ~ Origin * Trt +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(8.8447,1)

modelint<-lmer(Wilt  ~ Origin +Trt +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(Wilt  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Wilt ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Wilt  ~ Trt +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

####models with Trt included####
#not working
# frPLR.harvest_DKtrt<- lapply(names(frend)[c(37:43)],function(n) CGtrait.LR_snglcov_trt("Harvest.date",subset(frend, Origin%in%c("inv", "nat")), covariate=n, family=poisson))
# frPLR.wilt_DKtrt<- lapply(names(frend)[c(37:43)],function(n) CGtrait.LR_snglcov_trt("Wilt",subset(frend, Origin%in%c("inv", "nat")), covariate=n, family=poisson))
# frPLR.death_DKtrt<- lapply(names(frend)[c(37:43)],function(n) CGtrait.LR_snglcov_trt("Death.date",subset(frend, Origin%in%c("inv", "nat")), covariate=n, family=poisson))
# 
# 
# names(frend)[c(37:43)]
# CGtrait_sigaov_func_Fr(frPLR.harvest_DKtrt, selectaov=1:7, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frPLR.wilt_DKtrt, selectaov=1:7, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frPLR.death_DKtrt, selectaov=1:7, cutoff=0.05)

###Harvest.date####
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$Harvest.date),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
# 
#PC1
model1<-lmer(Harvest.date  ~ Origin * PC1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
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
# 
#PC2
model1<-lmer(Harvest.date  ~ Origin * PC2 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * PC2 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * PC2 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(27.237,1)
modelint<-lmer(Harvest.date  ~ Origin +PC2 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov

modelT <- lmer(Harvest.date  ~ Origin * PC2+(1|Pop/Mom), family=poisson,data=modeldata)
trtAov <- anova(model1, modelT)
trtAov
# # # 
#PC3
model1<-lmer(Harvest.date  ~ Origin * PC3 +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * PC3 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * PC3 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(41.522,1)
# # 
modelint<-lmer(Harvest.date  ~ Origin +PC3 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov

modelcov<-lmer(Harvest.date  ~ Origin +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Harvest.date  ~ Trt +(1|Pop/Mom), family=poisson,data=modeldata)
originAov <- anova(modelcov, modelO)
originAov

modelT <- lmer(Harvest.date  ~ (1|Pop/Mom), family=poisson,data=modeldata)
trtAov <- anova(modelO, modelT)
trtAov
# # # 
#bio11
#false convergence
modeldata$bio11.1 <- modeldata$bio11/100
model1<-lmer(Harvest.date  ~ Origin * bio11.1 +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * bio11.1 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * bio11.1 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(9.2195,1)
# 
modelint<-lmer(Harvest.date  ~ Origin +bio11.1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov

modelcov<-lmer(Harvest.date  ~ Origin +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelOC<-lmer(Harvest.date  ~ bio11.1+Trt +(1|Pop/Mom), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

modelT<-lmer(Harvest.date  ~ bio11.1+(1|Pop/Mom), family=poisson,data=modeldata)
(trtAov <- anova(modelOC, modelT))
# # # # 
#bio6
#false convergence
modeldata$bio6.1 <- modeldata$bio6/100
model1<-lmer(Harvest.date  ~ Origin * bio6.1 +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * bio6.1 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * bio6.1 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(12.354,1)

modelint<-lmer(Harvest.date  ~ Origin +bio6.1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov

modelcov<-lmer(Harvest.date  ~ Origin +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Harvest.date  ~ Trt +(1|Pop/Mom), family=poisson,data=modeldata)
originAov <- anova(modelcov, modelO)
originAov
 
modelT <- lmer(Harvest.date  ~ (1|Pop/Mom), family=poisson,data=modeldata)
trtAov <- anova(modelO, modelT)
trtAov
# # 
#bio9
#false convergence
modeldata$bio9.1 <- modeldata$bio9/100
model1<-lmer(Harvest.date  ~ Origin * bio9.1 +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * bio9.1 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * bio9.1 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# # 1-pchisq(5.7915,1)
# 
modelint<-lmer(Harvest.date  ~ Origin +bio9.1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov

modelcov<-lmer(Harvest.date  ~ Origin +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Harvest.date  ~ bio9.1+Trt +(1|Pop/Mom), family=poisson,data=modeldata)
originAov <- anova(modelint, modelO)
originAov
# # 
modelT <- lmer(Harvest.date  ~ Origin + bio9.1 +(1|Pop/Mom), family=poisson,data=modeldata)
trtAov <- anova(modelint, modelT)
trtAov
# # 
#lat
model1<-lmer(Harvest.date  ~ Origin * Latitude +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * Latitude +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * Latitude +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# # 1-pchisq(64.593,1)
# # # 
modelint<-lmer(Harvest.date  ~ Origin +Latitude +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov
# # 
modelT <- lmer(Harvest.date  ~ Origin * Latitude +(1|Pop/Mom), family=poisson,data=modeldata)
trtAov <- anova(model1, modelT)
trtAov


###wilt####
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$Wilt),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
# 
#PC1
model1<-lmer(Wilt  ~ Origin * PC1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * PC1 +Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * PC1 +Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(6.56,1)

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

#PC2
model1<-lmer(Wilt  ~ Origin * PC2 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * PC2 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * PC2 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.2004,1)
# #try glm
modelg <- glm(Wilt ~ Origin*PC2+Trt, family=poisson,data=modeldata)
modelg1 <- glm(Wilt ~ Origin+PC2+Trt, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#chisq value

modelgT<- glm(Wilt ~ Origin*PC2, family=poisson,data=modeldata)
anova(modelgT,modelg, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value

#PC3
model1<-lmer(Wilt  ~ Origin * PC3 +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * PC3 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * PC3 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.5748,1)
# # 
# #try glm
modelg <- glm(Wilt ~ Origin*PC3+Trt, family=poisson,data=modeldata)
modelg1 <- glm(Wilt ~ Origin+PC3+Trt, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#chisq value

modelg2 <- glm(Wilt ~ Origin + Trt, family=poisson, data=modeldata)
anova(modelg2, modelg1, test="LRT")

modelg3 <- glm(Wilt ~ PC3+Trt, family=poisson, data=modeldata)
anova(modelg3, modelg1, test="LRT")

modelgT<- glm(Wilt ~ PC3, family=poisson,data=modeldata)
anova(modelgT,modelg3, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
# # # # 
#bio11
#false convergence
modeldata$bio11.1 <- modeldata$bio11/100
model1<-lmer(Wilt  ~ Origin * bio11.1 +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * bio11.1 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * bio11.1 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(6.4321,1)

modelint<-lmer(Wilt  ~ Origin +bio11.1 +Trt +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov<-lmer(Wilt  ~ Origin +Trt +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov
# # # # # 
#bio6
#false convergence
modeldata$bio6.1 <- modeldata$bio6/100
model1<-lmer(Wilt  ~ Origin * bio6.1 +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * bio6.1 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * bio6.1 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(7.03,1)

modelint<-lmer(Wilt  ~ Origin +bio6.1 +Trt +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov<-lmer(Wilt  ~ Origin +Trt +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov
# # # 
#bio9
#false convergence
modeldata$bio9.1 <- modeldata$bio9/100
model1<-lmer(Wilt  ~ Origin * bio9.1 +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * bio9.1 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * bio9.1 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(5.9548,1)
# # 
modelint<-lmer(Wilt  ~ Origin +bio9.1 +Trt +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov<-lmer(Wilt  ~ Origin +Trt +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov
#
modelT<-lmer(Wilt  ~ Origin * bio9.1 +(1|Pop), family=poisson,data=modeldata)
(trtAov <- anova(model2, modelT))

#lat
model1<-lmer(Wilt  ~ Origin * Latitude +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * Latitude +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * Latitude +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.8515,1)
# # # # 
modelint<-lmer(Wilt  ~ Origin +Latitude +Trt +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov<-lmer(Wilt  ~ Origin +Trt +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov
#
modelT<-lmer(Wilt  ~ Origin * Latitude +(1|Pop), family=poisson,data=modeldata)
(trtAov <- anova(model2, modelT))


###Death.date####
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$Death.date),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
# 
#PC1
model1<-lmer(Death.date  ~ Origin * PC1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
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

# 
#PC2
model1<-lmer(Death.date  ~ Origin * PC2 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Death.date  ~ Origin * PC2 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death.date  ~ Origin * PC2 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(3.0825,1)

modelint<-lmer(Death.date  ~ Origin +PC2 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov

modelcov<-lmer(Death.date  ~ Origin +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

#PC3
model1<-lmer(Death.date  ~ Origin * PC3 +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Death.date  ~ Origin * PC3 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death.date  ~ Origin * PC3 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(5.1332,1)
# # # 
modelint<-lmer(Death.date  ~ Origin +PC3 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov

modelcov<-lmer(Death.date  ~ Origin +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov
# # # # # 
#bio11
#false convergence
modeldata$bio11.1 <- modeldata$bio11/100
model1<-lmer(Death.date  ~ Origin * bio11.1 +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Death.date  ~ Origin * bio11.1 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death.date  ~ Origin * bio11.1 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.6424,1)

modelint<-lmer(Death.date  ~ Origin +bio11.1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov

modelcov<-lmer(Death.date  ~ Origin +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov
# # # # # # 
#bio6
#false convergence
modeldata$bio6.1 <- modeldata$bio6/100
model1<-lmer(Death.date  ~ Origin * bio6.1 +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Death.date  ~ Origin * bio6.1 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death.date  ~ Origin * bio6.1 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(5.0136,1)
# 
modelint<-lmer(Death.date  ~ Origin +bio6.1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov

modelcov<-lmer(Death.date  ~ Origin +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov
# # # # 
#bio9
#false convergence
modeldata$bio9.1 <- modeldata$bio9/100
model1<-lmer(Death.date  ~ Origin * bio9.1 +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Death.date  ~ Origin * bio9.1 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death.date  ~ Origin * bio9.1 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.0829,1)
# # # 
modelint<-lmer(Death.date  ~ Origin +bio9.1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov
#
modelcov<-lmer(Death.date  ~ Origin +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Death.date  ~ bio9.1+Trt +(1|Pop/Mom), family=poisson,data=modeldata)
(ocAov <- anova(modelint, modelOC))
# 
modelT <- lmer(Death.date  ~ bio9.1+(1|Pop/Mom), family=poisson,data=modeldata)
trtAov <- anova(modelOC, modelT)
trtAov

#lat
#false convergence
modeldata$lat.1 <- modeldata$Latitude/100
model1<-lmer(Death.date  ~ Origin * lat.1 +Trt+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Death.date  ~ Origin * lat.1 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death.date  ~ Origin * lat.1 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.6215,1)
# # # # # 
modelint<-lmer(Death.date  ~ Origin +lat.1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov
# #
modelcov<-lmer(Death.date  ~ Origin +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov
