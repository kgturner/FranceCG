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

#PC2
frGLR.PC2_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="PC2"))#apply func to all gaussian traits
frPLR.PC2_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="PC2", family=poisson))#apply func to all poisson traits
frBLR.PC2_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="PC2", family=binomial)) #apply to binomial trait

#PC3
frGLR.PC3_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="PC3"))#apply func to all gaussian traits
frPLR.PC3_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="PC3", family=poisson))#apply func to all poisson traits
frBLR.PC3_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="PC3", family=binomial)) #apply to binomial trait

#bio11
#to fix false convergence
frend$bio11.1 <- frend$bio11/100
frGLR.bio11_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio11"))#apply func to all gaussian traits
frPLR.bio11.1_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio11.1", family=poisson))#apply func to all poisson traits
frBLR.bio11_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio11", family=binomial)) #apply to binomial trait

#bio6
#to fix false convergence
frend$bio6.1 <- frend$bio6/100
frGLR.bio6_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio6"))#apply func to all gaussian traits
frPLR.bio6.1_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio6.1", family=poisson))#apply func to all poisson traits
frBLR.bio6_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio6", family=binomial)) #apply to binomial trait

#bio9
#to fix false convergence
frend$bio9.1 <- frend$bio9/100
frGLR.bio9_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio9"))#apply func to all gaussian traits
frPLR.bio9.1_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio9.1", family=poisson))#apply func to all poisson traits
frBLR.bio9_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="bio9", family=binomial)) #apply to binomial trait

#lat
frGLR.Latitude_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="Latitude"))#apply func to all gaussian traits
frPLR.Latitude_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="Latitude", family=poisson))#apply func to all poisson traits
frBLR.Latitude_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="Latitude", family=binomial)) #apply to binomial trait

#trt
frGLR.Trt_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="Trt"))#apply func to all gaussian traits
frPLR.Trt_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="Trt", family=poisson))#apply func to all poisson traits
frBLR.Trt_DKend <- lapply(names(subset(frend, Origin%in%c("inv", "nat")))[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,subset(frend, Origin%in%c("inv", "nat")), covariate="Trt", family=binomial)) #apply to binomial trait

#which anovas have sig covariate or origin?
# snglcov_DKint <- c(frGLR.PC1_SKint,frGLR.PC2_SKint,frGLR.bio11_SKint,frGLR.bio9_SKint,frGLR.bio6_SKint,frGLR.Trt_SKint,
#                    frPLR.PC1_SKint,frPLR.PC3_SKint,frPLR.Trt_SKint,
#                    boltLR.PC1_SKint,boltLR.PC2_SKint,boltLR.PC3_SKint,boltLR.bio11_SKint,boltLR.bio9_SKint,boltLR.bio6_SKint,boltLR.lat_SKint,boltLR.Trt_SKint)
# # frPLR.bio4_SK, frPLR.bio7_SK,frPLR.bio19_SK,frGLR.lat_SK,frPLR.lat_SK,boltLR.bio4_SK,boltLR.bio7_SK,
# save(snglcov_SKint, file="FrDKSKaovlists.RData")
# snglcov_SKint <- load(file="FrDKSKaovlists.RData")


CGtrait_sigaov_func_Fr(frGLR.PC1_DKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.PC1_DKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.PC1_DKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.PC2_DKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.PC2_DKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.PC2_DKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.PC3_DKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.PC3_DKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.PC3_DKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.bio11_DKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.bio11.1_DKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.bio11_DKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.bio6_DKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.bio6.1_DKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.bio6_DKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.bio9_DKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.bio9.1_DKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.bio9_DKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.Latitude_DKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.Latitude_DKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.Latitude_DKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.Trt_DKend, selectaov=1:6, cutoff=0.05)
CGtrait_sigaov_func_Fr(frPLR.Trt_DKend, selectaov=1:6, cutoff=0.05)
CGtrait_sigaov_func_Fr(frBLR.Trt_DKend, selectaov=1:6, cutoff=0.05)


##########DK only single traits##########################
#focus on single timept measures, poisson model fails: 

########
###Death.date###
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
1-pchisq(4.9443,1)

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

###Bolt.date####################
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$Bolt.date),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

# check pop sig: all
#pc1
model1<-lmer(Bolt.date  ~ Origin * PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * PC1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * PC1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.1616,1)
# 
#pc2
model1<-lmer(Bolt.date  ~ Origin * PC2 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * PC2 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * PC2 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.5538,1)

#pc3
model1<-lmer(Bolt.date  ~ Origin * PC3 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * PC3 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * PC3 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(17.779,1)

#bio6
model1<-lmer(Bolt.date  ~ Origin * bio6 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * bio6 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * bio6 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(12.104,1)
# 
#trt
model1<-lmer(Bolt.date  ~ Origin * Trt +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(12.538,1)
# 
#lat
model1<-lmer(Bolt.date  ~ Origin * Latitude +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * Latitude +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(5.4115,1)

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
1-pchisq(5.2321,1)

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
model1<-lmer(Yellow  ~ Origin * PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Yellow  ~ Origin * PC1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Yellow  ~ Origin * PC1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(21.955,1)

modelint<-lmer(Yellow  ~ Origin +PC1 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
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

#pc2
model1<-lmer(Yellow  ~ Origin * PC2 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Yellow  ~ Origin * PC2 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Yellow  ~ Origin * PC2 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.5649,1)
#try glm
modelg <- glm(Yellow ~ Origin*PC2, family=poisson,data=modeldata)
modelg1 <- glm(Yellow ~ Origin+PC2, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(Yellow ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(Yellow ~ PC2, family=poisson,data=modeldata)
anova(modelg2,modelg1, test="LRT")
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

modelO<-lmer(Yellow ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

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
###############
###Mass.log###
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$Mass.log),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#check pop sig only
#PC1
model1<-lmer(Mass.log  ~ Origin * PC1 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log  ~ Origin * PC1 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * PC1 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.2919,1)

#PC2
model1<-lmer(Mass.log  ~ Origin * PC2 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log  ~ Origin * PC2 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * PC2 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(20.107,1)

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
#############
######Harvest.date###
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

#PC2
model1<-lmer(Harvest.date  ~ Origin * PC2 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * PC2 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * PC2 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(232.07,1)
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
##########
###bolt.bin###
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$bolt.bin),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#check pop sig only
#PC1
model1<-lmer(bolt.bin  ~ Origin * PC1 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * PC1 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * PC1 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.3804,1)

#PC2
model1<-lmer(bolt.bin  ~ Origin * PC2 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * PC2 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * PC2 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.8399,1)

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
1-pchisq(11.151,1)

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
1-pchisq(2.7865,1)

#Latitude
model1<-lmer(bolt.bin  ~ Origin * Latitude +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * Latitude +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * Latitude +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.0104,1)

#trt
model1<-lmer(bolt.bin  ~ Origin * Trt +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * Trt +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * Trt +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(30.13,1)

################
###end.bin###
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$end.bin),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#all full models
#PC1
model1<-lmer(end.bin  ~ Origin * PC1 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(end.bin  ~ Origin * PC1 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(end.bin  ~ Origin * PC1 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.554,1)

modelint<-lmer(end.bin  ~ Origin +PC1 +(1|Pop), family=binomial,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(end.bin  ~ Origin +(1|Pop), family=binomial,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(end.bin ~ (1|Pop), family=binomial,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(end.bin  ~ PC1 +(1|Pop), family=binomial,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

# #try glm
# modelg <- glm(end.bin ~ Origin*PC1, family=binomial,data=modeldata)
# modelg1 <- glm(end.bin ~ Origin+PC1, family=binomial,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# 
# modelg3<- glm(end.bin ~ Origin, family=binomial,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# # qchisq(0.9672,1,lower=FALSE)#chisq value
# anova(modelg3, test="LRT")
# modelg2<- glm(end.bin ~ PC1, family=binomial,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# # qchisq(5.0702,1,lower=FALSE)#chisq value
# 
# # 
# # # modelg
# # # summary(modelg)
# # # 
# CI.LS.binomial(modelg1)
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
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(end.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(end.bin ~ PC2, family=binomial,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value
# 
# # 
# # # modelg
# # # summary(modelg)
# # # 
# CI.LS.binomial(modelg1)
# # # 
# # interaction.plot(response = modeldata$end.bin, x.factor = modeldata$PC2, trace.factor = modeldata$Origin)
# # plot(modeldata$PC2, modeldata$Origin)
# qplot(data=modeldata, PC2, end.bin, color=Origin, geom = "jitter") 
# 
# moddata <- ddply(modeldata, .(Pop, Origin, PC2), summarize, popCount=length(Pop), popend.bin=mean(end.bin, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,PC2, popend.bin, color = Origin, 
#       xlab="PC2", 
#       ylab="Population mean end.bin", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# 
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
try glm
modelg <- glm(end.bin ~ Origin*PC3, family=binomial,data=modeldata)
modelg1 <- glm(end.bin ~ Origin+PC3, family=binomial,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(end.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(end.bin ~ PC3, family=binomial,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

# # 
# # # modelg
# # # summary(modelg)
# # # 
# CI.LS.binomial(modelg1)
# # # 
# # interaction.plot(response = modeldata$end.bin, x.factor = modeldata$PC3, trace.factor = modeldata$Origin)
# # plot(modeldata$PC2, modeldata$Origin)
# qplot(data=modeldata, PC3, end.bin, color=Origin, geom = "jitter") 
# 
# moddata <- ddply(modeldata, .(Pop, Origin, PC3), summarize, popCount=length(Pop), popend.bin=mean(end.bin, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,PC3, popend.bin, color = Origin, 
#       xlab="PC3", 
#       ylab="Population mean end.bin", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# 
# 
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
modelg2<- glm(end.bin ~ bio11, family=binomial,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value
# 
# # 
# # # modelg
# # # summary(modelg)
# # # 
# CI.LS.binomial(modelg1)
# # # 
# # interaction.plot(response = modeldata$end.bin, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# # plot(modeldata$bio11, modeldata$Origin)
# qplot(data=modeldata, bio11, end.bin, color=Origin, geom = "jitter") 
# 
# moddata <- ddply(modeldata, .(Pop, Origin, bio11), summarize, popCount=length(Pop), popend.bin=mean(end.bin, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,bio11, popend.bin, color = Origin, 
#       xlab="bio11", 
#       ylab="Population mean end.bin", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# 
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
modelg2<- glm(end.bin ~ bio6, family=binomial,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value
# 
# # 
# # # modelg
# # # summary(modelg)
# # # 
# CI.LS.binomial(modelg1)
# # # 
# # interaction.plot(response = modeldata$end.bin, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# # plot(modeldata$bio11, modeldata$Origin)
# qplot(data=modeldata, bio6, end.bin, color=Origin, geom = "jitter") 
# 
# moddata <- ddply(modeldata, .(Pop, Origin, bio6), summarize, popCount=length(Pop), popend.bin=mean(end.bin, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,bio6, popend.bin, color = Origin, 
#       xlab="bio6", 
#       ylab="Population mean end.bin", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
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
modelg2<- glm(end.bin ~ bio9, family=binomial,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

# # 
# # # modelg
# # # summary(modelg)
# # # 
# CI.LS.binomial(modelg1)
# # # 
# # interaction.plot(response = modeldata$end.bin, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# # plot(modeldata$bio11, modeldata$Origin)
# qplot(data=modeldata, bio9, end.bin, color=Origin, geom = "jitter") 
# 
# moddata <- ddply(modeldata, .(Pop, Origin, bio9), summarize, popCount=length(Pop), popend.bin=mean(end.bin, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,bio9, popend.bin, color = Origin, 
#       xlab="bio9", 
#       ylab="Population mean end.bin", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# 
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
modelg2<- glm(end.bin ~ Trt, family=binomial,data=modeldata)
anova(modelg2,modelg1, test="LRT")

#no other variable seems to have sig effect on ending... def not trt
anova(modelg2, test="LRT")
modelgr<- glm(end.bin ~ Longitude, family=binomial,data=modeldata)
anova(modelgr, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

# # 
# # # modelg
# # # summary(modelg)
# # # 
# CI.LS.binomial(modelg1)
# # # 
# # interaction.plot(response = modeldata$end.bin, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# # plot(modeldata$bio11, modeldata$Origin)
# qplot(data=modeldata, Trt, end.bin, color=Origin, geom = "jitter") 
# 
# moddata <- ddply(modeldata, .(Pop, Origin, Trt), summarize, popCount=length(Pop), popend.bin=mean(end.bin, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,Trt, popend.bin, color = Origin, 
#       xlab="Trt", 
#       ylab="Population mean end.bin", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# 
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
modelg2<- glm(end.bin ~ Latitude, family=binomial,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

# # 
# # # modelg
# # # summary(modelg)
# # # 
# CI.LS.binomial(modelg1)
# # # 
# # interaction.plot(response = modeldata$end.bin, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# # plot(modeldata$bio11, modeldata$Origin)
# qplot(data=modeldata, Latitude, end.bin, color=Origin, geom = "jitter") 
# 
# moddata <- ddply(modeldata, .(Pop, Origin, Latitude), summarize, popCount=length(Pop), popend.bin=mean(end.bin, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,Latitude, popend.bin, color = Origin, 
#       xlab="Latitude", 
#       ylab="Population mean end.bin", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# 
#######
########sla.log
modeldata <- droplevels(subset(frend, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$sla.log),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#full models
#PC1
model1<-lmer(sla.log  ~ Origin * PC1 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(sla.log  ~ Origin * PC1 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(sla.log  ~ Origin * PC1 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.2919,1)
# 
#try glm
modelg <- glm(sla.log ~ Origin*PC1, family=gaussian,data=modeldata)
modelg1 <- glm(sla.log ~ Origin+PC1, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(sla.log ~ PC1, family=gaussian,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

# # 
# # # modelg
# # # summary(modelg)
# # # 
# CI.LS.gaussian(modelg1)
# # # 
# # interaction.plot(response = modeldata$sla.log, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# # plot(modeldata$bio11, modeldata$Origin)
# qplot(data=modeldata, PC1, sla.log, color=Origin, geom = "jitter") 
# 
# moddata <- ddply(modeldata, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popsla.log=mean(sla.log, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,PC1, popsla.log, color = Origin, 
#       xlab="PC1", 
#       ylab="Population mean sla.log", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
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
modelg2<- glm(sla.log ~ PC3, family=gaussian,data=modeldata)
anova(modelg2,modelg1, test="LRT")
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
modelg2<- glm(sla.log ~ bio11, family=gaussian,data=modeldata)
anova(modelg2,modelg1, test="LRT")
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
modelg2<- glm(sla.log ~ bio6, family=gaussian,data=modeldata)
anova(modelg2,modelg1, test="LRT")
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
modelg2<- glm(sla.log ~ bio9, family=gaussian,data=modeldata)
anova(modelg2,modelg1, test="LRT")
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
modelg2<- glm(sla.log ~ Latitude, family=gaussian,data=modeldata)
anova(modelg2,modelg1, test="LRT")
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
modelg2<- glm(sla.log ~ Trt, family=gaussian,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

###############
###Wilt###
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

##################
#models with Trt included
frPLR.harvest_SKtrt<- lapply(names(frend)[c(37:43)],function(n) CGtrait.LR_snglcov_trt("Harvest.date",frend, covariate=n, family=poisson))
frPLR.wilt_SKtrt<- lapply(names(frend)[c(27,29:31, 33:35)],function(n) CGtrait.LR_snglcov_trt("Wilt",frend, covariate=n, family=poisson))

######
##Harvest.date
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
1-pchisq(4.8901,1)
modelint<-lmer(Harvest.date  ~ Origin +PC1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov
# 
modelT <- lmer(Harvest.date  ~ Origin * PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
trtAov <- anova(model1, modelT)
trtAov
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

modelT<-lmer(Harvest.date  ~ Origin * bio11.1+(1|Pop/Mom), family=poisson,data=modeldata)
(trtAov <- anova(model1, modelT))
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
 
modelT <- lmer(Harvest.date  ~ Origin * bio6.1 +(1|Pop/Mom), family=poisson,data=modeldata)
trtAov <- anova(model1, modelT)
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
# # 
modelT <- lmer(Harvest.date  ~ Origin * bio9.1 +(1|Pop/Mom), family=poisson,data=modeldata)
trtAov <- anova(model1, modelT)
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

#################
##wilt
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
1-pchisq(6.0649,1)

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

#PC2
model1<-lmer(Wilt  ~ Origin * PC2 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * PC2 +Trt+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * PC2 +Trt+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.0825,1)
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
1-pchisq(3.6762,1)
# # 
# #try glm
modelg <- glm(Wilt ~ Origin*PC3+Trt, family=poisson,data=modeldata)
modelg1 <- glm(Wilt ~ Origin+PC3+Trt, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#chisq value

modelgT<- glm(Wilt ~ Origin*PC3, family=poisson,data=modeldata)
anova(modelgT,modelg, test="LRT")
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
1-pchisq(6.3089,1)

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
1-pchisq(6.8946,1)

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
1-pchisq(3.9096,1)
# # # # 
modelint<-lmer(Wilt  ~ Origin +Latitude +Trt +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov
#
modelT<-lmer(Wilt  ~ Origin * Latitude +(1|Pop), family=poisson,data=modeldata)
(trtAov <- anova(model2, modelT))

#################
##Death.date
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
1-pchisq(3.8198,1)

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
1-pchisq(8.1833,1)
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
1-pchisq(6.3089,1)

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
1-pchisq(6.7279,1)
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
1-pchisq(2.0511,1)
# # # 
modelint<-lmer(Death.date  ~ Origin +bio9.1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov
#
modelcov<-lmer(Death.date  ~ Origin +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelOC<-lmer(Death.date  ~ bio9.1+Trt +(1|Pop/Mom), family=poisson,data=modeldata)
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
1-pchisq(7.1413,1)
# # # # # 
modelint<-lmer(Death.date  ~ Origin +lat.1 +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov
# #
modelcov<-lmer(Death.date  ~ Origin +Trt +(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov
