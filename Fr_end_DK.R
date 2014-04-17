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

CGtrait_sigaov_func_Fr(frGLR.Trt_DKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.Trt_DKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.Trt_DKend, selectaov=1:6)


##########DK only single traits##########################
#focus on single timept measures, poisson model fails: 

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

