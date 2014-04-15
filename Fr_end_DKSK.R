#Fr end lmer models, DK + SK
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

###########sngl cov with interaction########################
#so for each cov and distribution

#PC1
frGLR.PC1_SKend <- lapply(names(frend)[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC1"))#apply func to all gaussian traits
frPLR.PC1_SKend <- lapply(names(frend)[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC1", family=poisson))#apply func to all poisson traits
frBLR.PC1_SKend <- lapply(names(frend)[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC1", family=binomial)) #apply to binomial trait

#PC2
frGLR.PC2_SKend <- lapply(names(frend)[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC2"))#apply func to all gaussian traits
frPLR.PC2_SKend <- lapply(names(frend)[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC2", family=poisson))#apply func to all poisson traits
frBLR.PC2_SKend <- lapply(names(frend)[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC2", family=binomial)) #apply to binomial trait

#PC3
frGLR.PC3_SKend <- lapply(names(frend)[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC3"))#apply func to all gaussian traits
frPLR.PC3_SKend <- lapply(names(frend)[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC3", family=poisson))#apply func to all poisson traits
frBLR.PC3_SKend <- lapply(names(frend)[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC3", family=binomial)) #apply to binomial trait

#bio11
frGLR.bio11_SKend <- lapply(names(frend)[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio11"))#apply func to all gaussian traits
frPLR.bio11_SKend <- lapply(names(frend)[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio11", family=poisson))#apply func to all poisson traits
frBLR.bio11_SKend <- lapply(names(frend)[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio11", family=binomial)) #apply to binomial trait

#bio6
frGLR.bio6_SKend <- lapply(names(frend)[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio6"))#apply func to all gaussian traits
frPLR.bio6_SKend <- lapply(names(frend)[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio6", family=poisson))#apply func to all poisson traits
frBLR.bio6_SKend <- lapply(names(frend)[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio6", family=binomial)) #apply to binomial trait

#bio9
frGLR.bio9_SKend <- lapply(names(frend)[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio9"))#apply func to all gaussian traits
frPLR.bio9_SKend <- lapply(names(frend)[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio9", family=poisson))#apply func to all poisson traits
frBLR.bio9_SKend <- lapply(names(frend)[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio9", family=binomial)) #apply to binomial trait

#lat
frGLR.Latitude_SKend <- lapply(names(frend)[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="Latitude"))#apply func to all gaussian traits
frPLR.Latitude_SKend <- lapply(names(frend)[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="Latitude", family=poisson))#apply func to all poisson traits
frBLR.Latitude_SKend <- lapply(names(frend)[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="Latitude", family=binomial)) #apply to binomial trait

#trt
frGLR.Trt_SKend <- lapply(names(frend)[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="Trt"))#apply func to all gaussian traits
frPLR.Trt_SKend <- lapply(names(frend)[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="Trt", family=poisson))#apply func to all poisson traits
frBLR.Trt_SKend <- lapply(names(frend)[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="Trt", family=binomial)) #apply to binomial trait

#which anovas have sig covariate or origin?
# snglcov_SKint <- c(frGLR.PC1_SKint,frGLR.PC2_SKint,frGLR.bio11_SKint,frGLR.bio9_SKint,frGLR.bio6_SKint,frGLR.Trt_SKint,
#                    frPLR.PC1_SKint,frPLR.PC3_SKint,frPLR.Trt_SKint,
#                    boltLR.PC1_SKint,boltLR.PC2_SKint,boltLR.PC3_SKint,boltLR.bio11_SKint,boltLR.bio9_SKint,boltLR.bio6_SKint,boltLR.lat_SKint,boltLR.Trt_SKint)
# # frPLR.bio4_SK, frPLR.bio7_SK,frPLR.bio19_SK,frGLR.lat_SK,frPLR.lat_SK,boltLR.bio4_SK,boltLR.bio7_SK,
# save(snglcov_SKint, file="FrDKSKaovlists.RData")
# snglcov_SKint <- load(file="FrDKSKaovlists.RData")


CGtrait_sigaov_func_Fr(frGLR.PC1_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.PC1_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.PC1_SKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.PC2_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.PC2_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.PC2_SKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.PC3_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.PC3_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.PC3_SKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.bio11_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.bio11_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.bio11_SKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.bio6_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.bio6_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.bio6_SKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.bio9_SKend, selectaov=1:6)
# CGtrait_sigaov_func_Fr(frPLR.bio9_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.bio9_SKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.Latitude_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.Latitude_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.Latitude_SKend, selectaov=1:6)

# CGtrait_sigaov_func_Fr(frGLR.Trt_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.Trt_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.Trt_SKend, selectaov=1:6)


##########DK+SK single traits##########################
#to do: bio9 for every poisson trait; trt for gaussian traits

###Bolt.date####################
modeldata<-frend[!is.na(frend$Bolt.date),]

modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#check pop sig only bio6, lat, PCs, trt
#pc1
model1<-lmer(Bolt.date  ~ Origin * PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * PC1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * PC1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.2467,1)

#pc2
model1<-lmer(Bolt.date  ~ Origin * PC2 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * PC2 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * PC2 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.6832,1)

#pc3
model1<-lmer(Bolt.date  ~ Origin * PC3 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * PC3 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * PC3 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(16.147,1)

#bio6
model1<-lmer(Bolt.date  ~ Origin * bio6 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * bio6 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * bio6 +(1|blank), family=poisson,data=modeldata) # Test population effect
#false convergence
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(10.941,1)

#trt
model1<-lmer(Bolt.date  ~ Origin * Trt +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(15.662,1)

#lat
model1<-lmer(Bolt.date  ~ Origin * Latitude +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * Latitude +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.3436,1)

# #full models

#bio11
model1<-lmer(Bolt.date  ~ Origin * bio11 +(1|Pop/Mom), family=poisson,data=modeldata)
#false convergence
model2<-lmer(Bolt.date  ~ Origin * bio11 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * bio11 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.2187,1)

modelint<-lmer(Bolt.date  ~ Origin +bio11 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(Bolt.date  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Bolt.date ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Bolt.date  ~ bio11 +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

# #try glm
# modelg <- glm(Bolt.date ~ Origin*bio11, family=poisson,data=modeldata)
# modelg1 <- glm(Bolt.date ~ Origin+bio11, family=poisson,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# 
# modelg3<- glm(Bolt.date ~ Origin, family=poisson,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# # qchisq(0.9672,1,lower=FALSE)#chisq value
# anova(modelg3, test="LRT")
# modelg2<- glm(Bolt.date ~ bio11, family=poisson,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# # qchisq(0.5399,1,lower=FALSE)#chisq value
# # 
# # 
# # # modelg
# # # summary(modelg)
# # # 
CI.LS.poisson(modelint)
# # # 
# # # interaction.plot(response = modeldata$Bolt.date, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# # # plot(modeldata$bio11, modeldata$Origin)
# # qplot(data=modeldata, bio11, Bolt.date, color=Origin, geom = "jitter")
# # 
# # moddata <- ddply(modeldata, .(Pop, Origin, bio11), summarize, popCount=length(Pop), popBolt.date=mean(Bolt.date, na.rm=TRUE))
# # 
# # #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# # qplot(data=moddata,bio11, popBolt.date, color = Origin, 
# #       xlab="bio11", 
# #       ylab="Population mean Bolt.date", main="") +geom_smooth(method=glm, se=TRUE)
# # # dev.off()
# # # 
# 
#bio9
# model1<-lmer(Bolt.date  ~ Origin * bio9 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * bio9 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * bio9 +(1|blank), family=poisson,data=modeldata) # Test population effect
#false convergence
# momAov <- anova(model2,model1) # mom is sig!
# momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.8971,1)
# # 
modelint<-lmer(Bolt.date  ~ Origin +bio9 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
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
# 
# # #try glm
# # modelg <- glm(Bolt.date ~ Origin*bio9, family=poisson,data=modeldata)
# # modelg1 <- glm(Bolt.date ~ Origin+bio9, family=poisson,data=modeldata)
# # anova(modelg1, modelg, test="LRT") 
# # qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# # 
# # modelg3<- glm(Bolt.date ~ Origin, family=poisson,data=modeldata)
# # anova(modelg3,modelg1, test="LRT")
# # # qchisq(0.9672,1,lower=FALSE)#chisq value
# # # anova(modelg3, test="LRT")
# # modelg2<- glm(Bolt.date ~ bio9, family=poisson,data=modeldata)
# # anova(modelg2,modelg1, test="LRT")
# # # qchisq(5.0702,1,lower=FALSE)#chisq value
# # 
# # 
# # # modelg
# # # summary(modelg)
# # # 
CI.LS.poisson(modelint)
# # # 
# # # interaction.plot(response = modeldata$Bolt.date, x.factor = modeldata$bio9, trace.factor = modeldata$Origin)
# # # plot(modeldata$bio9, modeldata$Origin)
# # qplot(data=modeldata, bio9, Bolt.date, color=Origin, geom = "jitter")
# # 
# # moddata <- ddply(modeldata, .(Pop, Origin, bio9), summarize, popCount=length(Pop), popBolt.date=mean(Bolt.date, na.rm=TRUE))
# # 
# # #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# # qplot(data=moddata,bio9, popBolt.date, color = Origin, 
# #       xlab="bio9", 
# #       ylab="Population mean Bolt.date", main="") +geom_smooth(method=glm, se=TRUE)
# # # dev.off()
# # # 


#######Mass.log
# modeldata<-frend[!is.na(frend$Mass.log),]
# 
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
# 
# #check pop sig only
# #PC1
# model1<-lmer(Mass.log  ~ Origin * PC1 +(1|Pop/Mom), family=gaussian,data=modeldata)
# model2<-lmer(Mass.log  ~ Origin * PC1 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Mass.log  ~ Origin * PC1 +(1|blank), family=gaussian,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(1.2919,1)
# 
# #PC2
# model1<-lmer(Mass.log  ~ Origin * PC2 +(1|Pop/Mom), family=gaussian,data=modeldata)
# model2<-lmer(Mass.log  ~ Origin * PC2 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Mass.log  ~ Origin * PC2 +(1|blank), family=gaussian,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(22.107,1)
# 
# #PC3
# model1<-lmer(Mass.log  ~ Origin * PC3 +(1|Pop/Mom), family=gaussian,data=modeldata)
# model2<-lmer(Mass.log  ~ Origin * PC3 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Mass.log  ~ Origin * PC3 +(1|blank), family=gaussian,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(52.525,1)
# 
# #bio11
# model1<-lmer(Mass.log  ~ Origin * bio11 +(1|Pop/Mom), family=gaussian,data=modeldata)
# model2<-lmer(Mass.log  ~ Origin * bio11 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Mass.log  ~ Origin * bio11 +(1|blank), family=gaussian,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(11.714,1)
# 
# #bio6
# model1<-lmer(Mass.log  ~ Origin * bio6 +(1|Pop/Mom), family=gaussian,data=modeldata)
# model2<-lmer(Mass.log  ~ Origin * bio6 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Mass.log  ~ Origin * bio6 +(1|blank), family=gaussian,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(17.861,1)
# 
# #bio9
# model1<-lmer(Mass.log  ~ Origin * bio9 +(1|Pop/Mom), family=gaussian,data=modeldata)
# model2<-lmer(Mass.log  ~ Origin * bio9 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Mass.log  ~ Origin * bio9 +(1|blank), family=gaussian,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(17.861,1)
# 
# #lat
# model1<-lmer(Mass.log  ~ Origin * Latitude +(1|Pop/Mom), family=gaussian,data=modeldata)
# model2<-lmer(Mass.log  ~ Origin * Latitude +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Mass.log  ~ Origin * Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(17.861,1)
# 
# #trt
# model1<-lmer(Mass.log  ~ Origin * Trt +(1|Pop/Mom), family=gaussian,data=modeldata)
# model2<-lmer(Mass.log  ~ Origin * Trt +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Mass.log  ~ Origin * Trt +(1|blank), family=gaussian,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(61.831,1)
# 
# ###Harvest.date###
# modeldata<-frend[!is.na(frend$Harvest.date),]
# 
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
# 
# #check pop sig only
# #PC1
# model1<-lmer(Harvest.date  ~ Origin * PC1 +(1|Pop/Mom), family=poisson,data=modeldata)
# model2<-lmer(Harvest.date  ~ Origin * PC1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Harvest.date  ~ Origin * PC1 +(1|blank), family=poisson,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(232.07,1)
# 
# #PC2
# model1<-lmer(Harvest.date  ~ Origin * PC2 +(1|Pop/Mom), family=poisson,data=modeldata)
# model2<-lmer(Harvest.date  ~ Origin * PC2 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Harvest.date  ~ Origin * PC2 +(1|blank), family=poisson,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(232.07,1)
# 
# #PC3
# model1<-lmer(Harvest.date  ~ Origin * PC3 +(1|Pop/Mom), family=poisson,data=modeldata)
# model2<-lmer(Harvest.date  ~ Origin * PC3 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Harvest.date  ~ Origin * PC3 +(1|blank), family=poisson,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(232.07,1)
# 
# #bio11
# model1<-lmer(Harvest.date  ~ Origin * bio11 +(1|Pop/Mom), family=poisson,data=modeldata)
# model2<-lmer(Harvest.date  ~ Origin * bio11 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Harvest.date  ~ Origin * bio11 +(1|blank), family=poisson,data=modeldata) # Test population effect
# #false convergence? 
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(232.07,1)
# 
# #bio6
# model1<-lmer(Harvest.date  ~ Origin * bio6 +(1|Pop/Mom), family=poisson,data=modeldata)
# model2<-lmer(Harvest.date  ~ Origin * bio6 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Harvest.date  ~ Origin * bio6 +(1|blank), family=poisson,data=modeldata) # Test population effect
# #false convergence? 
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(232.07,1)
# 
# #bio9
# model1<-lmer(Harvest.date  ~ Origin * bio9 +(1|Pop/Mom), family=poisson,data=modeldata)
# model2<-lmer(Harvest.date  ~ Origin * bio9 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Harvest.date  ~ Origin * bio9 +(1|blank), family=poisson,data=modeldata) # Test population effect
# #false convergence
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(232.07,1)
# 
# #lat
# model1<-lmer(Harvest.date  ~ Origin * Latitude +(1|Pop/Mom), family=poisson,data=modeldata)
# model2<-lmer(Harvest.date  ~ Origin * Latitude +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Harvest.date  ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(232.07,1)
# 
# #trt
# model1<-lmer(Harvest.date  ~ Origin * Trt +(1|Pop/Mom), family=poisson,data=modeldata)
# model2<-lmer(Harvest.date  ~ Origin * Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Harvest.date  ~ Origin * Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(232.07,1)
# 
# ###bolt.bin###
# modeldata<-frend[!is.na(frend$bolt.bin),]
# 
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
# 
# #check pop sig only
# #PC1
# model1<-lmer(bolt.bin  ~ Origin * PC1 +(1|Pop/Mom), family=binomial,data=modeldata)
# model2<-lmer(bolt.bin  ~ Origin * PC1 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(bolt.bin  ~ Origin * PC1 +(1|blank), family=binomial,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(1.1438,1)
# 
# #PC2
# model1<-lmer(bolt.bin  ~ Origin * PC2 +(1|Pop/Mom), family=binomial,data=modeldata)
# model2<-lmer(bolt.bin  ~ Origin * PC2 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(bolt.bin  ~ Origin * PC2 +(1|blank), family=binomial,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(3.5274,1)
# 
# #PC3
# model1<-lmer(bolt.bin  ~ Origin * PC3 +(1|Pop/Mom), family=binomial,data=modeldata)
# model2<-lmer(bolt.bin  ~ Origin * PC3 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(bolt.bin  ~ Origin * PC3 +(1|blank), family=binomial,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(22.824,1)
# 
# #bio11
# model1<-lmer(bolt.bin  ~ Origin * bio11 +(1|Pop/Mom), family=binomial,data=modeldata)
# model2<-lmer(bolt.bin  ~ Origin * bio11 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(bolt.bin  ~ Origin * bio11 +(1|blank), family=binomial,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(11.151,1)
# 
# #bio6
# model1<-lmer(bolt.bin  ~ Origin * bio6 +(1|Pop/Mom), family=binomial,data=modeldata)
# model2<-lmer(bolt.bin  ~ Origin * bio6 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(bolt.bin  ~ Origin * bio6 +(1|blank), family=binomial,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(14.923,1)
# 
# #bio9
# model1<-lmer(bolt.bin  ~ Origin * bio9 +(1|Pop/Mom), family=binomial,data=modeldata)
# model2<-lmer(bolt.bin  ~ Origin * bio9 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(bolt.bin  ~ Origin * bio9 +(1|blank), family=binomial,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(1.1723,1)
# 
# #Latitude
# model1<-lmer(bolt.bin  ~ Origin * Latitude +(1|Pop/Mom), family=binomial,data=modeldata)
# model2<-lmer(bolt.bin  ~ Origin * Latitude +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(bolt.bin  ~ Origin * Latitude +(1|blank), family=binomial,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(2.3273,1)
# 
# #trt
# model1<-lmer(bolt.bin  ~ Origin * Trt +(1|Pop/Mom), family=binomial,data=modeldata)
# model2<-lmer(bolt.bin  ~ Origin * Trt +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(bolt.bin  ~ Origin * Trt +(1|blank), family=binomial,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(30.13,1)
