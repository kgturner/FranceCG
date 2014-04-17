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
#to fix false convergence
frend$PC1.1 <- frend$PC1/100
frGLR.PC1.1_SKend <- lapply(names(frend)[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC1.1"))#apply func to all gaussian traits
frPLR.PC1_SKend <- lapply(names(frend)[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC1", family=poisson))#apply func to all poisson traits
frBLR.PC1_SKend <- lapply(names(frend)[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC1", family=binomial)) #apply to binomial trait

#PC2
#to fix false convergence
frend$PC2.1 <- frend$PC2/100
frGLR.PC2.1_SKend <- lapply(names(frend)[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC2.1"))#apply func to all gaussian traits
frPLR.PC2_SKend <- lapply(names(frend)[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC2", family=poisson))#apply func to all poisson traits
frBLR.PC2_SKend <- lapply(names(frend)[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC2", family=binomial)) #apply to binomial trait

#PC3
#to fix false convergence
frend$PC3.1 <- frend$PC3/100
frGLR.PC3.1_SKend <- lapply(names(frend)[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC3.1"))#apply func to all gaussian traits
frPLR.PC3_SKend <- lapply(names(frend)[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC3", family=poisson))#apply func to all poisson traits
frBLR.PC3_SKend <- lapply(names(frend)[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="PC3", family=binomial)) #apply to binomial trait

#bio11
#to fix false convergence
frend$bio11.1 <- frend$bio11/100
frGLR.bio11.1_SKend <- lapply(names(frend)[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio11.1"))#apply func to all gaussian traits
frPLR.bio11.1_SKend <- lapply(names(frend)[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio11.1", family=poisson))#apply func to all poisson traits
frBLR.bio11_SKend <- lapply(names(frend)[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio11", family=binomial)) #apply to binomial trait

#bio6
#to fix false convergence
frend$bio6.1 <- frend$bio6/100
frGLR.bio6.1_SKend <- lapply(names(frend)[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio6.1"))#apply func to all gaussian traits
frPLR.bio6.1_SKend <- lapply(names(frend)[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio6.1", family=poisson))#apply func to all poisson traits
frBLR.bio6_SKend <- lapply(names(frend)[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio6", family=binomial)) #apply to binomial trait

#bio9
#to fix false convergence
frend$bio9.1 <- frend$bio9/100
frGLR.bio9.1_SKend <- lapply(names(frend)[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio9.1"))#apply func to all gaussian traits
frPLR.bio9.1_SKend <- lapply(names(frend)[c(8:10,12,15:16)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio9.1", family=poisson))#apply func to all poisson traits
frBLR.bio9.1_SKend <- lapply(names(frend)[c(21,25)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="bio9.1", family=binomial)) #apply to binomial trait

#lat
#to fix false convergence
frend$lat.1 <- frend$Latitude/100
frGLR.Lat.1_SKend <- lapply(names(frend)[c(23:24)],function(n) CGtrait.LR_snglcov_int(n,frend, covariate="lat.1"))#apply func to all gaussian traits
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


CGtrait_sigaov_func_Fr(frGLR.PC1.1_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.PC1_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.PC1_SKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.PC2.1_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.PC2_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.PC2_SKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.PC3.1_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.PC3_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.PC3_SKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.bio11.1_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.bio11.1_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.bio11_SKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.bio6.1_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.bio6.1_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.bio6_SKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.bio9.1_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.bio9.1_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.bio9.1_SKend, selectaov=1:6)

CGtrait_sigaov_func_Fr(frGLR.Lat.1_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.Latitude_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.Latitude_SKend, selectaov=1:6)

# CGtrait_sigaov_func_Fr(frGLR.Trt_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.Trt_SKend, selectaov=1:6)
CGtrait_sigaov_func_Fr(frBLR.Trt_SKend, selectaov=1:6)


##########DK+SK single traits##########################
#check sig level for mom: Death.date, yellow
Death.date <- CGtrait.LR_snglcov_int(Death.date,frend,covariate,family=gaussian)

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
#to fix false convergence
modeldata$bio11.1 <- modeldata$bio11/100
model1<-lmer(Bolt.date  ~ Origin * bio11.1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * bio11.1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * bio11.1 +(1|blank), family=poisson,data=modeldata) # Test population effect
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
 
#bio9
#to fix false convergence
modeldata$bio9.1 <- modeldata$bio9/100
model1<-lmer(Bolt.date  ~ Origin * bio9.1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * bio9.1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * bio9.1 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
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

##############
#######Mass.log
modeldata<-frend[!is.na(frend$Mass.log),]

modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#check pop sig only: all
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
1-pchisq(22.107,1)

#PC3
model1<-lmer(Mass.log  ~ Origin * PC3 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log  ~ Origin * PC3 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * PC3 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(52.525,1)

#bio11
model1<-lmer(Mass.log  ~ Origin * bio11 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log  ~ Origin * bio11 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * bio11 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(11.714,1)

#bio6
model1<-lmer(Mass.log  ~ Origin * bio6 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log  ~ Origin * bio6 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * bio6 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(17.861,1)

#bio9
model1<-lmer(Mass.log  ~ Origin * bio9 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log  ~ Origin * bio9 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * bio9 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(17.861,1)

#lat
model1<-lmer(Mass.log  ~ Origin * Latitude +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log  ~ Origin * Latitude +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(17.861,1)

#trt
model1<-lmer(Mass.log  ~ Origin * Trt +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log  ~ Origin * Trt +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * Trt +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(61.831,1)

################
###Harvest.date###
modeldata<-frend[!is.na(frend$Harvest.date),]

modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#check pop sig only: all except bio11, bio9
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
1-pchisq(232.07,1)

#PC3
model1<-lmer(Harvest.date  ~ Origin * PC3 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * PC3 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * PC3 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(232.07,1)

#bio6
model1<-lmer(Harvest.date  ~ Origin * bio6 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * bio6 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * bio6 +(1|blank), family=poisson,data=modeldata) # Test population effect
#false convergence? 
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(232.07,1)

#lat
model1<-lmer(Harvest.date  ~ Origin * Latitude +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * Latitude +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(232.07,1)

#trt
model1<-lmer(Harvest.date  ~ Origin * Trt +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(232.07,1)

#full models
#bio11
modeldata$bio11.1 <- modeldata$bio11/100
model1<-lmer(Harvest.date  ~ Origin * bio11.1 +(1|Pop/Mom), family=poisson,data=modeldata)
#false convergence
model2<-lmer(Harvest.date  ~ Origin * bio11.1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * bio11.1 +(1|blank), family=poisson,data=modeldata) # Test population effect
#false convergence
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.8851,1)

modelint<-lmer(Harvest.date  ~ Origin +bio11.1 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(Harvest.date  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Harvest.date ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Harvest.date  ~ bio11.1 +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

# #try glm
# modelg <- glm(Harvest.date ~ Origin*bio11, family=poisson,data=modeldata)
# modelg1 <- glm(Harvest.date ~ Origin+bio11, family=poisson,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# 
# modelg3<- glm(Harvest.date ~ Origin, family=poisson,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# # qchisq(0.9672,1,lower=FALSE)#chisq value
# anova(modelg3, test="LRT")
# modelg2<- glm(Harvest.date ~ bio11, family=poisson,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# qchisq(0.5399,1,lower=FALSE)#chisq value
# 
# 
# # modelg
# # summary(modelg)
# # 
CI.LS.poisson(modelint)
# # 
# # interaction.plot(response = modeldata$Harvest.date, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# # plot(modeldata$bio11, modeldata$Origin)
# qplot(data=modeldata, bio11, Harvest.date, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, bio11), summarize, popCount=length(Pop), popHarvest.date=mean(Harvest.date, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,bio11, popHarvest.date, color = Origin, 
#       xlab="bio11", 
#       ylab="Population mean Harvest.date", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# # 

#bio9
model1<-lmer(Harvest.date  ~ Origin * bio9.1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * bio9.1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Harvest.date  ~ Origin * bio9 +(1|blank), family=poisson,data=modeldata, verbose=TRUE) # Test population effect
#false convergence
modeldata$bio9.1 <- modeldata$bio9/100
model3<-lmer(Harvest.date  ~ Origin * bio9.1 +(1|blank), family=poisson,data=modeldata) # Test population effect

momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.8581,1)
# 
modelint<-lmer(Harvest.date  ~ Origin +bio9.1 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(Harvest.date  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Harvest.date ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Harvest.date  ~ bio9.1 +(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

# #try glm
# modelg <- glm(Harvest.date ~ Origin*bio9, family=poisson,data=modeldata)
# modelg1 <- glm(Harvest.date ~ Origin+bio9, family=poisson,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# 
# modelg3<- glm(Harvest.date ~ Origin, family=poisson,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# # qchisq(0.9672,1,lower=FALSE)#chisq value
# # anova(modelg3, test="LRT")
# modelg2<- glm(Harvest.date ~ bio9, family=poisson,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# # qchisq(5.0702,1,lower=FALSE)#chisq value
# 
# 
# # modelg
# # summary(modelg)
# # 
CI.LS.poisson(modelg1)
# # 
# # interaction.plot(response = modeldata$Harvest.date, x.factor = modeldata$bio9, trace.factor = modeldata$Origin)
# # plot(modeldata$bio9, modeldata$Origin)
# qplot(data=modeldata, bio9, Harvest.date, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, bio9), summarize, popCount=length(Pop), popHarvest.date=mean(Harvest.date, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,bio9, popHarvest.date, color = Origin, 
#       xlab="bio9", 
#       ylab="Population mean Harvest.date", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# # 
# 
##############
###bolt.bin###
modeldata<-frend[!is.na(frend$bolt.bin),]

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
1-pchisq(1.554,1)

#PC2
model1<-lmer(bolt.bin  ~ Origin * PC2 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * PC2 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * PC2 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.8637,1)

#PC3
model1<-lmer(bolt.bin  ~ Origin * PC3 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * PC3 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * PC3 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(22.824,1)

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
1-pchisq(1.8101,1)

#trt
model1<-lmer(bolt.bin  ~ Origin * Trt +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * Trt +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * Trt +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(30.13,1)

#Latitude
model1<-lmer(bolt.bin  ~ Origin * Latitude +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * Latitude +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * Latitude +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.0479,1)

################
###end.bin###
modeldata<-frend[!is.na(frend$end.bin),]

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
modelg <- glm(end.bin ~ Origin*PC1, family=binomial,data=modeldata)
modelg1 <- glm(end.bin ~ Origin+PC1, family=binomial,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(end.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(end.bin ~ PC1, family=binomial,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

# 
# # modelg
# # summary(modelg)
# # 
CI.LS.binomial(modelg1)
# # 
# interaction.plot(response = modeldata$end.bin, x.factor = modeldata$PC1, trace.factor = modeldata$Origin)
# plot(modeldata$PC1, modeldata$Origin)
qplot(data=modeldata, PC1, end.bin, color=Origin, geom = "jitter")

moddata <- ddply(modeldata, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popend.bin=mean(end.bin, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC1, popend.bin, color = Origin, 
      xlab="PC1", 
      ylab="Population mean end.bin", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

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
# # modelg
# # summary(modelg)
# # 
CI.LS.binomial(modelg1)
# # 
# interaction.plot(response = modeldata$end.bin, x.factor = modeldata$PC2, trace.factor = modeldata$Origin)
# plot(modeldata$PC2, modeldata$Origin)
qplot(data=modeldata, PC2, end.bin, color=Origin, geom = "jitter") 

moddata <- ddply(modeldata, .(Pop, Origin, PC2), summarize, popCount=length(Pop), popend.bin=mean(end.bin, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC2, popend.bin, color = Origin, 
      xlab="PC2", 
      ylab="Population mean end.bin", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()


#PC3
model1<-lmer(end.bin  ~ Origin * PC3 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(end.bin  ~ Origin * PC3 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(end.bin  ~ Origin * PC3 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(22.824,1)
#try glm
modelg <- glm(end.bin ~ Origin*PC3, family=binomial,data=modeldata)
modelg1 <- glm(end.bin ~ Origin+PC3, family=binomial,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(end.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(end.bin ~ PC3, family=binomial,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

# 
# # modelg
# # summary(modelg)
# # 
CI.LS.binomial(modelg1)
# # 
# interaction.plot(response = modeldata$end.bin, x.factor = modeldata$PC3, trace.factor = modeldata$Origin)
# plot(modeldata$PC2, modeldata$Origin)
qplot(data=modeldata, PC3, end.bin, color=Origin, geom = "jitter") 

moddata <- ddply(modeldata, .(Pop, Origin, PC3), summarize, popCount=length(Pop), popend.bin=mean(end.bin, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC3, popend.bin, color = Origin, 
      xlab="PC3", 
      ylab="Population mean end.bin", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()


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
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(end.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(end.bin ~ bio11, family=binomial,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

# 
# # modelg
# # summary(modelg)
# # 
CI.LS.binomial(modelg1)
# # 
# interaction.plot(response = modeldata$end.bin, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# plot(modeldata$bio11, modeldata$Origin)
qplot(data=modeldata, bio11, end.bin, color=Origin, geom = "jitter") 

moddata <- ddply(modeldata, .(Pop, Origin, bio11), summarize, popCount=length(Pop), popend.bin=mean(end.bin, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,bio11, popend.bin, color = Origin, 
      xlab="bio11", 
      ylab="Population mean end.bin", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

# 
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
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(end.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(end.bin ~ bio6, family=binomial,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

# 
# # modelg
# # summary(modelg)
# # 
CI.LS.binomial(modelg1)
# # 
# interaction.plot(response = modeldata$end.bin, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# plot(modeldata$bio11, modeldata$Origin)
qplot(data=modeldata, bio6, end.bin, color=Origin, geom = "jitter") 

moddata <- ddply(modeldata, .(Pop, Origin, bio6), summarize, popCount=length(Pop), popend.bin=mean(end.bin, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,bio6, popend.bin, color = Origin, 
      xlab="bio6", 
      ylab="Population mean end.bin", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

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
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(end.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(end.bin ~ bio9, family=binomial,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

# 
# # modelg
# # summary(modelg)
# # 
CI.LS.binomial(modelg1)
# # 
# interaction.plot(response = modeldata$end.bin, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# plot(modeldata$bio11, modeldata$Origin)
qplot(data=modeldata, bio9, end.bin, color=Origin, geom = "jitter") 

moddata <- ddply(modeldata, .(Pop, Origin, bio9), summarize, popCount=length(Pop), popend.bin=mean(end.bin, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,bio9, popend.bin, color = Origin, 
      xlab="bio9", 
      ylab="Population mean end.bin", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

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
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

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

# 
# # modelg
# # summary(modelg)
# # 
CI.LS.binomial(modelg1)
# # 
# interaction.plot(response = modeldata$end.bin, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# plot(modeldata$bio11, modeldata$Origin)
qplot(data=modeldata, Trt, end.bin, color=Origin, geom = "jitter") 

moddata <- ddply(modeldata, .(Pop, Origin, Trt), summarize, popCount=length(Pop), popend.bin=mean(end.bin, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,Trt, popend.bin, color = Origin, 
      xlab="Trt", 
      ylab="Population mean end.bin", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

# 
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
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(end.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(end.bin ~ Latitude, family=binomial,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

# 
# # modelg
# # summary(modelg)
# # 
CI.LS.binomial(modelg1)
# # 
# interaction.plot(response = modeldata$end.bin, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# plot(modeldata$bio11, modeldata$Origin)
qplot(data=modeldata, Latitude, end.bin, color=Origin, geom = "jitter") 

moddata <- ddply(modeldata, .(Pop, Origin, Latitude), summarize, popCount=length(Pop), popend.bin=mean(end.bin, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,Latitude, popend.bin, color = Origin, 
      xlab="Latitude", 
      ylab="Population mean end.bin", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

#######
########sla.log
modeldata<-frend[!is.na(frend$sla.log),]

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

#try glm
modelg <- glm(sla.log ~ Origin*PC1, family=gaussian,data=modeldata)
modelg1 <- glm(sla.log ~ Origin+PC1, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(sla.log ~ PC1, family=gaussian,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

# 
# # modelg
# # summary(modelg)
# # 
CI.LS.gaussian(modelg1)
# # 
# interaction.plot(response = modeldata$sla.log, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# plot(modeldata$bio11, modeldata$Origin)
qplot(data=modeldata, PC1, sla.log, color=Origin, geom = "jitter") 

moddata <- ddply(modeldata, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popsla.log=mean(sla.log, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC1, popsla.log, color = Origin, 
      xlab="PC1", 
      ylab="Population mean sla.log", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()
# 
#PC2
model1<-lmer(sla.log  ~ Origin * PC2 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(sla.log  ~ Origin * PC2 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(sla.log  ~ Origin * PC2 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(22.107,1)

modelint<-lmer(sla.log  ~ Origin +PC2 +(1|Pop/Mom), family=gaussian,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(sla.log  ~ Origin +(1|Pop/Mom), family=gaussian,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(sla.log ~ (1|Pop/Mom), family=gaussian,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(sla.log  ~ PC2 +(1|Pop/Mom), family=gaussian,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov
#try glm
modelg <- glm(sla.log ~ Origin*PC2, family=gaussian,data=modeldata)
modelg1 <- glm(sla.log ~ Origin+PC2, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

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
1-pchisq(1.2996,1)
#try glm
modelg <- glm(sla.log ~ Origin*PC3, family=gaussian,data=modeldata)
modelg1 <- glm(sla.log ~ Origin+PC3, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(sla.log ~ PC3, family=gaussian,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value


#bio11
model1<-lmer(sla.log  ~ Origin * bio11 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(sla.log  ~ Origin * bio11 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(sla.log  ~ Origin * bio11 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(6.2833,1)
#try glm
modelg <- glm(sla.log ~ Origin*bio11, family=gaussian,data=modeldata)
modelg1 <- glm(sla.log ~ Origin+bio11, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

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
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(sla.log ~ bio6, family=gaussian,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

#bio9
model1<-lmer(sla.log  ~ Origin * bio9 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(sla.log  ~ Origin * bio9 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(sla.log  ~ Origin * bio9 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(17.861,1)

modelint<-lmer(sla.log  ~ Origin +bio9 +(1|Pop/Mom), family=gaussian,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(sla.log  ~ Origin +(1|Pop/Mom), family=gaussian,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(sla.log ~ (1|Pop/Mom), family=gaussian,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(sla.log  ~ bio9 +(1|Pop/Mom), family=gaussian,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

#lat
model1<-lmer(sla.log  ~ Origin * Latitude +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(sla.log  ~ Origin * Latitude +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(sla.log  ~ Origin * Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(17.861,1)
#try glm
modelg <- glm(sla.log ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(sla.log ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(sla.log ~ Latitude, family=gaussian,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

#trt
# Error in mer_finalize(ans) : Downdated X'X is not positive definite, 6. 
# So think I can't test interaction because not all combos of Trt and Origin are present (no drought SKs in sla data)
xtabs(sla.log~Origin+Trt, data=modeldata)
model1<-lmer(sla.log  ~ Origin + Trt +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(sla.log  ~ Origin + Trt +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(sla.log  ~ Origin + Trt +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.3008,1)
#try glm
modelg <- glm(sla.log ~ Origin*Trt, family=gaussian,data=modeldata)
modelg1 <- glm(sla.log ~ Origin+Trt, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(sla.log ~ Trt, family=gaussian,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

#############
###Wilt###
modeldata<-frend[!is.na(frend$Wilt),]

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
1-pchisq(5.0125,1)

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
1-pchisq(2.1236,1)

#try glm
modelg <- glm(Wilt ~ Origin*PC2, family=poisson,data=modeldata)
modelg1 <- glm(Wilt ~ Origin+PC2, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(Wilt ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(Wilt ~ PC2, family=poisson,data=modeldata)
anova(modelg2,modelg1, test="LRT")
qchisq(0.5399,1,lower=FALSE)#chisq value

#PC3
model1<-lmer(Wilt  ~ Origin * PC3 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * PC3 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * PC3 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.465,1)

#try glm
modelg <- glm(Wilt ~ Origin*PC3, family=poisson,data=modeldata)
modelg1 <- glm(Wilt ~ Origin+PC3, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(Wilt ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(Wilt ~ PC3, family=poisson,data=modeldata)
anova(modelg2,modelg1, test="LRT")
qchisq(0.5399,1,lower=FALSE)#chisq value

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
1-pchisq(5.1394,1)

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

# #try glm
# modelg <- glm(Wilt ~ Origin*bio11, family=poisson,data=modeldata)
# modelg1 <- glm(Wilt ~ Origin+bio11, family=poisson,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# 
# modelg3<- glm(Wilt ~ Origin, family=poisson,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# # qchisq(0.9672,1,lower=FALSE)#chisq value
# anova(modelg3, test="LRT")
# modelg2<- glm(Wilt ~ bio11, family=poisson,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# qchisq(0.5399,1,lower=FALSE)#chisq value
# 
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
#false convergence? 
modeldata$bio6.1 <- modeldata$bio6/100
model1<-lmer(Wilt  ~ Origin * bio6.1 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * bio6.1 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * bio6.1 +(1|blank), family=poisson,data=modeldata) # Test population effect

momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(5.769,1)

modelint<-lmer(Wilt  ~ Origin +bio6.1 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(Wilt  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Wilt ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Wilt  ~ bio6.1 +(1|Pop), family=poisson,data=modeldata)
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
1-pchisq(4.7814,1)
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

# #try glm
# modelg <- glm(Wilt ~ Origin*bio9, family=poisson,data=modeldata)
# modelg1 <- glm(Wilt ~ Origin+bio9, family=poisson,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# 
# modelg3<- glm(Wilt ~ Origin, family=poisson,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# # qchisq(0.9672,1,lower=FALSE)#chisq value
# # anova(modelg3, test="LRT")
# modelg2<- glm(Wilt ~ bio9, family=poisson,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# # qchisq(5.0702,1,lower=FALSE)#chisq value
# 
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
1-pchisq(2.8644,1)

#try glm
modelg <- glm(Wilt ~ Origin*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(Wilt ~ Origin+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(Wilt ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
modelg2<- glm(Wilt ~ Latitude, family=poisson,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value

#trt
model1<-lmer(Wilt  ~ Origin * Trt +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt  ~ Origin * Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt  ~ Origin * Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(8.229,1)

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
