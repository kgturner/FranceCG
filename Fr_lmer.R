#mixed-effects models of univariate traits
#lots of climate variables! Including climate PCA
#Sept 2013

#read
frdat<- read.table("FrTraitClimDat.txt", header=T, sep="\t",quote='"', row.names=1)

#OR
FrdatSK<- read.table("FrTraitClimDat_SK.txt", header=T, sep="\t",quote='"', row.names=1)
#for DK only include:
subset(FrdatSK, Origin%in%c("inv", "nat"))

#Control and drought, REML, using lme4
#mixed effect models 
library(lme4)
library(lsmeans)
library(ggplot2)
library(plyr)

#for each normal trait, compare this general set of models
modelfull<-lmer(trait  ~ Origin* Trt+PC1+PC2+bio11+bio9+bio6 +(Origin|PopID/Mom), family=gaussian,data=modeldata)#not inlcuded in CGtrait.LR functions

# model1<-lmer(trait  ~ Origin* Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelobar, model1)
# model2<-lmer(trait  ~ Origin* Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(trait  ~ Origin* Latitude + (1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # mom is sig!
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# modelI <- lmer(trait  ~ Origin + Latitude + (1|PopID), family=family,data=modeldata)
# anova(modelI,model2)
# modelL<-lmer(trait  ~ Origin + (1|PopID), family=gaussian,data=modeldata)
# anova(modelL, model1)
# modelO<-lmer(trait ~ Latitude +(1|PopID), family=gaussian,data=modeldata)
# anova(modelO,model1) #test for significance of origin - origin only marginally sig....!

# ##########does Trt matter? ###########################
# ###############try Shoot.mass.gH, Bolt.date, Rose.AreaH.m2, manova of all....?
# modeldata <- frdat[!is.na(frdat$Shoot.mass.gH),]
# # modeldata$Mom <- as.factor(modeldata$Mom)
# modelg <- glm(Shoot.mass.gH~Trt, family=gaussian,data=modeldata)
# a1 <- anova(modelg, test="LRT")
# 
# modeldata <- frdat[!is.na(frdat$Bolt.date),]
# # modeldata$Mom <- as.factor(modeldata$Mom)
# modelg <- glm(Bolt.date~Trt, family=poisson,data=modeldata)
# anova(modelg, test="LRT")
# 
# modeldata <- frdat[!is.na(frdat$Rose.AreaH.m2),]
# # modeldata$Mom <- as.factor(modeldata$Mom)
# modelg <- glm(Rose.AreaH.m2~Trt, family=gaussian,data=modeldata)
# anova(modelg, test="LRT")
# 
# qplot(data=modeldata, Rose.AreaH.m2, Shoot.mass.gH, color=Trt)+geom_smooth(method=glm, se=FALSE)
# 
# mantraits<-cbind(frdat$Shoot.mass.gH, frdat$Rose.AreaH.m2, frdat$Bolt.date)
# #fitOrigin<-manova(m2traits~Frm2DKdatdes$Origin)
# #fittrt<-manova(m2traits~Frm2datTag$trt)
# #fitOrigintrt<-manova(m2traits~Frm2datTag$Origin+Frm2datTag$trt)
# #fitOriginbytrt<-manova(m2traits~Frm2DKdatdes$Origin+Frm2DKdatdes$Trt+Frm2DKdatdes$Origin*Frm2DKdatdes$Trt)
# man<-manova(mantraits~Trt, data=frdat)
# summary.aov(man)
# summary(man)
# 
# allmantraits<-as.matrix(cbind(frdat[c(9:10,16,18:19,27:34,8,17,24,25,35)]))#all traits except bolt.bin
# allman<-manova(allmantraits~Trt, data=frdat)
# summary.aov(allman)#only sig in crown diameter
# summary(allman)#marginal
# 
# #Trt
# frGLR.Trt <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="Trt"))#apply func to all gaussian traits
# frPLR.Trt <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="Trt", family=poisson))#apply func to all poisson traits
# boltLR.Trt <- CGtrait.LR_snglcov(trait="bolt.bin",df=frdat,covariate="Trt",family=binomial) #apply to single binomial trait
# 
# CGtrait_sigaov_func_Fr(frGLR.Trt, selectaov=3:6)
# CGtrait_sigaov_func_Fr(frPLR.Trt, selectaov=3:6)
# boltLR.Trt
# 
# #########
# #test for one trait, one df, specify non default (gaussian) family
# CGtrait.LR_snglcov(trait="Mass.log",df=frdat,covariate="PC1",family=gaussian)
# CGtrait.LR_snglcov(trait="Mass.log",df=frdat,covariate="bio4",family=gaussian)
# CGtrait.LR_snglcov(trait="Mass.log",df=frdat,covariate="bio19",family=gaussian)
# CGtrait.LR_snglcov(trait="Mass.log",df=frdat,covariate="Latitude",family=gaussian)
# CGtrait.LR_snglcov(trait="Mass.log",df=frdat,covariate="bio7",family=gaussian)
# CGtrait.LR_snglcov(trait="Mass.log",df=frdat,covariate="PC2",family=gaussian)
# CGtrait.LR_snglcov(trait="Mass.log",df=frdat,covariate="PC3",family=gaussian)
# CGtrait.LR_snglcov(trait="Mass.log",df=frdat,covariate="PC4",family=gaussian)
# 
# #for all traits in a df
# #make sure all traits analyzed this way are the same distribution family
# names(frdat)#find col numbers for traits of interestes
# frGLR <- lapply(names(frdat)[41:42],function(n) CGtrait.LR_snglcov(n,frdat, covariate="PC1"))#apply func to all things in list
# 
# 
# #to get models
# frGmodels <- lapply(names(frdat)[c(34,42)],function(n) CGtrait.models_snglcov(n,frdat, covariate="PC1"))
# names(frGmodels) <- names(frdat[c(34,42)])
# 
# #to get one model
# frGmodels[[1]][1] #first number is trait in column order of df, second number is model number
# names(frGmodels[1]) #to verify trait
# 
# #to check normality of residuals
# mass.lmer <- frGmodels[[1]]$model2
# plot(resid(mass.lmer) ~ fitted(mass.lmer),main="residual plot")
# abline(h=0)
# 
# # checking the normality of residuals e_i:
# qqnorm(resid(mass.lmer), main="Q-Q plot for residuals")
# qqline(resid(mass.lmer))
# 

##########single covariate, interaction###############
#so for each cov and distribution

#PC1
frGLR.PC1_int <- lapply(names(frdat)[c(9:10,13:16,18:23,27:34,36:42)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="PC1"))
#apply func to all gaussian traits/transformed traits
frPLR.PC1_int <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="PC1", family=poisson))
#apply func to all poisson traits
boltLR.PC1_int <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=frdat,covariate="PC1",family=binomial) #apply to single binomial trait

#PC2
frGLR.PC2_int <- lapply(names(frdat)[c(9:10,13:16,18:23,27:34,36:42)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="PC2"))#apply func to all gaussian traits
frPLR.PC2_int <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="PC2", family=poisson))#apply func to all poisson traits
boltLR.PC2_int <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=frdat,covariate="PC2",family=binomial) #apply to single binomial trait

#PC3
frGLR.PC3_int <- lapply(names(frdat)[c(9:10,13:16,18:23,27:34,36:42)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="PC3"))#apply func to all gaussian traits
frPLR.PC3_int <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="PC3", family=poisson))#apply func to all poisson traits
boltLR.PC3_int <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=frdat,covariate="PC3",family=binomial) #apply to single binomial trait

#bio11
frGLR.bio11_int <- lapply(names(frdat)[c(9:10,13:16,18:23,27:34,36:42)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="bio11"))#apply func to all gaussian traits
frPLR.bio11_int <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="bio11", family=poisson))#apply func to all poisson traits
boltLR.bio11_int <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=frdat,covariate="bio11",family=binomial) #apply to single binomial trait

#bio9
frGLR.bio9_int <- lapply(names(frdat)[c(9:10,13:16,18:23,27:34,36:42)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="bio9"))#apply func to all gaussian traits
frPLR.bio9_int <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="bio9", family=poisson))#apply func to all poisson traits
boltLR.bio9_int <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=frdat,covariate="bio9",family=binomial) #apply to single binomial trait

#bio6
frGLR.bio6_int <- lapply(names(frdat)[c(9:10,13:16,18:23,27:34,36:42)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="bio6"))#apply func to all gaussian traits
frPLR.bio6_int <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="bio6", family=poisson))#apply func to all poisson traits
boltLR.bio6_int <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=frdat,covariate="bio6",family=binomial) #apply to single binomial trait

#Latitude
frGLR.lat_int <- lapply(names(frdat)[c(9:10,13:16,18:23,27:34,36:42)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="Latitude"))#apply func to all gaussian traits
frPLR.lat_int <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="Latitude", family=poisson))#apply func to all poisson traits
boltLR.lat_int <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=frdat,covariate="Latitude",family=binomial) #apply to single binomial trait

#Trt
frGLR.trt_int <- lapply(names(frdat)[c(9:10,13:16,18:23,27:34,36:42)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="Trt"))#apply func to all gaussian traits
frPLR.trt_int <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="Trt", family=poisson))#apply func to all poisson traits
boltLR.trt_int <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=frdat,covariate="Trt",family=binomial) #apply to single binomial trait


# #which anovas have sig covariate or origin?
# snglcov_int <- c(frGLR.PC1_int,frGLR.PC2_int,frGLR.PC3_int,frGLR.bio11_int,frGLR.bio9_int,frGLR.bio6_int,frGLR.lat_int,
#                  frPLR.PC1_int,frPLR.PC2_int,frPLR.PC3_int,boltLR.PC1_int,boltLR.PC2_int,boltLR.PC3_int,
#                  boltLR.bio11_int,boltLR.bio9_int,boltLR.bio6_int,boltLR.lat_int,frGLR.trt_int,frPLR.trt_int,boltLR.trt_int)
# save(snglcov_int, file="FrDKonlyAovlists.RData")
# snglcov_int <- load(file="FrDKonlyAovlists.RData")


CGtrait_sigaov_func_Fr(frGLR.PC1_int, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.PC1_int, selectaov=1:6)
boltLR.PC1_int

CGtrait_sigaov_func_Fr(frGLR.PC2_int, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.PC2_int, selectaov=1:6)
boltLR.PC2_int

CGtrait_sigaov_func_Fr(frGLR.PC3_int, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.PC3_int, selectaov=1:6)
boltLR.PC3_int

CGtrait_sigaov_func_Fr(frGLR.bio11_int, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.bio11_int, selectaov=1:6) #warnings
boltLR.bio11_int

CGtrait_sigaov_func_Fr(frGLR.bio9_int, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.bio9_int, selectaov=1:6) #errors
boltLR.bio9_int

CGtrait_sigaov_func_Fr(frGLR.bio6_int, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.bio6_int, selectaov=1:6) #warnings
boltLR.bio6_int

CGtrait_sigaov_func_Fr(frGLR.lat_int, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.lat_int, selectaov=1:6) #warnings
boltLR.lat_int

CGtrait_sigaov_func_Fr(frGLR.trt_int, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.trt_int, selectaov=1:6)
boltLR.trt_int

####Crown.log and RoseAh.log only####
# necessary? to get rosette area in m2
# fine as is for modeling, but for reporting means, multiply by 100

#PC1
frGLR.PC1_cr <- lapply(names(subset(FrdatSK, Origin%in%c("inv", "nat")))[c(45,48)],function(n) CGtrait.LR_snglcov_int(n,subset(FrdatSK, Origin%in%c("inv", "nat")), covariate="PC1"))

#adjust col numbers for new df updated with PCA values!
# #PC2
# frGLR.PC2_cr <- lapply(names(subset(FrdatSK, Origin%in%c("inv", "nat")))[c(49,52)],function(n) CGtrait.LR_snglcov_int(n,subset(FrdatSK, Origin%in%c("inv", "nat")), covariate="PC2"))#apply func to all gaussian traits
# 
# #PC3
# frGLR.PC3_cr <-  lapply(names(subset(FrdatSK, Origin%in%c("inv", "nat")))[c(49,52)],function(n) CGtrait.LR_snglcov_int(n,subset(FrdatSK, Origin%in%c("inv", "nat")), covariate="PC3"))#apply func to all gaussian traits
# 
# #bio11
# frGLR.bio11_cr <- lapply(names(subset(FrdatSK, Origin%in%c("inv", "nat")))[c(49,52)],function(n) CGtrait.LR_snglcov_int(n,subset(FrdatSK, Origin%in%c("inv", "nat")), covariate="bio11"))#apply func to all gaussian traits
# 
# #bio9
# frGLR.bio9_cr <- lapply(names(subset(FrdatSK, Origin%in%c("inv", "nat")))[c(49,52)],function(n) CGtrait.LR_snglcov_int(n,subset(FrdatSK, Origin%in%c("inv", "nat")), covariate="bio9"))#apply func to all gaussian traits
# 
# #bio6
# frGLR.bio6_cr <- lapply(names(subset(FrdatSK, Origin%in%c("inv", "nat")))[c(49,52)],function(n) CGtrait.LR_snglcov_int(n,subset(FrdatSK, Origin%in%c("inv", "nat")), covariate="bio6"))#apply func to all gaussian traits
# 
# #Latitude
# frGLR.lat_cr <- lapply(names(subset(FrdatSK, Origin%in%c("inv", "nat")))[c(49,52)],function(n) CGtrait.LR_snglcov_int(n,subset(FrdatSK, Origin%in%c("inv", "nat")), covariate="Latitude"))#apply func to all gaussian traits
# 
# #Trt
# frGLR.trt_cr <- lapply(names(subset(FrdatSK, Origin%in%c("inv", "nat")))[c(49,52)],function(n) CGtrait.LR_snglcov_int(n,subset(FrdatSK, Origin%in%c("inv", "nat")), covariate="Trt"))#apply func to all gaussian traits

CGtrait_sigaov_func_Fr(frGLR.PC1_cr, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frGLR.PC2_cr, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frGLR.PC3_cr, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frGLR.bio11_cr, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frGLR.bio9_cr, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frGLR.bio6_cr, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frGLR.lat_cr, selectaov=1:6, cutoff=0.05)
# CGtrait_sigaov_func_Fr(frGLR.trt_cr, selectaov=1:6, cutoff=0.05)


##########DK single traits##########################
#focus on single timept measures, poisson model fails: 
#MaxBoltHtH(all), Bolt.date (bio11), poisson measures for bio9 (just bolt.date and harvest.date...)
#use transform for Rose area, crown, shoot mass

###MaxBoltHtH##############
# CGtrait.LR_snglcov_int(trait="MaxBoltHtH", df=frdat, covariate="PC1", family=gaussian)
# CGtrait.models_snglcov_int(trait="MaxBoltHtH", df=frdat, covariate="PC1", family=gaussian)

modeldata<-frdat[!is.na(frdat$MaxBoltHtH),]

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
# qchisq(558.65,1,lower=FALSE)#chisq value
# 
# modelint<-lmer(MaxBoltHtH  ~ Origin +PC1 +(1|Pop), family=gaussian,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(MaxBoltHtH  ~ Origin +(1|Pop), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(MaxBoltHtH ~ (1|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(MaxBoltHtH  ~ PC1 +(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov
#try glm
modelg <- glm(MaxBoltHtH ~ Origin*PC1, family=gaussian,data=modeldata)
modelg1 <- glm(MaxBoltHtH ~ Origin+PC1, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(MaxBoltHtH ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(MaxBoltHtH ~ PC1, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
qchisq(0.5399,1,lower=FALSE)#chisq value


modelg3
summary(modelg3)

lsmeans(modelg3, ~ Origin, conf=95)

# interaction.plot(response = modeldata$MaxBoltHtH, x.factor = modeldata$PC1, trace.factor = modeldata$Origin)
# plot(modeldata$PC1, modeldata$Origin)
qplot(data=modeldata, PC1, MaxBoltHtH, color=Origin, geom = "jitter")

moddata <- ddply(modeldata, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popMaxBoltHtH=mean(MaxBoltHtH, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC1, popMaxBoltHtH, color = Origin, 
      xlab="PC1", 
      ylab="Population mean MaxBoltHtH", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

#pc2
model1<-lmer(MaxBoltHtH  ~ Origin * PC2 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(MaxBoltHtH  ~ Origin * PC2 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(MaxBoltHtH  ~ Origin * PC2 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.7459,1)
# qchisq(558.65,1,lower=FALSE)#chisq value
# 
# modelint<-lmer(MaxBoltHtH  ~ Origin +PC2 +(1|Pop), family=gaussian,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(MaxBoltHtH  ~ Origin +(1|Pop), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(MaxBoltHtH ~ (1|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(MaxBoltHtH  ~ PC2 +(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov

#try glm
modelg <- glm(MaxBoltHtH ~ Origin*PC2, family=gaussian,data=modeldata)
modelg1 <- glm(MaxBoltHtH ~ Origin+PC2, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(MaxBoltHtH ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(MaxBoltHtH ~ PC2, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
# qchisq(0.5399,1,lower=FALSE)#chisq value
# 
# 
# modelg
# summary(modelg)
# 
# lsmeans(modelg1, ~ Origin, conf=95)
# 
# interaction.plot(response = modeldata$MaxBoltHtH, x.factor = modeldata$PC2, trace.factor = modeldata$Origin)
# plot(modeldata$PC2, modeldata$Origin)
qplot(data=modeldata, PC2, MaxBoltHtH, color=Origin, geom = "jitter")

moddata <- ddply(modeldata, .(Pop, Origin, PC2), summarize, popCount=length(Pop), popMaxBoltHtH=mean(MaxBoltHtH, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC2, popMaxBoltHtH, color = Origin, 
      xlab="PC2", 
      ylab="Population mean MaxBoltHtH", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()
# 

#pc3
model1<-lmer(MaxBoltHtH  ~ Origin * PC3 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(MaxBoltHtH  ~ Origin * PC3 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(MaxBoltHtH  ~ Origin * PC3 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.1361,1)
# qchisq(558.65,1,lower=FALSE)#chisq value
# 
# modelint<-lmer(MaxBoltHtH  ~ Origin +PC3 +(1|Pop), family=gaussian,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(MaxBoltHtH  ~ Origin +(1|Pop), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(MaxBoltHtH ~ (1|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(MaxBoltHtH  ~ PC3 +(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov

#try glm
modelg <- glm(MaxBoltHtH ~ Origin*PC3, family=gaussian,data=modeldata)
modelg1 <- glm(MaxBoltHtH ~ Origin+PC3, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(MaxBoltHtH ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(MaxBoltHtH ~ PC3, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
# qchisq(0.5399,1,lower=FALSE)#chisq value
# 
# 
# # modelg
# # summary(modelg)
# # 
# # lsmeans(modelg1, ~ Origin, conf=95)
# # 
# # interaction.plot(response = modeldata$MaxBoltHtH, x.factor = modeldata$PC3, trace.factor = modeldata$Origin)
# # plot(modeldata$PC3, modeldata$Origin)
# qplot(data=modeldata, PC3, MaxBoltHtH, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, PC3), summarize, popCount=length(Pop), popMaxBoltHtH=mean(MaxBoltHtH, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,PC3, popMaxBoltHtH, color = Origin, 
#       xlab="PC3", 
#       ylab="Population mean MaxBoltHtH", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# # 

#bio11
model1<-lmer(MaxBoltHtH  ~ Origin * bio11 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(MaxBoltHtH  ~ Origin * bio11 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(MaxBoltHtH  ~ Origin * bio11 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.1361,1)
# qchisq(558.65,1,lower=FALSE)#chisq value
# 
# modelint<-lmer(MaxBoltHtH  ~ Origin +bio11 +(1|Pop), family=gaussian,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(MaxBoltHtH  ~ Origin +(1|Pop), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(MaxBoltHtH ~ (1|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(MaxBoltHtH  ~ bio11 +(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov

#try glm
modelg <- glm(MaxBoltHtH ~ Origin*bio11, family=gaussian,data=modeldata)
modelg1 <- glm(MaxBoltHtH ~ Origin+bio11, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(MaxBoltHtH ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(MaxBoltHtH ~ bio11, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
# qchisq(0.5399,1,lower=FALSE)#chisq value
# 
# 
# # modelg
# # summary(modelg)
# # 
# # lsmeans(modelg1, ~ Origin, conf=95)
# # 
# # interaction.plot(response = modeldata$MaxBoltHtH, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# # plot(modeldata$bio11, modeldata$Origin)
# qplot(data=modeldata, bio11, MaxBoltHtH, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, bio11), summarize, popCount=length(Pop), popMaxBoltHtH=mean(MaxBoltHtH, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,bio11, popMaxBoltHtH, color = Origin, 
#       xlab="bio11", 
#       ylab="Population mean MaxBoltHtH", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# # 

#bio9
model1<-lmer(MaxBoltHtH  ~ Origin * bio9 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(MaxBoltHtH  ~ Origin * bio9 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(MaxBoltHtH  ~ Origin * bio9 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.1361,1)
# qchisq(558.65,1,lower=FALSE)#chisq value
# 
# modelint<-lmer(MaxBoltHtH  ~ Origin +bio9 +(1|Pop), family=gaussian,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(MaxBoltHtH  ~ Origin +(1|Pop), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(MaxBoltHtH ~ (1|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(MaxBoltHtH  ~ bio9 +(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov

#try glm
modelg <- glm(MaxBoltHtH ~ Origin*bio9, family=gaussian,data=modeldata)
modelg1 <- glm(MaxBoltHtH ~ Origin+bio9, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(MaxBoltHtH ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(MaxBoltHtH ~ bio9, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
# qchisq(0.5399,1,lower=FALSE)#chisq value
# 
# 
# # modelg
# # summary(modelg)
# # 
# # lsmeans(modelg1, ~ Origin, conf=95)
# # 
# # interaction.plot(response = modeldata$MaxBoltHtH, x.factor = modeldata$bio9, trace.factor = modeldata$Origin)
# # plot(modeldata$bio9, modeldata$Origin)
# qplot(data=modeldata, bio9, MaxBoltHtH, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, bio9), summarize, popCount=length(Pop), popMaxBoltHtH=mean(MaxBoltHtH, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,bio9, popMaxBoltHtH, color = Origin, 
#       xlab="bio9", 
#       ylab="Population mean MaxBoltHtH", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# # 
#bio6
model1<-lmer(MaxBoltHtH  ~ Origin * bio6 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(MaxBoltHtH  ~ Origin * bio6 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(MaxBoltHtH  ~ Origin * bio6 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.1361,1)
# qchisq(558.65,1,lower=FALSE)#chisq value
# 
# modelint<-lmer(MaxBoltHtH  ~ Origin +bio6 +(1|Pop), family=gaussian,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(MaxBoltHtH  ~ Origin +(1|Pop), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(MaxBoltHtH ~ (1|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(MaxBoltHtH  ~ bio6 +(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov

#try glm
modelg <- glm(MaxBoltHtH ~ Origin*bio6, family=gaussian,data=modeldata)
modelg1 <- glm(MaxBoltHtH ~ Origin+bio6, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(MaxBoltHtH ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(MaxBoltHtH ~ bio6, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
# qchisq(0.5399,1,lower=FALSE)#chisq value
# 
# 
# # modelg
# # summary(modelg)
# # 
# # lsmeans(modelg1, ~ Origin, conf=95)
# # 
# # interaction.plot(response = modeldata$MaxBoltHtH, x.factor = modeldata$bio6, trace.factor = modeldata$Origin)
# # plot(modeldata$bio6, modeldata$Origin)
# qplot(data=modeldata, bio6, MaxBoltHtH, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, bio6), summarize, popCount=length(Pop), popMaxBoltHtH=mean(MaxBoltHtH, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,bio6, popMaxBoltHtH, color = Origin, 
#       xlab="bio6", 
#       ylab="Population mean MaxBoltHtH", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# # 
#lat
model1<-lmer(MaxBoltHtH  ~ Origin * Latitude +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(MaxBoltHtH  ~ Origin * Latitude +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(MaxBoltHtH  ~ Origin * Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.1361,1)
# qchisq(558.65,1,lower=FALSE)#chisq value
# 
# modelint<-lmer(MaxBoltHtH  ~ Origin +Latitude +(1|Pop), family=gaussian,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(MaxBoltHtH  ~ Origin +(1|Pop), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(MaxBoltHtH ~ (1|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(MaxBoltHtH  ~ Latitude +(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov

#try glm
modelg <- glm(MaxBoltHtH ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(MaxBoltHtH ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(MaxBoltHtH ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(MaxBoltHtH ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
# qchisq(0.5399,1,lower=FALSE)#chisq value
# 
# 
# # modelg
# # summary(modelg)
# # 
# # lsmeans(modelg1, ~ Origin, conf=95)
# # 
# # interaction.plot(response = modeldata$MaxBoltHtH, x.factor = modeldata$Latitude, trace.factor = modeldata$Origin)
# # plot(modeldata$Latitude, modeldata$Origin)
# qplot(data=modeldata, Latitude, MaxBoltHtH, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, Latitude), summarize, popCount=length(Pop), popMaxBoltHtH=mean(MaxBoltHtH, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,Latitude, popMaxBoltHtH, color = Origin, 
#       xlab="Latitude", 
#       ylab="Population mean MaxBoltHtH", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# # 

#trt
model1<-lmer(MaxBoltHtH  ~ Origin * Trt +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(MaxBoltHtH  ~ Origin * Trt +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(MaxBoltHtH  ~ Origin * Trt +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.2615,1)
# qchisq(558.65,1,lower=FALSE)#chisq value
# 
# modelint<-lmer(MaxBoltHtH  ~ Origin +Trt +(1|Pop), family=gaussian,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(MaxBoltHtH  ~ Origin +(1|Pop), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(MaxBoltHtH ~ (1|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(MaxBoltHtH  ~ Trt +(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov

#try glm
modelg <- glm(MaxBoltHtH ~ Origin*Trt, family=gaussian,data=modeldata)
modelg1 <- glm(MaxBoltHtH ~ Origin+Trt, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(MaxBoltHtH ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(MaxBoltHtH ~ Trt, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
# qchisq(0.5399,1,lower=FALSE)#chisq value
# 
# 
# # modelg
# # summary(modelg)
# # 
# # lsmeans(modelg1, ~ Origin, conf=95)
# # 
# # interaction.plot(response = modeldata$MaxBoltHtH, x.factor = modeldata$Trt, trace.factor = modeldata$Origin)
# # plot(modeldata$Trt, modeldata$Origin)
# qplot(data=modeldata, Trt, MaxBoltHtH, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, Trt), summarize, popCount=length(Pop), popMaxBoltHtH=mean(MaxBoltHtH, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,Trt, popMaxBoltHtH, color = Origin, 
#       xlab="Trt", 
#       ylab="Population mean MaxBoltHtH", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# # 

###Bolt.date####################
modeldata<-frdat[!is.na(frdat$Bolt.date),]

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

#pc2
model1<-lmer(Bolt.date  ~ Origin * PC2 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * PC2 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * PC2 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.7723,1)

#pc3
model1<-lmer(Bolt.date  ~ Origin * PC3 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * PC3 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * PC3 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(13.508,1)

#bio6
model1<-lmer(Bolt.date  ~ Origin * bio6 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * bio6 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * bio6 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(8.8184,1)

#trt
model1<-lmer(Bolt.date  ~ Origin * Trt +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * Trt +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * Trt +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(12.538,1)

#lat
model1<-lmer(Bolt.date  ~ Origin * Latitude +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Bolt.date  ~ Origin * Latitude +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.7632,1)

###full models
#bio11
model1<-lmer(Bolt.date  ~ Origin * bio11 +(1|Pop/Mom), family=poisson,data=modeldata)
#false convergence
model2<-lmer(Bolt.date  ~ Origin * bio11 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Bolt.date  ~ Origin * bio11 +(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.4168,1)
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
# anova(modelg3, test="LRT")
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
1-pchisq(4.9494,1)
# qchisq(558.65,1,lower=FALSE)#chisq value
# 
# modelint<-lmer(Bolt.date  ~ Origin +bio9 +(1|Pop/Mom), family=poisson,data=modeldata)
# intAov <- anova(model1, modelint)
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
# modelOC <- lmer(Bolt.date  ~ bio9 +(1|Pop/Mom), family=poisson,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov

#try glm
modelg <- glm(Bolt.date ~ Origin*bio9, family=poisson,data=modeldata)
modelg1 <- glm(Bolt.date ~ Origin+bio9, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(Bolt.date ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
# anova(modelg3, test="LRT")
modelg2<- glm(Bolt.date ~ bio9, family=poisson,data=modeldata)
anova(modelg2,modelg1, test="LRT")
# qchisq(5.0702,1,lower=FALSE)#chisq value
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

############check pop sig################
#skip uneccessary models, such as those in repeated measures or non-optimal transformations
#focus on:bolt.bin, Mass.log, Harvest.date, Crown.log

###Crown.log####
modeldata <- subset(FrdatSK, Origin%in%c("inv", "nat"))
modeldata<-modeldata[!is.na(modeldata$Crown.log),]

modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)

#check pop sig only
#PC1
model1<-lmer(Crown.log  ~ Origin * PC1 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Crown.log  ~ Origin * PC1 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Crown.log  ~ Origin * PC1 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.2991,1)

modelint <- lmer(Crown.log ~ Origin +  PC1+ (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelint, model1)

model1
modelg <- glm(Crown.log ~ Origin*PC1, family=gaussian,data=modeldata)
summary(modelg)

CI.LS.gaussian.log(modelint)

qplot(data=modeldata,PC1, Crown.log, color = Origin)+geom_point(position="jitter")

#sk included in plot 
moddata <- ddply(FrdatSK, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popCrown.log=mean(Crown.log, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC1, popCrown.log, color = Origin, 
      xlab="PC1", 
      ylab="Population mean Crown.log", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

# modelg <- glm(Crown.log ~ Origin*PC1, family=poisson,data=modeldata)
# modelg1 <- glm(Wilt ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.8596,1,lower=FALSE)#chisq value

#PC2
model1<-lmer(Crown.log  ~ Origin * PC2 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Crown.log  ~ Origin * PC2 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Crown.log  ~ Origin * PC2 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(18.094,1)

modelint <- lmer(Crown.log ~ Origin +  PC2+ (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelint, model1)

CI.LS.gaussian.log(modelint)

qplot(data=modeldata,PC2, Crown.log, color = Origin)+geom_point(position="jitter")

#sk included in plot 
moddata <- ddply(FrdatSK, .(Pop, Origin, PC2), summarize, popCount=length(Pop), popCrown.log=mean(Crown.log, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC2, popCrown.log, color = Origin, 
      xlab="PC2", 
      ylab="Population mean Crown.log", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

#PC3
model1<-lmer(Crown.log  ~ Origin * PC3 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Crown.log  ~ Origin * PC3 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Crown.log  ~ Origin * PC3 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(16.323,1)

#bio11
model1<-lmer(Crown.log  ~ Origin * bio11 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Crown.log  ~ Origin * bio11 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Crown.log  ~ Origin * bio11 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(11.235,1)

#bio6
model1<-lmer(Crown.log  ~ Origin * bio6 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Crown.log  ~ Origin * bio6 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Crown.log  ~ Origin * bio6 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(13.97,1)

#bio9
model1<-lmer(Crown.log  ~ Origin * bio9 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Crown.log  ~ Origin * bio9 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Crown.log  ~ Origin * bio9 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(13.97,1)

#lat
model1<-lmer(Crown.log  ~ Origin * Latitude +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Crown.log  ~ Origin * Latitude +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Crown.log  ~ Origin * Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.0526,1)

#trt
model1<-lmer(Crown.log  ~ Origin * Trt +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Crown.log  ~ Origin * Trt +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Crown.log  ~ Origin * Trt +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(44.874,1)

###Mass.log###
modeldata<-frdat[!is.na(frdat$Mass.log),]

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
1-pchisq(22.107,1)

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
1-pchisq(13.54,1)

#bio6
model1<-lmer(Mass.log  ~ Origin * bio6 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log  ~ Origin * bio6 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * bio6 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(17.861,1)
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
#####Harvest.date###
modeldata<-frdat[!is.na(frdat$Harvest.date),]

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
model1<-lmer(Harvest.date  ~ Origin * bio11 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * bio11 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * bio11 +(1|blank), family=poisson,data=modeldata) # Test population effect
#false convergence? 
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(232.07,1)
# 
#bio6
model1<-lmer(Harvest.date  ~ Origin * bio6 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * bio6 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * bio6 +(1|blank), family=poisson,data=modeldata) # Test population effect
#false convergence? 
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
# 
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
model1<-lmer(Harvest.date  ~ Origin * bio9 +(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(Harvest.date  ~ Origin * bio9 +(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Harvest.date  ~ Origin * bio9 +(1|blank), family=poisson,data=modeldata) # Test population effect
#false convergence
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(232.07,1)

modelint<-lmer(Harvest.date  ~ Origin +bio9 +(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(Harvest.date  ~ Origin +(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(Harvest.date ~ (1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(Harvest.date  ~ bio9 +(1|Pop), family=poisson,data=modeldata)
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
 
###bolt.bin###
modeldata<-frdat[!is.na(frdat$bolt.bin),]

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
1-pchisq(1.8597,1)

#PC2
model1<-lmer(bolt.bin  ~ Origin * PC2 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * PC2 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * PC2 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.4517,1)

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
1-pchisq(1.1723,1)

#Latitude
model1<-lmer(bolt.bin  ~ Origin * Latitude +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * Latitude +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * Latitude +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(3.1535,1)

#trt
model1<-lmer(bolt.bin  ~ Origin * Trt +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * Trt +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * Trt +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(30.13,1)

##RoseAh.log without trt##############
modeldata <- subset(FrdatSK, Origin%in%c("inv", "nat"))
modeldata<-modeldata[!is.na(modeldata$RoseAh.log),]

modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

# #bio9
# model1<-lmer(RoseAh.log  ~ Origin * bio9 +(1|Pop/Mom), family=gaussian,data=modeldata)
# model2<-lmer(RoseAh.log  ~ Origin * bio9 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(RoseAh.log  ~ Origin * bio9 +(1|blank), family=gaussian,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(7.5152,1)

#PC1
model1<-lmer(RoseAh.log  ~ Origin * PC1 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(RoseAh.log  ~ Origin * PC1 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RoseAh.log  ~ Origin * PC1 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(6.6783,1)

modelint <- lmer(RoseAh.log ~ Origin +  PC1+ (1|Pop), family=gaussian,data=modeldata)
anova(modelint, model2)

CI.LS.gaussian.log(modelint)

qplot(data=modeldata,PC1, RoseAh.log, color = Origin)+geom_point(position="jitter")

#sk included in plot 
moddata <- ddply(FrdatSK, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popRoseAh.log=mean(RoseAh.log, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC1, popRoseAh.log, color = Origin, 
      xlab="PC1", 
      ylab="Population mean RoseAh.log", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()
# 
#PC2
model1<-lmer(RoseAh.log  ~ Origin * PC2 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(RoseAh.log  ~ Origin * PC2 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RoseAh.log  ~ Origin * PC2 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(26.895,1)

qplot(data=modeldata,PC2, RoseAh.log, color = Origin)+geom_point(position="jitter")

#sk included in plot 
moddata <- ddply(FrdatSK, .(Pop, Origin, PC2), summarize, popCount=length(Pop), popRoseAh.log=mean(RoseAh.log, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC2, popRoseAh.log, color = Origin, 
      xlab="PC2", 
      ylab="Population mean RoseAh.log", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

# 
#full models
#lat
model1<-lmer(RoseAh.log  ~ Origin * Latitude +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(RoseAh.log  ~ Origin * Latitude +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RoseAh.log  ~ Origin * Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.4424,1)

# modelint<-lmer(RoseAh.log  ~ Origin +Latitude +(1|Pop), family=gaussian,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(RoseAh.log  ~ Origin +(1|Pop), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(RoseAh.log ~ (1|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(RoseAh.log  ~ Latitude +(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov
#try glm
modelg <- glm(RoseAh.log ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(RoseAh.log ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelg3<- glm(RoseAh.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.9672,1,lower=FALSE)#chisq value
# anova(modelg3, test="LRT")
modelg2<- glm(RoseAh.log ~ Latitude, family=gaussian,data=modeldata)
anova(modelg2,modelg1)
qchisq(0.2969,1,lower=FALSE)#chisq value

modelg
summary(modelg)

CI.LS.gaussian.log(modelg1)

# interaction.plot(response = modeldata$RoseAh.log, x.factor = modeldata$Latitude, trace.factor = modeldata$Origin)
# plot(modeldata$Latitude, modeldata$Origin)
qplot(data=modeldata, Latitude, RoseAh.log, color=Origin, geom = "jitter")

moddata <- ddply(modeldata, .(Pop, Origin, Latitude), summarize, popCount=length(Pop), popRoseAh.log=mean(RoseAh.log, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,Latitude, popRoseAh.log, color = Origin, 
      xlab="Latitude", 
      ylab="Population mean RoseAh.log", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

#PC3
model1<-lmer(RoseAh.log  ~ Origin * PC3 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(RoseAh.log  ~ Origin * PC3 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RoseAh.log  ~ Origin * PC3 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(41.522,1)

modelint<-lmer(RoseAh.log  ~ Origin +PC3 +(1|Pop), family=gaussian,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(RoseAh.log  ~ Origin +(1|Pop), family=gaussian,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(RoseAh.log ~ (1|Pop), family=gaussian,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(RoseAh.log  ~ PC3 +(1|Pop), family=gaussian,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov
# # #try glm
# # modelg <- glm(RoseAh.log ~ Origin*PC3, family=gaussian,data=modeldata)
# # modelg1 <- glm(RoseAh.log ~ Origin+PC3, family=gaussian,data=modeldata)
# # anova(modelg1, modelg, test="LRT") 
# # qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# # 
# # modelg3<- glm(RoseAh.log ~ Origin, family=gaussian,data=modeldata)
# # anova(modelg3,modelg1, test="LRT")
# # qchisq(0.9672,1,lower=FALSE)#chisq value
# # anova(modelg3, test="LRT")
# # # modelg2<- glm(RoseAh.log ~ PC3, family=gaussian,data=modeldata)
# # # anova(modelg2,modelg1)
# # qchisq(0.5399,1,lower=FALSE)#chisq value
# # 
# # 
# # modelg3
# # summary(modelg3)
# # 
# # lsmeans(modelg3, ~ Origin, conf=95)
# # 
# # # interaction.plot(response = modeldata$RoseAh.log, x.factor = modeldata$PC3, trace.factor = modeldata$Origin)
# # # plot(modeldata$PC3, modeldata$Origin)
# # qplot(data=modeldata, PC3, RoseAh.log, color=Origin, geom = "jitter")
# # 
# # moddata <- ddply(modeldata, .(Pop, Origin, PC3), summarize, popCount=length(Pop), popRoseAh.log=mean(RoseAh.log, na.rm=TRUE))
# # 
# # #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# # qplot(data=moddata,PC3, popRoseAh.log, color = Origin, 
# #       xlab="PC3", 
# #       ylab="Population mean RoseAh.log", main="") +geom_smooth(method=glm, se=TRUE)
# # # dev.off()
# 
#bio6
model1<-lmer(RoseAh.log  ~ Origin * bio6 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(RoseAh.log  ~ Origin * bio6 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RoseAh.log  ~ Origin * bio6 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(12.238,1)

modelint<-lmer(RoseAh.log  ~ Origin +bio6 +(1|Pop), family=gaussian,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(RoseAh.log  ~ Origin +(1|Pop), family=gaussian,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(RoseAh.log ~ (1|Pop), family=gaussian,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(RoseAh.log  ~ bio6 +(1|Pop), family=gaussian,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov
# # #try glm
# # modelg <- glm(RoseAh.log ~ Origin*bio6, family=gaussian,data=modeldata)
# # modelg1 <- glm(RoseAh.log ~ Origin+bio6, family=gaussian,data=modeldata)
# # anova(modelg1, modelg, test="LRT") 
# # qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# # 
# # modelg3<- glm(RoseAh.log ~ Origin, family=gaussian,data=modeldata)
# # anova(modelg3,modelg1, test="LRT")
# # qchisq(0.9672,1,lower=FALSE)#chisq value
# # anova(modelg3, test="LRT")
# # # modelg2<- glm(RoseAh.log ~ bio6, family=gaussian,data=modeldata)
# # # anova(modelg2,modelg1)
# # qchisq(0.5399,1,lower=FALSE)#chisq value
# # 
# # 
# # modelg3
# # summary(modelg3)
# # 
# # lsmeans(modelg3, ~ Origin, conf=95)
# # 
# # # interaction.plot(response = modeldata$RoseAh.log, x.factor = modeldata$bio6, trace.factor = modeldata$Origin)
# # # plot(modeldata$bio6, modeldata$Origin)
# # qplot(data=modeldata, bio6, RoseAh.log, color=Origin, geom = "jitter")
# # 
# # moddata <- ddply(modeldata, .(Pop, Origin, bio6), summarize, popCount=length(Pop), popRoseAh.log=mean(RoseAh.log, na.rm=TRUE))
# # 
# # #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# # qplot(data=moddata,bio6, popRoseAh.log, color = Origin, 
# #       xlab="bio6", 
# #       ylab="Population mean RoseAh.log", main="") +geom_smooth(method=glm, se=TRUE)
# # # dev.off()
# 
#bio11
model1<-lmer(RoseAh.log  ~ Origin * bio11 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(RoseAh.log  ~ Origin * bio11 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RoseAh.log  ~ Origin * bio11 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(11.373,1)

modelint<-lmer(RoseAh.log  ~ Origin +bio11 +(1|Pop), family=gaussian,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(RoseAh.log  ~ Origin +(1|Pop), family=gaussian,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(RoseAh.log ~ (1|Pop), family=gaussian,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(RoseAh.log  ~ bio11 +(1|Pop), family=gaussian,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov
# # #try glm
# # modelg <- glm(RoseAh.log ~ Origin*bio11, family=gaussian,data=modeldata)
# # modelg1 <- glm(RoseAh.log ~ Origin+bio11, family=gaussian,data=modeldata)
# # anova(modelg1, modelg, test="LRT") 
# # qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# # 
# # modelg3<- glm(RoseAh.log ~ Origin, family=gaussian,data=modeldata)
# # anova(modelg3,modelg1, test="LRT")
# # qchisq(0.9672,1,lower=FALSE)#chisq value
# # anova(modelg3, test="LRT")
# # # modelg2<- glm(RoseAh.log ~ bio11, family=gaussian,data=modeldata)
# # # anova(modelg2,modelg1)
# # qchisq(0.5399,1,lower=FALSE)#chisq value
# # 
# # 
# # modelg3
# # summary(modelg3)
# # 
# # lsmeans(modelg3, ~ Origin, conf=95)
# # 
# # # interaction.plot(response = modeldata$RoseAh.log, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# # # plot(modeldata$bio11, modeldata$Origin)
# # qplot(data=modeldata, bio11, RoseAh.log, color=Origin, geom = "jitter")
# # 
# # moddata <- ddply(modeldata, .(Pop, Origin, bio11), summarize, popCount=length(Pop), popRoseAh.log=mean(RoseAh.log, na.rm=TRUE))
# # 
# # #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# # qplot(data=moddata,bio11, popRoseAh.log, color = Origin, 
# #       xlab="bio11", 
# #       ylab="Population mean RoseAh.log", main="") +geom_smooth(method=glm, se=TRUE)
# # # dev.off()
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
# # #try glm
# # modelg <- glm(RoseAh.log ~ Origin*Trt, family=gaussian,data=modeldata)
# # modelg1 <- glm(RoseAh.log ~ Origin+Trt, family=gaussian,data=modeldata)
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
# # 
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

##################
#models with Trt included

####RoseAh.log####
# frGLR.rose_SKtrt<- lapply(names(FrdatSK)[c(29,31:33,35:37)],function(n) CGtrait.LR_snglcov_trt("RoseAh.log",FrdatSK, covariate=n, family=gaussian))
# CGtrait_sigaov_func_Fr(frGLR.rose_SKtrt, selectaov=1:7, cutoff=0.05)
# frGLR.rose_SKtrt
# names(FrdatSK)[c(29,31:33,35:37)]
#

modeldata <- droplevels(subset(FrdatSK, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$RoseAh.log),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#PC1
model1<-lmer(RoseAh.log  ~ Origin * PC1 +Trt +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(RoseAh.log  ~ Origin * PC1 +Trt +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RoseAh.log  ~ Origin * PC1 +Trt +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(6.1418,1)
modelint<-lmer(RoseAh.log  ~ Origin +PC1 +Trt +(1|Pop), family=gaussian,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelT <- lmer(RoseAh.log  ~ Origin * PC1 +(1|Pop), family=gaussian,data=modeldata)
trtAov <- anova(model2, modelT)
trtAov

model2
modelg <- glm(RoseAh.log ~ Origin*PC1+Trt, family=gaussian,data=modeldata)
summary(modelg)


#for lsmeans, control only: 
modeldata <- droplevels(subset(FrdatSK, Origin%in%c("inv", "nat")&Trt%in%"control"))
modeldata<-modeldata[!is.na(modeldata$RoseAh.log),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)
modelint<-lmer(RoseAh.log  ~ Origin +PC1  +(1|Pop), family=gaussian,data=modeldata)
CI.LS.gaussian.log(modelint)

#for lsmeans, dr only: 
modeldata <- droplevels(subset(FrdatSK, Origin%in%c("inv", "nat")&Trt%in%"drought"))
modeldata<-modeldata[!is.na(modeldata$RoseAh.log),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)
modelint<-lmer(RoseAh.log  ~ Origin +PC1  +(1|Pop), family=gaussian,data=modeldata)
CI.LS.gaussian.log(modelint)

# 
#PC2
model1<-lmer(RoseAh.log  ~ Origin * PC2 +Trt +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(RoseAh.log  ~ Origin * PC2 +Trt+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RoseAh.log  ~ Origin * PC2 +Trt+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(27.237,1)
modelint<-lmer(RoseAh.log  ~ Origin +PC2 +Trt +(1|Pop), family=gaussian,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelT <- lmer(RoseAh.log  ~ Origin * PC2 +(1|Pop), family=gaussian,data=modeldata)
trtAov <- anova(model2, modelT)
trtAov

#for lsmeans, control only: 
modeldata <- droplevels(subset(FrdatSK, Origin%in%c("inv", "nat")&Trt%in%"control"))
modeldata<-modeldata[!is.na(modeldata$RoseAh.log),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)
modelint<-lmer(RoseAh.log  ~ Origin +PC2  +(1|Pop), family=gaussian,data=modeldata)
CI.LS.gaussian.log(modelint)

#for lsmeans, dr only: 
modeldata <- droplevels(subset(FrdatSK, Origin%in%c("inv", "nat")&Trt%in%"drought"))
modeldata<-modeldata[!is.na(modeldata$RoseAh.log),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)
modelint<-lmer(RoseAh.log  ~ Origin +PC2  +(1|Pop), family=gaussian,data=modeldata)
CI.LS.gaussian.log(modelint)
# # 
#PC3
model1<-lmer(RoseAh.log  ~ Origin * PC3 +Trt+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(RoseAh.log  ~ Origin * PC3 +Trt+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RoseAh.log  ~ Origin * PC3 +Trt+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(41.522,1)

modelint<-lmer(RoseAh.log  ~ Origin +PC3 +Trt +(1|Pop), family=gaussian,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(RoseAh.log  ~ Origin +Trt+(1|Pop), family=gaussian,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelT <- lmer(RoseAh.log  ~ Origin  +(1|Pop), family=gaussian,data=modeldata)
trtAov <- anova(modelcov, modelT)
trtAov

modelO<-lmer(RoseAh.log ~ Trt+(1|Pop), family=gaussian,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

#bio11
model1<-lmer(RoseAh.log  ~ Origin * bio11 +Trt+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(RoseAh.log  ~ Origin * bio11 +Trt+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RoseAh.log  ~ Origin * bio11 +Trt+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(11.522,1)

modelint<-lmer(RoseAh.log  ~ Origin +bio11 +Trt +(1|Pop), family=gaussian,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelT <- lmer(RoseAh.log  ~ Origin * bio11 +(1|Pop), family=gaussian,data=modeldata)
trtAov <- anova(model2, modelT)
trtAov
# # 
#bio6
model1<-lmer(RoseAh.log  ~ Origin * bio6 +Trt+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(RoseAh.log  ~ Origin * bio6 +Trt+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RoseAh.log  ~ Origin * bio6 +Trt+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(12.354,1)

modelint<-lmer(RoseAh.log  ~ Origin +bio6 +Trt +(1|Pop), family=gaussian,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelT <- lmer(RoseAh.log  ~ Origin * bio6 +(1|Pop), family=gaussian,data=modeldata)
trtAov <- anova(model2, modelT)
trtAov

#bio9
model1<-lmer(RoseAh.log  ~ Origin * bio9 +Trt+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(RoseAh.log  ~ Origin * bio9 +Trt+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RoseAh.log  ~ Origin * bio9 +Trt+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(7.5369,1)

modelint<-lmer(RoseAh.log  ~ Origin +bio9 +Trt +(1|Pop), family=gaussian,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelT <- lmer(RoseAh.log  ~ Origin * bio9 +(1|Pop), family=gaussian,data=modeldata)
trtAov <- anova(model2, modelT)
trtAov

#lat
model1<-lmer(RoseAh.log  ~ Origin * Latitude +Trt+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(RoseAh.log  ~ Origin * Latitude +Trt+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RoseAh.log  ~ Origin * Latitude +Trt+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(64.593,1)
# 
#try glm
modelg <- glm(RoseAh.log ~ Origin*Latitude+Trt, family=gaussian,data=modeldata)
modelg1 <- glm(RoseAh.log ~ Origin+Latitude+Trt, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value

modelgT<- glm(RoseAh.log ~ Origin*Latitude, family=gaussian,data=modeldata)
anova(modelgT,modelg, test="LRT")
qchisq(0.9672,1,lower=FALSE)#chisq value
# anova(modelg3, test="LRT")
# # modelg2<- glm(RoseAh.log ~ bio6, family=gaussian,data=modeldata)
# # anova(modelg2,modelg1)
# qchisq(0.5399,1,lower=FALSE)#chisq value


#######univariate versions of tmpt data below here######
# ###MaxLfWdth2##############
# CGtrait.LR_snglcov_int(trait="MaxLfWdth2", df=frdat, covariate="PC1", family=gaussian)
# CGtrait.models_snglcov_int(trait="MaxLfWdth2", df=frdat, covariate="PC1", family=gaussian)
# 
# modeldata<-frdat[!is.na(frdat$MaxLfWdth2),]
# 
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
# # 
# #pc1
# model1<-lmer(MaxLfWdth2  ~ Origin * PC1 +(1|Pop/Mom), family=gaussian,data=modeldata)
# model2<-lmer(MaxLfWdth2  ~ Origin * PC1 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(MaxLfWdth2  ~ Origin * PC1 +(1|blank), family=gaussian,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(6.1514,1)
# # qchisq(558.65,1,lower=FALSE)#chisq value
# # 
# # modelint<-lmer(MaxLfWdth2  ~ Origin +PC1 +(1|Pop), family=gaussian,data=modeldata)
# # intAov <- anova(model2, modelint)
# # intAov
# # 
# # modelcov <- lmer(MaxLfWdth2  ~ Origin +(1|Pop), family=gaussian,data=modeldata)
# # covAov <- anova(modelint, modelcov)
# # covAov
# # 
# # modelO<-lmer(MaxLfWdth2 ~ (1|Pop), family=gaussian,data=modeldata)
# # originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# # originAov
# # 
# # modelOC <- lmer(MaxLfWdth2  ~ PC1 +(1|Pop), family=gaussian,data=modeldata)
# # ocAov <- anova(modelint, modelOC)
# # ocAov
# #try glm
# modelg <- glm(MaxLfWdth2 ~ Origin*PC1, family=gaussian,data=modeldata)
# modelg1 <- glm(MaxLfWdth2 ~ Origin+PC1, family=gaussian,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# 
# modelg3<- glm(MaxLfWdth2 ~ Origin, family=gaussian,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
# anova(modelg3, test="LRT")
# # modelg2<- glm(MaxLfWdth2 ~ PC1, family=gaussian,data=modeldata)
# # anova(modelg2,modelg1)
# qchisq(0.5399,1,lower=FALSE)#chisq value
# 
# 
# modelg3
# summary(modelg3)
# 
# lsmeans(modelg3, ~ Origin, conf=95)
# 
# interaction.plot(response = modeldata$MaxLfWdth2, x.factor = modeldata$PC1, trace.factor = modeldata$Origin)
# plot(modeldata$PC1, modeldata$Origin)
# qplot(data=modeldata, PC1, MaxLfWdth2, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, PC1), summarize, popCount=length(Pop), popMaxLfWdth2=mean(MaxLfWdth2, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,PC1, popMaxLfWdth2, color = Origin, 
#       xlab="PC1", 
#       ylab="Population mean MaxLfWdth2", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# 
# # checking the normality of residuals e_i:
# # MaxLfWdth2 vs LfWdth2.log
# plot(resid(modelg3) ~ fitted(modelg3),main="residual plot")
# abline(h=0)
# qqnorm(resid(modelg3), main="Q-Q plot for residuals")
# qqline(resid(modelg3))
# 
# modelg3log<- glm(LfWdth2.log ~ Origin, family=gaussian,data=modeldata)
# qqnorm(resid(modelg3log), main="Q-Q plot for residuals")
# qqline(resid(modelg3log))
# #log is worse, DO NOT TRANSFORM
# 
# #pc2
# model1<-lmer(MaxLfWdth2  ~ Origin * PC2 +(1|Pop/Mom), family=gaussian,data=modeldata)
# model2<-lmer(MaxLfWdth2  ~ Origin * PC2 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(MaxLfWdth2  ~ Origin * PC2 +(1|blank), family=gaussian,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
# popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# popAov
# 1-pchisq(3.7459,1)
# # qchisq(558.65,1,lower=FALSE)#chisq value
# # 
# # modelint<-lmer(MaxLfWdth2  ~ Origin +PC2 +(1|Pop), family=gaussian,data=modeldata)
# # intAov <- anova(model2, modelint)
# # intAov
# # 
# # modelcov <- lmer(MaxLfWdth2  ~ Origin +(1|Pop), family=gaussian,data=modeldata)
# # covAov <- anova(modelint, modelcov)
# # covAov
# # 
# # modelO<-lmer(MaxLfWdth2 ~ (1|Pop), family=gaussian,data=modeldata)
# # originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# # originAov
# # 
# # modelOC <- lmer(MaxLfWdth2  ~ PC2 +(1|Pop), family=gaussian,data=modeldata)
# # ocAov <- anova(modelint, modelOC)
# # ocAov
# 
# #try glm
# modelg <- glm(MaxLfWdth2 ~ Origin*PC2, family=gaussian,data=modeldata)
# modelg1 <- glm(MaxLfWdth2 ~ Origin+PC2, family=gaussian,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#put in pval to get chisq value
# 
# modelg3<- glm(MaxLfWdth2 ~ Origin, family=gaussian,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
# anova(modelg3, test="LRT")
# modelg2<- glm(MaxLfWdth2 ~ PC2, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
# qchisq(0.5399,1,lower=FALSE)#chisq value
# 
# 
# modelg
# summary(modelg)
# 
# lsmeans(modelg1, ~ Origin, conf=95)
# 
# interaction.plot(response = modeldata$MaxLfWdth2, x.factor = modeldata$PC2, trace.factor = modeldata$Origin)
# plot(modeldata$PC2, modeldata$Origin)
# qplot(data=modeldata, PC2, MaxLfWdth2, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, PC2), summarize, popCount=length(Pop), popMaxLfWdth2=mean(MaxLfWdth2, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,PC2, popMaxLfWdth2, color = Origin, 
#       xlab="PC2", 
#       ylab="Population mean MaxLfWdth2", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# # 
# # checking the normality of residuals e_i:
# # MaxLfWdth2 vs LfWdth2.log
# # plot(resid(modelg3) ~ fitted(modelg3),main="residual plot")
# # abline(h=0)
# qqnorm(resid(modelg), main="Q-Q plot for residuals")
# qqline(resid(modelg))
# 
# modelglog<- glm(LfWdth2.log ~ Origin*PC2, family=gaussian,data=modeldata)
# qqnorm(resid(modelg3log), main="Q-Q plot for residuals")
# qqline(resid(modelg3log))
# #log is worse, DO NOT TRANSFORM

#######################example##########################
# ######Allo, Origin * Lat models######
# al<-read.table("STAllosubset.txt", header=T, sep="\t", quote='"', row.names=1) #allosubset
# head(al)
# alLR <- lapply(names(al)[c(11:13, 20)],function(n) CGtrait.LR.int(n,al)) #crow, shoot, root, root.log, all gaussian
# names(alLR) <- names(al)[c(11:13, 20)]
# alLR #check out LRs of models. Model progression logical?
# almodels <- CGtrait.models.int("CrownDiam.mmA",al)
# almodels
# 
# ###allo shoot, mom is sig, do by hand
# modeldata<-al[!is.na(al$ShootMass.gA),]
# modeldata$blank<-1
# modeldata$blank<-as.factor(modeldata$blank)
# modeldata$Mom<-as.factor(modeldata$Mom)
# 
# # modelRack<-lmer(ShootMass.gA  ~ Origin* Latitude +(1|PopID/Mom)+ (1|Rack), family=gaussian,data=modeldata)
# modelobar<-lmer(ShootMass.gA  ~ Origin* Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
# model1raw<-lmer(ShootMass.gA  ~ Origin* Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelobar, model1raw)
# # anova(modelRack, model1raw)
# model2raw<-lmer(ShootMass.gA  ~ Origin* Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3raw<-lmer(ShootMass.gA  ~ Origin* Latitude + (1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2raw,model1raw) # mom is sig!
# anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 1-pchisq(13.097,1)
# 
# modelI <- lmer(ShootMass.gA  ~ Origin + Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelI,model1raw)
# 
# modelL<-lmer(ShootMass.gA  ~ Origin + (1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelL, modelI)
# 
# modelOraw<-lmer(ShootMass.gA ~ Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelOraw,modelI) #test for significance of origin - origin not sig
# 
# lsmeans(modelI, ~ Origin, conf=95)
###################demo#############
# #try glm
# modelg <- glm(sla.log ~ Origin*Latitude, family=gaussian,data=modeldata)
# modelg1 <- glm(sla.log ~ Origin+Latitude, family=gaussian,data=modeldata)
# anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#chisq value
# 
# modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
# anova(modelg3, test="LRT")
# # modelg2<- glm(sla.log ~ Latitude, family=gaussian,data=modeldata)
# # anova(modelg2,modelg1)
# qchisq(0.5399,1,lower=FALSE)#chisq value

# # checking the normality of residuals e_i:
# plot(resid(modelg3) ~ fitted(modelg3),main="residual plot")
# abline(h=0)
# qqnorm(resid(modelg3), main="Q-Q plot for residuals")
# qqline(resid(modelg3))
##################early stuff##############
# #so for each cov and distribution
# 
# #PC1
# frGLR.PC1 <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="PC1"))#apply func to all gaussian traits
# frPLR.PC1 <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="PC1", family=poisson))#apply func to all poisson traits
# boltLR.PC1 <- CGtrait.LR_snglcov(trait="bolt.bin",df=frdat,covariate="PC1",family=binomial) #apply to single binomial trait
# 
# #PC2
# frGLR.PC2 <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="PC2"))#apply func to all gaussian traits
# frPLR.PC2 <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="PC2", family=poisson))#apply func to all poisson traits
# boltLR.PC2 <- CGtrait.LR_snglcov(trait="bolt.bin",df=frdat,covariate="PC2",family=binomial) #apply to single binomial trait
# 
# #PC3
# frGLR.PC3 <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="PC3"))#apply func to all gaussian traits
# frPLR.PC3 <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="PC3", family=poisson))#apply func to all poisson traits
# boltLR.PC3 <- CGtrait.LR_snglcov(trait="bolt.bin",df=frdat,covariate="PC3",family=binomial) #apply to single binomial trait
# 
# #bio4
# frGLR.bio4 <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="bio4"))#apply func to all gaussian traits
# frPLR.bio4 <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="bio4", family=poisson))#apply func to all poisson traits
# boltLR.bio4 <- CGtrait.LR_snglcov(trait="bolt.bin",df=frdat,covariate="bio4",family=binomial) #apply to single binomial trait
# 
# #bio19
# frGLR.bio19 <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="bio19"))#apply func to all gaussian traits
# frPLR.bio19 <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="bio19", family=poisson))#apply func to all poisson traits
# boltLR.bio19 <- CGtrait.LR_snglcov(trait="bolt.bin",df=frdat,covariate="bio19",family=binomial) #apply to single binomial trait
# 
# #bio7
# frGLR.bio7 <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="bio7"))#apply func to all gaussian traits
# frPLR.bio7 <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="bio7", family=poisson))#apply func to all poisson traits
# boltLR.bio7 <- CGtrait.LR_snglcov(trait="bolt.bin",df=frdat,covariate="bio7",family=binomial) #apply to single binomial trait
# 
# #Latitude
# frGLR.lat <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="Latitude"))#apply func to all gaussian traits
# frPLR.lat <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="Latitude", family=poisson))#apply func to all poisson traits
# boltLR.lat <- CGtrait.LR_snglcov(trait="bolt.bin",df=frdat,covariate="Latitude",family=binomial) #apply to single binomial trait
# 
# #which anovas have sig covariate or origin?
# snglcov <- c(frGLR.PC1, frPLR.PC1, boltLR.PC1,frGLR.PC2, frPLR.PC2, boltLR.PC2,frGLR.PC3, frPLR.PC3, boltLR.PC3,
# #              frGLR.bio4,frPLR.bio4, boltLR.bio4,frGLR.bio7, frPLR.bio7, boltLR.bio7,frGLR.bio19, frPLR.bio19, boltLR.bio19,
#              frGLR.lat, frPLR.lat, boltLR.lat, frGLR.Trt, frPLR.Trt, boltLR.Trt)
# save(snglcov, file="Fr_aovlists.RData")
# load()
# 
# 
# CGtrait_sigaov_func_Fr(frGLR.PC1)
# CGtrait_sigaov_func_Fr(frGLR.PC2)
# CGtrait_sigaov_func_Fr(frGLR.PC3)
# CGtrait_sigaov_func_Fr(frGLR.bio4)
# CGtrait_sigaov_func_Fr(frGLR.bio7)
# CGtrait_sigaov_func_Fr(frGLR.bio19)
# CGtrait_sigaov_func_Fr(frGLR.lat)
# 
# CGtrait_sigaov_func_Fr(frPLR.PC1)
# CGtrait_sigaov_func_Fr(frPLR.PC2)
# CGtrait_sigaov_func_Fr(frPLR.PC3)
# CGtrait_sigaov_func_Fr(frPLR.bio4)
# CGtrait_sigaov_func_Fr(frPLR.bio7)
# CGtrait_sigaov_func_Fr(frPLR.bio19)
# CGtrait_sigaov_func_Fr(frPLR.lat)
# 
# boltLR.PC1
# boltLR.PC2
# boltLR.PC3
# boltLR.bio4
# boltLR.bio7
# boltLR.bio19
# boltLR.lat
# 