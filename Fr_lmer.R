#mixed-effects models of univariate traits
#lots of climate variables! Including climate PCA
#Sept 2013

#read
frdat<- read.table("FrTraitClimDat.txt", header=T, sep="\t",quote='"', row.names=1)

#Control and drought, REML, using lme4
#mixed effect models 
library(lme4)
library(lsmeans)
library(ggplot2)
library(plyr)

#for each normal trait, compare this general set of models
modelfull<-lmer(trait  ~ Origin* Trt+PC1+PC2+bio19+bio4+bio7 +(Origin|PopID/Mom), family=gaussian,data=modeldata)#not inlcuded in CGtrait.LR functions

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

##########does Trt matter? ###########################
###############try Shoot.mass.gH, Bolt.date, Rose.AreaH.m2, manova of all....?
modeldata <- frdat[!is.na(frdat$Shoot.mass.gH),]
# modeldata$Mom <- as.factor(modeldata$Mom)
modelg <- glm(Shoot.mass.gH~Trt, family=gaussian,data=modeldata)
anova(modelg, test="LRT")

modeldata <- frdat[!is.na(frdat$Bolt.date),]
# modeldata$Mom <- as.factor(modeldata$Mom)
modelg <- glm(Bolt.date~Trt, family=poisson,data=modeldata)
anova(modelg, test="LRT")

modeldata <- frdat[!is.na(frdat$Rose.AreaH.m2),]
# modeldata$Mom <- as.factor(modeldata$Mom)
modelg <- glm(Rose.AreaH.m2~Trt, family=gaussian,data=modeldata)
anova(modelg, test="LRT")

qplot(data=modeldata, Rose.AreaH.m2, Shoot.mass.gH, color=Trt)+geom_smooth(method=glm, se=FALSE)

mantraits<-cbind(frdat$Shoot.mass.gH, frdat$Rose.AreaH.m2, frdat$Bolt.date)
#fitOrigin<-manova(m2traits~Frm2DKdatdes$Origin)
#fittrt<-manova(m2traits~Frm2datTag$trt)
#fitOrigintrt<-manova(m2traits~Frm2datTag$Origin+Frm2datTag$trt)
#fitOriginbytrt<-manova(m2traits~Frm2DKdatdes$Origin+Frm2DKdatdes$Trt+Frm2DKdatdes$Origin*Frm2DKdatdes$Trt)
man<-manova(mantraits~Trt, data=frdat)
summary.aov(man)
summary(man)

allmantraits<-as.matrix(cbind(frdat[c(9:10,16,18:19,27:34,8,17,24,25,35)]))#all traits except bolt.bin
allman<-manova(allmantraits~Trt, data=frdat)
summary.aov(allman)#only sig in crown diameter
summary(allman)#marginal

#########
#test for one trait, one df, specify non default (gaussian) family
CGtrait.LR_snglcov(trait="Mass.log",df=frdat,covariate="PC1",family=gaussian)
CGtrait.LR_snglcov(trait="Mass.log",df=frdat,covariate="bio4",family=gaussian)
CGtrait.LR_snglcov(trait="Mass.log",df=frdat,covariate="bio19",family=gaussian)
CGtrait.LR_snglcov(trait="Mass.log",df=frdat,covariate="Latitude",family=gaussian)
CGtrait.LR_snglcov(trait="Mass.log",df=frdat,covariate="bio7",family=gaussian)
CGtrait.LR_snglcov(trait="Mass.log",df=frdat,covariate="PC2",family=gaussian)
CGtrait.LR_snglcov(trait="Mass.log",df=frdat,covariate="PC3",family=gaussian)
CGtrait.LR_snglcov(trait="Mass.log",df=frdat,covariate="PC4",family=gaussian)

#for all traits in a df
#make sure all traits analyzed this way are the same distribution family
names(frdat)#find col numbers for traits of interestes
frGLR <- lapply(names(frdat)[41:42],function(n) CGtrait.LR_snglcov(n,frdat, covariate="PC1"))#apply func to all things in list


#to get models
frGmodels <- lapply(names(frdat)[c(34,42)],function(n) CGtrait.models_snglcov(n,frdat, covariate="PC1"))
names(frGmodels) <- names(frdat[c(34,42)])

#to get one model
frGmodels[[1]][1] #first number is trait in column order of df, second number is model number
names(frGmodels[1]) #to verify trait

#to check normality of residuals
mass.lmer <- frGmodels[[1]]$model2
plot(resid(mass.lmer) ~ fitted(mass.lmer),main="residual plot")
abline(h=0)

# checking the normality of residuals e_i:
qqnorm(resid(mass.lmer), main="Q-Q plot for residuals")
qqline(resid(mass.lmer))

#so for each cov and distribution

#PC1
frGLR.PC1 <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="PC1"))#apply func to all gaussian traits
frPLR.PC1 <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="PC1", family=poisson))#apply func to all poisson traits
boltLR.PC1 <- CGtrait.LR_snglcov(trait="bolt.bin",df=frdat,covariate="PC1",family=binomial) #apply to single binomial trait

#PC2
frGLR.PC2 <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="PC2"))#apply func to all gaussian traits
frPLR.PC2 <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="PC2", family=poisson))#apply func to all poisson traits
boltLR.PC2 <- CGtrait.LR_snglcov(trait="bolt.bin",df=frdat,covariate="PC2",family=binomial) #apply to single binomial trait

#PC3
frGLR.PC3 <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="PC3"))#apply func to all gaussian traits
frPLR.PC3 <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="PC3", family=poisson))#apply func to all poisson traits
boltLR.PC3 <- CGtrait.LR_snglcov(trait="bolt.bin",df=frdat,covariate="PC3",family=binomial) #apply to single binomial trait

#bio4
frGLR.bio4 <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="bio4"))#apply func to all gaussian traits
frPLR.bio4 <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="bio4", family=poisson))#apply func to all poisson traits
boltLR.bio4 <- CGtrait.LR_snglcov(trait="bolt.bin",df=frdat,covariate="bio4",family=binomial) #apply to single binomial trait

#bio19
frGLR.bio19 <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="bio19"))#apply func to all gaussian traits
frPLR.bio19 <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="bio19", family=poisson))#apply func to all poisson traits
boltLR.bio19 <- CGtrait.LR_snglcov(trait="bolt.bin",df=frdat,covariate="bio19",family=binomial) #apply to single binomial trait

#bio7
frGLR.bio7 <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="bio7"))#apply func to all gaussian traits
frPLR.bio7 <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="bio7", family=poisson))#apply func to all poisson traits
boltLR.bio7 <- CGtrait.LR_snglcov(trait="bolt.bin",df=frdat,covariate="bio7",family=binomial) #apply to single binomial trait

#Latitude
frGLR.lat <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="Latitude"))#apply func to all gaussian traits
frPLR.lat <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="Latitude", family=poisson))#apply func to all poisson traits
boltLR.lat <- CGtrait.LR_snglcov(trait="bolt.bin",df=frdat,covariate="Latitude",family=binomial) #apply to single binomial trait



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