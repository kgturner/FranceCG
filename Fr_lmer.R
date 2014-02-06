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
a1 <- anova(modelg, test="LRT")

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

#Trt
frGLR.Trt <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="Trt"))#apply func to all gaussian traits
frPLR.Trt <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="Trt", family=poisson))#apply func to all poisson traits
boltLR.Trt <- CGtrait.LR_snglcov(trait="bolt.bin",df=frdat,covariate="Trt",family=binomial) #apply to single binomial trait

CGtrait_sigaov_func_Fr(frGLR.Trt, selectaov=3:6)
CGtrait_sigaov_func_Fr(frPLR.Trt, selectaov=3:6)
boltLR.Trt

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

#which anovas have sig covariate or origin?
snglcov <- c(frGLR.PC1, frPLR.PC1, boltLR.PC1,frGLR.PC2, frPLR.PC2, boltLR.PC2,frGLR.PC3, frPLR.PC3, boltLR.PC3,
#              frGLR.bio4,frPLR.bio4, boltLR.bio4,frGLR.bio7, frPLR.bio7, boltLR.bio7,frGLR.bio19, frPLR.bio19, boltLR.bio19,
             frGLR.lat, frPLR.lat, boltLR.lat, frGLR.Trt, frPLR.Trt, boltLR.Trt)
save(snglcov, file="Fr_aovlists.RData")
load()


CGtrait_sigaov_func_Fr(frGLR.PC1)
CGtrait_sigaov_func_Fr(frGLR.PC2)
CGtrait_sigaov_func_Fr(frGLR.PC3)
CGtrait_sigaov_func_Fr(frGLR.bio4)
CGtrait_sigaov_func_Fr(frGLR.bio7)
CGtrait_sigaov_func_Fr(frGLR.bio19)
CGtrait_sigaov_func_Fr(frGLR.lat)

CGtrait_sigaov_func_Fr(frPLR.PC1)
CGtrait_sigaov_func_Fr(frPLR.PC2)
CGtrait_sigaov_func_Fr(frPLR.PC3)
CGtrait_sigaov_func_Fr(frPLR.bio4)
CGtrait_sigaov_func_Fr(frPLR.bio7)
CGtrait_sigaov_func_Fr(frPLR.bio19)
CGtrait_sigaov_func_Fr(frPLR.lat)

boltLR.PC1
boltLR.PC2
boltLR.PC3
boltLR.bio4
boltLR.bio7
boltLR.bio19
boltLR.lat

##########single covariate, interaction###############
#so for each cov and distribution

#PC1
frGLR.PC1_int <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="PC1"))#apply func to all gaussian traits
frPLR.PC1_int <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="PC1", family=poisson))#apply func to all poisson traits
boltLR.PC1_int <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=frdat,covariate="PC1",family=binomial) #apply to single binomial trait

#PC2
frGLR.PC2_int <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="PC2"))#apply func to all gaussian traits
frPLR.PC2_int <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="PC2", family=poisson))#apply func to all poisson traits
boltLR.PC2_int <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=frdat,covariate="PC2",family=binomial) #apply to single binomial trait

#PC3
frGLR.PC3_int <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="PC3"))#apply func to all gaussian traits
frPLR.PC3_int <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="PC3", family=poisson))#apply func to all poisson traits
boltLR.PC3_int <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=frdat,covariate="PC3",family=binomial) #apply to single binomial trait

#bio4
frGLR.bio4_int <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="bio4"))#apply func to all gaussian traits
frPLR.bio4_int <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="bio4", family=poisson))#apply func to all poisson traits
boltLR.bio4_int <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=frdat,covariate="bio4",family=binomial) #apply to single binomial trait

#bio19
frGLR.bio19_int <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="bio19"))#apply func to all gaussian traits
frPLR.bio19_int <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="bio19", family=poisson))#apply func to all poisson traits
boltLR.bio19_int <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=frdat,covariate="bio19",family=binomial) #apply to single binomial trait

#bio7
frGLR.bio7_int <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="bio7"))#apply func to all gaussian traits
frPLR.bio7_int <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="bio7", family=poisson))#apply func to all poisson traits
boltLR.bio7_int <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=frdat,covariate="bio7",family=binomial) #apply to single binomial trait

#Latitude
frGLR.lat_int <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="Latitude"))#apply func to all gaussian traits
frPLR.lat_int <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="Latitude", family=poisson))#apply func to all poisson traits
boltLR.lat_int <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=frdat,covariate="Latitude",family=binomial) #apply to single binomial trait

#Trt
frGLR.trt_int <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="Trt"))#apply func to all gaussian traits
frPLR.trt_int <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov_int(n,frdat, covariate="Trt", family=poisson))#apply func to all poisson traits
boltLR.trt_int <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=frdat,covariate="Trt",family=binomial) #apply to single binomial trait


#which anovas have sig covariate or origin?
snglcov_int <- c(frGLR.PC1_int, frPLR.PC1_int, boltLR.PC1_int,frGLR.PC2_int, frPLR.PC2_int, boltLR.PC2_int,frGLR.PC3_int, frPLR.PC3_int, boltLR.PC3_int,
            frGLR.bio4_int, frGLR.bio7_int,  frGLR.bio19_int,  boltLR.bio19_int,
             frGLR.lat_int,  boltLR.lat_int, frGLR.trt_int, frPLR.trt_int, boltLR.trt_int)
# frPLR.bio4_int,frPLR.bio7_int,frPLR.bio19_int,frPLR.lat_int,boltLR.bio4_int,boltLR.bio7_int,
save(snglcov_int, file="Fr_int_aovlists.RData")
load()


CGtrait_sigaov_func_Fr(frGLR.PC1_int, selectaov=3:6)
CGtrait_sigaov_func_Fr(frGLR.PC2_int, selectaov=3:6)
CGtrait_sigaov_func_Fr(frGLR.PC3_int, selectaov=3:6)
CGtrait_sigaov_func_Fr(frGLR.bio4_int, selectaov=3:6)
CGtrait_sigaov_func_Fr(frGLR.bio7_int, selectaov=3:6)
CGtrait_sigaov_func_Fr(frGLR.bio19_int, selectaov=3:6)
CGtrait_sigaov_func_Fr(frGLR.lat_int, selectaov=3:6)

CGtrait_sigaov_func_Fr(frPLR.PC1_int, selectaov=3:6)
CGtrait_sigaov_func_Fr(frPLR.PC2_int, selectaov=3:6)
CGtrait_sigaov_func_Fr(frPLR.PC3_int, selectaov=3:6)
# CGtrait_sigaov_func_Fr(frPLR.bio4_int)
# CGtrait_sigaov_func_Fr(frPLR.bio7_int)
# CGtrait_sigaov_func_Fr(frPLR.bio19_int)
# CGtrait_sigaov_func_Fr(frPLR.lat_int)

boltLR.PC1_int
boltLR.PC2_int
boltLR.PC3_int
# boltLR.bio4_int
# boltLR.bio7_int
boltLR.bio19_int
boltLR.lat_int

CGtrait_sigaov_func_Fr(frGLR.trt_int, selectaov=3:6)
CGtrait_sigaov_func_Fr(frPLR.trt_int, selectaov=3:6)
boltLR.trt_int

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