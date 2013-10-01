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



#
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
frGLR <- lapply(names(frdat)[c(9:10,16,18:19,27:34)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="PC1"))#apply func to all gaussian traits
frPLR <- lapply(names(frdat)[c(8,17,24,25,35)],function(n) CGtrait.LR_snglcov(n,frdat, covariate="PC1", family=poisson))#apply func to all poisson traits
boltLR <- CGtrait.LR_snglcov(trait="bolt.bin",df=frdat,covariate="PC1",family=binomial) #apply to single binomial trait





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