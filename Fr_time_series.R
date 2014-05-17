#Fr data
#time series analysis

library(lme4)
library(ggplot2)
library(plyr)

#DK only
#read
frdat.l<- read.table("FrTraitClimDat_DKonly_long.txt", header=T, sep="\t",quote='"', row.names=1)

#OR
Frdatsk.l<- read.table("FrTraitClimDat_SK_long.txt", header=T, sep="\t",quote='"', row.names=1)
#for DK only include:
subset(Frdatsk.l, Origin%in%c("inv", "nat"))

###########sngl cov with interaction########################
#so for each cov and distribution
CGtrait.LR_snglcov_int_mdate()
CGtrait.models_snglcov_int_mdate()

#PC1
frGLR.PC1_time <- lapply(names(frdat.l)[c(30,31,33)],function(n) CGtrait.LR_snglcov_int_mdate(n,frdat.l, covariate="PC1"))#apply func to all gaussian traits
frPLR.PC1_time <- CGtrait.LR_snglcov_int_mdate(trait="lfc",df=frdat.l, covariate="PC1", family=poisson)#apply func to all poisson traits

#PC2
frGLR.PC2_time <- lapply(names(frdat.l)[c(30,31,33)],function(n) CGtrait.LR_snglcov_int_mdate(n,frdat.l, covariate="PC2"))#apply func to all gaussian traits
frPLR.PC2_time <- CGtrait.LR_snglcov_int_mdate(trait="lfc",df=frdat.l, covariate="PC2", family=poisson)#apply func to all poisson traits

#PC3
frGLR.PC3_time <- lapply(names(frdat.l)[c(30,31,33)],function(n) CGtrait.LR_snglcov_int_mdate(n,frdat.l, covariate="PC3"))#apply func to all gaussian traits
frPLR.PC3_time <- CGtrait.LR_snglcov_int_mdate(trait="lfc",df=frdat.l, covariate="PC3", family=poisson)#apply func to all poisson traits

#bio11
frGLR.bio11_time <- lapply(names(frdat.l)[c(30,31,33)],function(n) CGtrait.LR_snglcov_int_mdate(n,frdat.l, covariate="bio11"))#apply func to all gaussian traits
frPLR.bio11_time <- CGtrait.LR_snglcov_int_mdate(trait="lfc",df=frdat.l, covariate="bio11", family=poisson)#apply func to all poisson traits

#bio9
frGLR.bio9_time <- lapply(names(frdat.l)[c(30,31,33)],function(n) CGtrait.LR_snglcov_int_mdate(n,frdat.l, covariate="bio9"))#apply func to all gaussian traits
frPLR.bio9_time <- CGtrait.LR_snglcov_int_mdate(trait="lfc",df=frdat.l, covariate="bio9", family=poisson)#apply func to all poisson traits

#bio6
frGLR.bio6_time <- lapply(names(frdat.l)[c(30,31,33)],function(n) CGtrait.LR_snglcov_int_mdate(n,frdat.l, covariate="bio6"))#apply func to all gaussian traits
frPLR.bio6_time <- CGtrait.LR_snglcov_int_mdate(trait="lfc",df=frdat.l, covariate="bio6", family=poisson)#apply func to all poisson traits

#Latitude
frGLR.lat_time <- lapply(names(frdat.l)[c(30,31,33)],function(n) CGtrait.LR_snglcov_int_mdate(n,frdat.l, covariate="Latitude"))#apply func to all gaussian traits
frPLR.lat_time <- CGtrait.LR_snglcov_int_mdate(trait="lfc",df=frdat.l, covariate="Latitude", family=poisson)#apply func to all poisson traits

#Treatment
frGLR.trt_time <- lapply(names(frdat.l)[c(30,31,33)],function(n) CGtrait.LR_snglcov_int_mdate(n,frdat.l, covariate="Trt"))#apply func to all gaussian traits
frPLR.trt_time <- CGtrait.LR_snglcov_int_mdate(trait="lfc",df=frdat.l, covariate="Trt", family=poisson)#apply func to all poisson traits

# all the lfc had non-convergences, have to do by hand; remove harvest measure? time series needed at all?

CGtrait_sigaov_func_Fr(frGLR.PC1_time, selectaov=1:7)
CGtrait_sigaov_func_Fr(frGLR.PC2_time, selectaov=1:7,cutoff=0.05)
CGtrait_sigaov_func_Fr(frGLR.PC3_time, selectaov=1:7)
CGtrait_sigaov_func_Fr(frGLR.bio11_time, selectaov=1:7,cutoff=0.05)
CGtrait_sigaov_func_Fr(frGLR.bio9_time, selectaov=1:7)
CGtrait_sigaov_func_Fr(frGLR.bio6_time, selectaov=1:7,cutoff=0.05)
CGtrait_sigaov_func_Fr(frGLR.lat_time, selectaov=1:7)
CGtrait_sigaov_func_Fr(frGLR.trt_time, selectaov=1:7)

##########DK single traits##########################
###lfc##############
CGtrait.LR_snglcov_int_mdate(trait="lfc", df=subset(Frdatsk.l, Origin%in%c("inv", "nat")), covariate="PC1", family=poisson)
CGtrait.models_snglcov_int_mdate(trait="lfc", df=frdat.l, covariate="PC1", family=poisson)

# modeldata<-frdat.l[!is.na(subset(Frdatsk.l, Origin%in%c("inv", "nat")).l$lfc),]
# 
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)#
# modeldata <- modeldata[modeldata$lfc<200,]
# # modeldata <- modeldata[modeldata$time==3,] #only 95/226 individuals have leaf counts for harvest

modeldata <- droplevels(subset(Frdatsk.l, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$lfc),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- modeldata[modeldata$lfc<200,]
modeldata <- modeldata[modeldata$m.date<80,]

ggplot(modeldata,aes(m.date, lfc, color=Origin))+
  geom_point()+xlab("date")+ geom_smooth(method=glm, se=FALSE)+
  ylab("lf count")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))

#pc1
# #false convergence 
# modeldata$PC1.1 <- modeldata$PC1/100
# model1<-lmer(lfc  ~ Origin + PC1.1 + m.date+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(lfc  ~ Origin * PC1 + m.date+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * PC1 + m.date+(1|blank), family=poisson,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(558.65,1)
# qchisq(558.65,1,lower=FALSE)#chisq value

modelint<-lmer(lfc  ~ Origin +PC1 + m.date+(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

# modelcov <- lmer(lfc  ~ Origin + m.date+(1|Pop), family=poisson,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(lfc ~ m.date+(1|Pop), family=poisson,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(lfc  ~ PC1 + m.date+(1|Pop), family=poisson,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov

modelmdate<-lmer(lfc ~ Origin*PC1+(1|Pop), family=poisson,data=modeldata)
mdateAov <- anova(modelmdate,model2) #test for significance of origin - origin only marginally sig....!
mdateAov

model2
summary(model2)

# #means and CI #needs work
# CI.LS.poisson.mdate(modelcov)
# # ls <- as.data.frame(lsmeans(modelcov, ~ Origin +m.date, conf=95))    
# ls2 <- as.data.frame(lsmeans(modelcov, ~ Origin, conf=95))  
# modelmtime <- lmer(lfc  ~ Origin + time +(1|Pop), family=poisson,data=modeldata)
# ls3 <- as.data.frame(lsmeans(modelmtime, ~ Origin +time, conf=95)) 

interaction.plot(response = modeldata$lfc, x.factor = modeldata$PC1, trace.factor = modeldata$Origin)
plot(modeldata$PC1, modeldata$Origin)
qplot(data=modeldata, PC1, lfc, color=Origin, geom = "jitter")

moddata <- ddply(modeldata, .(Pop, Origin, PC1), summarize, popCount=length(Pop), poplfc=mean(lfc, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC1, poplfc, color = Origin, 
      xlab="PC1", 
      ylab="Population mean lf count", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

#pc2
# model1<-lmer(lfc  ~ Origin + PC2 + m.date+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(lfc  ~ Origin * PC2 + m.date+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * PC2 + m.date+(1|blank), family=poisson,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(558.65,1)
# qchisq(558.65,1,lower=FALSE)#chisq value

modelint<-lmer(lfc  ~ Origin +PC2 + m.date+(1|Pop), family=poisson,data=modeldata)
#false convergence??
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(lfc  ~ Origin + m.date+(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(lfc ~ m.date+(1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(lfc  ~ PC2 + m.date+(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

modelmdate<-lmer(lfc ~ PC2+(1|Pop), family=poisson,data=modeldata)
mdateAov <- anova(modelmdate,modelOC) #test for significance of origin - origin only marginally sig....!
mdateAov

summary(modelOC)

# #means and CI #needs work #means across mean cov
# CI.LS.poisson.mdate(model2)
# # ls <- as.data.frame(lsmeans(modelcov, ~ Origin +m.date, conf=95))    
# ls2 <- as.data.frame(lsmeans(modelcov, ~ Origin, conf=95))  
# modelmtime <- lmer(lfc  ~ Origin + time +(1|Pop), family=poisson,data=modeldata)
# ls3 <- as.data.frame(lsmeans(modelmtime, ~ Origin +time, conf=95)) 

interaction.plot(response = modeldata$lfc, x.factor = modeldata$PC2, trace.factor = modeldata$Origin)
plot(modeldata$PC2, modeldata$Origin)
qplot(data=modeldata, PC2, lfc, color=Origin, geom = "jitter")

moddata <- ddply(modeldata, .(Pop, Origin, PC2), summarize, popCount=length(Pop), poplfc=mean(lfc, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC2, poplfc, color = Origin, 
      xlab="PC2", 
      ylab="Population mean lf count", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()
# 
#pc3
# model1<-lmer(lfc  ~ Origin + PC3 + m.date+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(lfc  ~ Origin * PC3 + m.date+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * PC3 + m.date+(1|blank), family=poisson,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(558.65,1)
# qchisq(558.65,1,lower=FALSE)#chisq value

modelint<-lmer(lfc  ~ Origin +PC3 + m.date+(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(lfc  ~ Origin + m.date+(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(lfc ~ m.date+(1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(lfc  ~ PC3 + m.date+(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

modelmdate<-lmer(lfc ~ (1|Pop), family=poisson,data=modeldata)
mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
mdateAov

summary(modelO)

# #means and CI #needs work #means across mean cov
# CI.LS.poisson.mdate(modelcov)
# # ls <- as.data.frame(lsmeans(modelcov, ~ Origin +m.date, conf=95))    
# ls2 <- as.data.frame(lsmeans(modelcov, ~ Origin, conf=95))  
# modelmtime <- lmer(lfc  ~ Origin + time +(1|Pop), family=poisson,data=modeldata)
# ls3 <- as.data.frame(lsmeans(modelmtime, ~ Origin +time, conf=95)) 

interaction.plot(response = modeldata$lfc, x.factor = modeldata$PC3, trace.factor = modeldata$Origin)
plot(modeldata$PC3, modeldata$Origin)
qplot(data=modeldata, PC3, lfc, color=Origin, geom = "jitter")

moddata <- ddply(modeldata, .(Pop, Origin, PC3), summarize, popCount=length(Pop), poplfc=mean(lfc, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC3, poplfc, color = Origin, 
      xlab="PC3", 
      ylab="Population mean lf count", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()
# 
#bio11
# model1<-lmer(lfc  ~ Origin + bio11 + m.date+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(lfc  ~ Origin * bio11 + m.date+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * bio11 + m.date+(1|blank), family=poisson,data=modeldata) # Test population effect
#false convergence?
# momAov <- anova(model2,model1) # mom is sig!
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(558.65,1)
# qchisq(558.65,1,lower=FALSE)#chisq value

modelint<-lmer(lfc  ~ Origin +bio11 + m.date+(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(lfc  ~ Origin + m.date+(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(lfc ~ m.date+(1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(lfc  ~ bio11 + m.date+(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

modelmdate<-lmer(lfc ~ (1|Pop), family=poisson,data=modeldata)
mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
mdateAov

summary(modelO)

# #means and CI #needs work #means across mean cov
# CI.LS.poisson.mdate(modelcov)
# # ls <- as.data.frame(lsmeans(modelcov, ~ Origin +m.date, conf=95))    
# ls2 <- as.data.frame(lsmeans(modelcov, ~ Origin, conf=95))  
# modelmtime <- lmer(lfc  ~ Origin + time +(1|Pop), family=poisson,data=modeldata)
# ls3 <- as.data.frame(lsmeans(modelmtime, ~ Origin +time, conf=95)) 


interaction.plot(response = modeldata$lfc, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
plot(modeldata$bio11, modeldata$Origin)
qplot(data=modeldata, bio11, lfc, color=Origin, geom = "jitter")

moddata <- ddply(modeldata, .(Pop, Origin, bio11), summarize, popCount=length(Pop), poplfc=mean(lfc, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,bio11, poplfc, color = Origin, 
      xlab="bio11", 
      ylab="Population mean lf count", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()
# 
#bio9
# model1<-lmer(lfc  ~ Origin + bio9 + m.date+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(lfc  ~ Origin * bio9 + m.date+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * bio9 + m.date+(1|blank), family=poisson,data=modeldata) # Test population effect
#false convergence?
# momAov <- anova(model2,model1) # mom is sig!
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(558.65,1)
# qchisq(558.65,1,lower=FALSE)#chisq value

modelint<-lmer(lfc  ~ Origin +bio9 + m.date+(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(lfc  ~ Origin + m.date+(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(lfc ~ m.date+(1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(lfc  ~ bio9 + m.date+(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

modelmdate<-lmer(lfc ~ Origin +bio9+ (1|Pop), family=poisson,data=modeldata)
mdateAov <- anova(modelmdate,modelint) #test for significance of origin - origin only marginally sig....!
mdateAov

summary(modelint)

# #means and CI #needs work #means across mean cov
# CI.LS.poisson.mdate(modelcov)
# # ls <- as.data.frame(lsmeans(modelcov, ~ Origin +m.date, conf=95))    
# ls2 <- as.data.frame(lsmeans(modelcov, ~ Origin, conf=95))  
# modelmtime <- lmer(lfc  ~ Origin + time +(1|Pop), family=poisson,data=modeldata)
# ls3 <- as.data.frame(lsmeans(modelmtime, ~ Origin +time, conf=95)) 

interaction.plot(response = modeldata$lfc, x.factor = modeldata$bio9, trace.factor = modeldata$Origin)
plot(modeldata$bio9, modeldata$Origin)
qplot(data=modeldata, bio9, lfc, color=Origin, geom = "jitter")

moddata <- ddply(modeldata, .(Pop, Origin, bio9), summarize, popCount=length(Pop), poplfc=mean(lfc, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,bio9, poplfc, color = Origin, 
      xlab="bio9", 
      ylab="Population mean lf count", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()
# 
#bio6
# model1<-lmer(lfc  ~ Origin + bio6 + m.date+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(lfc  ~ Origin * bio6 + m.date+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * bio6 + m.date+(1|blank), family=poisson,data=modeldata) # Test population effect
#false convergence?
# momAov <- anova(model2,model1) # mom is sig!
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(558.65,1)
# qchisq(558.65,1,lower=FALSE)#chisq value

modelint<-lmer(lfc  ~ Origin +bio6 + m.date+(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(lfc  ~ Origin + m.date+(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(lfc ~ m.date+(1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(lfc  ~ bio6 + m.date+(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

modelmdate<-lmer(lfc ~  (1|Pop), family=poisson,data=modeldata)
mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
mdateAov

summary(modelO)

# #means and CI #needs work #means across mean cov
# CI.LS.poisson.mdate(modelcov)
# # ls <- as.data.frame(lsmeans(modelcov, ~ Origin +m.date, conf=95))    
# ls2 <- as.data.frame(lsmeans(modelcov, ~ Origin, conf=95))  
# modelmtime <- lmer(lfc  ~ Origin + time +(1|Pop), family=poisson,data=modeldata)
# ls3 <- as.data.frame(lsmeans(modelmtime, ~ Origin +time, conf=95)) 

interaction.plot(response = modeldata$lfc, x.factor = modeldata$bio6, trace.factor = modeldata$Origin)
plot(modeldata$bio6, modeldata$Origin)
qplot(data=modeldata, bio6, lfc, color=Origin, geom = "jitter")

moddata <- ddply(modeldata, .(Pop, Origin, bio6), summarize, popCount=length(Pop), poplfc=mean(lfc, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,bio6, poplfc, color = Origin, 
      xlab="bio6", 
      ylab="Population mean lf count", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()
# 
#lat
# model1<-lmer(lfc  ~ Origin + Latitude + m.date+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(lfc  ~ Origin * Latitude + m.date+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * Latitude + m.date+(1|blank), family=poisson,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(558.65,1)
# qchisq(558.65,1,lower=FALSE)#chisq value

modelint<-lmer(lfc  ~ Origin +Latitude + m.date+(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(lfc  ~ Origin + m.date+(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(lfc ~ m.date+(1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(lfc  ~ Latitude + m.date+(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

modelmdate<-lmer(lfc ~  (1|Pop), family=poisson,data=modeldata)
mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
mdateAov

summary(modelO)

# #means and CI #needs work #means across mean cov
# CI.LS.poisson.mdate(modelcov)
# # ls <- as.data.frame(lsmeans(modelcov, ~ Origin +m.date, conf=95))    
# ls2 <- as.data.frame(lsmeans(modelcov, ~ Origin, conf=95))  
# modelmtime <- lmer(lfc  ~ Origin + time +(1|Pop), family=poisson,data=modeldata)
# ls3 <- as.data.frame(lsmeans(modelmtime, ~ Origin +time, conf=95)) 

interaction.plot(response = modeldata$lfc, x.factor = modeldata$Latitude, trace.factor = modeldata$Origin)
plot(modeldata$Latitude, modeldata$Origin)
qplot(data=modeldata, Latitude, lfc, color=Origin, geom = "jitter")

moddata <- ddply(modeldata, .(Pop, Origin, Latitude), summarize, popCount=length(Pop), poplfc=mean(lfc, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,Latitude, poplfc, color = Origin, 
      xlab="lat", 
      ylab="Population mean lf count", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

#trt
# model1<-lmer(lfc  ~ Origin * Trt + m.date+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(lfc  ~ Origin * Trt + m.date+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * Trt + m.date+(1|blank), family=poisson,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
# momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(558.65,1)
# qchisq(558.65,1,lower=FALSE)#chisq value

modelint<-lmer(lfc  ~ Origin +Trt + m.date+(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(lfc  ~ Origin + m.date+(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(lfc ~ m.date+(1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelOC <- lmer(lfc  ~ Trt + m.date+(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

modelmdate<-lmer(lfc ~ Trt+(1|Pop), family=poisson,data=modeldata)
mdateAov <- anova(modelmdate,modelOC) #test for significance of origin - origin only marginally sig....!
mdateAov

summary(model2)

# #means and CI #needs work #means across mean cov
# CI.LS.poisson.mdate(modelcov)
# # ls <- as.data.frame(lsmeans(modelcov, ~ Origin +m.date, conf=95))    
# ls2 <- as.data.frame(lsmeans(modelcov, ~ Origin, conf=95))  
# modelmtime <- lmer(lfc  ~ Origin + time +(1|Pop), family=poisson,data=modeldata)
# ls3 <- as.data.frame(lsmeans(modelmtime, ~ Origin +time, conf=95)) 

interaction.plot(response = modeldata$lfc, x.factor = modeldata$Trt, trace.factor = modeldata$Origin)
plot(modeldata$Trt, modeldata$Origin)
qplot(data=modeldata, Trt, lfc, color=Origin, geom = "jitter")

moddata <- ddply(modeldata, .(Pop, Origin, Trt), summarize, popCount=length(Pop), poplfc=mean(lfc, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,Trt, poplfc, color = Origin, 
      xlab="trt", 
      ylab="Population mean lf count", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

##################################
####lfw#######
CGtrait.LR_snglcov_int_time(trait="lfw", df=frdat.l, covariate="PC1", family=gaussian)
CGtrait.models_snglcov_int_time(trait="lfw", df=frdat.l, covariate="PC1", family=gaussian)

modeldata<-frdat.l[!is.na(frdat.l$lfw),]

modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#pc1
model1<-lmer(lfw  ~ Origin * PC1 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(lfw  ~ Origin * PC1 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfw  ~ Origin * PC1 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(21.199,1)
# 
# modelint<-lmer(lfw  ~ Origin +PC1 + m.date+(1|Pop), family=gaussian,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(lfw  ~ Origin + m.date+(1|Pop), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(lfw ~ m.date+(1|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(lfw  ~ PC1 + m.date+(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov
# 
# modelmdate<-lmer(lfw ~ (1|Pop), family=gaussian,data=modeldata)
# mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
# mdateAov
# 
# modelO
# summary(modelO)

# # #means and CI #needs work
# # CI.LS.gaussian.mdate(modelcov)
# # # ls <- as.data.frame(lsmeans(modelcov, ~ Origin +m.date, conf=95))    
# # ls2 <- as.data.frame(lsmeans(modelcov, ~ Origin, conf=95))  
# # modelmtime <- lmer(lfw  ~ Origin + time +(1|Pop), family=gaussian,data=modeldata)
# # ls3 <- as.data.frame(lsmeans(modelmtime, ~ Origin +time, conf=95)) 
# 
# interaction.plot(response = modeldata$lfw, x.factor = modeldata$PC1, trace.factor = modeldata$Origin)
# plot(modeldata$PC1, modeldata$Origin)
# qplot(data=modeldata, PC1, lfw, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, PC1), summarize, popCount=length(Pop), poplfw=mean(lfw, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,PC1, poplfw, color = Origin, 
#       xlab="PC1", 
#       ylab="Population mean lf width", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# 
#pc2
model1<-lmer(lfw  ~ Origin * PC2 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(lfw  ~ Origin * PC2 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfw  ~ Origin * PC2 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(11.108,1)
# # qchisq(558.65,1,lower=FALSE)#chisq value
# 
# modelint<-lmer(lfw  ~ Origin +PC2 + m.date+(1|Pop), family=gaussian,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(lfw  ~ Origin + m.date+(1|Pop), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(lfw ~ m.date+(1|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(lfw  ~ PC2 + m.date+(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov
# 
# modelmdate<-lmer(lfw ~ (1|Pop), family=gaussian,data=modeldata)
# mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
# mdateAov
# 
# summary(modelO)
# 
# # #means and CI #needs work #means across mean cov
# # CI.LS.gaussian.mdate(model2)
# # # ls <- as.data.frame(lsmeans(modelcov, ~ Origin +m.date, conf=95))    
# # ls2 <- as.data.frame(lsmeans(modelcov, ~ Origin, conf=95))  
# # modelmtime <- lmer(lfw  ~ Origin + time +(1|Pop), family=gaussian,data=modeldata)
# # ls3 <- as.data.frame(lsmeans(modelmtime, ~ Origin +time, conf=95)) 
# 
# # interaction.plot(response = modeldata$lfw, x.factor = modeldata$PC2, trace.factor = modeldata$Origin)
# plot(modeldata$PC2, modeldata$Origin)
# qplot(data=modeldata, PC2, lfw, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, PC2), summarize, popCount=length(Pop), poplfw=mean(lfw, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,PC2, poplfw, color = Origin, 
#       xlab="PC2", 
#       ylab="Population mean lf width", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# 
#pc3
model1<-lmer(lfw  ~ Origin * PC3 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(lfw  ~ Origin * PC3 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfw  ~ Origin * PC3 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(12.2,1)
# 
# modelint<-lmer(lfw  ~ Origin +PC3 + m.date+(1|Pop), family=gaussian,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(lfw  ~ Origin + m.date+(1|Pop), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(lfw ~ m.date+(1|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(lfw  ~ PC3 + m.date+(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov
# 
# modelmdate<-lmer(lfw ~ (1|Pop), family=gaussian,data=modeldata)
# mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
# mdateAov
# 
# summary(modelO)
# 
# # #means and CI #needs work #means across mean cov
# # CI.LS.gaussian.mdate(modelcov)
# # # ls <- as.data.frame(lsmeans(modelcov, ~ Origin +m.date, conf=95))    
# # ls2 <- as.data.frame(lsmeans(modelcov, ~ Origin, conf=95))  
# # modelmtime <- lmer(lfw  ~ Origin + time +(1|Pop), family=gaussian,data=modeldata)
# # ls3 <- as.data.frame(lsmeans(modelmtime, ~ Origin +time, conf=95)) 
# 
# # interaction.plot(response = modeldata$lfw, x.factor = modeldata$PC3, trace.factor = modeldata$Origin)
# plot(modeldata$PC3, modeldata$Origin)
# qplot(data=modeldata, PC3, lfw, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, PC3), summarize, popCount=length(Pop), poplfw=mean(lfw, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,PC3, poplfw, color = Origin, 
#       xlab="PC3", 
#       ylab="Population mean lf width", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# 
#bio11
model1<-lmer(lfw  ~ Origin * bio11 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(lfw  ~ Origin * bio11 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfw  ~ Origin * bio11 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(15.829,1)
# 
# modelint<-lmer(lfw  ~ Origin +bio11 + m.date+(1|Pop), family=gaussian,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(lfw  ~ Origin + m.date+(1|Pop), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(lfw ~ m.date+(1|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(lfw  ~ bio11 + m.date+(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov
# 
# modelmdate<-lmer(lfw ~ (1|Pop), family=gaussian,data=modeldata)
# mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
# mdateAov
# 
# summary(modelO)
# 
# # #means and CI #needs work #means across mean cov
# # CI.LS.gaussian.mdate(modelcov)
# # # ls <- as.data.frame(lsmeans(modelcov, ~ Origin +m.date, conf=95))    
# # ls2 <- as.data.frame(lsmeans(modelcov, ~ Origin, conf=95))  
# # modelmtime <- lmer(lfw  ~ Origin + time +(1|Pop), family=gaussian,data=modeldata)
# # ls3 <- as.data.frame(lsmeans(modelmtime, ~ Origin +time, conf=95)) 
# # 
# # interaction.plot(response = modeldata$lfw, x.factor = modeldata$bio11, trace.factor = modeldata$Origin)
# plot(modeldata$bio11, modeldata$Origin)
# qplot(data=modeldata, bio11, lfw, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, bio11), summarize, popCount=length(Pop), poplfw=mean(lfw, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,bio11, poplfw, color = Origin, 
#       xlab="bio11", 
#       ylab="Population mean lf width", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# 
#bio9
model1<-lmer(lfw  ~ Origin * bio9 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(lfw  ~ Origin * bio9 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfw  ~ Origin * bio9 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(19.984,1)
# # qchisq(558.65,1,lower=FALSE)#chisq value
# 
# modelint<-lmer(lfw  ~ Origin +bio9 + m.date+(1|Pop), family=gaussian,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(lfw  ~ Origin + m.date+(1|Pop), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(lfw ~ m.date+(1|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(lfw  ~ bio9 + m.date+(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov
# 
# modelmdate<-lmer(lfw ~  (1|Pop), family=gaussian,data=modeldata)
# mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
# mdateAov
# 
# summary(modelO)
# 
# # #means and CI #needs work #means across mean cov
# # CI.LS.gaussian.mdate(modelcov)
# # # ls <- as.data.frame(lsmeans(modelcov, ~ Origin +m.date, conf=95))    
# # ls2 <- as.data.frame(lsmeans(modelcov, ~ Origin, conf=95))  
# # modelmtime <- lmer(lfw  ~ Origin + time +(1|Pop), family=gaussian,data=modeldata)
# # ls3 <- as.data.frame(lsmeans(modelmtime, ~ Origin +time, conf=95)) 
# 
# interaction.plot(response = modeldata$lfw, x.factor = modeldata$bio9, trace.factor = modeldata$Origin)
# plot(modeldata$bio9, modeldata$Origin)
# qplot(data=modeldata, bio9, lfw, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, bio9), summarize, popCount=length(Pop), poplfw=mean(lfw, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,bio9, poplfw, color = Origin, 
#       xlab="bio9", 
#       ylab="Population mean lf width", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# 
#bio6
model1<-lmer(lfw  ~ Origin * bio6 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(lfw  ~ Origin * bio6 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfw  ~ Origin * bio6 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(18.712,1)
# 
# modelint<-lmer(lfw  ~ Origin +bio6 + m.date+(1|Pop), family=gaussian,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(lfw  ~ Origin + m.date+(1|Pop), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(lfw ~ m.date+(1|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(lfw  ~ bio6 + m.date+(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov
# 
# modelmdate<-lmer(lfw ~  (1|Pop), family=gaussian,data=modeldata)
# mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
# mdateAov
# 
# summary(modelO)
# 
# # #means and CI #needs work #means across mean cov
# # CI.LS.gaussian.mdate(modelcov)
# # # ls <- as.data.frame(lsmeans(modelcov, ~ Origin +m.date, conf=95))    
# # ls2 <- as.data.frame(lsmeans(modelcov, ~ Origin, conf=95))  
# # modelmtime <- lmer(lfw  ~ Origin + time +(1|Pop), family=gaussian,data=modeldata)
# # ls3 <- as.data.frame(lsmeans(modelmtime, ~ Origin +time, conf=95)) 
# 
# # interaction.plot(response = modeldata$lfw, x.factor = modeldata$bio6, trace.factor = modeldata$Origin)
# plot(modeldata$bio6, modeldata$Origin)
# qplot(data=modeldata, bio6, lfw, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, bio6), summarize, popCount=length(Pop), poplfw=mean(lfw, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,bio6, poplfw, color = Origin, 
#       xlab="bio6", 
#       ylab="Population mean lf width", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# 
#lat
model1<-lmer(lfw  ~ Origin * Latitude + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(lfw  ~ Origin * Latitude + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfw  ~ Origin * Latitude + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(17.082,1)
# 
# modelint<-lmer(lfw  ~ Origin +Latitude + m.date+(1|Pop), family=gaussian,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(lfw  ~ Origin + m.date+(1|Pop), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(lfw ~ m.date+(1|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(lfw  ~ Latitude + m.date+(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov
# 
# modelmdate<-lmer(lfw ~ (1|Pop), family=gaussian,data=modeldata)
# mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
# mdateAov
# 
# summary(modelOC)
# 
# # #means and CI #needs work #means across mean cov
# # CI.LS.gaussian.mdate(modelcov)
# # # ls <- as.data.frame(lsmeans(modelcov, ~ Origin +m.date, conf=95))    
# # ls2 <- as.data.frame(lsmeans(modelcov, ~ Origin, conf=95))  
# # modelmtime <- lmer(lfw  ~ Origin + time +(1|Pop), family=gaussian,data=modeldata)
# # ls3 <- as.data.frame(lsmeans(modelmtime, ~ Origin +time, conf=95)) 
# 
# interaction.plot(response = modeldata$lfw, x.factor = modeldata$Latitude, trace.factor = modeldata$Origin)
# plot(modeldata$Latitude, modeldata$Origin)
# qplot(data=modeldata, Latitude, lfw, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, Latitude), summarize, popCount=length(Pop), poplfw=mean(lfw, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,Latitude, poplfw, color = Origin, 
#       xlab="lat", 
#       ylab="Population mean lf count", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
# 
#trt
model1<-lmer(lfw  ~ Origin * Trt + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(lfw  ~ Origin * Trt + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfw  ~ Origin * Trt + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(24.549,1)
# # qchisq(558.65,1,lower=FALSE)#chisq value
# 
# modelint<-lmer(lfw  ~ Origin +Trt + m.date+(1|Pop), family=gaussian,data=modeldata)
# intAov <- anova(model2, modelint)
# intAov
# 
# modelcov <- lmer(lfw  ~ Origin + m.date+(1|Pop), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(lfw ~ m.date+(1|Pop), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
# modelOC <- lmer(lfw  ~ Trt + m.date+(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov
# 
# modelmdate<-lmer(lfw ~  (1|Pop), family=gaussian,data=modeldata)
# mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
# mdateAov
# 
# summary(modelO)
# 
# # #means and CI #needs work #means across mean cov
# # CI.LS.gaussian.mdate(modelcov)
# # # ls <- as.data.frame(lsmeans(modelcov, ~ Origin +m.date, conf=95))    
# # ls2 <- as.data.frame(lsmeans(modelcov, ~ Origin, conf=95))  
# # modelmtime <- lmer(lfw  ~ Origin + time +(1|Pop), family=gaussian,data=modeldata)
# # ls3 <- as.data.frame(lsmeans(modelmtime, ~ Origin +time, conf=95)) 
# 
# interaction.plot(response = modeldata$lfw, x.factor = modeldata$Trt, trace.factor = modeldata$Origin)
# plot(modeldata$Trt, modeldata$Origin)
# qplot(data=modeldata, Trt, lfw, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, Trt), summarize, popCount=length(Pop), poplfw=mean(lfw, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,Trt, poplfw, color = Origin, 
#       xlab="trt", 
#       ylab="Population mean lf width", main="") #+geom_smooth(method=glm, se=TRUE)
# # dev.off()
 
####rd#######
modeldata<-frdat.l[!is.na(frdat.l$rd),]

modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#pc1
model1<-lmer(rd  ~ Origin * PC1 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(rd  ~ Origin * PC1 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(rd  ~ Origin * PC1 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(21.294,1)

#pc2
model1<-lmer(rd  ~ Origin * PC2 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(rd  ~ Origin * PC2 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(rd  ~ Origin * PC2 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(21.294,1)

#pc3
model1<-lmer(rd  ~ Origin * PC3 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(rd  ~ Origin * PC3 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(rd  ~ Origin * PC3 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(16.787,1)

#bio11
model1<-lmer(rd  ~ Origin * bio11 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(rd  ~ Origin * bio11 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(rd  ~ Origin * bio11 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(16.787,1)

#bio6
model1<-lmer(rd  ~ Origin * bio6 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(rd  ~ Origin * bio6 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(rd  ~ Origin * bio6 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(16.787,1)

#bio9
model1<-lmer(rd  ~ Origin * bio9 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(rd  ~ Origin * bio9 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(rd  ~ Origin * bio9 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(16.787,1)

#lat
model1<-lmer(rd  ~ Origin * Latitude + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(rd  ~ Origin * Latitude + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(rd  ~ Origin * Latitude + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(16.787,1)

#trt
model1<-lmer(rd  ~ Origin * Trt + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(rd  ~ Origin * Trt + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(rd  ~ Origin * Trt + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(16.787,1)

####lfl#######
modeldata<-frdat.l[!is.na(frdat.l$lfl),]

modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#pc1
model1<-lmer(lfl  ~ Origin * PC1 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(lfl  ~ Origin * PC1 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfl  ~ Origin * PC1 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(21.294,1)

#pc2
model1<-lmer(lfl  ~ Origin * PC2 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(lfl  ~ Origin * PC2 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfl  ~ Origin * PC2 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(21.294,1)

#pc3
model1<-lmer(lfl  ~ Origin * PC3 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(lfl  ~ Origin * PC3 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfl  ~ Origin * PC3 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(16.787,1)

#bio11
model1<-lmer(lfl  ~ Origin * bio11 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(lfl  ~ Origin * bio11 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfl  ~ Origin * bio11 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(16.787,1)

#bio6
model1<-lmer(lfl  ~ Origin * bio6 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(lfl  ~ Origin * bio6 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfl  ~ Origin * bio6 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(16.787,1)

#bio9
model1<-lmer(lfl  ~ Origin * bio9 + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(lfl  ~ Origin * bio9 + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfl  ~ Origin * bio9 + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(16.787,1)

#lat
model1<-lmer(lfl  ~ Origin * Latitude + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(lfl  ~ Origin * Latitude + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfl  ~ Origin * Latitude + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(16.787,1)

#trt
model1<-lmer(lfl  ~ Origin * Trt + m.date+(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(lfl  ~ Origin * Trt + m.date+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfl  ~ Origin * Trt + m.date+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(16.787,1)

##########
####models w/ Trt included
###lfc##############
# CGtrait.LR_snglcov_trt_mdate(trait="lfc",df=Frdatsk.l,covariate="Latitude",family=gaussian)
# frPLR.lfc_SKtrt<- lapply(names(Frdatsk.l)[c(15,17:19,21:23)],function(n) CGtrait.LR_snglcov_trt_mdate("lfc",Frdatsk.l, covariate=n, family=poisson))
modeldata <- droplevels(subset(Frdatsk.l, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$lfc),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

#pc1
# #false convergence
# modeldata$PC1.1 <- modeldata$PC1/100
# model1<-lmer(lfc  ~ Origin + PC1.1 +Trt+ m.date+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(lfc  ~ Origin * PC1 + Trt+m.date+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * PC1 + Trt+m.date+(1|blank), family=poisson,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(558.65,1)

modelint<-lmer(lfc  ~ Origin +PC1 +Trt+ m.date+(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

# modelcov <- lmer(lfc  ~ Origin + Trt+m.date+(1|Pop), family=poisson,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(lfc ~ Trt+m.date+(1|Pop), family=poisson,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov
# 
modelT <- lmer(lfc ~ Origin*PC1+m.date+(1|Pop), family=poisson,data=modeldata)
trtAov <- anova(model2, modelT)
trtAov

modelmdate<-lmer(lfc ~ Origin*PC1+Trt+(1|Pop), family=poisson,data=modeldata)
mdateAov <- anova(model2, modelmdate) #test for significance of origin - origin only marginally sig....!
mdateAov

#lsmeans, ctrl only
modeldata <- droplevels(subset(Frdatsk.l, Origin%in%c("inv", "nat")&Trt%in%"control"))
modeldata<-modeldata[!is.na(modeldata$lfc),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modelint<-lmer(lfc  ~ Origin +PC1 + m.date+(1|Pop), family=poisson,data=modeldata)
CI.LS.poisson(modelint)
summary(modeldata$Origin)
summary(modeldata$Pop)

#lsmeans, dr only
modeldata <- droplevels(subset(Frdatsk.l, Origin%in%c("inv", "nat")&Trt%in%"drought"))
modeldata<-modeldata[!is.na(modeldata$lfc),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modelint<-lmer(lfc  ~ Origin +PC1 + m.date+(1|Pop), family=poisson,data=modeldata)
CI.LS.poisson(modelint)
summary(modeldata$Origin)
summary(modeldata$Pop)

# # #try glm
# # modelg <- glm(lfc ~ Origin*Latitude+Trt+m.date, family=poisson,data=modeldata)
# # modelg1 <- glm(lfc ~ Origin+Latitude+Trt+m.date, family=poisson,data=modeldata)
# # anova(modelg1, modelg, test="LRT") 
# # # qchisq(0.0964,1,lower=FALSE)#chisq value
# # 
# # modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
# # anova(modelg3,modelg1, test="LRT")
# # qchisq(0.9672,1,lower=FALSE)#chisq value
# # anova(modelg3, test="LRT")
# # # modelg2<- glm(sla.log ~ Latitude, family=gaussian,data=modeldata)
# # # anova(modelg2,modelg1)
# # qchisq(0.5399,1,lower=FALSE)#chisq value
# 
#pc2
# model1<-lmer(lfc  ~ Origin + PC2 +Trt+ m.date+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(lfc  ~ Origin * PC2 +Trt+ m.date+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * PC2 +Trt+ m.date+(1|blank), family=poisson,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(558.65,1)

modelint<-lmer(lfc  ~ Origin +PC2 +Trt+ m.date+(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(lfc  ~ Origin + Trt+m.date+(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelOc<-lmer(lfc ~ PC2+Trt+m.date+(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelOc,modelint) #test for significance of origin - origin only marginally sig....!
ocAov

modelT <- lmer(lfc ~ PC2+m.date+(1|Pop), family=poisson,data=modeldata)
trtAov <- anova(modelOc, modelT)
trtAov

modelmdate<-lmer(lfc ~ PC2+Trt+(1|Pop), family=poisson,data=modeldata)
mdateAov <- anova(modelmdate,modelOc) #test for significance of origin - origin only marginally sig....!
mdateAov

#pc3
# model1<-lmer(lfc  ~ Origin + PC3 + m.date+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(lfc  ~ Origin * PC3 +Trt+ m.date+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * PC3 +Trt+ m.date+(1|blank), family=poisson,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(558.65,1)

modelint<-lmer(lfc  ~ Origin +PC3 +Trt+ m.date+(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(lfc  ~ Origin + Trt+m.date+(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(lfc ~ Trt+m.date+(1|Pop), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelT <- lmer(lfc ~ m.date+(1|Pop), family=poisson,data=modeldata)
trtAov <- anova(modelO, modelT)
trtAov

modelmdate<-lmer(lfc ~ Trt+(1|Pop), family=poisson,data=modeldata)
mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
mdateAov

#bio11
# #false convergence
modeldata$bio11.1 <- modeldata$bio11/100
# model1<-lmer(lfc  ~ Origin + bio11 + Trt+m.date+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(lfc  ~ Origin * bio11.1 + Trt+m.date+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * bio11.1 + Trt+m.date+(1|blank), family=poisson,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(558.65,1)

modelint<-lmer(lfc  ~ Origin +bio11.1 +Trt+ m.date+(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(lfc  ~ Origin + Trt+m.date+(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov
# # 
# # modelO<-lmer(lfc ~ m.date+(1|Pop), family=poisson,data=modeldata)
# # originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# # originAov
# # 
# # modelOC <- lmer(lfc  ~ bio11 + m.date+(1|Pop), family=poisson,data=modeldata)
# # ocAov <- anova(modelint, modelOC)
# # ocAov
# # 
# # modelmdate<-lmer(lfc ~ (1|Pop), family=poisson,data=modeldata)
# # mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
# # mdateAov
# # 
#bio9
# #false convergence
modeldata$bio9.1 <- modeldata$bio9/100
# model1<-lmer(lfc  ~ Origin + bio9 + m.date+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(lfc  ~ Origin * bio9.1 + Trt+m.date+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * bio9.1 + Trt+m.date+(1|blank), family=poisson,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(558.65,1)

modelint<-lmer(lfc  ~ Origin +bio9.1 + Trt+m.date+(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(lfc  ~ Origin + Trt+m.date+(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelOC <- lmer(lfc  ~ bio9.1 + Trt+ m.date+(1|Pop), family=poisson,data=modeldata)
ocAov <- anova(modelint, modelOC)
ocAov

modelT <- lmer(lfc ~ bio9.1+m.date+(1|Pop), family=poisson,data=modeldata)
trtAov <- anova(modelOC, modelT)
trtAov

modelmdate<-lmer(lfc ~ bio9.1+ Trt+ (1|Pop), family=poisson,data=modeldata)
mdateAov <- anova(modelmdate,modelOC) #test for significance of origin - origin only marginally sig....!
mdateAov
# # 
#bio6
# #false convergence
modeldata$bio6.1 <- modeldata$bio6/100
# model1<-lmer(lfc  ~ Origin + bio6 + m.date+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(lfc  ~ Origin * bio6.1 + Trt+m.date+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * bio6.1 + Trt+m.date+(1|blank), family=poisson,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(558.65,1)

modelint<-lmer(lfc  ~ Origin +bio6.1 + Trt+m.date+(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(lfc  ~ Origin + Trt+m.date+(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov
# # 
# # modelO<-lmer(lfc ~ m.date+(1|Pop), family=poisson,data=modeldata)
# # originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# # originAov
# # 
# # modelOC <- lmer(lfc  ~ bio6 + m.date+(1|Pop), family=poisson,data=modeldata)
# # ocAov <- anova(modelint, modelOC)
# # ocAov
# # 
# # modelmdate<-lmer(lfc ~  (1|Pop), family=poisson,data=modeldata)
# # mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
# # mdateAov
# 
#lat
# #false convergence
# modeldata$Lat.1 <- modeldata$Latitude/100
# model1<-lmer(lfc  ~ Origin + Latitude + m.date+(1|Pop/Mom), family=poisson,data=modeldata)
model2<-lmer(lfc  ~ Origin * Latitude + Trt+m.date+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * Latitude + Trt+m.date+(1|blank), family=poisson,data=modeldata) # Test population effect
# momAov <- anova(model2,model1) # mom is sig!
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
# 1-pchisq(558.65,1)

modelint<-lmer(lfc  ~ Origin +Latitude+Trt + m.date+(1|Pop), family=poisson,data=modeldata)
intAov <- anova(model2, modelint)
intAov

modelcov <- lmer(lfc  ~ Origin + Trt+m.date+(1|Pop), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov
# # 
# # modelO<-lmer(lfc ~ m.date+(1|Pop), family=poisson,data=modeldata)
# # originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# # originAov
# # 
# # modelOC <- lmer(lfc  ~ Latitude + m.date+(1|Pop), family=poisson,data=modeldata)
# # ocAov <- anova(modelint, modelOC)
# # ocAov
# # 
# # modelmdate<-lmer(lfc ~  Latitude+(1|Pop), family=poisson,data=modeldata)
# # mdateAov <- anova(modelmdate,modelOC) #test for significance of origin - origin only marginally sig....!
# # mdateAov
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

