#Fr repeated measures lmer models, DK only for cline ms 12/3/2014

#for modeling function, se CGtrait_lmer_func_Fr.R
#for confidence interval function, see lmerMeansCIfunc_Fr.R

#Control and drought, REML, using lme4
#mixed effect models 
library(lme4.0)
library(lsmeans)
library(ggplot2)
library(plyr)
#read
Frdatcline.l<- read.table("FrTraitClimDat_cline_long.txt", header=T, sep="\t",quote='"', row.names=1)

# #argh! use pc1 from pca that includes only experimental pops, maybe, hmmm?
# #load climate table
# Frclimdat.dk <- read.table("FrbioclimPCA_DKdat.txt", header=TRUE)
# Frdatcline.l<- subset(Frdatcline.l, select=c(1:23))
# 
# Frdatcline.l<- merge(Frdatcline.l,Frclimdat.dk[,c(1,22:27)], all.x=TRUE ) #add pc1, 2,3 by=c("Pop","Origin","Latitude","Longitude",
# #write table
# write.table(Frdatcline.l, file="FrTraitClimDat_cline_long.txt")

####models with (m.date|tagged)####
###lfc##############
modeldata <- Frdatcline.l
modeldata<-modeldata[!is.na(modeldata$lfc),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata$time <- as.factor(modeldata$time)
modeldata <- subset(modeldata, lfc<200)


summary(modeldata$Origin)
summary(modeldata$Pop)

# modeldata$PopMom <- as.factor(paste0(modeldata$Pop,"_",modeldata$Mom))

#pc1
# # partial colinearity between mom and trt?
# xtabs(~Origin +Trt, data=modeldata)
# xtabs(~Origin +m.date, data=modeldata)
# xtabs(~Origin +m.date+time, data=modeldata)
# count(modeldata, vars=c("Pop","Mom","Trt","m.date"))
# summary(glm(lfc  ~ Origin * PC1 + Trt+m.date, family=poisson, data=modeldata))
# # #or not???
# # library(caret)
# # modmatrix <- subset(modeldata, select=c("Pop","Origin","Mom","Trt","tagged","m.date","lfc","PC1"))
# # modmatrix$Pop <- as.numeric(modmatrix$Pop)
# # modmatrix$Origin <- as.numeric(modmatrix$Origin)
# # modmatrix$Mom <- as.numeric(modmatrix$Mom)
# # modmatrix$Trt <- as.numeric(modmatrix$Trt)
# # modmatrix$tagged <- as.numeric(modmatrix$tagged)
# # modmatrix <- as.matrix(modmatrix)
# # findLinearCombos(modmatrix) #none
# # #test findLinearCombos
# # modmatrix$Ortest <- modmatrix$Origin
# # modmatrix <- as.matrix(modmatrix)
# # findLinearCombos(modmatrix)

#LRTs get crazy Chisq values, try scale/center lfc
modeldata$lfc.scale <-as.vector(scale(modeldata$lfc, center=FALSE, scale=TRUE))
modeldata$lfc.sc <- as.vector(scale(modeldata$lfc, center=TRUE, scale=TRUE))

modelOr <- lmer(lfc.scale  ~ Origin * PC1 + Trt+(m.date|tagged)+(Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(lfc.scale  ~ Origin * PC1 + Trt+(m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1,modelOr)
model2<-lmer(lfc.scale  ~ Origin * PC1 + Trt+(m.date|tagged)+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc.scale  ~ Origin * PC1 + Trt+(m.date|tagged)+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
momAov
popAov
1-pchisq(2.2837,1)
modelmdate <- lmer(lfc.scale  ~ Origin * PC1 + Trt+(1|blank), family=gaussian,data=modeldata)
anova(modelmdate, model3)

# model4 <- lmer(lfc  ~ Origin * PC1 +(time|tagged), family=poisson,data=modeldata, verbose=TRUE)
# model5 <- lmer(lfc  ~ Origin * PC1 +(blank|tagged), family=poisson,data=modeldata, verbose=TRUE)
# model6 <- lmer(lfc  ~ Origin * PC1 +(1|tagged), family=poisson,data=modeldata, verbose=TRUE)
# model7 <- lmer(lfc  ~ Origin * PC1 +(1|blank), family=poisson,data=modeldata, verbose=TRUE)
# anova(model4,model6)
# anova(model4, model5, model6)
# anova(model5, model6)
# anova(model7, model6)
# #try count now, blech, chisq values still crazy
# modelmdate2 <- lmer(lfc ~ Origin*PC1+Trt+(m.date|tagged), family=poisson,data=modeldata)
# modelT <- lmer(lfc ~ Origin*PC1+(m.date|tagged), family=poisson,data=modeldata)
# trtAov <- anova(modelT, modelmdate2)
# trtAov
#scaled again
modelmdate2 <- lmer(lfc.scale ~ Origin*PC1+Trt+(m.date|tagged), family=gaussian,data=modeldata)
modelT <- lmer(lfc.scale ~ Origin*PC1+(m.date|tagged), family=gaussian,data=modeldata)
trtAov <- anova(modelT, modelmdate2)
trtAov

modelint<-lmer(lfc.scale  ~ Origin +PC1 + (m.date|tagged), family=gaussian,data=modeldata)
intAov <- anova(modelint, modelT)
intAov

modelcov <- lmer(lfc.scale  ~ Origin +(m.date|tagged), family=gaussian,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(lfc.scale ~ (m.date|tagged), family=gaussian,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin 
originAov

#to get a p value
modelcov
# library(lsmeans)
# lsmeans(modelcov, "Origin")
# library(nlme)
# lmecov <- lme(lfc  ~ Origin, data=modeldata, random=~ m.date | tagged)
# anova(lmecov)
# library(RLRsim)
# library("lme4")
detach(package:lme4.0)
library("lmerTest")
library("lme4")
modelcov <- lmer(lfc.scale  ~ Origin +(m.date|tagged), data=modeldata)
summary(modelcov)


#lsmeans, ctrl only
# modeldata <- droplevels(subset(Frdatsk.l, Origin%in%c("inv", "nat")&Trt%in%"control"))
# modeldata<-modeldata[!is.na(modeldata$lfc),]
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
# modelint<-lmer(lfc  ~ Origin +PC1 + m.date+(1|Pop), family=poisson,data=modeldata)
CI.LS.poisson(modelcov)
summary(modeldata$Origin)
summary(modeldata$Pop)


#try glm
modelg <- glm(lfc ~ Origin*PC1+Trt+m.date, family=poisson,data=modeldata)
modelg1 <- glm(lfc ~ Origin*PC1+m.date, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#chisq value
modelg4 <- glm(lfc ~ Origin+PC1+Trt+m.date, family=poisson,data=modeldata)
anova(modelg4, modelg, test="LRT") 

modelgm <- glm(lfc ~ Origin*PC1+Trt, family=poisson,data=modeldata)
anova(modelgm, modelg, test="LRT") 

modelg3<- glm(lfc ~ Origin + m.date, family=gaussian,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# qchisq(0.9672,1,lower=FALSE)#chisq value
# anova(modelg3, test="LRT")
# modelg2<- glm(lfc ~ PC1, family=poisson,data=modeldata)
# anova(modelg2,modelg1)
# qchisq(0.5399,1,lower=FALSE)#chisq value
summary(modelg3)

qplot(data=modeldata, time, lfc, color=Origin, geom = "jitter")

moddata <- ddply(modeldata, .(Pop, Origin, time), summarize, popCount=length(Pop), poplfc=mean(lfc, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,time, poplfc, color = Origin, 
      xlab="trt", 
      ylab="Population mean lf count", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

####lfc~origin*trt####
#not run
modelOr <- lmer(lfc.scale  ~ Origin * Trt+PC1+(m.date|tagged)+(Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(lfc.scale  ~ Origin * Trt+PC1+ (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(lfc.scale  ~ Origin * Trt +PC1+ (m.date|tagged)+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc.scale  ~ Origin * Trt +PC1+ (m.date|tagged)+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.8929,1)
# qchisq(558.65,1,lower=FALSE)#chisq value
modelmdate <- lmer(lfc.scale  ~ Origin * Trt +PC1+ (1|blank), family=gaussian,data=modeldata)
anova(modelmdate, model3)

modelmdate2 <- lmer(lfc.scale  ~ Origin * Trt +PC1+ (m.date|tagged), family=gaussian,data=modeldata)
modelP <- lmer(lfc.scale  ~ Origin * Trt+ (m.date|tagged), family=gaussian,data=modeldata)
anova(modelP, modelmdate2)

modelint<-lmer(lfc.scale  ~ Origin +Trt + (m.date|tagged), family=gaussian,data=modeldata)
intAov <- anova(modelP, modelint)
intAov

modelcov <- lmer(lfc.scale  ~ Origin + (m.date|tagged), family=gaussian,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(lfc.scale ~ (m.date|tagged), family=gaussian,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

# modelOC <- lmer(lfc.scale  ~ Trt + (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov

summary(modelcov)

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

####lfw#######
modeldata<-Frdatcline.l
modeldata<-modeldata[!is.na(modeldata$lfw),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
# modeldata$time <- as.factor(modeldata$time)
# modeldata <- subset(modeldata, lfc<200)

summary(modeldata$Origin)
summary(modeldata$Pop)

#pc1
modelOr <- lmer(lfw  ~ Origin * PC1 + Trt+(m.date|tagged)+(Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(lfw  ~ Origin * PC1 + Trt+(m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(lfw  ~ Origin * PC1 + Trt+(m.date|tagged)+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfw  ~ Origin * PC1 + Trt+(m.date|tagged)+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.0242,1)

modelmdate <- lmer(lfw  ~ Origin * PC1 + Trt+(1|blank), family=gaussian,data=modeldata)
anova(model3, modelmdate)

model4 <- lmer(lfw  ~ Origin * PC1 + Trt+(m.date|tagged), family=gaussian,data=modeldata)
# anova(model3, model4)
# anova(model2, model4)
modelT <- lmer(lfw  ~ Origin * PC1 + (m.date|tagged), family=gaussian,data=modeldata)
anova(modelT, model4)

modelint<-lmer(lfw  ~ Origin +PC1 + (m.date|tagged), family=gaussian,data=modeldata)
intAov <- anova(modelT, modelint)
intAov

# modelcov <- lmer(lfw  ~ Origin + (m.date|tagged), family=gaussian,data=modeldata)
# covAov <- anova(modelint, modelcov)
# covAov
# 
# modelO<-lmer(lfw ~ (m.date|tagged), family=gaussian,data=modeldata)
# originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
# originAov

modelT
#to get pvalue
detach(package:lme4.0)
library("lmerTest")
library("lme4")
modelT <- lmer(lfw  ~ Origin * PC1 + (m.date|tagged), data=modeldata)
summary(modelT)

modelg <- glm(lfw~Origin*PC1 +m.date, family=gaussian, data=modeldata)
summary(modelg)

# # #means and CI #needs work
# # CI.LS.gaussian.mdate(modelcov)
# # # ls <- as.data.frame(lsmeans(modelcov, ~ Origin +m.date, conf=95))    
# # ls2 <- as.data.frame(lsmeans(modelcov, ~ Origin, conf=95))  
# # modelmtime <- lmer(lfw  ~ Origin + time +(1|Pop), family=gaussian,data=modeldata)
# # ls3 <- as.data.frame(lsmeans(modelmtime, ~ Origin +time, conf=95)) 
# 
# interaction.plot(response = modeldata$lfw, x.factor = modeldata$PC1, trace.factor = modeldata$Origin)
# plot(modeldata$PC1, modeldata$Origin)
qplot(data=modeldata, PC1, lfw, color=Origin, geom = "jitter")

moddata <- ddply(modeldata, .(Pop, Origin, PC1), summarize, popCount=length(Pop), poplfw=mean(lfw, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC1, poplfw, color = Origin, 
      xlab="PC1", 
      ylab="Population mean lf width", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()
####lfw~origin*trt####
#not run
modelOr <- lmer(lfw  ~ Origin * Trt+PC1+(m.date|tagged)+(Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(lfw  ~ Origin * Trt+PC1 + (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(lfw  ~ Origin * Trt +PC1+ (m.date|tagged)+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfw  ~ Origin * Trt +PC1+ (m.date|tagged)+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(5.1033,1)
# # qchisq(558.65,1,lower=FALSE)#chisq value
modelmdate <- lmer(lfw  ~ Origin * Trt +PC1+ (1|Pop), family=gaussian,data=modeldata)
anova(model2, modelmdate)
# model4 <- lmer(lfw  ~ Origin * Trt + (m.date|tagged), family=gaussian,data=modeldata)
# anova(model2, model4)

modelP <- lmer(lfw  ~ Origin * Trt + (m.date|tagged)+(1|Pop), family=gaussian,data=modeldata)
anova(modelP, model2)

modelint<-lmer(lfw  ~ Origin +Trt + (m.date|tagged)+(1|Pop), family=gaussian,data=modeldata)
intAov <- anova(modelP, modelint)
intAov

modelcov <- lmer(lfw  ~ Origin + (m.date|tagged)+(1|Pop), family=gaussian,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

# modelpop <- lmer(lfw  ~ Origin + (m.date|tagged), family=gaussian,data=modeldata)
# anova(modelcov, modelpop)

modelO<-lmer(lfw ~ (m.date|tagged)+(1|Pop), family=gaussian,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

# modelOC <- lmer(lfw  ~ Trt + (m.date|tagged)+(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov
# 
summary(modelcov)
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
modeldata<-Frdatcline.l
modeldata<-modeldata[!is.na(modeldata$rd),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
# modeldata$time <- as.factor(modeldata$time)
# modeldata <- subset(modeldata, lfc<200)
summary(modeldata$Origin)
summary(modeldata$Pop)

#pc1
modelOr <- lmer(rd  ~ Origin * PC1 + Trt+(m.date|tagged)+(Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(rd  ~ Origin * PC1 + Trt+ (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(rd  ~ Origin * PC1 + Trt+(m.date|tagged)+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(rd  ~ Origin * PC1 + Trt+(m.date|tagged)+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.5766,1)
modelmdate <- lmer(rd  ~ Origin * PC1 + Trt+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelmdate, model1)

modelT <- lmer(rd  ~ Origin * PC1 + (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelT, model1)

modelint <- lmer(rd  ~ Origin + PC1 + (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelint, modelT)

modelcov <- lmer(rd  ~ Origin + (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelcov, modelint)

modelO <- lmer(rd  ~ (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelO, modelcov)

modelcov

modelg <- glm(rd~Origin, family=gaussian, data=modeldata)
summary(modelg)
####rd~origin*trt####
#not run
modelOr <- lmer(rd  ~ Origin * Trt+PC1+(m.date|tagged)+(Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(rd  ~ Origin * Trt+PC1+ (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(rd  ~ Origin * Trt +PC1+ (m.date|tagged)+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(rd  ~ Origin * Trt +PC1+ (m.date|tagged)+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(4.94,1)
modelmdate <- lmer(rd  ~ Origin * Trt +PC1+ (1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelmdate)
# model4 <- lmer(rd  ~ Origin * Trt + (m.date|tagged), family=gaussian,data=modeldata)
# anova(model2, model4)

modelP <- lmer(rd  ~ Origin * Trt+ (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelP, model1)

modelint<-lmer(rd  ~ Origin +Trt + (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
intAov <- anova(modelP, modelint)
intAov

modelcov <- lmer(rd  ~ Origin + (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

# modelpop <- lmer(rd  ~ Origin + (m.date|tagged), family=gaussian,data=modeldata)
# anova(modelcov, modelpop)

modelO<-lmer(rd ~ (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelcov

####lfl#######
modeldata<-Frdatcline.l
modeldata<-modeldata[!is.na(modeldata$rd),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
# modeldata$time <- as.factor(modeldata$time)
# modeldata <- subset(modeldata, lfc<200)
summary(modeldata$Origin)
summary(modeldata$Pop)

#pc1
modelOr <- lmer(lfl  ~ Origin * PC1 + Trt+(m.date|tagged)+(Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(lfl  ~ Origin * PC1 + Trt+ (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(lfl  ~ Origin * PC1 + Trt+(m.date|tagged)+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfl  ~ Origin * PC1 +Trt+ (m.date|tagged)+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(7.0337,1)
modelmdate <- lmer(lfl  ~ Origin * PC1 + Trt+ (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelmdate, model1)

modelT <- lmer(lfl  ~ Origin * PC1 +(m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelT, model1)

modelint <- lmer(lfl  ~ Origin + PC1 +(m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelint, modelT)

modelcov <- lmer(lfl  ~ Origin +(m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelcov,modelint)

modelO <- lmer(lfl  ~ (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelO, modelcov)

modelcov

modelg <- glm(lfl~Origin + m.date, family=gaussian, data=modeldata)
summary(modelg)
####lfl~origin*trt####
modelOr <- lmer(lfl  ~ Origin *Trt+PC1+(m.date|tagged)+(Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(lfl  ~ Origin *Trt+PC1+ (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(lfl  ~ Origin * Trt+PC1 + (m.date|tagged)+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfl  ~ Origin * Trt+PC1 + (m.date|tagged)+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(8.0027,1)
modelmdate <- lmer(lfl  ~ Origin * Trt+PC1+ (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelmdate, model1)

modelP <- lmer(lfl  ~ Origin *Trt+ (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelP, model1)

modelint <- lmer(lfl  ~ Origin + Trt +(m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelint, modelP)

modelcov <- lmer(lfl  ~ Origin +(m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelcov,modelint)

modelO <- lmer(lfl  ~ (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelO, modelcov)

modelcov

