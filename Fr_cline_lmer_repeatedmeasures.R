#Fr repeated measures lmer models, DK only for cline ms 12/3/2014

#for modeling function, se CGtrait_lmer_func_Fr.R
#for confidence interval function, see lmerMeansCIfunc_Fr.R

#Control and drought, REML, using lme4
#mixed effect models 
library(lme4.0)
# library(lsmeans)
library(ggplot2)
library(plyr)
#read
Frdatcline.l<- read.table("FrTraitClimDat_cline_long.txt", header=T, sep="\t",quote='"', row.names=1)

####models with (m.date|tagged)####
###lfc##############
modeldata <- droplevels(subset(Frdatsk.l, Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$lfc),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

summary(modeldata$Origin)
summary(modeldata$Pop)

# modeldata$PopMom <- as.factor(paste0(modeldata$Pop,"_",modeldata$Mom))

#pc1
# # partial colinearity between mom and trt?
# xtabs(lfc~Trt+ Pop + Mom, data=modeldata)
# count(modeldata, vars=c("Pop","Mom","Trt"))
# summary(glm(lfc  ~ Origin * PC1 + Trt+m.date, family=poisson, data=modeldata))

modelOr <- lmer(lfc  ~ Origin * PC1 + Trt+(m.date|tagged)+(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(lfc  ~ Origin * PC1 + Trt+(m.date|tagged)+(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1,modelOr)
model2<-lmer(lfc  ~ Origin * PC1 + Trt+(m.date|tagged)+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * PC1 + Trt+(m.date|tagged)+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
momAov
popAov
1-pchisq(2.2837,1)
modelmdate <- lmer(lfc  ~ Origin * PC1 + Trt+(1|Pop/Mom), family=poisson,data=modeldata)
anova(modelmdate, model1)

modelT <- lmer(lfc ~ Origin*PC1+(m.date|tagged)+(1|Pop/Mom), family=poisson,data=modeldata)
trtAov <- anova(model1, modelT)
trtAov

modelint<-lmer(lfc  ~ Origin +PC1 + (m.date|tagged)+(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(modelT, modelint)
intAov

modelcov <- lmer(lfc  ~ Origin +(m.date|tagged)+(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(lfc ~ (m.date|tagged)+(1|Pop/Mom), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelO

# #lsmeans, ctrl only
# modeldata <- droplevels(subset(Frdatsk.l, Origin%in%c("inv", "nat")&Trt%in%"control"))
# modeldata<-modeldata[!is.na(modeldata$lfc),]
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
# modelint<-lmer(lfc  ~ Origin +PC1 + m.date+(1|Pop), family=poisson,data=modeldata)
# CI.LS.poisson(modelint)
# summary(modeldata$Origin)
# summary(modeldata$Pop)
# 
# #lsmeans, dr only
# modeldata <- droplevels(subset(Frdatsk.l, Origin%in%c("inv", "nat")&Trt%in%"drought"))
# modeldata<-modeldata[!is.na(modeldata$lfc),]
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
# modelint<-lmer(lfc  ~ Origin +PC1 + m.date+(1|Pop), family=poisson,data=modeldata)
# CI.LS.poisson(modelint)
# summary(modeldata$Origin)
# summary(modeldata$Pop)

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
####lfc~origin*trt####
modelOr <- lmer(lfc  ~ Origin * Trt+PC1+(m.date|tagged)+(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(lfc  ~ Origin * Trt+PC1+ (m.date|tagged)+(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1, modelOr)
model2<-lmer(lfc  ~ Origin * Trt +PC1+ (m.date|tagged)+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * Trt +PC1+ (m.date|tagged)+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(2.8929,1)
# qchisq(558.65,1,lower=FALSE)#chisq value
modelmdate <- lmer(lfc  ~ Origin * Trt +PC1+ (1|Pop/Mom), family=poisson,data=modeldata)
anova(modelmdate, model1)

modelP <- lmer(lfc  ~ Origin * Trt+ (m.date|tagged)+(1|Pop/Mom), family=poisson,data=modeldata)
anova(modelP, model1)

modelint<-lmer(lfc  ~ Origin +Trt + (m.date|tagged)+(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(modelP, modelint)
intAov

modelcov <- lmer(lfc  ~ Origin + (m.date|tagged)+(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(lfc ~ (m.date|tagged)+(1|Pop/Mom), family=poisson,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

# modelOC <- lmer(lfc  ~ Trt + (m.date|tagged)+(1|Pop/Mom), family=poisson,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov

summary(modelO)

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
modeldata<-subset(Frdatsk.l, !is.na(lfw)&Origin%in%c("inv", "nat"))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

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
1-pchisq(3.1142,1)
modelmdate <- lmer(lfw  ~ Origin * PC1 + Trt+(1|blank), family=gaussian,data=modeldata)
anova(model3, modelmdate)

model4 <- lmer(lfw  ~ Origin * PC1 + Trt+(m.date|tagged), family=gaussian,data=modeldata)
anova(model3, model4)
anova(model2, model4)

modelT <- lmer(lfw  ~ Origin * PC1 + (m.date|tagged), family=gaussian,data=modeldata)
anova(modelT, model4)

modelint<-lmer(lfw  ~ Origin +PC1 + (m.date|tagged), family=gaussian,data=modeldata)
intAov <- anova(modelT, modelint)
intAov

modelcov <- lmer(lfw  ~ Origin + (m.date|tagged), family=gaussian,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modelO<-lmer(lfw ~ (m.date|tagged), family=gaussian,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

modelcov

modelg <- glm(lfw~Origin, family=gaussian, data=modeldata)
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
# qplot(data=modeldata, PC1, lfw, color=Origin, geom = "jitter")
# 
# moddata <- ddply(modeldata, .(Pop, Origin, PC1), summarize, popCount=length(Pop), poplfw=mean(lfw, na.rm=TRUE))
# 
# #png("MF_    .png", height = 600, width = 600, pointsize = 16)
# qplot(data=moddata,PC1, poplfw, color = Origin, 
#       xlab="PC1", 
#       ylab="Population mean lf width", main="") +geom_smooth(method=glm, se=TRUE)
# # dev.off()
####lfw~origin*trt####
modelOr <- lmer(lfw  ~ Origin * Trt+PC1+(m.date|tagged)+(Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(lfw  ~ Origin * Trt+PC1 + (m.date|tagged)+(1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(lfw  ~ Origin * Trt +PC1+ (m.date|tagged)+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfw  ~ Origin * Trt +PC1+ (m.date|tagged)+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(5.12,1)
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

modelpop <- lmer(lfw  ~ Origin + (m.date|tagged), family=gaussian,data=modeldata)
anova(modelcov, modelpop)

modelO<-lmer(lfw ~ (m.date|tagged)+(1|Pop), family=gaussian,data=modeldata)
originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
originAov

# modelOC <- lmer(lfw  ~ Trt + (m.date|tagged)+(1|Pop), family=gaussian,data=modeldata)
# ocAov <- anova(modelint, modelOC)
# ocAov
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
modeldata<-subset(Frdatsk.l, !is.na(rd)&Origin%in%c("inv", "nat"))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

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
1-pchisq(4.4322,1)
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

####lfl#######
modeldata<-subset(Frdatsk.l, !is.na(lfl)&Origin%in%c("inv", "nat"))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

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
1-pchisq(1.2216,1)
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

modelO

modelg <- glm(lfl~1, family=gaussian, data=modeldata)
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
1-pchisq(1.8736,1)
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

modelO

