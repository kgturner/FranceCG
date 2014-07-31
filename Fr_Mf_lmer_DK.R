#France/Maternal effects comparison_modeling
#for DK only material

#REML, using lme4
#mixed effect models 
library(plyr)
library(lme4)
library(lsmeans)
library(ggplot2)

# #necessary...?
library(AER)
# dispersiontest(modelg1)

#read
Mf <- read.table("Fr_Mf_data.txt", header=T, sep="\t",quote='"', row.names=1)
Mf.l <- read.table("Fr_Mf_data_long.txt", header=T, sep="\t",quote='"', row.names=1)

#for equivalent traits only:
#crown, bolt.bin, mass, lfc

####final models####

####lfc####
modeldata<-Mf.l[!is.na(Mf.l$lfc),]
modeldata <- subset(modeldata, Trt%in%"control"&Origin%in%c("inv", "nat"))
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)

summary(modeldata$Origin)
summary(modeldata$Pop)


#pc2
# # modeldata$PopMom <- as.factor(paste0(modeldata$Pop,"_",modeldata$Mom))
# # xtabs(lfc~ m.date+PopMom, data=modeldata)
# # count(modeldata, vars=c("Pop","Mom", "m.date"))
# # count(modeldata, vars=c("PopMom", "m.date"))
# # count(modeldata, vars=c("Pop", "m.date"))
# # # modeldata <- subset(modeldata, subset=PopMom!="BG001_16N")
# # # summary(glm(lfc  ~ Origin * PC1 +m.date, family=poisson, data=modeldata))
# # modeldata$PC2.1 <- modeldata$PC2/100
# # modeldata$PC2x <- (modeldata$PC2-mean(modeldata$PC2))/sd(modeldata$PC2) #to standarize PC2
# modeldata$lfc.sqrt <- sqrt(modeldata$lfc) #to avoid false convergence, sqrt transform lfc and use gaussian distribution
# summary(modeldata$)

modelOr <- lmer(lfc  ~ Origin * PC2 + (m.date|Barcode)+(Origin|Pop/Mom), family=poisson,data=modeldata)
model1<-lmer(lfc  ~ Origin * PC2 + (m.date|Barcode)+(1|Pop/Mom), family=poisson,data=modeldata)
anova(model1,modelOr)
model2<-lmer(lfc  ~ Origin * PC2 + (m.date|Barcode)+(1|Pop), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfc  ~ Origin * PC2 + (m.date|Barcode)+(1|blank), family=poisson,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
momAov
popAov
1-pchisq(2.2837,1)
modelmdate <- lmer(lfc  ~ Origin * PC2 + (1|Pop/Mom), family=poisson,data=modeldata)
anova(modelmdate, model1)

modelint<-lmer(lfc  ~ Origin +PC2 + (m.date|Barcode)+(1|Pop/Mom), family=poisson,data=modeldata)
intAov <- anova(model1, modelint)
intAov

modelcov <- lmer(lfc  ~ Origin +(m.date|Barcode)+(1|Pop/Mom), family=poisson,data=modeldata)
covAov <- anova(modelint, modelcov)
covAov

modeloc<-lmer(lfc ~ PC2+ (m.date|Barcode)+(1|Pop/Mom), family=poisson,data=modeldata)
ocAov <- anova(modeloc,modelint) #test for significance of origin - origin only marginally sig....!
ocAov

modeloc

# 
# interaction.plot(response = modeldata$lfc, x.factor = modeldata$PC2, trace.factor = modeldata$Origin)
# plot(modeldata$PC2, modeldata$Origin)
# qplot(data=modeldata, PC2, lfc, color=Origin, geom = "jitter")
# 
moddata <- ddply(modeldata, .(Pop, Origin, PC2), summarize, popCount=length(Pop), poplfc=mean(lfc, na.rm=TRUE))
# 
png("MF_lfcvsMfPc2.png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC2, poplfc, color = Origin, 
      xlab="PC2", 
      ylab="Population mean lf count", main="") +geom_smooth(method=glm, se=TRUE)
dev.off()
####mass.log####
modeldata <- droplevels(subset(Mf, Trt%in%"control"&Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$Mass.log),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)

#PC2
modelOr <- lmer(Mass.log  ~ Origin * PC2+ (Origin|Pop/Mom), family=gaussian,data=modeldata)
model1<-lmer(Mass.log  ~ Origin * PC2+ (1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelOr)
model2<-lmer(Mass.log  ~ Origin * PC2+(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log  ~ Origin * PC2+(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.2993,1)

modelint<- lmer(Mass.log  ~ Origin + PC2 + (1|Pop/Mom), family=gaussian,data=modeldata)
anova(model1, modelint)

model1
modelg <- glm(Mass.log~Origin*PC2, family=gaussian, data=modeldata)
summary(modelg)
CI.LS.gaussian.log(modelint)

# qplot(data=modeldata,PC2, Mass.log, color = Origin)+geom_point(position="jitter")
# 
#sk included in plot 
moddata <- ddply(modeldata, .(Pop, Origin, PC2), summarize, popCount=length(Pop), popMass.log=mean(Mass.log, na.rm=TRUE))

png("MF_MassvsMfPC2.png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC2, popMass.log, color = Origin, 
      xlab="PC2", 
      ylab="Population mean Mass.log", main="") +geom_smooth(method=glm, se=TRUE)
dev.off()
####bolt.bin####
modeldata <- droplevels(subset(Mf, Trt%in%"control"&Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$bolt.bin),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)


#PC2
model1<-lmer(bolt.bin  ~ Origin * PC2 +(1|Pop/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin  ~ Origin * PC2 +(1|Pop), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin * PC2 +(1|blank), family=binomial,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(1.4876,1)

#try glm
modelg <- glm(bolt.bin ~ Origin*PC2, family=binomial,data=modeldata)
modelg1 <- glm(bolt.bin ~ Origin+PC2, family=binomial,data=modeldata)
anova(modelg1, modelg, test="LRT") 
# qchisq(0.0964,1,lower=FALSE)#chisq value

# modelg3<- glm(bolt.bin ~ Origin, family=binomial,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# # qchisq(0.9672,1,lower=FALSE)#chisq value
# 
# modelg2<- glm(bolt.bin ~ PC2, family=binomial,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# qchisq(0.5399,1,lower=FALSE)#chisq value
# modelg4<- glm(bolt.bin ~ Origin+PC2, family=binomial,data=modeldata)
# anova(modelg4,modelg1, test="LRT")

modelg
summary(modelg)

#means and CI
CI.LS.binomial(modelg1)

#overdispersion
deviance(modelg) 
summary(modelg)$dispersion 
dfr <- df.residual(modelg)
deviance(modelg)/dfr 
d_2 <- sum(residuals(modelg,"pearson")^2) 
(disp2 <- d_2/dfr)  
pchisq(d_2,df=dfr,lower.tail=FALSE) 

moddata <- ddply(modeldata, .(Pop, Origin, PC2), summarize, popCount=length(Pop), popbolt=mean(bolt.bin, na.rm=TRUE))

png("MF_bolt.binvsMfPc2.png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC2, popbolt, color = Origin, 
      xlab="Mf PC2", 
      ylab="Population mean proportion bolted at harvest", main="") +geom_smooth(method=glm, se=TRUE)
dev.off()
####crown.log####
modeldata <- droplevels(subset(Mf, Trt%in%"control"&Origin%in%c("inv", "nat")))
modeldata<-modeldata[!is.na(modeldata$Crown.log),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)

#PC2
model1<-lmer(Crown.log  ~ Origin * PC2 +(1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Crown.log  ~ Origin * PC2 +(1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Crown.log  ~ Origin * PC2 +(1|blank), family=gaussian,data=modeldata) # Test population effect
momAov <- anova(model2,model1) # mom is sig!
momAov
popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
popAov
1-pchisq(16.323,1)

#try glm
modelg <- glm(Crown.log ~ Origin*PC2, family=gaussian,data=modeldata)
modelg1 <- glm(Crown.log ~ Origin+PC2, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.004088,1,lower=FALSE)#chisq value

# modelg3<- glm(Crown.log ~ Origin, family=gaussian,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# # qchisq(0.9672,1,lower=FALSE)#chisq value
# 
# modelg2<- glm(Crown.log ~ PC2, family=gaussian,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
# qchisq(0.5399,1,lower=FALSE)#chisq value
# modelg4<- glm(Crown.log ~ Origin+PC2, family=gaussian,data=modeldata)
# anova(modelg4,modelg1, test="LRT")

modelg
summary(modelg)

#means and CI
CI.LS.gaussian.log(modelg1)

#overdispersion
deviance(modelg) 
summary(modelg)$dispersion 
dfr <- df.residual(modelg)
deviance(modelg)/dfr 
d_2 <- sum(residuals(modelg,"pearson")^2) 
(disp2 <- d_2/dfr)  
pchisq(d_2,df=dfr,lower.tail=FALSE) 

moddata <- ddply(modeldata, .(Pop, Origin, PC2), summarize, popCount=length(Pop), popCrown=mean(Crown.log, na.rm=TRUE))

png("MF_CrownvsMfPc2.png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,PC2, popCrown, color = Origin, 
      xlab="Mf PC2", 
      ylab="Population mean Crown.log", main="") +geom_smooth(method=glm, se=TRUE)
dev.off()
