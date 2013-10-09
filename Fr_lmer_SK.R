#Fr_lmer_SK
#mixed-effects models of univariate traits
#lots of climate variables! Including climate PCA
#Oct 2013
#see end for data formatting, and Fr_lmer.R for modeling descriptions

#read
FrdatSK<- read.table("FrTraitClimDat_SK.txt", header=T, sep="\t",quote='"', row.names=1)

#Control and drought, REML, using lme4
#mixed effect models 
library(lme4)
library(lsmeans)
library(ggplot2)
library(plyr)

##########does Trt matter? ###########################
FrdatSK$Trt <- droplevels(FrdatSK$Trt)

#Trt
frGLR.Trt_SK <- lapply(names(FrdatSK)[c(9:11,13:14,18:25, 38:51, 53:54)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="Trt"))
#apply func to all gaussian traits. cols 38:51, 53:54 are transformed variables
frPLR.Trt_SK <- lapply(names(FrdatSK)[c(8,12,15:16,26)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="Trt", family=poisson))#apply func to all poisson traits
boltLR.Trt_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="Trt",family=binomial) #apply to single binomial trait

CGtrait_sigaov_func_Fr(frGLR.Trt_SK)
CGtrait_sigaov_func_Fr(frPLR.Trt_SK)
boltLR.Trt_SK

#########
#so for each cov and distribution

#PC1
frGLR.PC1_SK <- lapply(names(FrdatSK)[c(9:11,13:14,18:25, 38:51, 53:54)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC1"))#apply func to all gaussian traits
frPLR.PC1_SK <- lapply(names(FrdatSK)[c(8,12,15:16,26)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC1", family=poisson))#apply func to all poisson traits
boltLR.PC1_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="PC1",family=binomial) #apply to single binomial trait

#PC2
frGLR.PC2_SK <- lapply(names(FrdatSK)[c(9:11,13:14,18:25, 38:51, 53:54)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC2"))#apply func to all gaussian traits
frPLR.PC2_SK <- lapply(names(FrdatSK)[c(8,12,15:16,26)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC2", family=poisson))#apply func to all poisson traits
boltLR.PC2_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="PC2",family=binomial) #apply to single binomial trait

#PC3
frGLR.PC3_SK <- lapply(names(FrdatSK)[c(9:11,13:14,18:25, 38:51, 53:54)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC3"))#apply func to all gaussian traits
frPLR.PC3_SK <- lapply(names(FrdatSK)[c(8,12,15:16,26)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC3", family=poisson))#apply func to all poisson traits
boltLR.PC3_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="PC3",family=binomial) #apply to single binomial trait

#bio4
frGLR.bio4_SK <- lapply(names(FrdatSK)[c(9:11,13:14,18:25, 38:51, 53:54)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio4"))#apply func to all gaussian traits
frPLR.bio4_SK <- lapply(names(FrdatSK)[c(8,12,15:16,26)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio4", family=poisson))#apply func to all poisson traits
boltLR.bio4_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="bio4",family=binomial) #apply to single binomial trait

#bio19
frGLR.bio19_SK <- lapply(names(FrdatSK)[c(9:11,13:14,18:25, 38:51, 53:54)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio19"))#apply func to all gaussian traits
frPLR.bio19_SK <- lapply(names(FrdatSK)[c(8,12,15:16,26)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio19", family=poisson))#apply func to all poisson traits
boltLR.bio19_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="bio19",family=binomial) #apply to single binomial trait

#bio7
frGLR.bio7_SK <- lapply(names(FrdatSK)[c(9:11,13:14,18:25, 38:51, 53:54)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio7"))#apply func to all gaussian traits
frPLR.bio7_SK <- lapply(names(FrdatSK)[c(8,12,15:16,26)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio7", family=poisson))#apply func to all poisson traits
boltLR.bio7_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="bio7",family=binomial) #apply to single binomial trait

#Latitude
frGLR.lat_SK <- lapply(names(FrdatSK)[c(9:11,13:14,18:25, 38:51, 53:54)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="Latitude"))#apply func to all gaussian traits
frPLR.lat_SK <- lapply(names(FrdatSK)[c(8,12,15:16,26)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="Latitude", family=poisson))#apply func to all poisson traits
boltLR.lat_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="Latitude",family=binomial) #apply to single binomial trait

#which anovas have sig covariate or origin?
snglcov_SK <- c(frGLR.PC1_SK, frPLR.PC1_SK, boltLR.PC1_SK,frGLR.PC2_SK, frPLR.PC2_SK, boltLR.PC2_SK,frGLR.PC3_SK, frPLR.PC3_SK, boltLR.PC3_SK,
            frGLR.bio4_SK, boltLR.bio4_SK,frGLR.bio7_SK,  boltLR.bio7_SK,frGLR.bio19_SK,  boltLR.bio19_SK,
             frGLR.lat_SK, frPLR.lat_SK, boltLR.lat_SK, frGLR.Trt_SK, frPLR.Trt_SK, boltLR.Trt_SK)
#frPLR.bio4_SK, frPLR.bio7_SK,frPLR.bio19_SK,
save(snglcov_SK, file="Fr_aovlists_SK.RData")
load()


CGtrait_sigaov_func_Fr(frGLR.PC1_SK)
CGtrait_sigaov_func_Fr(frGLR.PC2_SK)
CGtrait_sigaov_func_Fr(frGLR.PC3_SK)
CGtrait_sigaov_func_Fr(frGLR.bio4_SK)
CGtrait_sigaov_func_Fr(frGLR.bio7_SK)
CGtrait_sigaov_func_Fr(frGLR.bio19_SK)
CGtrait_sigaov_func_Fr(frGLR.lat_SK)

CGtrait_sigaov_func_Fr(frPLR.PC1_SK)
CGtrait_sigaov_func_Fr(frPLR.PC2_SK)
CGtrait_sigaov_func_Fr(frPLR.PC3_SK)
# CGtrait_sigaov_func_Fr(frPLR.bio4_SK)
# CGtrait_sigaov_func_Fr(frPLR.bio7_SK)
# CGtrait_sigaov_func_Fr(frPLR.bio19_SK)
CGtrait_sigaov_func_Fr(frPLR.lat_SK)

boltLR.PC1_SK
boltLR.PC2_SK
boltLR.PC3_SK
boltLR.bio4_SK
boltLR.bio7_SK
boltLR.bio19_SK
boltLR.lat_SK

####
#maxlfwidth2
modeldata <- FrdatSK[!is.na(FrdatSK$MaxLfWdth2),]
# modeldata<-df[!is.na(df[[trait]]),]
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
modeldata$Origin <- factor(modeldata$Origin, levels=c("nat", "inv","sk"))

p1 <- ggplot(modeldata,aes(Trt, MaxLfWdth2, fill=Origin))+
  geom_boxplot()+xlab("Stress Treatment")+
  ylab("lfwidth2")+ 
  theme(legend.justification=c(1,1), legend.position=c(1,1))
p1

p2 <- ggplot(modeldata,aes(Latitude, MaxLfWdth2, color=Origin))+
  geom_point()+xlab("latitude")+ geom_smooth(method=glm, se=FALSE)+
  ylab("lfwidth2")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))

multiplot(p1, p2, cols=2)

#lflegnthH
modeldata <- FrdatSK[!is.na(FrdatSK$LfLgth),]
# modeldata<-df[!is.na(df[[trait]]),]
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
modeldata$Origin <- factor(modeldata$Origin, levels=c("nat", "inv","sk"))

p1 <- ggplot(modeldata,aes(Trt, LfLgth, fill=Origin))+
  geom_boxplot()+xlab("Stress Treatment")+
  ylab("max lf length at harvest")+ 
  theme(legend.justification=c(1,1), legend.position=c(1,1))
p1

p2 <- ggplot(modeldata,aes(Latitude, LfLgth, color=Origin))+
  geom_point()+xlab("latitude")+ geom_smooth(method=glm, se=FALSE)+
  ylab("max lf length at harvest")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))

multiplot(p1, p2, cols=2)

# #####format datas#####
# #get sk datas
# FrdesSK <- Frdes[Frdes$Origin=="sk" & Frdes$Trt!="edge",]
# FrdesSK <- FrdesSK[!is.na(FrdesSK$Pop),]
# 
# Frm1 <- read.table("FrMeasure1.txt", header=T, sep="\t", quote='"')
# FrdatSK <- merge(FrdesSK,Frm1, all.x=TRUE)
# 
# Frshoot <- read.table("FrShootMassH.txt", header=T, sep="\t", quote='"')
# FrdatSK <- merge(FrdatSK,Frshoot, all.x=TRUE)
# 
# Frm2 <- read.table("FrMeasure2.txt", header=T, sep="\t", quote='"')
# FrdatSK <- merge(FrdatSK,Frm2, all.x=TRUE)
# 
# Frh <- read.table("FrMeasureHarvest.txt", header=T, sep="\t", quote='"')
# FrdatSK <- merge(FrdatSK,Frh, all.x=TRUE)
# 
# #load climate table
# Frclimdat2 <- read.table("FrbioclimPCAdat.txt", header=TRUE)
# FrdatSK <- merge(FrdatSK,Frclimdat2[,c(1,13,16,19,22:27)], all.x=TRUE)
# row.names(FrdatSK) <- FrdatSK$tagged
# 
# setdiff(colnames(frdat), colnames(FrdatSK))
# setdiff(colnames(FrdatSK), colnames(frdat)) #some extraneous columns/transformations, nothing major.
# 
# #merge
# colnames(FrdatSK)[2] <- "tagged"
# FrdatSKonly <- FrdatSK
# #write
# write.table(FrdatSKonly, file="FrTraitClimDat_SKonly.txt",sep="\t", quote=F)
# 
# #convert dates
# FrdatSKonly$Harvest.date2 <- strptime(FrdatSKonly$Harvest.date, format="%A, %B %d, %Y")
# FrdatSKonly$Harvest.date2 <- as.Date(FrdatSKonly$Harvest.date2)
# day0 <- as.Date("2011-05-12") #planting date
# FrdatSKonly$Harvest.date3 <- as.numeric(FrdatSKonly$Harvest.date2-day0)
# FrdatSKonly$Harvest.date <- FrdatSKonly$Harvest.date3
# FrdatSKonly <- FrdatSKonly[,1:47]
# 
# FrdatSKonly$Bolt.date2 <- strptime(FrdatSKonly$Bolt.date, format="%A, %B %d, %Y")
# FrdatSKonly$Bolt.date2 <- as.Date(FrdatSKonly$Bolt.date2)
# day0 <- as.Date("2011-05-12")
# FrdatSKonly$Bolt.date3 <- as.numeric(FrdatSKonly$Bolt.date2-day0)
# FrdatSKonly$Bolt.date <- FrdatSKonly$Bolt.date3
# FrdatSKonly <- FrdatSKonly[,1:47]
# 
# FrdatSKonly$Bolt.date <- as.integer(FrdatSKonly$Bolt.date)
# FrdatSKonly$Harvest.date <- as.integer(FrdatSKonly$Harvest.date)
# 
# FrdatSK <- merge(frdat, FrdatSKonly[,c(1:7,12:14, 17:21, 23:26, 31:37, 39:47)], all=TRUE)
# 
# #Transform datas!
# #log transform continuous
# #sqrt transform count data
# #arcsine sqrt for proportions angularx=57.295*asin(sqrt(x))?
# #or for percentages angularx=57.295*asin(sqrt(x/100))
# #or maybe logit? logit=log(prop/(1-prop))
# FrdatSK$LfLgth1.log <- log(FrdatSK$MaxLfLgth1)
# FrdatSK$LfWdth1.log <- log(FrdatSK$MaxLfWdth1)
# FrdatSK$RoseD2.log <- log(FrdatSK$Rose.diam2)
# FrdatSK$LfLgth2.log <- log(FrdatSK$MaxLfLgth2)
# FrdatSK$LfWdth2.log <- log(FrdatSK$MaxLfWdth2)
# FrdatSK$MaxRoseDh.log <- log(FrdatSK$ Max.Rose.diamH)
# FrdatSK$MinRoseDh.log <- log(FrdatSK$Min.Rose.diamH)
# FrdatSK$RoseAh.log <- log(FrdatSK$Rose.AreaH.m2)
# FrdatSK$LfLgthH.log <- log(FrdatSK$LfLgth)
# FrdatSK$LfWdthH.log <- log(FrdatSK$Lf.Width)
# FrdatSK$Crown.log <- log(FrdatSK$CrownDiam.mm)
# FrdatSK$Mass.log <- log(FrdatSK$Shoot.mass.gH)
# 
# FrdatSK$LfCount1.sq <- sqrt(FrdatSK$LfCount1)
# FrdatSK$LfCount2.sq <- sqrt(FrdatSK$LfCount2)
# FrdatSK$Bolt.sq <- sqrt(FrdatSK$Bolt.date)
# FrdatSK$Harvest.sq <- sqrt(FrdatSK$Harvest.date)

#write
write.table(FrdatSK, file="FrTraitClimDat_SK.txt",sep="\t", quote=F)

