#Fr_lmer_SK
#mixed-effects models of univariate traits
#lots of climate variables! Including climate PCA
#Oct 2013
#see FrSkdata_format.R for data formatting, and Fr_lmer.R for modeling descriptions

###tests2 tests2 tests2###


#read
FrdatSK<- read.table("FrTraitClimDat_SK.txt", header=T, sep="\t",quote='"', row.names=1)

#Control and drought, REML, using lme4
#mixed effect models 
library(lme4)
library(lsmeans)
library(ggplot2)
library(plyr)

# ##########does Trt matter? ###########################
# FrdatSK$Trt <- droplevels(FrdatSK$Trt)
# 
# #Trt
# frGLR.Trt_SK <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="Trt"))
# #apply func to all gaussian traits. cols 38:51, 53:54 are transformed variables
# frPLR.Trt_SK <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="Trt", family=poisson))#apply func to all poisson traits
# boltLR.Trt_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="Trt",family=binomial) #apply to single binomial trait
# 
# CGtrait_sigaov_func_Fr(frGLR.Trt_SK)
# CGtrait_sigaov_func_Fr(frPLR.Trt_SK)
# boltLR.Trt_SK
# 
# ###########################################
# #so for each cov and distribution
# 
# #PC1
# frGLR.PC1_SK <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC1"))#apply func to all gaussian traits
# frPLR.PC1_SK <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC1", family=poisson))#apply func to all poisson traits
# boltLR.PC1_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="PC1",family=binomial) #apply to single binomial trait
# 
# #PC2
# frGLR.PC2_SK <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC2"))#apply func to all gaussian traits
# frPLR.PC2_SK <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC2", family=poisson))#apply func to all poisson traits
# boltLR.PC2_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="PC2",family=binomial) #apply to single binomial trait
# 
# #PC3
# frGLR.PC3_SK <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC3"))#apply func to all gaussian traits
# frPLR.PC3_SK <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="PC3", family=poisson))#apply func to all poisson traits
# boltLR.PC3_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="PC3",family=binomial) #apply to single binomial trait
# 
# #bio11
# frGLR.bio11_SK <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio11"))#apply func to all gaussian traits
# frPLR.bio11_SK <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio11", family=poisson))#apply func to all poisson traits
# boltLR.bio11_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="bio11",family=binomial) #apply to single binomial trait
# 
# #bio9
# frGLR.bio9_SK <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio9"))#apply func to all gaussian traits
# frPLR.bio9_SK <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio9", family=poisson))#apply func to all poisson traits
# boltLR.bio9_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="bio9",family=binomial) #apply to single binomial trait
# 
# #bio6
# frGLR.bio6_SK <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio6"))#apply func to all gaussian traits
# frPLR.bio6_SK <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="bio6", family=poisson))#apply func to all poisson traits
# boltLR.bio6_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="bio6",family=binomial) #apply to single binomial trait
# 
# #Latitude
# frGLR.lat_SK <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="Latitude"))#apply func to all gaussian traits
# frPLR.lat_SK <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov(n,FrdatSK, covariate="Latitude", family=poisson))#apply func to all poisson traits
# boltLR.lat_SK <- CGtrait.LR_snglcov(trait="bolt.bin",df=FrdatSK,covariate="Latitude",family=binomial) #apply to single binomial trait
# 
# #which anovas have sig covariate or origin?
# snglcov_SK <- c(frGLR.PC1_SK, frPLR.PC1_SK, boltLR.PC1_SK,frGLR.PC2_SK, frPLR.PC2_SK, boltLR.PC2_SK,frGLR.PC3_SK, frPLR.PC3_SK, boltLR.PC3_SK,
#             frGLR.bio4_SK, boltLR.bio4_SK,frGLR.bio7_SK,  boltLR.bio7_SK,frGLR.bio19_SK,  boltLR.bio19_SK,
#              frGLR.lat_SK, frPLR.lat_SK, boltLR.lat_SK, frGLR.Trt_SK, frPLR.Trt_SK, boltLR.Trt_SK)
# #frPLR.bio4_SK, frPLR.bio7_SK,frPLR.bio19_SK,
# save(snglcov_SK, file="Fr_aovlists_SK.RData")
# load(file="Fr_aovlists_SK.RData")
# 
# 
# CGtrait_sigaov_func_Fr(frGLR.PC1_SK)
# CGtrait_sigaov_func_Fr(frGLR.PC2_SK)
# CGtrait_sigaov_func_Fr(frGLR.PC3_SK)
# CGtrait_sigaov_func_Fr(frGLR.bio4_SK)
# CGtrait_sigaov_func_Fr(frGLR.bio7_SK)
# CGtrait_sigaov_func_Fr(frGLR.bio19_SK)
# CGtrait_sigaov_func_Fr(frGLR.lat_SK)
# 
# CGtrait_sigaov_func_Fr(frPLR.PC1_SK)
# CGtrait_sigaov_func_Fr(frPLR.PC2_SK)
# CGtrait_sigaov_func_Fr(frPLR.PC3_SK)
# # CGtrait_sigaov_func_Fr(frPLR.bio4_SK)
# # CGtrait_sigaov_func_Fr(frPLR.bio7_SK)
# # CGtrait_sigaov_func_Fr(frPLR.bio19_SK)
# # CGtrait_sigaov_func_Fr(frPLR.lat_SK)
# 
# boltLR.PC1_SK
# boltLR.PC2_SK
# boltLR.PC3_SK
# # boltLR.bio4_SK
# # boltLR.bio7_SK
# boltLR.bio19_SK
# boltLR.lat_SK
# 
# ####Latitude####
# #maxlfwidth2
# modeldata <- FrdatSK[!is.na(FrdatSK$MaxLfWdth2),]
# # modeldata<-df[!is.na(df[[trait]]),]
# # modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# # modeldata$Mom<-as.factor(modeldata$Mom)
# modeldata$Origin <- factor(modeldata$Origin, levels=c("nat", "inv","sk"))
# 
# p1 <- ggplot(modeldata,aes(Trt, MaxLfWdth2, fill=Origin))+
#   geom_boxplot()+xlab("Stress Treatment")+
#   ylab("lfwidth2")+ 
#   theme(legend.justification=c(1,1), legend.position=c(1,1))
# p1
# 
# p2 <- ggplot(modeldata,aes(Latitude, MaxLfWdth2, color=Origin))+
#   geom_point()+xlab("latitude")+ geom_smooth(method=glm, se=FALSE)+
#   ylab("lfwidth2")+ 
#   theme(legend.justification=c(0,1), legend.position=c(0,1))
# 
# multiplot(p1, p2, cols=2)
# 
# #lflegnthH
# modeldata <- FrdatSK[!is.na(FrdatSK$LfLgth),]
# # modeldata<-df[!is.na(df[[trait]]),]
# # modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# # modeldata$Mom<-as.factor(modeldata$Mom)
# modeldata$Origin <- factor(modeldata$Origin, levels=c("nat", "inv","sk"))
# 
# p1 <- ggplot(modeldata,aes(Trt, LfLgth, fill=Origin))+
#   geom_boxplot()+xlab("Stress Treatment")+
#   ylab("max lf length at harvest")+ 
#   theme(legend.justification=c(1,1), legend.position=c(1,1))
# p1
# 
# p2 <- ggplot(modeldata,aes(Latitude, LfLgth, color=Origin))+
#   geom_point()+xlab("latitude")+ geom_smooth(method=glm, se=FALSE)+
#   ylab("max lf length at harvest")+ 
#   theme(legend.justification=c(0,1), legend.position=c(0,1))
# 
# multiplot(p1, p2, cols=2)
# 
# ####PC2####
# #maxBoltHtH
# modeldata <- FrdatSK[!is.na(FrdatSK$MaxBoltHtH),]
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
# modeldata$Origin <- factor(modeldata$Origin, levels=c("nat", "inv","sk"))
# 
# p1 <- ggplot(modeldata,aes(Trt, MaxBoltHtH, fill=Origin))+
#   geom_boxplot()+xlab("Stress Treatment")+
#   ylab("MaxBoltHtH")+ 
#   theme(legend.justification=c(1,1), legend.position=c(1,1))
# p1
# 
# p2 <- ggplot(modeldata,aes(PC2, MaxBoltHtH, color=Origin))+
#   geom_point()+xlab("PC2")+ geom_smooth(method=glm, se=FALSE)+
#   ylab("MaxBoltHtH")+ 
#   theme(legend.justification=c(0,1), legend.position=c(0,1))
# p2
# multiplot(p1, p2, cols=2)
# 
# ####bio19####
# #LfCount1.sq
# modeldata <- FrdatSK[!is.na(FrdatSK$LfCount1.sq),]
# modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
# modeldata$Mom<-as.factor(modeldata$Mom)
# modeldata$Origin <- factor(modeldata$Origin, levels=c("nat", "inv","sk"))
# 
# p1 <- ggplot(modeldata,aes(Trt, LfCount1.sq, fill=Origin))+
#   geom_boxplot()+xlab("Stress Treatment")+
#   ylab("LfCount1.sq")+ 
#   theme(legend.justification=c(1,1), legend.position=c(1,1))
# p1
# 
# p2 <- ggplot(modeldata,aes(bio19, LfCount1.sq, color=Origin))+
#   geom_point()+xlab("bio19")+ geom_smooth(method=glm, se=FALSE)+
#   ylab("LfCount1.sq")+ 
#   theme(legend.justification=c(0,1), legend.position=c(0,1))
# p2
# multiplot(p1, p2, cols=2)

###########sngl cov with interaction########################
#so for each cov and distribution

#PC1
frGLR.PC1_SKint <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="PC1"))#apply func to all gaussian traits
frPLR.PC1_SKint <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="PC1", family=poisson))#apply func to all poisson traits
boltLR.PC1_SKint <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=FrdatSK,covariate="PC1",family=binomial) #apply to single binomial trait

#PC2
frGLR.PC2_SKint <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="PC2"))#apply func to all gaussian traits
frPLR.PC2_SKint <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="PC2", family=poisson))#apply func to all poisson traits
boltLR.PC2_SKint <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=FrdatSK,covariate="PC2",family=binomial) #apply to single binomial trait

#PC3
frGLR.PC3_SKint <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="PC3"))#apply func to all gaussian traits
frPLR.PC3_SKint <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="PC3", family=poisson))#apply func to all poisson traits
boltLR.PC3_SKint <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=FrdatSK,covariate="PC3",family=binomial) #apply to single binomial trait

#bio11
frGLR.bio11_SKint <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="bio11"))#apply func to all gaussian traits
frPLR.bio11_SKint <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="bio11", family=poisson))#apply func to all poisson traits
boltLR.bio11_SKint <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=FrdatSK,covariate="bio11",family=binomial) #apply to single binomial trait

#bio9
frGLR.bio9_SKint <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="bio9"))#apply func to all gaussian traits
frPLR.bio9_SKint <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="bio9", family=poisson))#apply func to all poisson traits
boltLR.bio9_SKint <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=FrdatSK,covariate="bio9",family=binomial) #apply to single binomial trait

#bio6
frGLR.bio6_SKint <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="bio6"))#apply func to all gaussian traits
frPLR.bio6_SKint <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="bio6", family=poisson))#apply func to all poisson traits
boltLR.bio6_SKint <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=FrdatSK,covariate="bio6",family=binomial) #apply to single binomial trait

#Latitude
frGLR.lat_SKint <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="Latitude"))#apply func to all gaussian traits
frPLR.lat_SKint <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="Latitude", family=poisson))#apply func to all poisson traits
boltLR.lat_SKint <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=FrdatSK,covariate="Latitude",family=binomial) #apply to single binomial trait

#Treatment
frGLR.Trt_SKint <- lapply(names(FrdatSK)[c(9:10,12,14:15,19:26,40:53, 55:56)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="Trt"))#apply func to all gaussian traits
frPLR.Trt_SKint <- lapply(names(FrdatSK)[c(8,13,16:17,27)],function(n) CGtrait.LR_snglcov_int(n,FrdatSK, covariate="Trt", family=poisson))#apply func to all poisson traits
boltLR.Trt_SKint <- CGtrait.LR_snglcov_int(trait="bolt.bin",df=FrdatSK,covariate="Trt",family=binomial) #apply to single binomial trait

#which anovas have sig covariate or origin?
snglcov_SKint <- c(frGLR.PC1_SKint,frGLR.PC2_SKint,frGLR.bio11_SKint,frGLR.bio9_SKint,frGLR.bio6_SKint,frGLR.Trt_SKint,
                  frPLR.PC1_SKint,frPLR.PC3_SKint,frPLR.Trt_SKint,
                    boltLR.PC1_SKint,boltLR.PC2_SKint,boltLR.PC3_SKint,boltLR.bio11_SKint,boltLR.bio9_SKint,boltLR.bio6_SKint,boltLR.lat_SKint,boltLR.Trt_SKint)
# frPLR.bio4_SK, frPLR.bio7_SK,frPLR.bio19_SK,frGLR.lat_SK,frPLR.lat_SK,boltLR.bio4_SK,boltLR.bio7_SK,
save(snglcov_SKint, file="FrDKSKaovlists.RData")
snglcov_SKint <- load(file="FrDKSKaovlists.RData")


CGtrait_sigaov_func_Fr(frGLR.PC1_SKint, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.PC1_SKint, selectaov=1:6)
boltLR.PC1_SKint

CGtrait_sigaov_func_Fr(frGLR.PC2_SKint, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.PC2_SKint, selectaov=1:6)
boltLR.PC2_SKint

CGtrait_sigaov_func_Fr(frGLR.PC3_SKint, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.PC3_SKint, selectaov=1:6)
boltLR.PC3_SKint

CGtrait_sigaov_func_Fr(frGLR.bio11_SKint, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.bio11_SKint, selectaov=1:6)
boltLR.bio11_SKint

CGtrait_sigaov_func_Fr(frGLR.bio9_SKint, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.bio9_SKint, selectaov=1:6)
boltLR.bio9_SKint

CGtrait_sigaov_func_Fr(frGLR.bio6_SKint, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.bio6_SKint, selectaov=1:6)
boltLR.bio6_SKint

CGtrait_sigaov_func_Fr(frGLR.lat_SKint, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.lat_SKint, selectaov=1:6)
boltLR.lat_SKint

CGtrait_sigaov_func_Fr(frGLR.Trt_SKint, selectaov=1:6)
CGtrait_sigaov_func_Fr(frPLR.Trt_SKint, selectaov=1:6)
boltLR.Trt_SKint