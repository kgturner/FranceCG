#Fr dK only, PC1 and PC2 only covariates

#REML, using lme4
#mixed effect models 
library(lme4)
library(lsmeans)
library(ggplot2)
library(plyr)

#with SK
#for long data formating, see FrSKdata_format.R
#read
Frdatsk.l<- read.table("FrTraitClimDat_SK_long.txt", header=T, sep="\t",quote='"', row.names=1)
#read
FrdatSK<- read.table("FrTraitClimDat_SK.txt", header=T, sep="\t",quote='"', row.names=1)
#read
frend<- read.table("FrEnd.txt", header=T, sep="\t",quote='"', row.names=1)

#for DK only include:
subset(frend, Origin%in%c("inv", "nat"))

# CGtrait.LR_2cov_int(FrdatSK$RoseAh.log,FrdatSK,Latitude,family=gaussian)

#########Origin*PC1*PC2######
modeldata <- subset(frend, Origin%in%c("inv", "nat"))
frGLR.frend_DK2cov <- lapply(names(modeldata)[c(23:24)],function(n) CGtrait.LR_2cov_int(n,modeldata, covariate1="PC1", covariate2="PC2"))#apply func to all gaussian traits
frPLR.frend_DK2cov <- lapply(names(modeldata)[c(8:10,12,15:16)],function(n) CGtrait.LR_2cov_int(n,modeldata, covariate1="PC1", covariate2="PC2", family=poisson))#apply func to all poisson traits
frBLR.frend_DK2cov <- lapply(names(modeldata)[c(21,25)],function(n) CGtrait.LR_2cov_int(n,modeldata, covariate1="PC1", covariate2="PC2", family=binomial)) #apply to binomial trait

modeldata <- subset(FrdatSK, Origin%in%c("inv", "nat"))
frGLR.wide_DK2cov <- lapply(names(modeldata)[c(49, 53,52)],function(n) CGtrait.LR_2cov_int(n,modeldata, covariate1="PC1", covariate2="PC2"))#apply func to all gaussian traits

modeldata <- subset(Frdatsk.l, Origin%in%c("inv", "nat"))
frGLR.long_DK2cov <- lapply(names(modeldata)[c(28,29,31)],function(n) CGtrait.LR_2cov_int_mdate(n,modeldata, covariate1="PC1", covariate2="PC2"))#apply func to all gaussian traits
frPLR.long_DK2cov <- CGtrait.LR_2cov_int_mdate("lfc",modeldata, covariate1="PC1", covariate2="PC2", family=poisson)#apply func to all poisson traits

CGtrait_sigaov_func_Fr(frGLR.frend_DK2cov, selectaov=1:7, cutoff=0.05)
CGtrait_sigaov_func_Fr(frPLR.frend_DK2cov, selectaov=1:7, cutoff=0.05)
CGtrait_sigaov_func_Fr(frBLR.frend_DK2cov, selectaov=1:7, cutoff=0.05)

CGtrait_sigaov_func_Fr(frGLR.wide_DK2cov, selectaov=1:7, cutoff=0.05)

CGtrait_sigaov_func_Fr(frGLR.long_DK2cov, selectaov=1:7, cutoff=0.05)
# frPLR.long_DK2cov