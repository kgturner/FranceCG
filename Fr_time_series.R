#Fr data
#with SK
#time series analysis

#for long data formating, see FrSKdata_format.R
#read
Frdatsk.l<- read.table("FrTraitClimDat_SK_long.txt", header=T, sep="\t",quote='"', row.names=1)

qplot(data=Frdatsk.l, m.date, lfl, color=Origin)+geom_line(group=Origin)
ggplot(Frdatsk.l,aes(m.date, lfl, color=Origin))+
  geom_point()+xlab("date")+ geom_smooth(method=glm, se=FALSE)+
  ylab("lf length")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))

ggplot(Frdatsk.l,aes(m.date, lfw, color=Origin))+
  geom_point()+xlab("date")+ geom_smooth(method=glm, se=FALSE)+
  ylab("lf width")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))

ggplot(Frdatsk.l,aes(m.date, lfc, color=Origin))+
  geom_point()+xlab("date")+ geom_smooth(method=glm, se=FALSE)+
  ylab("lf count")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))

ggplot(Frdatsk.l,aes(m.date, rd, color=Origin))+
  geom_point()+xlab("date")+ geom_smooth(method=glm, se=FALSE)+
  ylab("rosette diameter")+ 
  theme(legend.justification=c(0,1), legend.position=c(0,1))

####modeling######
CGtrait.LR_snglcov_int_time(trait="lfl", df=Frdatsk.l, covariate="PC1")

###########sngl cov with interaction########################
#so for each cov and distribution
CGtrait.LR_snglcov_int_time()
CGtrait.models_snglcov_int_time()

#PC1
frGLR.PC1_SKtime <- lapply(names(Frdatsk.l)[c(28,27,30)],function(n) CGtrait.LR_snglcov_int_time(n,Frdatsk.l, covariate="PC1"))#apply func to all gaussian traits
frPLR.PC1_SKtime <- CGtrait.LR_snglcov_int_time(trait="lfc",df=Frdatsk.l, covariate="PC1", family=poisson)#apply func to all poisson traits

#PC2
frGLR.PC2_SKtime <- lapply(names(Frdatsk.l)[c(28,27,30)],function(n) CGtrait.LR_snglcov_int_time(n,Frdatsk.l, covariate="PC2"))#apply func to all gaussian traits
frPLR.PC2_SKtime <- CGtrait.LR_snglcov_int_time(trait="lfc",df=Frdatsk.l, covariate="PC2", family=poisson)#apply func to all poisson traits

#PC3
frGLR.PC3_SKtime <- lapply(names(Frdatsk.l)[c(28,27,30)],function(n) CGtrait.LR_snglcov_int_time(n,Frdatsk.l, covariate="PC3"))#apply func to all gaussian traits
frPLR.PC3_SKtime <- CGtrait.LR_snglcov_int_time(trait="lfc",df=Frdatsk.l, covariate="PC3", family=poisson)#apply func to all poisson traits

#bio4
frGLR.bio4_SKtime <- lapply(names(Frdatsk.l)[c(28,27,30)],function(n) CGtrait.LR_snglcov_int_time(n,Frdatsk.l, covariate="bio4"))#apply func to all gaussian traits
frPLR.bio4_SKtime <- CGtrait.LR_snglcov_int_time(trait="lfc",df=Frdatsk.l, covariate="bio4", family=poisson)#apply func to all poisson traits

#bio19
frGLR.bio19_SKtime <- lapply(names(Frdatsk.l)[c(28,27,30)],function(n) CGtrait.LR_snglcov_int_time(n,Frdatsk.l, covariate="bio19"))#apply func to all gaussian traits
frPLR.bio19_SKtime <- CGtrait.LR_snglcov_int_time(trait="lfc",df=Frdatsk.l, covariate="bio19", family=poisson)#apply func to all poisson traits

#bio7
frGLR.bio7_SKtime <- lapply(names(Frdatsk.l)[c(28,27,30)],function(n) CGtrait.LR_snglcov_int_time(n,Frdatsk.l, covariate="bio7"))#apply func to all gaussian traits
frPLR.bio7_SKtime <- CGtrait.LR_snglcov_int_time(trait="lfc",df=Frdatsk.l, covariate="bio7", family=poisson)#apply func to all poisson traits

#Latitude
frGLR.lat_SKtime <- lapply(names(Frdatsk.l)[c(28,27,30)],function(n) CGtrait.LR_snglcov_int_time(n,Frdatsk.l, covariate="Latitude"))#apply func to all gaussian traits
frPLR.lat_SKtime <- CGtrait.LR_snglcov_int_time(trait="lfc",df=Frdatsk.l, covariate="Latitude", family=poisson)#apply func to all poisson traits

#Treatment
frGLR.trt_SKtime <- lapply(names(Frdatsk.l)[c(28,27,30)],function(n) CGtrait.LR_snglcov_int_time(n,Frdatsk.l, covariate="Trt"))#apply func to all gaussian traits
frPLR.trt_SKtime <- CGtrait.LR_snglcov_int_time(trait="lfc",df=Frdatsk.l, covariate="Trt", family=poisson)#apply func to all poisson traits

#all the lfc had non-convergences, have to do by hand

CGtrait_sigaov_func_Fr(frGLR.PC1_SKtime, selectaov=1:7)
CGtrait_sigaov_func_Fr(frGLR.PC2_SKtime, selectaov=1:7)
CGtrait_sigaov_func_Fr(frGLR.PC3_SKtime, selectaov=1:7)
CGtrait_sigaov_func_Fr(frGLR.bio4_SKtime, selectaov=1:7)
CGtrait_sigaov_func_Fr(frGLR.bio7_SKtime, selectaov=1:7)
CGtrait_sigaov_func_Fr(frGLR.bio19_SKtime, selectaov=1:7)
CGtrait_sigaov_func_Fr(frGLR.lat_SKtime, selectaov=1:7)
CGtrait_sigaov_func_Fr(frGLR.trt_SKtime, selectaov=1:7)
