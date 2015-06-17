#France mixed model power analysis???
#1/14/15

#using simulations functions in lme4 (not lme4.0)
#http://rpubs.com/bbolker/11703

#read
Frdatcline<- read.table("FrTraitClimDat_cline.txt")

#read
Frdatcline.l<- read.table("FrTraitClimDat_cline_long.txt", header=T, sep="\t",quote='"', row.names=1)

####power analysis using mass####
modeldata <- frendcline
modeldata<-modeldata[!is.na(modeldata$Mass.log),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
summary(modeldata$Origin)
summary(modeldata$Pop)

detach(package:lme4.0)
library("lmerTest")
library("lme4")
modelT <- lmer(Mass.log  ~ Origin * PC1 + (1|Pop/Mom), data=modeldata)
summary(modelT)
params <- list(
  beta = fixef(modelT),
  theta = getME(modelT, "theta"),
  sigma = getME(modelT, "sigma"))

ss <- simulate(~ Origin * PC1 + (1|Pop/Mom), nsim = 20, family=gaussian,
               weights = rep(25, nrow(modeldata)), newdata = modeldata, newparams = params) #, 
modeldata$resp <- ss[, 1]
fit1 <- lmer(resp ~ Origin * PC1 + (1|Pop/Mom),  weights = rep(25, nrow(modeldata)), data = modeldata)
fit1B <- refit(fit1, ss[[2]])
fitsim <- function(i) {
  coef(summary(refit(fit1, ss[[i]])))["PC1", ]
}
t1 <- system.time(fitAll <- laply(seq(20), function(i) fitsim(i)))
## you can use .progress='text' to get a progress indicator ...
#breaks here! output from refit not the same as in the example
fitAll <- setNames(as.data.frame(fitAll), c("est", "stderr", "zval", "pval"))
with(fitAll, mean(pval < 0.05))
ggplot(fitAll, aes(x = est)) + geom_histogram() + geom_vline(xintercept = -0.2, colour = "red")

####with bolt date####
#with poisson trait
modeldata <- droplevels(frendcline)
modeldata<-modeldata[!is.na(modeldata$Bolt.date),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
modelT <- glmer(Bolt.date  ~ Origin * PC1 +(1|Pop), family=poisson,data=modeldata)

params <- list(
  beta = fixef(modelT),
  theta = getME(modelT, "theta"))

ss <- simulate(~ Origin * PC1 + (1|Pop), nsim = 20, family=poisson,
               weights = rep(25, nrow(modeldata)), newdata = modeldata, newparams = params) #, 
modeldata$resp <- ss[, 1]
fit1 <- lmer(resp ~ Origin * PC1 + (1|Pop),  weights = rep(25, nrow(modeldata)), data = modeldata)
fit1B <- refit(fit1, ss[[2]])
fitsim <- function(i) {
  coef(summary(refit(fit1, ss[[i]])))["PC1", ]
}
t1 <- system.time(fitAll <- laply(seq(20), function(i) fitsim(i)))
## you can use .progress='text' to get a progress indicator ...
fitAll <- setNames(as.data.frame(fitAll), c("est", "stderr", "tval"))
with(fitAll, mean(pval < 0.05))
ggplot(fitAll, aes(x = est)) + geom_histogram() + geom_vline(xintercept = -0.2, colour = "red")
# 
####pamm####
# library(pamm)
# #number of observations=(“number of groups”*”number of replicates”) all pops, or just inv pops?
# pwrtest <- EAMM(numsim=2, group=6, repl=15, fixed=c(3.93248,0.048734,0.5), intercept=3.98576)
# 
####pwr####
# library(pwr)
# # pwr.f2.test(u =, v = , f2 = , sig.level = , power = ) #where u and v are the numerator and denominator degrees of freedom. We use f2 as the effect size measure. Cohen suggests f2 values of 0.02, 0.15, and 0.35 represent small, medium, and large effect sizes. 
# # pwr.chisq.test(w =, N = , df = , sig.level =, power = ) 
# # pwr.t2n.test(n1 = , n2= , d = , sig.level =, power = )#where n1 and n2 are unequal sample sizes.d is the effect size
# 
# pwr.t2n.test(n1 = 93, n2=124 , d = 3.98 , sig.level =0.05)
# 
# 
# #if I was working w/ just population means???
# pwr.f2.test( u =, v = ,f2 =0.15 , sig.level =0.05) 
# 
# library(longpower) #for repeated measures
# # fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy) 
# # lmmpower(fm1, pct.change = 0.30, t = seq(0,9,1), power = 0.80)


####repeated measure power analysis using lfw####
library(longpower) #for repeated measures
# fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy) 
# lmmpower(fm1, pct.change = 0.30, t = seq(0,9,1), power = 0.80)
# liu.liang.linear.power()
modeldata<-Frdatcline.l
modeldata<-modeldata[!is.na(modeldata$lfw),]
modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
modeldata$Mom<-as.factor(modeldata$Mom)
detach(package:lme4.0)
# library("lmerTest")
library("lme4")
modelT <- lmer(lfw  ~ Origin * PC1 + (m.date|tagged), data=modeldata)
summary(modelT)


lmmpower(modelT, power=.8,  pct.change = .3,sig.level=0.05, parameter="PC1")