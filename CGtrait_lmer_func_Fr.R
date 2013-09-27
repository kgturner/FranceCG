####Common Garden trait analysis####FRANCE

#using lmer, REML mixed models#
library(lme4)
#with Origin and trt (others????) as fixed effects, population and mom as random effects#
#custom functions

# #####function######
#Origin *Trt + single covariate (such as PC1 +bio4 +bio19)##
CGtrait.LR_snglcov<- function(trait,df,covariate,family=gaussian){
  modeldata<-df[!is.na(df[[trait]]),]
  modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
  modeldata$Mom<-as.factor(modeldata$Mom)
  #browser()
  
  model1<-lmer(modeldata[[trait]]  ~ Origin * Trt +modeldata[[covariate]]+ (1|Pop/Mom), family=family,data=modeldata)
  model2<-lmer(modeldata[[trait]]  ~ Origin * Trt +modeldata[[covariate]]+ (1|Pop), family=family,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
  model3<-lmer(modeldata[[trait]]  ~ Origin * Trt +modeldata[[covariate]]+ (1|blank), family=family,data=modeldata) # Test population effect
  a1 <- anova(model2,model1) # mom is sig!
  a2 <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
  
  modelI <- lmer(modeldata[[trait]]  ~ Origin + Trt+modeldata[[covariate]] + (1|Pop/Mom), family=family,data=modeldata)
  a3 <- anova(modelI,model1)
  
  modelcov <- lmer(modeldata[[trait]]  ~ Origin + Trt + (1|Pop/Mom), family=family,data=modeldata)
  a4 <- anova(modelI, modelcov)
  
  modelL<-lmer(modeldata[[trait]]  ~ Origin + (1|Pop/Mom), family=family,data=modeldata)
  a5 <- anova(modelL, modelcov)
  
  modelO<-lmer(modeldata[[trait]] ~ Trt +(1|Pop/Mom), family=family,data=modeldata)
  a6 <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
  
  aovs <- list(a1,a2,a3,a4,a5,a6)
  names(aovs) <- c(paste(trait,"a1"), paste(trait,"a2"),paste(trait,"a3"),paste(trait, "a4"),paste(trait, "a5"),paste(trait, "a6"))
  models <- list(model1,model2,model3,modelI,modelcov,modelL,modelO)
  names(models) <- c("model1","model2","model3","modelI","modelcov","modelL","modelO")
  
#   print(aovs)
  return(aovs)
}

#return models
CGtrait.models_snglcov <- function(trait,df,covariate,family=gaussian){
  
  modeldata<-df[!is.na(df[[trait]]),]
  modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
  modeldata$Mom<-as.factor(modeldata$Mom)
  #browser()
  
  model1<-lmer(modeldata[[trait]]  ~ Origin * Trt +modeldata[[covariate]]+ (1|Pop/Mom), family=family,data=modeldata)
  model2<-lmer(modeldata[[trait]]  ~ Origin * Trt +modeldata[[covariate]]+ (1|Pop), family=family,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
  model3<-lmer(modeldata[[trait]]  ~ Origin * Trt +modeldata[[covariate]]+ (1|blank), family=family,data=modeldata) # Test population effect
  a1 <- anova(model2,model1) # mom is sig!
  a2 <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
  
  modelI <- lmer(modeldata[[trait]]  ~ Origin + Trt+modeldata[[covariate]] + (1|Pop/Mom), family=family,data=modeldata)
  a3 <- anova(modelI,model1)
  
  modelcov <- lmer(modeldata[[trait]]  ~ Origin + Trt + (1|Pop/Mom), family=family,data=modeldata)
  a4 <- anova(modelI, modelcov)
  
  modelL<-lmer(modeldata[[trait]]  ~ Origin + (1|Pop/Mom), family=family,data=modeldata)
  a5 <- anova(modelL, modelcov)
  
  modelO<-lmer(modeldata[[trait]] ~ Trt +(1|Pop/Mom), family=family,data=modeldata)
  a6 <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
  
  aovs <- list(a1,a2,a3,a4,a5,a6)
  names(aovs) <- c(paste(trait,"a1"), paste(trait,"a2"),paste(trait,"a3"),paste(trait, "a4"),paste(trait, "a5"),paste(trait, "a6"))
  models <- list(model1,model2,model3,modelI,modelcov,modelL,modelO)
  names(models) <- c("model1","model2","model3","modelI","modelcov","modelL","modelO")
  
  return(models)
}
# 
# #Origin (no latitude)#
# CGtrait.LR.O<- function(trait,df,family=gaussian){
#   modeldata<-df[!is.na(df[[trait]]),]
#   modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
#   modeldata$Mom<-as.factor(modeldata$Mom)
#   #browser()
#   
#   model1<-lmer(modeldata[[trait]]  ~ Origin +(1|Pop/Mom), family=family,data=modeldata)
#   model2<-lmer(modeldata[[trait]]  ~ Origin + (1|Pop), family=family,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
#   model3<-lmer(modeldata[[trait]]  ~ Origin + (1|blank), family=family,data=modeldata) # Test population effect
#   a1 <- anova(model2,model1) # mom is sig!
#   a2 <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
#   
#   modelO<-lmer(modeldata[[trait]] ~ (1|Pop), family=family,data=modeldata)
#   a3 <- anova(modelO,model2) #test for significance of origin - origin only marginally sig....!
#   
#   aovs <- list(a1,a2,a3)
#   names(aovs) <- c(paste(trait,"a1"), paste(trait,"a2"),paste(trait,"a3"))
#   models <- list(model1,model2,model3,modelO)
#   names(models) <- c("model1","model2","model3","modelO")
#   
#   print(aovs)
#   return(aovs)
# }
# 
# #return models
# CGtrait.models.O <- function(trait, df,family=gaussian){
#   modeldata<-df[!is.na(df[[trait]]),]
#   modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
#   modeldata$Mom<-as.factor(modeldata$Mom)
#   #browser()
#   
#   model1<-lmer(modeldata[[trait]]  ~ Origin +(1|Pop/Mom), family=family,data=modeldata)
#   model2<-lmer(modeldata[[trait]]  ~ Origin + (1|Pop), family=family,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
#   model3<-lmer(modeldata[[trait]]  ~ Origin + (1|blank), family=family,data=modeldata) # Test population effect
#   a1 <- anova(model2,model1) # mom is sig!
#   a2 <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
#   
#   modelO<-lmer(modeldata[[trait]] ~ (1|Pop), family=family,data=modeldata)
#   a3 <- anova(modelO,model2) #test for significance of origin - origin only marginally sig....!
#   
#   aovs <- list(a1,a2,a3)
#   names(aovs) <- c(paste(trait,"a1"), paste(trait,"a2"),paste(trait,"a3"))
#   models <- list(model1,model2,model3,modelO)
#   names(models) <- c("model1","model2","model3","modelO")
#   
#   return(models)
# }
# 
# 
# 
# 
# ###########normality???#####
# #to get one model
# almodels[[1]][1] #first number is trait in column order of df, second number is model number
# names(almodels[1]) #to verify trait
# 
# #to check normality of residuals
# cuRoot.lmer <- cumodels$model2
# plot(resid(nmodels[2]) ~ fitted(nmodels[2]),main="residual plot")
# abline(h=0)
# 
# # checking the normality of residuals e_i:
# qqnorm(resid(nRootlog.lmer), main="Q-Q plot for residuals")
# qqline(resid(nRootlog.lmer))
