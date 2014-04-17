####Common Garden trait analysis####FRANCE

#using lmer, REML mixed models#
library(lme4)
#with Origin and trt (others????) as fixed effects, population and mom as random effects#
#custom functions

# #####function######
# #Origin *Trt + single covariate (such as PC1 +bio4 +bio19)##
# CGtrait.LR_snglcov<- function(trait,df,covariate,family=gaussian){
#   modeldata<-df[!is.na(df[[trait]]),]
#   modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
#   modeldata$Mom<-as.factor(modeldata$Mom)
#   #browser()
#   
#   model1<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+ (1|Pop/Mom), family=family,data=modeldata)
#   model2<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+ (1|Pop), family=family,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
#   model3<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+ (1|blank), family=family,data=modeldata) # Test population effect
#   momAov <- anova(model2,model1) # mom is sig!
#   popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
#   
#   
#   modelcov <- lmer(modeldata[[trait]]  ~ Origin + (1|Pop/Mom), family=family,data=modeldata)
#   covAov <- anova(model1, modelcov)
#   
#   modelO<-lmer(modeldata[[trait]] ~ (1|Pop/Mom), family=family,data=modeldata)
#   originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
#   
#   modelOC <- lmer(modeldata[[trait]]  ~ modeldata[[covariate]]+ (1|Pop/Mom), family=family,data=modeldata)
#   ocAov <- anova(model1, modelOC)
#   
#   aovs <- list(momAov, popAov, covAov,originAov,ocAov)
#   names(aovs) <- c(paste(trait,"momAov"), paste(trait,"popAov"),paste(trait,"covAov"),paste(trait, "originAov"), paste(trait, "ocAov"))
#   models <- list(model1,model2,model3,modelcov,modelO, modelOC)
#   names(models) <- c("model1","model2","model3","modelcov","modelO", "modelOC")
#   
# #   print(aovs)
#   return(aovs)
# }
# 
# #return models
# CGtrait.models_snglcov <- function(trait,df,covariate,family=gaussian){
#   modeldata<-df[!is.na(df[[trait]]),]
#   modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
#   modeldata$Mom<-as.factor(modeldata$Mom)
#   
#   model1<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+ (1|Pop/Mom), family=family,data=modeldata)
#   model2<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+ (1|Pop), family=family,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
#   model3<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+ (1|blank), family=family,data=modeldata) # Test population effect
#   momAov <- anova(model2,model1) # mom is sig!
#   popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
#   
#   
#   modelcov <- lmer(modeldata[[trait]]  ~ Origin + (1|Pop/Mom), family=family,data=modeldata)
#   covAov <- anova(model1, modelcov)
#   
#   modelO<-lmer(modeldata[[trait]] ~ (1|Pop/Mom), family=family,data=modeldata)
#   originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
#   
#   modelOC <- lmer(modeldata[[trait]]  ~ modeldata[[covariate]]+ (1|Pop/Mom), family=family,data=modeldata)
#   ocAov <- anova(model1, modelOC)
#   
#   aovs <- list(momAov, popAov, covAov,originAov,ocAov)
#   names(aovs) <- c(paste(trait,"momAov"), paste(trait,"popAov"),paste(trait,"covAov"),paste(trait, "originAov"), paste(trait, "ocAov"))
#   models <- list(model1,model2,model3,modelcov,modelO, modelOC)
#   names(models) <- c("model1","model2","model3","modelcov","modelO", "modelOC")
#   
#   return(models)
# }


####which anovas have sig results?####
# #one anova
# temp <- frGLR.PC1[[1]]$"MaxLfLgth1 a1"
# str(temp)
# 
# temp$"Pr(>Chisq)"[2]
# #
# temp <- frGLR.PC1[[1]][[1]]
# str(temp)
# temp$"Pr(>Chisq)"[2]
# 
# temp <- frGLR.PC1[[1]]
# str(temp)
# temp[[1]]$"Pr(>Chisq)"[2]
# names(temp)


CGtrait_sigaov_func_Fr <- function(list, selectaov=3:5, cutoff=0.1){
  for(i in 1:length(list)){
    temp <- list[[i]]
    for(j in selectaov){
      aov <- temp[[j]]
      if(aov$"Pr(>Chisq)"[2]<cuttoff){
        print(names(temp)[j])
      }
      
    }
  }
}

CGtrait_sigaov_func_Fr(frGLR.PC1)

#sngl cov with interaction
#Origin *Trt + single covariate (such as PC1, bios...)##
CGtrait.LR_snglcov_int<- function(trait,df,covariate,family=gaussian){
  modeldata<-df[!is.na(df[[trait]]),]
  modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
  modeldata$Mom<-as.factor(modeldata$Mom)
  #browser()
  
  model1<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+ (1|Pop/Mom), family=family,data=modeldata)
  model2<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+ (1|Pop), family=family,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
  model3<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+ (1|blank), family=family,data=modeldata) # Test population effect
  momAov <- anova(model2,model1) # mom is sig!
  popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
  
  modelint<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+ (1|Pop/Mom), family=family,data=modeldata)
  intAov <- anova(model1, modelint)
  
  modelcov <- lmer(modeldata[[trait]]  ~ Origin + (1|Pop/Mom), family=family,data=modeldata)
  covAov <- anova(modelint, modelcov)
  
  modelO<-lmer(modeldata[[trait]] ~ (1|Pop/Mom), family=family,data=modeldata)
  originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
  
  modelOC <- lmer(modeldata[[trait]]  ~ modeldata[[covariate]]+ (1|Pop/Mom), family=family,data=modeldata)
  ocAov <- anova(modelint, modelOC)
  
  aovs <- list(momAov, popAov, intAov, covAov,originAov,ocAov)
  names(aovs) <- c(paste(trait,"momAov"), paste(trait,"popAov"),paste(trait, "intAov"),paste(trait,"covAov"),paste(trait, "originAov"), paste(trait, "ocAov"))
  models <- list(model1,model2,model3,modelint,modelcov,modelO, modelOC)
  names(models) <- c("model1","model2","model3","modelint","modelcov","modelO", "modelOC")
  
  #   print(aovs)
  return(aovs)
}


#return models
CGtrait.models_snglcov_int <- function(trait,df,covariate,family=gaussian){
  modeldata<-df[!is.na(df[[trait]]),]
  modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
  modeldata$Mom<-as.factor(modeldata$Mom)
  
  model1<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+ (1|Pop/Mom), family=family,data=modeldata)
  model2<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+ (1|Pop), family=family,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
  model3<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+ (1|blank), family=family,data=modeldata) # Test population effect
  momAov <- anova(model2,model1) # mom is sig!
  popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
  
  modelint<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+ (1|Pop/Mom), family=family,data=modeldata)
  intAov <- anova(model1, modelint)
  
  modelcov <- lmer(modeldata[[trait]]  ~ Origin + (1|Pop/Mom), family=family,data=modeldata)
  covAov <- anova(modelint, modelcov)
  
  modelO<-lmer(modeldata[[trait]] ~ (1|Pop/Mom), family=family,data=modeldata)
  originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
  
  modelOC <- lmer(modeldata[[trait]]  ~ modeldata[[covariate]]+ (1|Pop/Mom), family=family,data=modeldata)
  ocAov <- anova(modelint, modelOC)
  
  aovs <- list(momAov, popAov, intAov, covAov,originAov,ocAov)
  names(aovs) <- c(paste(trait,"momAov"), paste(trait,"popAov"),paste(trait, "intAov"),paste(trait,"covAov"),paste(trait, "originAov"), paste(trait, "ocAov"))
  models <- list(model1,model2,model3,modelint,modelcov,modelO, modelOC)
  names(models) <- c("model1","model2","model3","modelint","modelcov","modelO", "modelOC")
  
  
  return(models)
}


#############time series###########

#sngl cov with interaction
#Origin *Trt + single covariate (such as PC1 +bio4 +bio19)##
CGtrait.LR_snglcov_int_mdate<- function(trait,df,covariate,family=gaussian){
  modeldata<-df[!is.na(df[[trait]]),]
  modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
  modeldata$Mom<-as.factor(modeldata$Mom)
  #browser()
  
  model1<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+ m.date+(1|Pop/Mom), family=family,data=modeldata)
  model2<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+ m.date+(1|Pop), family=family,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
  model3<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+ m.date+(1|blank), family=family,data=modeldata) # Test population effect
  momAov <- anova(model2,model1) # mom is sig!
  popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
  
  modelint<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+ m.date+(1|Pop/Mom), family=family,data=modeldata)
  intAov <- anova(model1, modelint)
  
  modelcov <- lmer(modeldata[[trait]]  ~ Origin + m.date+(1|Pop/Mom), family=family,data=modeldata)
  covAov <- anova(modelint, modelcov)
  
  modelO<-lmer(modeldata[[trait]] ~ m.date+(1|Pop/Mom), family=family,data=modeldata)
  originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
  
  modelOC <- lmer(modeldata[[trait]]  ~ modeldata[[covariate]]+ m.date+(1|Pop/Mom), family=family,data=modeldata)
  ocAov <- anova(modelint, modelOC)
  
  modelmdate<-lmer(modeldata[[trait]] ~ (1|Pop/Mom), family=family,data=modeldata)
  mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
  
  aovs <- list(momAov, popAov, intAov, covAov,originAov,ocAov, mdateAov)
  names(aovs) <- c(paste(trait,"momAov"), paste(trait,"popAov"),paste(trait, "intAov"),paste(trait,"covAov"),paste(trait, "originAov"), paste(trait, "ocAov"), paste(trait, "mdateAov"))
  models <- list(model1,model2,model3,modelint,modelcov,modelO, modelOC, modelmdate)
  names(models) <- c("model1","model2","model3","modelint","modelcov","modelO", "modelOC","modelmdate")
  
  #   print(aovs)
  return(aovs)
}

#return models
CGtrait.models_snglcov_int_mdate <- function(trait,df,covariate,family=gaussian){
  modeldata<-df[!is.na(df[[trait]]),]
  modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
  modeldata$Mom<-as.factor(modeldata$Mom)
  
  model1<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+ m.date+(1|Pop/Mom), family=family,data=modeldata)
  model2<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+ m.date+(1|Pop), family=family,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
  model3<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+ m.date+(1|blank), family=family,data=modeldata) # Test population effect
  momAov <- anova(model2,model1) # mom is sig!
  popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
  
  modelint<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+ m.date+(1|Pop/Mom), family=family,data=modeldata)
  intAov <- anova(model1, modelint)
  
  modelcov <- lmer(modeldata[[trait]]  ~ Origin + m.date+(1|Pop/Mom), family=family,data=modeldata)
  covAov <- anova(modelint, modelcov)
  
  modelO<-lmer(modeldata[[trait]] ~ m.date+(1|Pop/Mom), family=family,data=modeldata)
  originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
  
  modelOC <- lmer(modeldata[[trait]]  ~ modeldata[[covariate]]+ m.date+(1|Pop/Mom), family=family,data=modeldata)
  ocAov <- anova(modelint, modelOC)
  
  modelmdate<-lmer(modeldata[[trait]] ~ (1|Pop/Mom), family=family,data=modeldata)
  mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
  
  aovs <- list(momAov, popAov, intAov, covAov,originAov,ocAov, mdateAov)
  names(aovs) <- c(paste(trait,"momAov"), paste(trait,"popAov"),paste(trait, "intAov"),paste(trait,"covAov"),paste(trait, "originAov"), paste(trait, "ocAov"), paste(trait, "mdateAov"))
  models <- list(model1,model2,model3,modelint,modelcov,modelO, modelOC, modelmdate)
  names(models) <- c("model1","model2","model3","modelint","modelcov","modelO", "modelOC","modelmdate")
  
  
  return(models)
}









################################
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
