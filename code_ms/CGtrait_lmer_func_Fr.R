####Common Garden trait analysis####FRANCE

#using lmer, REML mixed models#
library(lme4)
#with Origin and trt (others????) as fixed effects, population and mom as random effects#
#custom functions

# #####function######
#Origin and 2 covariates with interaction
CGtrait.LR_2cov_int<- function(trait,df,covariate1, covariate2,family=gaussian){
  modeldata<-df[!is.na(df[[trait]]),]
  modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
  modeldata$Mom<-as.factor(modeldata$Mom)
  #browser()
  
  model1<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate1]]*modeldata[[covariate2]]+ (1|Pop/Mom), family=family,data=modeldata)
  model2<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate1]]*modeldata[[covariate2]]+ (1|Pop), family=family,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
  model3<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate1]]*modeldata[[covariate2]]+ (1|blank), family=family,data=modeldata) # Test population effect
  momAov <- anova(model2,model1) # mom is sig!
  popAov <- anova(model3,model2)# pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
  
  model4 <- lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate1]]+modeldata[[covariate2]]+ (1|Pop/Mom), family=family,data=modeldata)
  int2Aov <- anova(model4,model1)

  modelint<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate1]]+modeldata[[covariate2]]+ (1|Pop/Mom), family=family,data=modeldata)
  intAov <- anova(model4, modelint)
  
  model5 <- lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate1]]+ (1|Pop/Mom), family=family,data=modeldata)
  cov2Aov <- anova(modelint, model5)
  
  model6 <- lmer(modeldata[[trait]]  ~ Origin+modeldata[[covariate2]] + (1|Pop/Mom), family=family,data=modeldata)
  cov1Aov <- anova(modelint, model6)
#   
#   modelO<-lmer(modeldata[[trait]] ~ (1|Pop/Mom), family=family,data=modeldata)
#   originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
#   
  modelOC <- lmer(modeldata[[trait]]  ~ modeldata[[covariate1]]+modeldata[[covariate2]]+ (1|Pop/Mom), family=family,data=modeldata)
  ocAov <- anova(modelint, modelOC)
  
  aovs <- list(momAov, popAov, int2Aov, intAov, cov2Aov,cov1Aov,ocAov)
  names(aovs) <- c(paste(trait,"momAov"), paste(trait,"popAov"),paste(trait, "int2Aov"),paste(trait, "intAov"),
                   paste(trait,"cov2Aov"),paste(trait,"cov1Aov"), paste(trait, "ocAov"))
  models <- list(model1,model2,model3,model4, modelint,model5,model6, modelOC)
  names(models) <- c("model1","model2","model3","model4","modelint","model5","model6", "modelOC")
  
  #   print(aovs)
  return(aovs)
}
CGtrait.LR_2cov_int(FrdatSK$RoseAh.log,FrdatSK,Latitude,family=gaussian)


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
      if(aov$"Pr(>Chisq)"[2]<cutoff){
        print(names(temp)[j])
      }
      
    }
  }
}

CGtrait_sigaov_func_Fr(frGLR.PC1)

#####sngl cov with interaction###
#Origin *single covariate (such as PC1, bios...)##
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
CGtrait.LR_snglcov_int(FrdatSK$RoseAh.log,FrdatSK,Latitude,family=gaussian)

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

#cov with interaction plus treatment
#Origin *single covariate(such as PC1, bios...) + Trt##
CGtrait.LR_snglcov_trt<- function(trait,df,covariate,family=gaussian){
  modeldata<-df[!is.na(df[[trait]]),]
  modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
  modeldata$Mom<-as.factor(modeldata$Mom)
#   browser()
  
  model1<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+ Trt+ (1|Pop/Mom), family=family,data=modeldata)
  model2<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+ Trt+(1|Pop), family=family,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
  model3<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+ Trt+(1|blank), family=family,data=modeldata) # Test population effect
  momAov <- anova(model2,model1) # mom is sig!
  popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
  
  modelint<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+ Trt+(1|Pop/Mom), family=family,data=modeldata)
  intAov <- anova(model1, modelint)
  
  modelcov <- lmer(modeldata[[trait]]  ~ Origin + Trt+(1|Pop/Mom), family=family,data=modeldata)
  covAov <- anova(modelint, modelcov)
  
  modelO<-lmer(modeldata[[trait]] ~ Trt+(1|Pop/Mom), family=family,data=modeldata)
  originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
  
  modelOC <- lmer(modeldata[[trait]]  ~ modeldata[[covariate]]+ Trt+(1|Pop/Mom), family=family,data=modeldata)
  ocAov <- anova(modelint, modelOC)
  
  modelT<-lmer(modeldata[[trait]] ~ (1|Pop/Mom), family=family,data=modeldata)
  trtAov <- anova(modelO, modelT)
  
  aovs <- list(momAov, popAov, intAov, covAov,originAov,ocAov, trtAov)
  names(aovs) <- c(paste(trait,"momAov"), paste(trait,"popAov"),paste(trait, "intAov"),paste(trait,"covAov"),paste(trait, "originAov"), paste(trait, "ocAov"), paste(trait, "trtAov"))
  models <- list(model1,model2,model3,modelint,modelcov,modelO, modelOC, modelT)
  names(models) <- c("model1","model2","model3","modelint","modelcov","modelO", "modelOC","modelT")
  
  #   print(aovs)
  return(aovs)
}

CGtrait.LR_snglcov_trt(trait="RoseAh.log",df=FrdatSK,covariate="Latitude",family=gaussian)


#############time series###########
#2 cov with interaction
#Origin * PC1*PC2 + measure date##
CGtrait.LR_2cov_int_mdate<- function(trait,df,covariate1, covariate2,family=gaussian){
  modeldata<-df[!is.na(df[[trait]]),]
  modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
  modeldata$Mom<-as.factor(modeldata$Mom)
  #browser()
  
  model1<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate1]]*modeldata[[covariate2]]+ m.date+(1|Pop/Mom), family=family,data=modeldata)
  model2<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate1]]*modeldata[[covariate2]]+ m.date+(1|Pop), family=family,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
  model3<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate1]]*modeldata[[covariate2]]+ m.date+(1|blank), family=family,data=modeldata) # Test population effect
  momAov <- anova(model2,model1) # mom is sig!
  popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
  
  model4 <- lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate1]]+modeldata[[covariate2]]+ m.date+(1|Pop/Mom), family=family,data=modeldata)
  int2Aov <- anova(model4,model1)
  
  modelint<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate1]]+modeldata[[covariate2]]+ m.date+(1|Pop/Mom), family=family,data=modeldata)
  intAov <- anova(model4, modelint)
  
  model5 <- lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate1]]+m.date+ (1|Pop/Mom), family=family,data=modeldata)
  cov2Aov <- anova(modelint, model5)
  
  model6 <- lmer(modeldata[[trait]]  ~ Origin+modeldata[[covariate2]] + m.date+(1|Pop/Mom), family=family,data=modeldata)
  cov1Aov <- anova(modelint, model6)
  #   
  #   modelO<-lmer(modeldata[[trait]] ~ (1|Pop/Mom), family=family,data=modeldata)
  #   originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
  #   
  modelOC <- lmer(modeldata[[trait]]  ~ modeldata[[covariate1]]+modeldata[[covariate2]]+ m.date+(1|Pop/Mom), family=family,data=modeldata)
  ocAov <- anova(modelint, modelOC)
  
  modelmdate<-lmer(modeldata[[trait]] ~ modeldata[[covariate1]]+modeldata[[covariate2]]+(1|Pop/Mom), family=family,data=modeldata)
  mdateAov <- anova(modelmdate,modelO) #test for significance of origin - origin only marginally sig....!
  
  aovs <- list(momAov, popAov, int2Aov, intAov, cov2Aov,cov1Aov,ocAov,mdateAov)
  names(aovs) <- c(paste(trait,"momAov"), paste(trait,"popAov"),paste(trait, "int2Aov"),paste(trait, "intAov"),
                   paste(trait,"cov2Aov"),paste(trait,"cov1Aov"), paste(trait, "ocAov"),paste(trait, "mdateAov"))
  models <- list(model1,model2,model3,model4, modelint,model5,model6, modelOC, modelmdate)
  names(models) <- c("model1","model2","model3","model4","modelint","model5","model6", "modelOC","modelmdate")
  
  #   print(aovs)
  return(aovs)
}


#sngl cov with interaction
#Origin * single covariate (such as PC1 +bio4 +bio19) + measure date##
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

#cov with interaction, measure date and trt
#Origin * single covariate (such as PC1 +bio4 +bio19) + Trt + measure date##
CGtrait.LR_snglcov_trt_mdate<- function(trait,df,covariate,family=gaussian){
  modeldata<-df[!is.na(df[[trait]]),]
  modeldata$blank <- as.factor(rep("A",times=nrow(modeldata)))
  modeldata$Mom<-as.factor(modeldata$Mom)
  #browser()
  
  model1<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+ Trt+ m.date+(1|Pop/Mom), family=family,data=modeldata)
  model2<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+Trt+ m.date+(1|Pop), family=family,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
  model3<-lmer(modeldata[[trait]]  ~ Origin *modeldata[[covariate]]+Trt+ m.date+(1|blank), family=family,data=modeldata) # Test population effect
  momAov <- anova(model2,model1) # mom is sig!
  popAov <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
  
  modelint<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+Trt+ m.date+(1|Pop/Mom), family=family,data=modeldata)
  intAov <- anova(model1, modelint)
  
  modelcov <- lmer(modeldata[[trait]]  ~ Origin +Trt+ m.date+(1|Pop/Mom), family=family,data=modeldata)
  covAov <- anova(modelint, modelcov)
  
  modelO<-lmer(modeldata[[trait]] ~ Trt+m.date+(1|Pop/Mom), family=family,data=modeldata)
  originAov <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
  
  modelOC <- lmer(modeldata[[trait]]  ~ modeldata[[covariate]]+Trt+ m.date+(1|Pop/Mom), family=family,data=modeldata)
  ocAov <- anova(modelint, modelOC)
  
  modelT <- lmer(modeldata[[trait]]  ~m.date+(1|Pop/Mom), family=family,data=modeldata)
  trtAov <- anova(modelO, modelT)  
  
  modelmdate<-lmer(modeldata[[trait]] ~ (1|Pop/Mom), family=family,data=modeldata)
  mdateAov <- anova(modelmdate,modelT) #test for significance of origin - origin only marginally sig....!
  
  aovs <- list(momAov, popAov, intAov, covAov,originAov,ocAov, trtAov, mdateAov)
  names(aovs) <- c(paste(trait,"momAov"), paste(trait,"popAov"),paste(trait, "intAov"),paste(trait,"covAov"),paste(trait, "originAov"), paste(trait, "ocAov"), paste(trait, "trtAov"),paste(trait, "mdateAov"))
  models <- list(model1,model2,model3,modelint,modelcov,modelO, modelOC, modelT, modelmdate)
  names(models) <- c("model1","model2","model3","modelint","modelcov","modelO", "modelOC","modelT","modelmdate")
  
  #   print(aovs)
  return(aovs)
}

CGtrait.LR_snglcov_trt_mdate(trait="lfl",df=Frdatsk.l,covariate="Latitude",family=gaussian)





################################

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
