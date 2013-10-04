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
  
  model1<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+ (1|Pop/Mom), family=family,data=modeldata)
  model2<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+ (1|Pop), family=family,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
  model3<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+ (1|blank), family=family,data=modeldata) # Test population effect
  a1 <- anova(model2,model1) # mom is sig!
  a2 <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
  
  
  modelcov <- lmer(modeldata[[trait]]  ~ Origin + (1|Pop/Mom), family=family,data=modeldata)
  a3 <- anova(model1, modelcov)
  
  modelO<-lmer(modeldata[[trait]] ~ (1|Pop/Mom), family=family,data=modeldata)
  a4 <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
  
  aovs <- list(a1,a2,a3,a4)
  names(aovs) <- c(paste(trait,"a1"), paste(trait,"a2"),paste(trait,"a3"),paste(trait, "a4"))
  models <- list(model1,model2,model3,modelcov,modelO)
  names(models) <- c("model1","model2","model3","modelcov","modelO")
  
#   print(aovs)
  return(aovs)
}

#return models
CGtrait.models_snglcov <- function(trait,df,covariate,family=gaussian){
  
  model1<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+ (1|Pop/Mom), family=family,data=modeldata)
  model2<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+ (1|Pop), family=family,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
  model3<-lmer(modeldata[[trait]]  ~ Origin +modeldata[[covariate]]+ (1|blank), family=family,data=modeldata) # Test population effect
  a1 <- anova(model2,model1) # mom is sig!
  a2 <- anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
  
  
  modelcov <- lmer(modeldata[[trait]]  ~ Origin + (1|Pop/Mom), family=family,data=modeldata)
  a3 <- anova(model1, modelcov)
  
  modelO<-lmer(modeldata[[trait]] ~ (1|Pop/Mom), family=family,data=modeldata)
  a4 <- anova(modelO,modelcov) #test for significance of origin - origin only marginally sig....!
  
  aovs <- list(a1,a2,a3,a4)
  names(aovs) <- c(paste(trait,"a1"), paste(trait,"a2"),paste(trait,"a3"),paste(trait, "a4"))
  models <- list(model1,model2,model3,modelcov,modelO)
  names(models) <- c("model1","model2","model3","modelcov","modelO")
  
  return(models)
}


####which anovas have sig results?####
#one anova
temp <- frGLR.PC1[[1]]$"MaxLfLgth1 a1"
str(temp)

temp$"Pr(>Chisq)"[2]
#
temp <- frGLR.PC1[[1]][[1]]
str(temp)
temp$"Pr(>Chisq)"[2]

temp <- frGLR.PC1[[1]]
str(temp)
temp[[1]]$"Pr(>Chisq)"[2]
names(temp)


CGtrait_sigaov_func_Fr <- function(list, selectaov="3:4"){
  for(i in 1:length(list)){
    temp <- list[[i]]
    for(j in 3:4){
      aov <- temp[[j]]
      if(aov$"Pr(>Chisq)"[2]<0.1){
        print(names(temp)[j])
      }
      
    }
  }
}

CGtrait_sigaov_func_Fr(frGLR.PC1)

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
