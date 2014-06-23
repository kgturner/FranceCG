###Getting means and CI from non-normally distributed mixed models (lmer)

####binomial####
# # coBatH <- CGtrait.LR.int("bolt.bin", mfco.dk1, family=binomial)
# # comodels <- CGtrait.models.int("bolt.bin", mfco.dk1, family=binomial)
# # comodels$model2
# # 
# # #to get effect size for binomial distribution
# # int<-9.5348 #inv mean
# # B<--39.8539 #Originnat estimate from model summary
# # pN<-exp(int+B)/(exp(int+B)+1) # Native
# # # Note that if Origin was a continuous variable you would substitute B with B*Origin
# # pI<-exp(int)/(exp(int)+1)# Introduced (B=0)
# # pI 
# # pN 
# # #report effect size as separate percentages, difference in percentage, or log-odds ratio: log(pI/pN)
# # #could also include the standard errors in equations to add upper/lower confidence intervals.
# # #check by looking at percentages
# # summary(mfco.dk1[mfco.dk1$Origin=="nat",]) #56 rows, 34 boltedatH = 60%
# # summary(mfco.dk1[mfco.dk1$Origin=="inv",]) #55 rows, 8 boltedatH = 14.5%
# # 
# # #confidence intervals
# # lower <- coef(summary(model1))[,1] + qnorm(.025)*coef(summary(model1))[,2]
# # upper <- coef(summary(model1))[,1] + qnorm(.975)*coef(summary(model1))[,2]
# # cbind(coef(summary(model1)), lower, upper)
# # #upper inv
# # int<-9.2014144 #inv mean
# # pI<-exp(int)/(exp(int)+1)# Introduced (B=0)
# # pI 
# # #lower inv
# # int<--60.5416037 #inv mean
# # pI<-exp(int)/(exp(int)+1)# Introduced (B=0)
# # pI 
# # #upper nat
# # int<-9.5348 #inv mean
# # B<--1.4109686 #Originnat estimate from model summary
# # pN<-exp(int+B)/(exp(int+B)+1) # Native
# # pN 
# # #lower nat
# # int<-9.5348 #inv mean
# # B<--78.2967929 #Originnat estimate from model summary
# # pN<-exp(int+B)/(exp(int+B)+1) # Native 
# # pN 
# # 
# # 
# # ci(model)
# # ci.binom(comodels$model2)
# # binmodel <- comodels$model2
# # ci.binom(binmodel)
# # 
# # 
# # coef(binmodel)
# # confint(binmodel, level=0.95)
# # 
# # #another way?
# # by(mfco.dk1, mfco.dk1$PopID, function(x) confint(glm(bolt.bin ~ Origin*Latitude, data=mfco.dk1, family=binomial))) 
# # #upper inv
# # int<-47.5884771 #inv mean
# # pI<-exp(int)/(exp(int)+1)# Introduced (B=0)
# # pI 
# # #lower inv
# # int<--18.7909674 #inv mean
# # pI<-exp(int)/(exp(int)+1)# Introduced (B=0)
# # pI 
# # 
# # 
# # #confint
# # c
# # # confint(lmList(binmodel))
# # # lmList(binmodel)
# # binmodel <- lmList(bolt.bin~ Origin * Latitude|PopID, family=binomial, data=mfco.dk1)
# # 
# # #bootMer
# 
####binomial lsmeans####
CI.LS.binomial <- function(model, conf=95){
  library(lsmeans)
  ls <- as.data.frame(lsmeans(model, ~ Origin, conf=conf))    
  
  #effect size, binomial
  intI<-ls[1,2]#inv mean
  BN<-ls[2,2]#Originnat estimate from model summary
  BS<-ls[3,2]#Originnat estimate from model summary
  
  pI<-exp(intI)/(exp(intI)+1)
  pN<-exp(BN)/(exp(BN)+1)
  pS <- exp(BS)/(exp(BS)+1)
  
  #inv upper from coeftbl confidence int.
  uI<-exp(ls[1,6])/(exp(ls[1,6])+1)
  #inv lower from coeftbl conf int
  lI<-exp(ls[1,5])/(exp(ls[1,5])+1)
  #nat upper from coeftbl conf int
  uN<-exp(ls[2,6])/(exp(ls[2,6])+1)
  #nat lower from coeftbl conf int
  lN<-exp(ls[2,5])/(exp(ls[2,5])+1)
  #nat upper from coeftbl conf int
  uS<-exp(ls[3,6])/(exp(ls[2,6])+1)
  #nat lower from coeftbl conf int
  lS<-exp(ls[3,5])/(exp(ls[2,5])+1)
  
#   print(paste("Native mean",pN))
#   print(paste("Native CI", lN, "-",uN))
#   print(paste("Invasive mean",pI))
#   print(paste("Invasive CI", lI, "-", uI))
  
df <- data.frame(Origin=c("Invasive","Native","SK"), mean=c(pI,pN,pS), 
                 uCL=c(uI,uN,uS),lCL=c(lI,lN,lS) )
  return(df)
}


# # ####Poisson#####
# CI.poisson <- function(model){
#   coeftbl <- as.data.frame(coef(summary(model)))
# #   coeftbl
#   ## 95% confidence intervals
#   CItbl <- with(coeftbl,+ Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))
#   #effect size, poisson
#   int<-coeftbl[1,1]#inv mean
#   B<-coeftbl[2,1]#Originnat estimate from model summary
#   pI<-exp(int)
#   pN<-exp(int+B)
#   #inv upper from coeftbl confidence int.
#   uI<-exp(CItbl[1,2])
#   #inv lower from coeftbl conf int
#   lI<-exp(CItbl[1,1])
#   #nat upper from coeftbl conf int
#   uN<-exp(int+CItbl[2,2])
#   #nat lower from coeftbl conf int
#   lN<-exp(int+CItbl[2,1])
# 
# #   print(paste("Native mean",pN))
# #   print(paste("Native CI", lN, "-",uN))
# #   print(paste("Invasive mean",pI))
# #   print(paste("Invasive CI", lI, "-", uI))
#   
#   df <- data.frame(Origin=c("Invasive","Native"), mean=c(pI,pN), 
#                    uCL=c(uI,uN),lCL=c(lI,lN) )
#   return(df)
# }
# 
# library(lsmeans)
#####
CI.LS.poisson <- function(model, conf=95){
  library(lsmeans)
  ls <- as.data.frame(lsmeans(model, ~ Origin, conf=conf))    
  #effect size, poisson
  intI<-ls[1,2]#inv mean
  BN<-ls[2,2]#Originnat estimate from model summary
  BS<-ls[3,2]#OriginSK estimate from model summary
  
  pI<-exp(intI)
  pN<-exp(BN)
  pS<-exp(BS)
  
  #inv upper from coeftbl confidence int.
  uI<-exp(ls[1,6])
  #inv lower from coeftbl conf int
  lI<-exp(ls[1,5])
  #nat upper from coeftbl conf int
  uN<-exp(ls[2,6])
  #nat lower from coeftbl conf int
  lN<-exp(ls[2,5])
  #Sk upper from coeftbl conf int
  uS<-exp(ls[3,6])
  #sk lower from coeftbl conf int
  lS<-exp(ls[3,5])
  
  #   print(paste("Native mean",pN))
  #   print(paste("Native CI", lN, "-",uN))
  #   print(paste("Invasive mean",pI))
  #   print(paste("Invasive CI", lI, "-", uI))
  
  
  df <- data.frame(Origin=c("Invasive","Native","SK"), mean=c(pI,pN,pS), 
                   uCL=c(uI,uN,uS),lCL=c(lI,lN,lS) )
  return(df)
}
# 
# ############Gaussian.log###########
# # coeftbl <- as.data.frame(coef(summary(modelI)))
# # coeftbl <- exp(coeftbl)
# # coeftbl$upper <- coeftbl$Estimate + 1.96*(coeftbl$"Std. Error") 
# # coeftbl$lower <- coeftbl$Estimate - 1.96*(coeftbl$"Std. Error") 
# 
CI.LS.gaussian.log<- function(model, conf=95){
  library(lsmeans)
  ls <- as.data.frame(lsmeans(model, ~ Origin, conf=conf))    
  #effect size, poisson
  intI<-ls[1,2]#inv mean
  BN<-ls[2,2]#Originnat estimate from model summary
  BS<-ls[3,2]#OriginSK estimate from model summary
  
  pI<-exp(intI)
  pN<-exp(BN)
  pS<-exp(BS)
  
  #inv upper from coeftbl confidence int.
  uI<-exp(ls[1,6])
  #inv lower from coeftbl conf int
  lI<-exp(ls[1,5])
  #nat upper from coeftbl conf int
  uN<-exp(ls[2,6])
  #nat lower from coeftbl conf int
  lN<-exp(ls[2,5])
  #Sk upper from coeftbl conf int
  uS<-exp(ls[3,6])
  #sk lower from coeftbl conf int
  lS<-exp(ls[3,5])
  
#   print(paste("Native mean",pN))
#   print(paste("Native CI", lN, "-",uN))
#   print(paste("Invasive mean",pI))
#   print(paste("Invasive CI", lI, "-", uI))
  

  df <- data.frame(Origin=c("Invasive","Native","SK"), mean=c(pI,pN,pS), 
                 uCL=c(uI,uN,uS),lCL=c(lI,lN,lS) )
  return(df)
}


#############Include mdate########
#means estimated at mean m.date
CI.LS.poisson.mdate <- function(model, conf=95){
  library(lsmeans)
  ls <- as.data.frame(lsmeans(model, ~ Origin+m.date, conf=conf))      
  #effect size, poisson
  intI<-ls[1,3]#inv mean
  BN<-ls[2,3]#Originnat estimate from model summary
  BS<-ls[3,3]#OriginSK estimate from model summary
  
  pI<-exp(intI)
  pN<-exp(BN)
  pS<-exp(BS)
  
  #inv upper from coeftbl confidence int.
  uI<-exp(ls[1,7])
  #inv lower from coeftbl conf int
  lI<-exp(ls[1,6])
  #nat upper from coeftbl conf int
  uN<-exp(ls[2,7])
  #nat lower from coeftbl conf int
  lN<-exp(ls[2,6])
  #Sk upper from coeftbl conf int
  uS<-exp(ls[3,7])
  #sk lower from coeftbl conf int
  lS<-exp(ls[3,6])
  
  
  df <- data.frame(Origin=c("Invasive","Native","SK"), mean=c(pI,pN,pS), 
                   uCL=c(uI,uN,uS),lCL=c(lI,lN,lS) )
#   print("Generation 0")
#   print(paste("Native mean",pN0))
#   print(paste("Native CI", lN0, "-",uN0))
#   print(paste("Invasive mean",pI0))
#   print(paste("Invasive CI", lI0, "-", uI0))
#   
#   print("Generation 1")
#   print(paste("Native mean",pN1))
#   print(paste("Native CI", lN1, "-",uN1))
#   print(paste("Invasive mean",pI1))
#   print(paste("Invasive CI", lI1, "-", uI1))
  
  return(df)
}

CI.LS.gaussian.sqrt.mdate <- function(model, conf=95){
  library(lsmeans)
  ls <- as.data.frame(lsmeans(model, ~ Origin+m.date, conf=conf))      
  #effect size, poisson
  intI<-ls[1,3]#inv mean
  BN<-ls[2,3]#Originnat estimate from model summary
  BS<-ls[3,3]#OriginSK estimate from model summary
  
  pI<-intI^2
  pN<-BN^2
  pS<-BS^2
  
  #inv upper from coeftbl confidence int.
  uI<-(ls[1,7])^2
  #inv lower from coeftbl conf int
  lI<-(ls[1,6])^2
  #nat upper from coeftbl conf int
  uN<-(ls[2,7])^2
  #nat lower from coeftbl conf int
  lN<-(ls[2,6])^2
  #Sk upper from coeftbl conf int
  uS<-(ls[3,7])^2
  #sk lower from coeftbl conf int
  lS<-(ls[3,6])^2
  
  
  df <- data.frame(Origin=c("Invasive","Native","SK"), mean=c(pI,pN,pS), 
                   uCL=c(uI,uN,uS),lCL=c(lI,lN,lS) )
  #   print("Generation 0")
  #   print(paste("Native mean",pN0))
  #   print(paste("Native CI", lN0, "-",uN0))
  #   print(paste("Invasive mean",pI0))
  #   print(paste("Invasive CI", lI0, "-", uI0))
  #   
  #   print("Generation 1")
  #   print(paste("Native mean",pN1))
  #   print(paste("Native CI", lN1, "-",uN1))
  #   print(paste("Invasive mean",pI1))
  #   print(paste("Invasive CI", lI1, "-", uI1))
  
  return(df)
}


# #############Include second term, binomial########
# # ##binomial lsmeans##
# # CI.LS.binomial <- function(model, conf=95){
# #   library(lsmeans)
# #   ls <- as.data.frame(lsmeans(model, ~ Origin, conf=conf))    
# #   #effect size, binomial
# #   int<-ls[1,2]#inv mean
# #   B<-ls[2,2]#Originnat estimate from model summary
# #   pI<-exp(int)/(exp(int)+1)
# #   pN<-exp(B)/(exp(B)+1)
# #   #inv upper from coeftbl confidence int.
# #   uI<-exp(ls[1,6])/(exp(ls[1,6])+1)
# #   #inv lower from coeftbl conf int
# #   lI<-exp(ls[1,5])/(exp(ls[1,5])+1)
# #   #nat upper from coeftbl conf int
# #   uN<-exp(ls[2,6])/(exp(ls[2,6])+1)
# #   #nat lower from coeftbl conf int
# #   lN<-exp(ls[2,5])/(exp(ls[2,5])+1)
# #
# 
# 
# CI.LS.binomial.2term <- function(model, conf=95){
#   library(lsmeans)
#   ls <- as.data.frame(lsmeans(model, ~ Origin+Generation, conf=conf))    
#   #effect size, binomial, gen 0
#   int0<-ls[1,3]#inv mean
#   B0<-ls[2,3]#Originnat estimate from model summary
#   pI0<-exp(int0)/(exp(int0)+1)
#   pN0<-exp(B0)/(exp(B0)+1)
#   #inv upper from coeftbl confidence int.
#   uI0<-exp(ls[1,7])/(exp(ls[1,7])+1)
#   #inv lower from coeftbl conf int
#   lI0<-exp(ls[1,6])/(exp(ls[1,6])+1)
#   #nat upper from coeftbl conf int
#   uN0<-exp(ls[2,7])/(exp(ls[2,7])+1)
#   #nat lower from coeftbl conf int
#   lN0<-exp(ls[2,6])/(exp(ls[2,6])+1)
#   
#   #effect size, binomial, gen 1
#   int1<-ls[3,3]#inv mean
#   B1<-ls[4,3]#Originnat estimate from model summary
#   pI1<-exp(int1)/(exp(int1)+1)
#   pN1<-exp(B1)/(exp(B1)+1)
#   #inv upper from coeftbl confidence int.
#   uI1<-exp(ls[3,7])/(exp(ls[3,7])+1)
#   #inv lower from coeftbl conf int
#   lI1<-exp(ls[3,6])/(exp(ls[3,6])+1)
#   #nat upper from coeftbl conf int
#   uN1<-exp(ls[4,7])/(exp(ls[4,7])+1)
#   #nat lower from coeftbl conf int
#   lN1<-exp(ls[4,6])/(exp(ls[4,6])+1)
#   
#   df <- data.frame(Origin=c("Invasive","Invasive","Native","Native"), Generation=c(0,1,0,1), mean=c(pI0,pI1,pN0,pN1), 
#                    uCL=c(uI0,uI1,uN0,uN1),lCL=c(lI0,lI1,lN0,lN1) )
#   
#   print("Generation 0")
#   print(paste("Native mean",pN0))
#   print(paste("Native CI", lN0, "-",uN0))
#   print(paste("Invasive mean",pI0))
#   print(paste("Invasive CI", lI0, "-", uI0))
#   
#   print("Generation 1")
#   print(paste("Native mean",pN1))
#   print(paste("Native CI", lN1, "-",uN1))
#   print(paste("Invasive mean",pI1))
#   print(paste("Invasive CI", lI1, "-", uI1))
#   
#   return(df)
# }

####gaussian.log, include trt####
####incomplete
# CI.LS.gaussian.log.2term<- function(model, conf=95){
#   library(lsmeans)
#   ls <- as.data.frame(lsmeans(model, ~ Origin+Trt, conf=conf))    
#   #effect size, trt=control
#   int<-ls[1,2]#inv mean
#   B<-ls[2,2]#Originnat estimate from model summary
#   pI<-exp(int)
#   pN<-exp(B)
#   #inv upper from coeftbl confidence int.
#   uI<-exp(ls[1,6])
#   #inv lower from coeftbl conf int
#   lI<-exp(ls[1,5])
#   #nat upper from coeftbl conf int
#   uN<-exp(ls[2,6])
#   #nat lower from coeftbl conf int
#   lN<-exp(ls[2,5])
#   
# 
#   
#   df <- data.frame(Origin=c("Invasive","Native"), mean=c(pI,pN), 
#                    uCL=c(uI,uN),lCL=c(lI,lN) )
#   return(df)
# }





#############O DF for lmer?#############
Data: modeldata
Models:
  model3: CrownH.log ~ Origin + (1 | blank)
model2: CrownH.log ~ Origin + (1 | PopID)
        Df     AIC     BIC logLik  Chisq Chi Df Pr(>Chisq)    
model3  4 -125.05 -110.90 66.523                             
model2  4 -162.13 -147.98 85.066 37.085      0  < 2.2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

dchisq(X,df) #gives appropriately adjusted p-val
#where X is the Chisq value from the anova table (37.085 in your case) and df is Df in the anova table +1 (0 + 1 in your case)


####fixed effects in lmer?####
summary(comodels$model2)
anova(comodels$model2)
anova(comodels$model2,ddf="Kenward-Roger")
anova(comodels$model2,ddf="lme4")
#don't use function for some reason......
anova(n,ddf="Kenward-Roger") 