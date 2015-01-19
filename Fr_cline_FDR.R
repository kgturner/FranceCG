#France - correct for mulitple comparisons?
#1/18/15

#when not to do it:
#http://www.biostathandbook.com/multiplecomparisons.html
# http://www.graphpad.com/support/faqid/1390/

#try out FDR, maybe it's no big deal
source("http://bioconductor.org/biocLite.R")
biocLite("qvalue")
library(qvalue)

#chisq val from sup mat LRT table
intX <- c(0.03, 0.39, 8.5,0.39,9.88,8.35,37.19,0.01,1.05,9.34,14.44,6.28,25.89)
intP <- 1-pchisq(intX,1)

intP <- as.numeric(as.vector(test.LRT$intLRT)) #vector of p values of LRTs of Origin * PC1
#from Kay's paper: FDR cuttoff %5 (i.e. fdr.level=0.05)
#otherwise, default from Storey and Tibshirani(2003)
#or maybe use pi0.method="bootstrap" from Storey, Taylor & Siegmund (2004)
intQ <- qvalue(p=intP, lambda=seq(0,0.90,0.05), pi0.method="smoother", fdr.level=0.05, robust=FALSE, gui=FALSE, 
               smooth.df=3, smooth.log.pi0=FALSE)
#ok, no change in int LRT significance
intQ.2 <- qvalue(p=intP, fdr.level=0.05)
#ok, no change in int LRT significance
intQ.3 <- qvalue(p=intP, fdr.level=0.05,pi0.method="bootstrap")

#all fixed effect LRTs?
allFEX <- c(0.03,0.39, 8.5,0.39,9.88,8.35,37.19,0.01,1.05,9.34,14.44,6.28,25.89,  #origin*PC1
          0.12, 2.13,0.82,1.45,0.82,5.23,0.06,0.66,0.02,0.07,1.71,21.42,0.46, #trt
          0.49,0,0,1.14,0.05, #PC1
          5.82,3.14,3.70,0.12,2.49) #origin
allFEP <- 1-pchisq(allFEX,1)
allFEQ <- qvalue(p=allFEP, lambda=seq(0,0.90,0.05), pi0.method="smoother", fdr.level=0.05, robust=FALSE, gui=FALSE, 
               smooth.df=3, smooth.log.pi0=FALSE)
#ok, trt in rose area and origin in lfc no longer significant
allFEQ.2 <- qvalue(p=allFEP, fdr.level=0.05)
# same
allFEQ.3 <- qvalue(p=allFEP, fdr.level=0.05,pi0.method="bootstrap")
isTRUE(all.equal(allFEQ.3$significant,allFEQ$siginificant))
# which(allFEQ.3$significant != allFEQ$siginificant)
#with bootstrap method, results the same as without correction

#all LRTs (except random effect of origin (remove), and repeated measure (cuz more df, and anyway, should always be in there/significant))
allX <- c(0.03,0.39, 8.5,0.39,9.88,8.35,37.19,0.01,1.05,9.34,14.44,6.28,25.89,  #origin*PC1
            0.12, 2.13,0.82,1.45,0.82,5.23,0.06,0.66,0.02,0.07,1.71,21.42,0.46, #trt
            0.49,0,0,1.14,0.05, #PC1
            5.82,3.14,3.70,0.12,2.49, #origin
            0.81, 7.03,1.02,4.58,14.89,24.23,0,1.30,1.3,4.84,14.82,4.76,4.45, #pop
          0,12.76,0.85,13.03,16.33,3.16,0,0,0,0,9.71,0,0) #mom 
allP <- 1-pchisq(allX,1)
allQ <- qvalue(p=allP, lambda=seq(0,0.90,0.05), pi0.method="smoother", fdr.level=0.05, robust=FALSE, gui=FALSE, 
                 smooth.df=3, smooth.log.pi0=FALSE)
isTRUE(all.equal(allFEQ$significant,allQ$significant[1:36]))
#ok, trt in rose area and origin in lfc no longer significant, neither is pop for rose diam, bolt date, wilt date, yellow date
allQ.2 <- qvalue(p=allP, fdr.level=0.05)
isTRUE(all.equal(allQ$significant,allQ.2$significant))
# same
allQ.3 <- qvalue(p=allP, fdr.level=0.05,pi0.method="bootstrap")
isTRUE(all.equal(allFEQ.3$significant,allQ.3$significant[1:36]))
isTRUE(all.equal(allQ$significant[37:62],allQ.3$significant[37:62]))
isTRUE(all.equal(allQ$significant,allQ.3$significant))
#with bootstrap method, results for FEs the same as without correction, but non sig for pop for rose diam, bolt date, wilt date, yellow date