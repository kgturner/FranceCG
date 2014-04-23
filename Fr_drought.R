#France drought, for traits with sig trt effect

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

#for drought only include:
subset(frend, Trt=="drought")

#formatting
frdr <- subset(frend, Trt=="drought")


#pop control means to look at stress treatments
se <- function(x) sqrt(var(x)/length(x))
ctrlmeans<- ddply(subset(frend, Trt=="control"), .(Pop, Origin, Latitude), summarize, 
                  CtrlPopCount=length(Pop), CtrlPopMass.log=mean(Mass.log), CtrlPopMass.logSE=se(Mass.log))
ctrlmeans
