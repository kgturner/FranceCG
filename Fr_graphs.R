#Fr data
#graphs for the looking

#see FrSkdata_format.R for data formatting, and Fr_lmer.R for modeling descriptions

#read
FrdatSK<- read.table("FrTraitClimDat_SK.txt", header=T, sep="\t",quote='"', row.names=1)

library(lme4)
library(lsmeans)
library(ggplot2)
library(plyr)

str(FrdatSK)

#PLOT ALL THE THINGS!
qplot(data=FrdatSK, y=LfCount1, x=Trt, color=Origin)
list <- names(FrdatSK[c(8:26,36:55)])
# plot.ts(FrdatSK[,c(8:26,36:55)]) #break up in to 10s?

par(ask=TRUE)

dfplot <- function(data.frame){
    df <- data.frame
    ln <- length(names(data.frame))
    for(i in 1:ln){
      mname <- substitute(df[,i])
      if(is.factor(df[,i])){
        plot(df[,i],main=names(df)[i])}
      else{hist(df[,i],main=names(df)[i])}
    }
}
dfplot(FrdatSK)


