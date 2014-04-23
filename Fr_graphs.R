#Fr data
#graphs for the looking

#see FrSkdata_format.R for data formatting, and Fr_lmer.R for modeling descriptions

#read
FrdatSK<- read.table("FrTraitClimDat_SK.txt", header=T, sep="\t",quote='"', row.names=1)
Frdatsk.l<- read.table("FrTraitClimDat_SK_long.txt", header=T, sep="\t",quote='"', row.names=1)

library(lme4)
library(lsmeans)
library(ggplot2)
library(plyr)

str(FrdatSK)
FrdatSK$Mom <- as.factor(FrdatSK$Mom)

#PLOT ALL THE THINGS!
ggplot(data=Frdatsk.l[Frdatsk.l$lfc<200,], aes(y=lfc, x=Trt, color=Origin))+geom_boxplot()
ggplot(data=Frdatsk.l[Frdatsk.l$lfc<200,], aes(y=lfc, x=Origin))+geom_boxplot()


# list <- names(FrdatSK[c(8:26,36:55)])
# plot.ts(FrdatSK[,c(8:26,36:55)]) #break up in to 10s?



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

dfplot2 <- function(data.frame){
  df <- data.frame
  ln <- length(names(data.frame))
  for(i in 1:ln){
    mname <- substitute(df[,i])
    if(is.factor(df[,i])){
      plot(df[,i],main=names(df)[i])}
    else{plot(df$Origin, df[,i],main=names(df)[i])}
  }
}

par(ask=TRUE)

dfplot2(FrdatSK[,c(1:7,49,52)])
dfplot2(Frdatsk.l[,c(1:7,28,30,31)])
dfplot2(frend[,c(1:7,8,10,15,21,24)])


par(ask=F)

#DK only
subset(FrdatSK, Origin%in%c("inv", "nat"))
