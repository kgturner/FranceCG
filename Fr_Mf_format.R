#France/Maternal effects comparison
#for SK material

#REML, using lme4
#mixed effect models 
library(plyr)
library(lme4)
library(lsmeans)
library(ggplot2)


#with SK
Mfcont<- read.table("MF_full_control.txt", header=T, sep="\t",quote='"', row.names=1)
Mfdr <- read.table("MF_full_drought.txt", header=T, sep="\t",quote='"', row.names=1)
Mfmom <- read.table("MF_mom.dk.txt", header=T, sep="\t",quote='"', row.names=1)

# #for long data formating, see FrSKdata_format.R
# #read
# Frdatsk.l<- read.table("FrTraitClimDat_SK_long.txt", header=T, sep="\t",quote='"', row.names=1)
# #read
# FrdatSK<- read.table("FrTraitClimDat_SK.txt", header=T, sep="\t",quote='"', row.names=1)
# #read
# frend<- read.table("FrEnd.txt", header=T, sep="\t",quote='"', row.names=1)

# #for DK only include:
# subset(frend, Origin%in%c("inv", "nat"))
# 
# #for drought only include:
# subset(frend, Trt=="drought")

summary(Mfcont)
count(Mfcont, vars="PopID")
#    PopID freq
# 1  BG001   34
# 2    BG3    7
# 3  CA001  182
# 4  GR002    9
# 5    HUG    1
# 6  RU008    6
# 7   SAND    7
# 8   SERG   26
# 9  TR001   10
# 10 US001   24
# 11 US002   36
# 12 US003  100

summary(Mfdr)
count(Mfdr, vars="PopID")
#   PopID freq
# 1 BG001    3
# 2 CA001    3
# 3 GR002    3
# 4 RU008    3
# 5  SAND    3
# 6 TR001    3
# 7 US001    3
# 8 US002    3
# 9 US003    3

Mf <- merge(Mfcont, Mfdr, all=TRUE)

summary(Mf)
head(Mf)
Mf <- Mf[,c(1:12, 18:21,31,33:34,36:38,41:42,44:48,66:68)]
str(Mf)
summary(subset(Mf, FlrHeadCount>0)) #86 indiv, evenly distributed among origins
#remove small pops, only one, HUG
count(Mf, vars=c("Origin","PopID","Trt"))
#    Origin PopID     Trt freq
# 1     inv CA001 control  179
# 2     inv CA001     dna    3
# 3     inv CA001 drought    3
# 4     inv US001 control   24
# 5     inv US001 drought    3
# 6     inv US002 control   36
# 7     inv US002 drought    3
# 8     inv US003 control  100
# 9     inv US003 drought    3
# 10    nat BG001 control   31
# 11    nat BG001     dna    3
# 12    nat BG001 drought    3
# 13    nat GR002 control    9
# 14    nat GR002 drought    3
# 15    nat RU008 control    6
# 16    nat RU008 drought    3
# 17    nat TR001 control   10
# 18    nat TR001 drought    3
# 19     SK   BG3 control    7
# 20     SK   HUG control    1
# 21     SK  SAND control    7
# 22     SK  SAND drought    3
# 23     SK  SERG control   23
# 24     SK  SERG     dna    3
Mf <- subset(Mf, PopID!="HUG")
#reset trt to control or drought
Mf[Mf$Trt!="drought",]$Trt <- "control"
Mf <- droplevels(Mf)

#dates
#leave all dates as int
day0 <- as.Date("2010-07-27") #stress start date

# FlwrDate 9/3/2010
Mf$FlwrDate2 <- strptime(Mf$FlwrDate, format="%m/%d/%Y")
Mf$FlwrDate2 <- as.Date(Mf$FlwrDate2)
Mf$FlwrDate3 <- as.numeric(Mf$FlwrDate2-day0)
str(Mf)
summary(Mf$FlwrDate2)
summary(Mf$FlwrDate3)
Mf$FlwrDate <- as.integer(Mf$FlwrDate3)
Mf <- Mf[,1:32]

# SLAdate 10/13/2010
Mf$SLAdate2 <- strptime(Mf$SLAdate, format="%m/%d/%Y")
Mf$SLAdate2 <- as.Date(Mf$SLAdate2)
Mf$SLAdate3 <- as.numeric(Mf$SLAdate2-day0)
str(Mf)
summary(Mf$SLAdate2)
summary(Mf$SLAdate3)
Mf$SLAdate <- as.integer(Mf$SLAdate3)
Mf <- Mf[,1:32]

Mf$m.date1 <- as.numeric(as.Date(strptime("07/27/2010", format="%m/%d/%Y"))-day0)
Mf$m.date2 <- as.numeric(as.Date(strptime("08/18/2010", format="%m/%d/%Y"))-day0)
Mf$m.dateH <- as.numeric(as.Date(strptime("10/13/2010", format="%m/%d/%Y"))-day0)

#mom data
Mf$CrossID <- paste0(Mf$PopID,"-",Mf$CrossNum)
Mfmom$CrossID <- as.character(Mfmom$CrossID)
Mf2 <- merge(Mf, Mfmom[,c(1:3,7:13)], by="CrossID", all.x=TRUE)
setdiff(Mf2$Origin.x, Mf2$Origin.y)
Mf2 <- Mf2[,c(1:36,39:44)]
colnames(Mf2)[5] <- "Origin"
colnames(Mf2)[8] <- "Pop"
colnames(Mf2)[9] <- "CrossNum"
summary(Mf2$Origin)
subset(Mf2, Origin%in%"SK")
Mf2$Mom <- as.character(Mf2$Mom)
Mf2[Mf2$Origin%in%"SK",]$Mom <- Mf2[Mf2$Origin%in%"SK",]$CrossNum
setdiff(Mf2[Mf2$Origin%in%"SK",]$Mom, Mf2[Mf2$Origin%in%"SK",]$CrossNum)
Mf2[Mf2$Origin%in%"SK",]$MomFam <- Mf2[Mf2$Origin%in%"SK",]$CrossNum
Mf2$Origin <- as.character(Mf2$Origin)
Mf2[Mf2$Origin%in%"SK",]$Origin <- "sk"
summary(Mf2)
Mf <- Mf2

#factors
Mf$CrossID <- as.factor(Mf$CrossID)
Mf$Origin <- as.factor(Mf$Origin)
Mf$Mom <- as.factor(Mf$Mom)
Mf$MomFam <- as.factor(Mf$MomFam)

Mf$bolt.bin <- as.numeric(Mf$BoltedatH)-1

Mf$sla <- Mf$SLAarea / Mf$SLAmass.g
Mf$sla.log <- log(Mf$sla)

Mf$Mass.log <- log(Mf$ShootMass.g)
Mf$Crown.log <- log(Mf$CrownDiam.mm)

#add climate datas
Mfclim <- read.table("MfbioclimPCAdat.txt", header=TRUE)
Mf <- merge(Mf, Mfclim[,c(1,2,22:25)],all.x=TRUE)

#write
write.table(Mf, file="Fr_Mf_data.txt",sep="\t", quote=F)
#read
Mf <- read.table("Fr_Mf_data.txt", header=T, sep="\t",quote='"', row.names=1)


####long data format####
Mf.l <- reshape(Mf, idvar="Barcode", direction="long", 
                     varying=list(m.date=c(34,35,36), lfl=c(12,22,15), lfw=c(13,23,16), lfc=c(11,21,14)),
                     v.names=c("m.date","lfl", "lfw","lfc"))
str(Mf.l)
Mf.l <- Mf.l[,c(1:11,20:21,25:31,34:44)]

#write
write.table(Mf.l, file="Fr_Mf_data_long.txt",sep="\t", quote=F)
#read
Mf.l <- read.table("Fr_Mf_data_long.txt", header=T, sep="\t",quote='"', row.names=1)
