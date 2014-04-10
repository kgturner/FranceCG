#Fr_death and SLA and mass and bolting
#format for mixed lmer
#March 2014
 
library(lme4)
library(lsmeans)
library(ggplot2)
library(plyr)

#data formating
#dates
frdeath<- read.table("Frdeath.txt", header=T, sep="\t",quote='"')

frdeath$Bolt.date2 <- strptime(frdeath$Bolt.date, format="%B %d, %Y")
frdeath$Bolt.date2 <- as.Date(frdeath$Bolt.date2)
day0 <- as.Date("2011-05-12")
frdeath$Bolt.date3 <- as.numeric(frdeath$Bolt.date2-day0)
frdeath$Bolt.date <- frdeath$Bolt.date3
frdeath <- frdeath[,1:13]

frdeath$Harvest.date2 <- strptime(frdeath$Harvest.date, format="%B %d, %Y")
frdeath$Harvest.date2 <- as.Date(frdeath$Harvest.date2)
day0 <- as.Date("2011-05-12")
frdeath$Harvest.date3 <- as.numeric(frdeath$Harvest.date2-day0)
frdeath$Harvest.date <- frdeath$Harvest.date3
frdeath <- frdeath[,1:13]

frdeath$Death.date2 <- strptime(frdeath$Death.date, format="%B %d, %Y")
frdeath$Death.date2 <- as.Date(frdeath$Death.date2)
day0 <- as.Date("2011-05-12")
frdeath$Death.date3 <- as.numeric(frdeath$Death.date2-day0)
frdeath$Death.date <- frdeath$Death.date3
frdeath <- frdeath[,1:13]

frdeath$Yellow2 <- strptime(frdeath$Yellow, format="%B %d, %Y")
frdeath$Yellow2 <- as.Date(frdeath$Yellow2)
day0 <- as.Date("2011-05-12")
frdeath$Yellow3 <- as.numeric(frdeath$Yellow2-day0)
frdeath$Yellow <- frdeath$Yellow3
frdeath <- frdeath[,1:13]

frdeath$TotWilt2 <- strptime(frdeath$TotWilt, format="%B %d, %Y")
frdeath$TotWilt2 <- as.Date(frdeath$TotWilt2)
day0 <- as.Date("2011-05-12")
frdeath$TotWilt3 <- as.numeric(frdeath$TotWilt2-day0)
frdeath$TotWilt <- frdeath$TotWilt3
frdeath <- frdeath[,1:13]

frdeath$Wilt2 <- strptime(frdeath$Wilt, format="%B %d, %Y")
frdeath$Wilt2 <- as.Date(frdeath$Wilt2)
day0 <- as.Date("2011-05-12")
frdeath$Wilt3 <- as.numeric(frdeath$Wilt2-day0)
frdeath$Wilt <- frdeath$Wilt3
frdeath <- frdeath[,1:13]

# #bolt.bin
# Frdatsk.l$bolt.bin <- as.numeric(Frdatsk.l$BoltedatH)-1
# head(Frdatsk.l[Frdatsk.l$Origin=="sk",])

#write
write.table(frdeath, file="Frdeath.txt",sep="\t", quote=F)
#read
frdeath<- read.table("Frdeath.txt", header=T, sep="\t",quote='"', row.names=1)

#add to Frdatsk
setdiff(FrdatSK$tagged, frdeath$tagged)

# frdeath$tagged <- as.character(frdeath$tagged)
# frdeath[343,3] <- "US022-3"
# frdeath$tagged <- as.factor(frdeath$tagged)

setdiff(frdeath$tagged, FrdatSK$tagged)
subset(frdeath, tagged%in%setdiff(frdeath$tagged, FrdatSK$tagged)&Trt==c("control", "drought")&Ending=="H")

###add sla, bolt.bin, mass
frend <- subset(frdeath, tagged!=""&Trt!="edge"&Trt!="")
frend <- droplevels(frend)

subset(frend, tagged%in%setdiff(frend$tagged, FrdatSK$tagged))

frdes <- read.table("Frdes.txt", header=T, sep="\t",quote='"', row.names=1)
frend <- merge(frdes[,1:6],frend, all.y=TRUE)
summary(frend)

frmass <- read.table("FrShootMassH.txt", header=T, sep="\t",quote='"', row.names=1)
head(frmass)
frend <- merge(frend, frmass, by.x="tagged",by.y="row.names", all.x=TRUE)

frSLA <- read.table("FrSLA.txt", header=T, sep="\t",quote='"')
frend <- merge(frend, frSLA, all.x=TRUE)
head(frend)

summary(frend)
frend[frend$BoltedatH=="no",]$BoltedatH <- "No"
frend[frend$BoltedatH=="",]$BoltedatH <- NA
frend$bolt.bin <- as.numeric(frend$BoltedatH)-1

frend$SLA <- frend$lfArea/frend$lfMass
frend$sla.log <- log(frend$SLA)

frend$Mass.log <- log(frend$Shoot.mass.gH)

frend <- subset(frend, !is.na(Origin))


#write
write.table(frend, file="FrEnd.txt",sep="\t", quote=F)
#read
frend<- read.table("FrEnd.txt", header=T, sep="\t",quote='"', row.names=1)