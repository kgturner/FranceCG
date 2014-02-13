#Fr SK data reshape/error check
library(reshape2)

#############long format datas################
#for repeated measures

#dates and what not
frm1 <- read.table("FrMeasure1.txt", header=T, sep="\t",quote='"', row.names=1)
frm2 <- read.table("FrMeasure2.txt", header=T, sep="\t",quote='"', row.names=1)
frm2.1 <- read.table(file.choose(), header=F, sep=",",quote='"', row.names=1) #"measure 2 day 1.txt"
frm2.2 <- read.table(file.choose(), header=F, sep=",",quote='"', row.names=1) #"measure 2 day 2.txt"
day1 <- row.names(frm2.1)
day2 <- row.names(frm2.2)
FrdatSK$m2.date <- NA
FrdatSK[unique(FrdatSK$tagged %in% day1),]$m2.date <- "6/12/2011"
FrdatSK[unique(FrdatSK$tagged %in% day2),]$m2.date <- "6/13/2011"

#write

#all dates as int -- m1.date (factor), m2.date (chr)
# FrdatSK$m1.date2 <- strptime(FrdatSK$m1.date, format="%m/%d/%Y")
# FrdatSK$m1.date2 <- as.Date(FrdatSK$m1.date2)
day0 <- as.Date("2011-05-12") #planting date
# FrdatSK$m1.date3 <- as.numeric(FrdatSK$m1.date2-day0)
str(FrdatSK)
# summary(FrdatSK$m1.date2)
# summary(FrdatSK$m1.date3)
# FrdatSK$m1.date <- FrdatSK$m1.date3
# FrdatSK <- FrdatSK[,1:57]

FrdatSK$m2.date2 <- strptime(FrdatSK$m2.date, format="%m/%d/%Y")
FrdatSK$m2.date2 <- as.Date(FrdatSK$m2.date2)
day0 <- as.Date("2011-05-12") #planting date
FrdatSK$m2.date3 <- as.numeric(FrdatSK$m2.date2-day0)
str(FrdatSK)
summary(FrdatSK$m2.date2)
summary(FrdatSK$m2.date3)
FrdatSK$m2.date <- FrdatSK$m2.date3
FrdatSK <- FrdatSK[,1:57]

FrdatSK$m1.date <- as.integer(FrdatSK$m1.date)
FrdatSK$m2.date <- as.integer(FrdatSK$m2.date)

#ugh NAs for SK plants, because not in original data set. POOP
summary(FrdatSK$m1.date)
FrdatSK[is.na(FrdatSK$m1.date),]

frm1.1 <- read.table(file.choose(), header=F, sep=",",quote='"', row.names=1) #"measure 1 day 1.txt" fix typo in file
frm1.2 <- read.table(file.choose(), header=F, sep=",",quote='"', row.names=1) #"measure 1 day 2.txt"
frm1.3 <- read.table(file.choose(), header=F, sep=",",quote='"') #"measure 1 day 3.txt"
frm1.4 <- read.table(file.choose(), header=F, sep=",",quote='"', row.names=1) #"measure 1 oops.txt"

day1 <- row.names(frm1.1)
day2 <- row.names(frm1.2)
day3 <- as.character(unique(frm1.3$V1))
day4 <- row.names(frm1.4)

FrdatSK[unique(FrdatSK$tagged %in% day1),]$m1.date <- "12"
FrdatSK[unique(FrdatSK$tagged %in% day2),]$m1.date <- "13"
FrdatSK[unique(FrdatSK$tagged %in% day3),]$m1.date <- "14"
FrdatSK[unique(FrdatSK$tagged %in% day4),]$m1.date <- "17"
FrdatSK[is.na(FrdatSK$m1.date),]
FrdatSK$m1.date <- as.integer(FrdatSK$m1.date)

#need to change m1.date for
# "GR002-9","11","9.0","3.2",""
# "CA009-7","10","8.2","2.2",""
# "UA007-5","12","7.9","2.9",""
# "CA001-9","25","10.4","3.0",""
#measured later, see notebook, guessing 17?
FrdatSK[FrdatSK$tagged %in% c("GR002-9","CA009-7","UA007-5","CA001-9"),]


# #for lf length only
# lfl  <- FrdatSK[c(1:7,9,13,22, 16, 27:36, 55)]
# head(lfl)
# lfl$tagged <- as.factor(lfl$tagged)
# 
# #using reshape
# lfl.long <- reshape(lfl, idvar="tagged",
#                     direction="long", varying=list(m.date=c(21,22,11), lfl=c(8,9,10)), v.names=c("m.date","lfl"))
# head(lfl.long)
# tail(lfl.long)
# summary(lfl.long$time)

#for whole table?
FrdatSK$tagged <- as.factor(FrdatSK$tagged)
# dat <- reshape(FrdatSK, idvar="tagged", direction="long", 
#                varying=list(m.date=c(36,55,16), lfl=c(9,13,22), lfw=c(10,14,23), lfc=c(8,12,26)),
#                v.names=c("m.date","lfl", "lfw","lfc"))
#with rose diameter...?
dat2 <- FrdatSK
dat2$Rose.diam1 <- NA

Frdatsk.l <- reshape(dat2, idvar="tagged", direction="long", 
               varying=list(m.date=c(11,57,17), lfl=c(9,14,23), lfw=c(10,15,24), lfc=c(8,13,27), rd=c(58,12,20)),
               v.names=c("m.date","lfl", "lfw","lfc","rd"))
str(Frdatsk.l)
Frdatsk.l <- Frdatsk.l[,c(1:10,12:25,41,44:49)]
Frdatsk.l[is.na(Frdatsk.l$m.date),]
str(Frdatsk.l)
Frdatsk.l$Mom <- as.factor(Frdatsk.l$Mom)
dat <- merge(Frdatsk.l, FrdatSK[,c(1:7,17)], all.x=TRUE) #keep harvest date

#outliers?
subset(Frdatsk.l,lfc>100)
#lfcount at harvest for BG001-1 ?= 515? Ditto EDGE-9 ?=241?
#

#bolt.bin
Frdatsk.l$bolt.bin <- as.numeric(Frdatsk.l$BoltedatH)-1
head(Frdatsk.l[Frdatsk.l$Origin=="sk",])

#remove pops <2, CA008, GR003, UA004
Frdatsk.l <- Frdatsk.l[!(Frdatsk.l$Pop %in% c("CA008", "GR003","UA004")),]
Frdatsk.l <- droplevels(Frdatsk.l)

#write
write.table(Frdatsk.l, file="FrTraitClimDat_SK_long.txt",sep="\t", quote=F)
#read
Frdatsk.l<- read.table("FrTraitClimDat_SK_long.txt", header=T, sep="\t",quote='"', row.names=1)

####################
# 
# #using melt from reshape2
# #id.vars = keep in each row pop, row, col, trt, mom, origin, tagged
# #measure.vars = the cols to reshape/split up/ the source data
# #variable.name= timepoint / column you are creating indicating what you are splitting rows on
# #value.name= new composit column of data
# lfl.long <- melt(lfl, id.vars=c("Pop", "Row","Column", "Trt", "Mom","Origin", "tagged", "m1.date", "m2.date","Harvest.date"), 
#      measure.vars=c("MaxLfLgth1", "MaxLfLgth2", "LfLgth"), variable.name="tmpt", value.name="lfl")
# head(lfl.long)

# #rename levels of tmpt
# levels(lfl.long$tmpt)[levels(lfl.long$tmpt)=="MaxLfLgth1"] <- "tmpt1" #one wk after planting? m1.date - change format
# levels(lfl.long$tmpt)[levels(lfl.long$tmpt)=="MaxLfLgth2"] <- "tmpt2" #?? wks after planting?
# levels(lfl.long$tmpt)[levels(lfl.long$tmpt)=="LfLgth"] <- "tmpt3" #harvest date
# 
# levels(lfl.long$tmpt) <- c(levels(lfl.long$tmpt), levels(lfl.long$m1.date))
# lfl.long$tmpt[lfl.long$tmpt == 'tmpt1'] <- lfl.long$m1.date[lfl.long$tmpt=="tmpt1"]
# 
# lfl.long$m2.date[is.na(lfl.long$m2.date)] <- "6/12/2011"
# lfl.long$m2.date <- as.factor(lfl.long$m2.date)
# 
# levels(lfl.long$tmpt) <- c(levels(lfl.long$tmpt), levels(lfl.long$m2.date))
# lfl.long$tmpt[lfl.long$tmpt == 'tmpt2'] <- lfl.long$m2.date[lfl.long$tmpt=="tmpt2"]
# 
# 
# head(lfl.long)
# str(lfl.long)
# 
# summary(lfl.long$tmpt)

#do for each set of traits, then merge
#to do: lfcount, lfwdth, rose.diam and max rose diamH,

# #for lf width only
# lfw  <- FrdatSK[c(1:7,10,14,23)]
# lfl$tagged <- as.factor(lfl$tagged)
# 
# #id.vars = keep in each row pop, row, col, trt, mom, origin, tagged
# #measure.vars = the cols to reshape/split up/ the source data
# #variable.name= timepoint / column you are creating indicating what you are splitting rows on
# #value.name= new composit column of data
# lfl.long <- melt(lfl, id.vars=c("Pop", "Row","Column", "Trt", "Mom","Origin", "tagged"), 
#                  measure.vars=c("MaxLfLgth1", "MaxLfLgth2", "LfLgth"), variable.name="tmpt", value.name="lfl")
# 
# #rename levels of tmpt
# levels(lfl.long$tmpt)[levels(lfl.long$tmpt)=="MaxLfLgth1"] <- "tmpt1" #one wk after planting? m1.date - change format
# levels(lfl.long$tmpt)[levels(lfl.long$tmpt)=="MaxLfLgth2"] <- "tmpt2" #?? wks after planting?
# levels(lfl.long$tmpt)[levels(lfl.long$tmpt)=="LfLgth"] <- "tmpt3" #harvest date




#merge





# #converting dates for tmpt1 and tmpt2, then copy over harvest date
# frdat$Harvest.date2 <- strptime(frdat$Harvest.date, format="%A, %B %d, %Y")
# frdat$Harvest.date2 <- as.Date(frdat$Harvest.date2)
# day0 <- as.Date("2011-05-12") #planting date
# frdat$Harvest.date3 <- as.numeric(frdat$Harvest.date2-day0)
# frdat$Harvest.date <- frdat$Harvest.date3
# frdat <- frdat[,1:52]

# 
# #Transform datas!
# #log transform continuous
# #sqrt transform count data
# #arcsine sqrt for proportions angularx=57.295*asin(sqrt(x))?
# #or for percentages angularx=57.295*asin(sqrt(x/100))
# #or maybe logit? logit=log(prop/(1-prop))

# #############wide format datas###################################
#get sk datas
FrdesSK <- Frdes[Frdes$Origin=="sk" & Frdes$Trt!="edge",]
FrdesSK <- FrdesSK[!is.na(FrdesSK$Pop),]

Frm1 <- read.table("FrMeasure1.txt", header=T, sep="\t", quote='"')
FrdatSK <- merge(FrdesSK,Frm1, all.x=TRUE)

Frshoot <- read.table("FrShootMassH.txt", header=T, sep="\t", quote='"')
FrdatSK <- merge(FrdatSK,Frshoot, all.x=TRUE)

Frm2 <- read.table("FrMeasure2.txt", header=T, sep="\t", quote='"')
FrdatSK <- merge(FrdatSK,Frm2, all.x=TRUE)

Frh <- read.table("FrMeasureHarvest.txt", header=T, sep="\t", quote='"')
FrdatSK <- merge(FrdatSK,Frh, all.x=TRUE)

#load climate table
Frclimdat2 <- read.table("FrbioclimPCAdat.txt", header=TRUE)
FrdatSK <- merge(FrdatSK,Frclimdat2[,c(1,2,5,16,18,21:27)], all.x=TRUE) #pick out top loadings bio11, bio9, bio6, bio4, alt, long, lat
row.names(FrdatSK) <- FrdatSK$Tagged

setdiff(colnames(frdat), colnames(FrdatSK))
setdiff(colnames(FrdatSK), colnames(frdat)) #some extraneous columns/transformations, nothing major.

#merge
colnames(FrdatSK)[2] <- "tagged"
FrdatSKonly <- FrdatSK

#convert dates
FrdatSKonly$Harvest.date2 <- strptime(FrdatSKonly$Harvest.date, format="%A, %B %d, %Y")
FrdatSKonly$Harvest.date2 <- as.Date(FrdatSKonly$Harvest.date2)
day0 <- as.Date("2011-05-12") #planting date
FrdatSKonly$Harvest.date3 <- as.numeric(FrdatSKonly$Harvest.date2-day0)
FrdatSKonly$Harvest.date <- FrdatSKonly$Harvest.date3
FrdatSKonly <- FrdatSKonly[,1:49]

FrdatSKonly$Bolt.date2 <- strptime(FrdatSKonly$Bolt.date, format="%A, %B %d, %Y")
FrdatSKonly$Bolt.date2 <- as.Date(FrdatSKonly$Bolt.date2)
day0 <- as.Date("2011-05-12")
FrdatSKonly$Bolt.date3 <- as.numeric(FrdatSKonly$Bolt.date2-day0)
FrdatSKonly$Bolt.date <- FrdatSKonly$Bolt.date3
FrdatSKonly <- FrdatSKonly[,1:49]

FrdatSK$m1.date2 <- strptime(FrdatSK$m1.date, format="%m/%d/%Y")
FrdatSK$m1.date2 <- as.Date(FrdatSK$m1.date2)
day0 <- as.Date("2011-05-12")
FrdatSK$m1.date3 <- as.numeric(FrdatSK$m1.date2-day0)
FrdatSK$m1.date <- FrdatSK$m1.date3
FrdatSK <- FrdatSK[,1:49]
# 
FrdatSKonly$Bolt.date <- as.integer(FrdatSKonly$Bolt.date)
FrdatSKonly$Harvest.date <- as.integer(FrdatSKonly$Harvest.date)
FrdatSKonly$m1.date <- as.integer(FrdatSKonly$m1.date)
# 
# #write
write.table(FrdatSKonly, file="FrTraitClimDat_SKonly.txt",sep="\t", quote=F)
#read
FrdatSKonly<- read.table("FrTraitClimDat_SKonly.txt", header=T, sep="\t",quote='"', row.names=1)

#combine with DK data
FrdatSK <- merge(frdat, FrdatSKonly[,c(1:7,12:15, 17:21, 23:26, 31:37, 39:49)], all=TRUE)



# #Transform datas!
# #log transform continuous
# #sqrt transform count data
# #arcsine sqrt for proportions angularx=57.295*asin(sqrt(x))?
# #or for percentages angularx=57.295*asin(sqrt(x/100))
# #or maybe logit? logit=log(prop/(1-prop))
FrdatSK$LfLgth1.log <- log(FrdatSK$MaxLfLgth1)
FrdatSK$LfWdth1.log <- log(FrdatSK$MaxLfWdth1)
FrdatSK$RoseD2.log <- log(FrdatSK$Rose.diam2)
FrdatSK$LfLgth2.log <- log(FrdatSK$MaxLfLgth2)
FrdatSK$LfWdth2.log <- log(FrdatSK$MaxLfWdth2)
FrdatSK$MaxRoseDh.log <- log(FrdatSK$ Max.Rose.diamH)
FrdatSK$MinRoseDh.log <- log(FrdatSK$Min.Rose.diamH)
FrdatSK$RoseAh.log <- log(FrdatSK$Rose.AreaH.m2)
FrdatSK$LfLgthH.log <- log(FrdatSK$LfLgth)
FrdatSK$LfWdthH.log <- log(FrdatSK$Lf.Width)
FrdatSK$Crown.log <- log(FrdatSK$CrownDiam.mm)
FrdatSK$Mass.log <- log(FrdatSK$Shoot.mass.gH)

FrdatSK$LfCount1.sq <- sqrt(FrdatSK$LfCount1)
FrdatSK$LfCount2.sq <- sqrt(FrdatSK$LfCount2)
FrdatSK$Bolt.sq <- sqrt(FrdatSK$Bolt.date)
FrdatSK$Harvest.sq <- sqrt(FrdatSK$Harvest.date)

#bolt.bin
FrdatSK$bolt.bin <- as.numeric(FrdatSK$BoltedatH)-1
head(FrdatSK[FrdatSK$Origin=="sk",])

##checking outliers
ggplot(data=FrdatSK, aes(y=LfCountH, x=Trt, fill=Origin))+geom_boxplot()
subset(FrdatSK, subset=LfCountH>150)

#remove pops <2, CA008, GR003, UA004
FrdatSK <- FrdatSK[!(FrdatSK$Pop %in% c("CA008", "GR003","UA004")),]
FrdatSK <- droplevels(FrdatSK)

#write
write.table(FrdatSK, file="FrTraitClimDat_SK.txt",sep="\t", quote=F)
#read
FrdatSK<- read.table("FrTraitClimDat_SK.txt", header=T, sep="\t",quote='"', row.names=1)
