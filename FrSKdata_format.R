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
FrdatSK[FrdatSK$tagged %in% day1,]$m2.date <- "6/12/2011"
FrdatSK[FrdatSK$tagged %in% day2,]$m2.date <- "6/13/2011"

#write

#for lf length only
lfl  <- FrdatSK[c(1:7,9,13,22, 16, 36, 55)]
head(lfl)
lfl$tagged <- as.factor(lfl$tagged)

#id.vars = keep in each row pop, row, col, trt, mom, origin, tagged
#measure.vars = the cols to reshape/split up/ the source data
#variable.name= timepoint / column you are creating indicating what you are splitting rows on
#value.name= new composit column of data
lfl.long <- melt(lfl, id.vars=c("Pop", "Row","Column", "Trt", "Mom","Origin", "tagged", "m1.date", "m2.date","Harvest.date"), 
     measure.vars=c("MaxLfLgth1", "MaxLfLgth2", "LfLgth"), variable.name="tmpt", value.name="lfl")
head(lfl.long)

#why NAs here???

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
# #get sk datas
# FrdesSK <- Frdes[Frdes$Origin=="sk" & Frdes$Trt!="edge",]
# FrdesSK <- FrdesSK[!is.na(FrdesSK$Pop),]
# 
# Frm1 <- read.table("FrMeasure1.txt", header=T, sep="\t", quote='"')
# FrdatSK <- merge(FrdesSK,Frm1, all.x=TRUE)
# 
# Frshoot <- read.table("FrShootMassH.txt", header=T, sep="\t", quote='"')
# FrdatSK <- merge(FrdatSK,Frshoot, all.x=TRUE)
# 
# Frm2 <- read.table("FrMeasure2.txt", header=T, sep="\t", quote='"')
# FrdatSK <- merge(FrdatSK,Frm2, all.x=TRUE)
# 
# Frh <- read.table("FrMeasureHarvest.txt", header=T, sep="\t", quote='"')
# FrdatSK <- merge(FrdatSK,Frh, all.x=TRUE)
# 
# #load climate table
# Frclimdat2 <- read.table("FrbioclimPCAdat.txt", header=TRUE)
# FrdatSK <- merge(FrdatSK,Frclimdat2[,c(1,13,16,19,22:27)], all.x=TRUE)
# row.names(FrdatSK) <- FrdatSK$tagged
# 
# setdiff(colnames(frdat), colnames(FrdatSK))
# setdiff(colnames(FrdatSK), colnames(frdat)) #some extraneous columns/transformations, nothing major.
# 
# #merge
# colnames(FrdatSK)[2] <- "tagged"
# FrdatSKonly <- FrdatSK
# #write
# write.table(FrdatSKonly, file="FrTraitClimDat_SKonly.txt",sep="\t", quote=F)
# 
# #convert dates
# FrdatSKonly$Harvest.date2 <- strptime(FrdatSKonly$Harvest.date, format="%A, %B %d, %Y")
# FrdatSKonly$Harvest.date2 <- as.Date(FrdatSKonly$Harvest.date2)
# day0 <- as.Date("2011-05-12") #planting date
# FrdatSKonly$Harvest.date3 <- as.numeric(FrdatSKonly$Harvest.date2-day0)
# FrdatSKonly$Harvest.date <- FrdatSKonly$Harvest.date3
# FrdatSKonly <- FrdatSKonly[,1:47]
# 
# FrdatSKonly$Bolt.date2 <- strptime(FrdatSKonly$Bolt.date, format="%A, %B %d, %Y")
# FrdatSKonly$Bolt.date2 <- as.Date(FrdatSKonly$Bolt.date2)
# day0 <- as.Date("2011-05-12")
# FrdatSKonly$Bolt.date3 <- as.numeric(FrdatSKonly$Bolt.date2-day0)
# FrdatSKonly$Bolt.date <- FrdatSKonly$Bolt.date3
# FrdatSKonly <- FrdatSKonly[,1:47]
# 
# FrdatSKonly$Bolt.date <- as.integer(FrdatSKonly$Bolt.date)
# FrdatSKonly$Harvest.date <- as.integer(FrdatSKonly$Harvest.date)
# 
# FrdatSK <- merge(frdat, FrdatSKonly[,c(1:7,12:14, 17:21, 23:26, 31:37, 39:47)], all=TRUE)
# 
# #Transform datas!
# #log transform continuous
# #sqrt transform count data
# #arcsine sqrt for proportions angularx=57.295*asin(sqrt(x))?
# #or for percentages angularx=57.295*asin(sqrt(x/100))
# #or maybe logit? logit=log(prop/(1-prop))
# FrdatSK$LfLgth1.log <- log(FrdatSK$MaxLfLgth1)
# FrdatSK$LfWdth1.log <- log(FrdatSK$MaxLfWdth1)
# FrdatSK$RoseD2.log <- log(FrdatSK$Rose.diam2)
# FrdatSK$LfLgth2.log <- log(FrdatSK$MaxLfLgth2)
# FrdatSK$LfWdth2.log <- log(FrdatSK$MaxLfWdth2)
# FrdatSK$MaxRoseDh.log <- log(FrdatSK$ Max.Rose.diamH)
# FrdatSK$MinRoseDh.log <- log(FrdatSK$Min.Rose.diamH)
# FrdatSK$RoseAh.log <- log(FrdatSK$Rose.AreaH.m2)
# FrdatSK$LfLgthH.log <- log(FrdatSK$LfLgth)
# FrdatSK$LfWdthH.log <- log(FrdatSK$Lf.Width)
# FrdatSK$Crown.log <- log(FrdatSK$CrownDiam.mm)
# FrdatSK$Mass.log <- log(FrdatSK$Shoot.mass.gH)
# 
# FrdatSK$LfCount1.sq <- sqrt(FrdatSK$LfCount1)
# FrdatSK$LfCount2.sq <- sqrt(FrdatSK$LfCount2)
# FrdatSK$Bolt.sq <- sqrt(FrdatSK$Bolt.date)
# FrdatSK$Harvest.sq <- sqrt(FrdatSK$Harvest.date)

#write
write.table(FrdatSK, file="FrTraitClimDat_SK.txt",sep="\t", quote=F)