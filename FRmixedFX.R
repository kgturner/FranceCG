#mixed effects models, REML, using lme4


#france DK data
d <- read.table("Frm1DKdatdes.txt", header=T, sep="\t",quote='"', row.names=1) #measure1 
d2<-read.table("Frm2DKdatdes.txt", header=T, sep="\t",quote='"', row.names=1)#measure 2 
h <- read.table("FrmHDKdatdes.txt", header=T, sep="\t",quote='"', row.names=1) #measure harvest 

#remove small pops (<3)
summary(d$Pop)
d<-d[d$Pop!="CA008",]
d<-d[d$Pop!="GR003",]
d<-d[d$Pop!="UA004",]
summary(d2$Pop)
d2<-d2[d2$Pop!="CA008",]
d2<-d2[d2$Pop!="GR003",]
d2<-d2[d2$Pop!="UA004",]
summary(h$Pop)
h<-h[h$Pop!="CA008",]
h<-h[h$Pop!="GR003",]
h<-h[h$Pop!="UA004",]

#write
write.table(d, file="Frm1DKdatdes.txt", sep="\t", quote=F)
write.table(d2, file="Frm2DKdatdes.txt", sep="\t", quote=F)
write.table(h, file="FrmHDKdatdes.txt", sep="\t", quote=F)

#load climate table
Frclimdat2 <- read.table("FrbioclimPCAdat.txt", header=TRUE)

#get rid of traitPC1
d <- d[,-15]
d2 <- d2[,-15]
h <- h[,-26]

#merge all the things!
d$tagged <- row.names(d)
d2$tagged <- row.names(d2)
h$tagged <- row.names(h)

frdat <- merge(d, d2, all=TRUE)
frdat <- merge(frdat, h, all=TRUE)
frdat.2 <- frdat[,c(1:42,52)]
frdat.3 <- merge(frdat.2,Frclimdat2[,c(1,2,5,16,18,21:27)], all.x=TRUE) #pick out top loadings bio11, bio9, bio6, bio4, alt, long, lat
frdat <- frdat.3
row.names(frdat) <- frdat$tagged

#formating
frdat <- cbind(frdat, bolt.bin=as.numeric(frdat$BoltedatH)-1)

frdat$LfCountH <- as.integer(frdat$LfCountH)

#dates
24
head(frdat[35])

frdat$m1.date2 <- strptime(frdat$m1.date, format="%m/%d/%Y")
frdat$m1.date2 <- as.Date(frdat$m1.date2)
day0 <- as.Date("2011-05-12")
frdat$m1.date3 <- as.numeric(frdat$m1.date2-day0)
frdat$m1.date <- frdat$m1.date3
frdat <- frdat[,1:52]

frdat$Harvest.date2 <- strptime(frdat$Harvest.date, format="%A, %B %d, %Y")
frdat$Harvest.date2 <- as.Date(frdat$Harvest.date2)
day0 <- as.Date("2011-05-12") #planting date
frdat$Harvest.date3 <- as.numeric(frdat$Harvest.date2-day0)
frdat$Harvest.date <- frdat$Harvest.date3
frdat <- frdat[,1:52]

frdat$Bolt.date2 <- strptime(frdat$Bolt.date, format="%A, %B %d, %Y")
frdat$Bolt.date2 <- as.Date(frdat$Bolt.date2)
day0 <- as.Date("2011-05-12")
frdat$Bolt.date3 <- as.numeric(frdat$Bolt.date2-day0)
frdat$Bolt.date <- frdat$Bolt.date3
frdat <- frdat[,1:52]

frdat$Bolt.date <- as.integer(frdat$Bolt.date)
frdat$Harvest.date <- as.integer(frdat$Harvest.date)


#bolt and harvest dates agree?
summary(frdat[frdat$Bolt.date>frdat$Harvest.date,])
summary(frdat[is.na(frdat$Bolt.date),])
frdat[is.na(frdat$Harvest.date),]

#bolt date and boltedatH/bolt.bin agree?
summary(frdat[!is.na(frdat$Bolt.date),])
summary(frdat[frdat$BoltedatH=="Yes",])

#remove pops <2, CA008, GR003, UA004
frdat <- frdat[!(frdat$Pop %in% c("CA008", "GR003","UA004")),]
frdat <- droplevels(frdat)

#write
write.table(frdat, file="FrTraitClimDat.txt",sep="\t", quote=F)




# #########################first round, Fall 2012################################################3
# #mixed effects linear model on PC1 of traits(not climate)... does that make sense?
# source("http://bioconductor.org/biocLite.R")
# biocLite("lme4")
# library(lme4)
# 
# #m1 on PC1
# str(d)
# dPCmodel1<-lmer(PC1~1+Origin+ (1|Origin/Pop), data=d)
# print(dPCmodel1)
# dPCmodel2<-lmer(PC1~Origin+(1|Origin/Pop), data=d)
# print(dPCmodel2)
# #model1 and model2 are the same
# dPCmodel3<-lmer(PC1~Origin+(1|Pop), data=d)
# print(dPCmodel3)
# summary(dPCmodel3)
# d$Mom<-factor(d$Mom)
# dPCmodel4<-lmer(PC1~Origin+(1|Origin/Pop/Mom), data=d)
# print(dPCmodel4)
# #model4 very similar results to model2...
# 
# anova(dPCmodel3,dPCmodel2)

###mixed effects linear model on transformed traits

d$Mom<-as.factor(d$Mom) # Convert Mom from numeric to factor
d2$Mom<-as.factor(d2$Mom) # Convert Mom from numeric to factor
h$Mom<-as.factor(h$Mom) # Convert Mom from numeric to factor


# shoot mass, harvest
str(h)
modeldata<-h[!is.na(h$Mass.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(Mass.log ~ Trt*Origin + BoltedatH+(BoltedatH|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log ~ Trt*Origin + BoltedatH+(BoltedatH|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.log ~ Trt*Origin + BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(Mass.log ~ Trt+Origin + BoltedatH+(BoltedatH|Pop), family=gaussian,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Trt and Origin - not sig

modelInt2<-lmer(Mass.log ~ Trt+Origin * BoltedatH+(BoltedatH|Pop), family=gaussian,data=modeldata)
modelInt3<-lmer(Mass.log~Trt*Origin*BoltedatH+(BoltedatH|Pop), family=gaussian, data=modeldata)
anova(model2, modelInt2)
anova(modelInt, modelInt2)
anova(model2,modelInt3)
anova(modelInt, modelInt3)

modelB0<-lmer(Mass.log ~ Trt+Origin + (1|Pop), family=gaussian,data=modeldata)
modelB1<-lmer(Mass.log ~ Trt+Origin + BoltedatH+(1|Pop), family=gaussian,data=modeldata)
modelB2<-lmer(Mass.log ~ Trt+Origin + (BoltedatH|Pop), family=gaussian,data=modeldata)
anova(modelB0, modelInt)#test for sig of bolting - bolting sig
anova(modelB1, modelInt)#test for sig of bolting - not sig - is bolting needed in random effect?
anova(modelB2, modelInt)#test for sig of bolting - bolting sig

modelT<-lmer(Mass.log ~ Origin + BoltedatH+(1|Pop), family=gaussian,data=modeldata)
anova(modelT,modelB1)#test for sig of Trt - trt is sig

modelO<-lmer(Mass.log ~ Trt + BoltedatH+(1|Pop), family=gaussian,data=modeldata)
anova(modelO,modelB1) #test for significance of origin - origin is sig!!!

#raw data, rather than transformed
modelB1Oraw<-lmer(Shoot.mass.gH~ Trt + BoltedatH+(1|Pop), family=gaussian,data=modeldata)
#all other families give errors
modelB1raw<-lmer(Shoot.mass.gH~ Trt+Origin + BoltedatH+(1|Pop), family=gaussian,data=modeldata)

anova(modelB1raw,modelB1Oraw)

#############leafXwidth, m1
str(d)
d$Tflxw<-d$LfLgth1.log*d$LfWdth1.log
dLeafmodel1<-lmer(Tflxw~Origin+(1|Pop), data=d)
print(dLeafmodel1)

#leafxwidth, m2
str(d2)
d2$tlxw2<-d2$LfLgth2.log*d2$LfWdth2.log
d2Leafmodel1<-lmer(tlxw2~Origin+(1|Pop), data=d2)
print(d2Leafmodel1)

d2Leafmodel2<-lmer(tlxw2~Origin*Trt+(1|Pop), data=d2)
print(d2Leafmodel2)

#leafxwidth, harvest

str(h)
h$tlxwh<-h$LfLgthH.log*h$LfWdthH.log
hLeafmodel1<-lmer(tlxwh~Origin+(1|Pop), data=h)
print(hLeafmodel1)

hLeafmodel2<-lmer(tlxwh~Origin*Trt+(1|Pop), data=h)
print(hLeafmodel2)

####From Rob C.#####
#library(lme4)

#setwd("C:/Users/Alliaria/Documents/Colautti 2012/Kathryn")
#getwd()

#data<-read.table("FrmHDKdatdes.txt",header=T,sep=";")
data$Mom<-as.factor(data$Mom) # Convert Mom from numeric to factor
strptime(data2010$Bolt.date,format="%d/%m/%y")  # converts dates to numeric

hist(data$Mass.log)
hist(data$Shoot.mass.gH)

modeldata<-data[!is.na(data$Mass.log),]
# modeldata$matfam<-paste(modeldata$Pop,modeldata$Mom,sep="-")
# modeldata$blank<-1
# modeldata$blank<-as.factor(modeldata$blank)

#simple test of population effect
model1<-lmer(Mass.log ~ Trt*Origin + (1|Pop/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.log ~ Trt*Origin + (1|Pop), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
anova(model2,model1) # P < 0.05 means that Population is significant
model3<-lmer(Mass.log ~ Trt+Origin + (1|Pop/Mom), family=gaussian,data=modeldata)
anova(model3,model1)
modelO<-lmer(Mass.log ~ Trt + (1|Pop/Mom), family=gaussian,data=modeldata)
modelT<-lmer(Mass.log ~ Origin + (1|Pop/Mom), family=gaussian,data=modeldata)
anova(modelO,model3)
anova(modelT,model3)


## Messing around below here

modelTrt<-lmer(Mass.log ~ Trt*Origin + (Trt-1|Pop), family=gaussian,data=modeldata) # Does genetic variation differ between treatments?
modelTrt2<-lmer(Mass.log ~ Trt*Origin + (1|Pop), family=gaussian,data=modeldata) # Does genetic variation differ between treatments?
anova(modelTrt2,modelTrt)

anova(model2,modelTrt)
modelOxT<-lmer(Mass.log ~ Trt*Origin + (Origin+Trt|Pop/Mom), family=gaussian,data=modeldata)


model4<-lmer(Mass.log ~ Trt*Origin + (Origin|Pop/Mom), family=gaussian,data=modeldata) 
# Estimate separate among-pop variances for native and introduced ranges
anova(model1,model3) # Tests if among-population variance differs between native and introduced ranges


####
#fit REML model for all trait resids, treat is fixed, line and linextreat are random
# intlm1     <- lmer(int1resid ~ treat + (1|linetext) + (1|linetext:treat), family=gaussian, int1resid); intlm1
# m1<-lmer(Glycogen~Treatment+(1|Treatment/Rat/Liver))
# (1|Treatment/Rat/Liver) = (1|Treatment)+(1|Treatment:Rat)+(1|Treatment:Rat:Liver)
#lmer(PC1~1+Origin+ (1|Origin/Pop), data=d)   =   lmer(PC1~Origin+(1|Origin/Pop), data=d)

# fixef(dPCmodel1)
# getME(dPCmodel1, "Z")
# ranef(dPCmodel1, drop=TRUE)
# fitted(dPCmodel1)
# print(dPCmodel1)
# qqmath(ranef(dPCmodel1, postVar=TRUE), strip=FALSE)$Origin





#####################REML###############################

#load the lme4 package to do REML analyses (must be pre-installed)
source("http://bioconductor.org/biocLite.R")
biocLite("lme4")

library(lme4)

#fit REML model for all trait resids, treat is fixed, line and linextreat are random
intlm1     <- lmer(int1resid ~ treat + (1|linetext) + (1|linetext:treat), family=gaussian, int1resid); intlm1
#fit REML model with linextreatment effects removed
intlm1_lxt     <- lmer(int1resid ~ treat + (1|linetext), family=gaussian, int1resid); intlm1_lxt
#fit REML model with line effects removed
intlm1_l     <- lmer(int1resid ~ treat + (1|linetext:treat), family=gaussian, int1resid); intlm1_l

#F-test for linextreatment effects, smaller model should be first in model statements
anova(intlm1_lxt, intlm1)
#F-test for line effects
anova(intlm1_l, intlm1)
#F-test for treatment effects
anova(intlm1)
