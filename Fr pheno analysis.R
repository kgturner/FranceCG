#France CG analysis

# Frm1dat <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1) #measure1
# duplicated(Frm1dat[,1])
# nrow(m1dat)
# ncol(m1dat)
# head(row.names(m1dat))
# colnames(m1dat)
# 
# Frm2dat <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1) #measure2
# #dup row names?
# #m2dat<- read.table(file.choose(), header=T, sep="\t",quote='"')
# #duplicated(m2dat[,1])
# #colnames(Frm2dat)[1]<-"Rose.diam2"
# # colnames(Frm2dat)[2]<-"LfCount2"
# # colnames(Frm2dat)[3]<-"MaxLfLgth2"
# # colnames(Frm2dat)[4]<-"MaxLfWdth2"
# # colnames(Frm2dat)[5]<-"m2.comments"
# 
# FrmHdat<- read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #FrmHdat.txt has shootmass
# #duplicate names
# #FrmHdat <- read.table(file.choose(), header=T, sep="\t", quote='"') 
# #duplicated(FrmHdat[,1])
# #FrmHdat[c(236,257,318,363,370),]
# colnames(FrmHdat)[13]<-"MaxLfLgthH"
# colnames(FrmHdat)[14]<-"MaxLfWdthH"
# colnames(FrmHdat)[16]<-"h.comments"
# 
# 
# FrmassH<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #shoot mass H
# #duplicated(FrmassH[,1])
# #FrmassH[c(78,132,181),]
# 
# #deathdat<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #death and bolt
# 
Frdes <- read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #Frdes.txt
# summary(Frdes)
# levels(Frdes$Pop)
# #duplicate names
# #Frdes <- read.table(file.choose(), header=T, sep="\t", quote='"') #stresstol design
# #desdup<-row.names(duplicated(Frdes[,1]))
# #nrow(des)
# 
# #allodat<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #allometry
# 
# #Frplant<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #FrPlanting for tags
# 
# # Frdes<-merge(Frdes, Frplant, by="row.names", all.x=TRUE, all.y=TRUE)
# # row.names(Frdes)<-Frdes[,1]
# # Frdes<-Frdes[,2:11]
# # write.table(Frdes, file="Frdes.txt", sep="\t", quote=F)
# 
# #diff rows?
# # head(row.names(des))
# # des.missing <- setdiff(rownames(m1dat), rownames(des))
# # des.missing
# # dat.missing <- setdiff(rownames(des), rownames(m1dat))
# # dat.missing
# # 
# # m1des<- des[rownames(m1dat),]
# 
# FrmHdat<-merge(FrmHdat, FrmassH, by="row.names", all.x=TRUE, all.y=TRUE)
# row.names(FrmHdat)<-FrmHdat[,1]
# head(FrmHdat)
# ncol(FrmHdat)
# FrmHdat<-FrmHdat[,2:18]
# write.table(FrmHdat, file="FrmHdat.txt", sep="\t", quote=F)
# 
# #merge for data and des, DK only!
# Frm2DKdatdes<-NULL
# Frdesm2<-Frdes[row.names(Frm2dat),]
# Frm2DKdatdes <- merge(Frdesm2, Frm2dat, by="row.names", all.x=TRUE, all.y=TRUE)
# head(Frm2DKdatdes)
# nrow(Frm2DKdatdes)
# ncol(Frm2DKdatdes)
# row.names(Frm2DKdatdes)<-Frm2DKdatdes[,1]
# Frm2DKdatdes<-Frm2DKdatdes[,c(2:7,12:15)]
# 
# Frm1DKdatdes<-NULL
# Frdesm1<-Frdes[row.names(Frm1dat),]
# Frm1DKdatdes <- merge(Frdesm1, Frm1dat, by="row.names", all.x=TRUE, all.y=TRUE)
# head(Frm1DKdatdes)
# nrow(Frm1DKdatdes)
# ncol(Frm1DKdatdes)
# row.names(Frm1DKdatdes)<-Frm1DKdatdes[,1]
# Frm1DKdatdes<-Frm1DKdatdes[,c(2:7,12:15)]
# 
# FrmHDKdatdes<-NULL
# FrdesmH<-Frdes[row.names(FrmHdat),]
# FrmHDKdatdes <- merge(FrdesmH, FrmHdat, by="row.names", all.x=TRUE, all.y=TRUE)
# head(FrmHDKdatdes)
# nrow(FrmHDKdatdes)
# ncol(FrmHDKdatdes)
# row.names(FrmHDKdatdes)<-FrmHDKdatdes[,1]
# FrmHDKdatdes<-FrmHDKdatdes[,c(2:7,12:15,20:26,28)]
# 
# #make DK only versions with des, no edge
# Frm1DKdatdes<-Frm1DKdatdes[Frm1DKdatdes$Origin!="sk",]
# Frm1DKdatdes$Trt==""
# Frm1DKdatdes<-Frm1DKdatdes[Frm1DKdatdes$Trt!="edge",]
# summary(Frm1DKdatdes)
# Frm1DKdatdes<-Frm1DKdatdes[-105,]
# Frm1DKdatdes$LfCount1!="NA"
# Frm1DKdatdes[106,]
# 
# write.table(Frm1DKdatdes, file="Frm1DKdatdes.txt", sep="\t", quote=F)
d <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1) #measure1 Frm1DKdatdes.txt

#d<-d[,-11]

# Frm2DKdatdes<-Frm2DKdatdes[Frm2DKdatdes$Origin!="sk",]
# #Frm2DKdatdes$Origin=="sk"
# summary(Frm2DKdatdes)
# Frm2DKdatdes<-Frm2DKdatdes[Frm2DKdatdes$Trt!="",]
# write.table(Frm2DKdatdes, file="Frm2DKdatdes.txt", sep="\t", quote=F)
Frm2DKdatdes <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1) #measure 2 Frm2DKdatdes.txt
d2<-read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1)

# FrmHDKdatdes<-FrmHDKdatdes[FrmHDKdatdes$Origin!="sk",]
# summary(FrmHDKdatdes)
# FrmHDKdatdes$Origin=="NA"
# FrmHDKdatdes<-FrmHDKdatdes[-c(78,159),]
# FrmHDKdatdes<-FrmHDKdatdes[FrmHDKdatdes$Trt!="EDGE",]
# FrmHDKdatdes<-FrmHDKdatdes[FrmHDKdatdes$Harvest.date!="NA",]
# FrmHDKdatdes[FrmHDKdatdes$Harvest.date=="NA",]
# FrmHDKdatdes<-FrmHDKdatdes[-c(9,106),]
# write.table(FrmHDKdatdes, file="FrmHDKdatdes.txt", sep="\t", quote=F)

h <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1) #measure harvest FrmHDKdatdes.txt

#means
PopMeansM1 <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1)
PopMeansM2 <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1)
PopMeansMh <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1)

# #control versions only
# Frm1DKcont<-Frm1DKdatdes[Frm1DKdatdes$Trt=="control",]
# head(Frm1DKcont)
# summary(Frm1DKcont)
# 
# Frm2DKcont<-Frm2DKdatdes[Frm2DKdatdes$Trt=="control",]
# summary(Frm2DKcont)
# 
# FrmHDKcont<-FrmHDKdatdes[FrmHDKdatdes$Trt=="control",]
# summary(FrmHDKcont)

#eset, for shits and giggles
# Frdesm2<-Frdes[row.names(Frm2dat),]
# Frm2datE<-Frm2dat[,-5]
# head(Frm2datE)
# nrow(Frm2datE)
# sum(is.na(Frm2datE))
# Frm2datE<-t(Frm2datE)
# 
# Frdesm2 <- Frdesm2[colnames(Frm2datE),]
# head(Frdesm2)
# all(row.names(Frdesm2) == colnames(Frm2datE))
# library(Biobase)
# Frm2eset<- new("ExpressionSet", phenoData = as(Frdesm2, "AnnotatedDataFrame"), exprs = log(as.matrix(Frm2datE)))

########
#manova for measure 2
m2traits<-cbind(Frm2DKdatdes$MaxLfLgth2, Frm2DKdatdes$MaxLfWdth2, Frm2DKdatdes$Rose.diam2)
#m2traitsImp<-cbind(Frm2Imp$lf.number, Frm2Imp$lf.length, Frm2Imp$lf.width, Frm2Imp$rosette.diam)

#fitOrigin<-manova(m2traits~Frm2DKdatdes$Origin)
#fittrt<-manova(m2traits~Frm2datTag$trt)
#fitOrigintrt<-manova(m2traits~Frm2datTag$Origin+Frm2datTag$trt)
#fitOriginbytrt<-manova(m2traits~Frm2DKdatdes$Origin+Frm2DKdatdes$Trt+Frm2DKdatdes$Origin*Frm2DKdatdes$Trt)
m2man<-manova(m2traits~Origin/Pop*Trt, data=Frm2DKdatdes)

summary.aov(m2man)
#summary(fitOrigin, test="Wilks")
summary(m2man)

#summary.aov(fitOriginbytrt, na.rm=TRUE)
#summary(fitOriginbytrt, test="Wilks")
#summary(fitOriginbytrt, na.rm=TRUE)

#test for normality
shapiro.test(resid(m2man))
qqnorm(resid(m2man))
qqline(resid(m2man))
hist(resid(m2man), breaks="FD")

#univariate normality
qqnorm((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$LfCount2)
qqline((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$LfCount2)
shapiro.test((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$LfCount2)
#not normal

qqnorm((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$Rose.diam2)
qqline((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$Rose.diam2)
shapiro.test((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$Rose.diam2)
#normal!

qqnorm((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$MaxLfLgth2)
qqline((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$MaxLfLgth2)
shapiro.test((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$MaxLfLgth2)
#normal!

qqnorm((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$MaxLfWdth2)
qqline((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$MaxLfWdth2)
shapiro.test((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$MaxLfWdth2)
#normal!

#leaf count non-normal, so transform, and re-run manova
#sqrt transform
# Frm2datTag<-cbind(Frm2datTag,sqrt(Frm2datTag$LfCount2))
# head(Frm2datTag)
# colnames(Frm2datTag)[16]<-"LfCount2.sqrt"
# qqnorm((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$LfCount2.sqrt)
# qqline((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$LfCount2.sqrt)
# shapiro.test((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$LfCount2.sqrt)

#log transform
# Frm2datTag<-cbind(Frm2datTag,log(Frm2datTag$LfCount2))
# head(Frm2datTag)
# colnames(Frm2datTag)[17]<-"LfCount2.log"
# qqnorm((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$LfCount2.log)
# qqline((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$LfCount2.log)
# shapiro.test((Frm2datTag[Frm2datTag$Trt=="control" & Frm2datTag$Origin=="nat",])$LfCount2.log)


#re-run manova w/ transformed lf count
# m2traits<-cbind(Frm2datTag$LfCount2.log, Frm2datTag$MaxLfLgth2, Frm2datTag$MaxLfWdth2, Frm2datTag$Rose.diam2)
# fitOriginbytrt<-manova(m2traits~Frm2datTag$Origin+Frm2datTag$Trt+Frm2datTag$Origin*Frm2datTag$Trt)
# summary.aov(fitOriginbytrt)
# summary(fitOriginbytrt, test="Wilks")
# summary(fitOriginbytrt)
#re-test for normality... not sure this works for multivariate
# shapiro.test(resid(fitOriginbytrt))
# qqnorm(resid(fitOriginbytrt))
# qqline(resid(fitOriginbytrt))
# hist(resid(fitOriginbytrt), breaks="FD")

#tests for multivariate normality


#DH test
#source("http://bioconductor.org/biocLite.R")
# biocLite("asbio")
# library(asbio)
# Frm2manM<-Frm2datTag[,c(11,13,14,17)]
# head(Frm2manM)
# # Frm2manM.names<-c(colnames(Frm2manM))
# #Frm2manM.names<-toString(Frm2manM.names)
# class(Frm2manM.names)
# Frm2manM<-as.matrix(Frm2manM)
# class(Frm2manM)
# head(Frm2manM)
# row.names(Frm2manM)<-NULL
# colnames(Frm2manM)<-NULL
# 
# DH.test(Frm2manM,Y.names=Frm2manM.names)
# DH.test(Frm2manM)

#shapiro-wilk test multivariate normality, needs data as matrix
source("http://bioconductor.org/biocLite.R")
biocLite("mvnormtest")
library(mvnormtest)
mshapiro.test(m2man)
mshapiro.test(t(Frm2manM))


#manova, no leaf count
# m2traits2<-cbind(Frm2datTag$MaxLfLgth2, Frm2datTag$MaxLfWdth2, Frm2datTag$Rose.diam2)
# fitOriginbytrt<-manova(m2traits2~Frm2datTag$Origin+Frm2datTag$Trt+Frm2datTag$Origin*Frm2datTag$Trt)
# summary(fitOriginbytrt)
# 
# shapiro.test(resid(fitOriginbytrt))
# qqnorm(resid(fitOriginbytrt))
# qqline(resid(fitOriginbytrt))
# hist(resid(fitOriginbytrt), breaks="FD")
# 
# Frm2manM2<-as.matrix(Frm2datTag[,c(11,13,14)])
# mshapiro.test(t(Frm2manM2))



#Manova for measure 1 - NO STRESS!
d <- Frm1DKdatdes
m1traits<-cbind(d$LfCount1, d$MaxLfLgth1, d$MaxLfWdth1)

qqnorm(d$LfCount1)
qqline(d$LfCount1)
shapiro.test(d$LfCount1)

m1man<-manova(m1traits~Origin/Pop, data=d)

summary.aov(m1man)
summary(m1man)


#manova for harvest
h <- FrmHDKdatdes
#mHtraits<-cbind(FrmHDKdatdes$LfLgth, FrmHDKdatdes$Lf.Width, FrmHDKdatdes$Rose.AreaH.m2, FrmHDKdatdes$CrownDiam.mm, FrmHDKdatdes$Shoot.mass.gH)

#fitOrigin<-manova(mHtraits~FrmHDKdatdes$Origin)

# fitOriginbytrt<-manova(mHtraits~FrmHDKdatdes$Origin+FrmHDKdatdes$Trt+FrmHDKdatdes$Origin*FrmHDKdatdes$Trt)
# 
# summary.aov(fitOriginbytrt)
# 
# summary(fitOriginbytrt)
# summary(fitOriginbytrt, test="Wilks")

#manova for harvest, bolters only
# hb<-FrmHDKdatdes
# head(hb)
# hb<-hb[hb$BoltedatH=="Yes",]
# summary(hb)
# mHbtraits<-cbind(hb$LfLgth, hb$Lf.Width, hb$Rose.AreaH.m2, hb$CrownDiam.mm, hb$Shoot.mass.gH)
# fitOriginbytrt<-manova(mHbtraits~hb$Origin+hb$Trt+hb$Origin*hb$Trt)
# summary.aov(fitOriginbytrt)
# 
# summary(fitOriginbytrt)

#manova for harvest, include bolting
# mhtraits<-cbind(h$LfLgth, h$Lf.Width, h$Rose.AreaH.m2, h$CrownDiam.mm, h$Shoot.mass.gH)
# fitOrigintrtbybolt<-manova(mhtraits~h$Origin+h$Trt+h$BoltedatH+h$Origin*h$BoltedatH+)
# summary.aov(fitOrigintrtbybolt)

# summary(fitOrigintrtbybolt)

#manova for harvest, include pop nested in origin
mhtraits<-cbind(h$LfLgth, h$Lf.Width, h$Rose.AreaH.m2, h$CrownDiam.mm, h$Shoot.mass.gH)
mhman<-manova(mhtraits~Origin/Pop*Trt+BoltedatH, data=h)
summary.aov(mhman)
summary(mhman)




#linear models...
#glm for count data
# z <- glm(response ~ explanatory, data=mydata, family = poisson(link="log")
#          #for overdispersion family = quasipoisson(link = "log")
#          summary(z) # parameter estimates and overall model fit
#          plot(z)    # plots of deviance residuals, q-q, leverage
#          coef(z)    # model coefficients
#          resid(z)   # deviance residuals
#          predict(z) # predicted values on the transformed scale
#          predict(z,se.fit=TRUE) # Includes SE's of predicted values
#          fitted(z)  # predicted values on the original scale
#          anova(z, test="Chi")     # Analysis of deviance - sequential
#          anova(z1,z2, test="Chi") # compare fits of 2 models, "reduced" vs "full"
#          
lfcount<- glm(LfCount1~Origin/Pop, data = Frm1DKdatdes, family= poisson(link="log"))       
summary(lfcount)
plot(lfcount)         

coef(lfcount)     
         
#glm for binary response, bolted at harvest
z <- glm(response ~ explanatory, data=mydata, family = binomial(link="logit")) #respons~explantory-1 to get parameters which are the group means
bolth<-glm(BoltedatH ~ Origin/Pop-1, data=h, family = binomial(link="logit"))       
summary(bolth)         
         
#contingency tables
         # 2-Way Frequency Table
#          attach(mydata)
#          mytable <- table(A,B) # A will be rows, B will be columns
#          mytable # print table
#          
#          margin.table(mytable, 1) # A frequencies (summed over B)
#          margin.table(mytable, 2) # B frequencies (summed over A)
#          
#          prop.table(mytable) # cell percentages
#          prop.table(mytable, 1) # row percentages
#          prop.table(mytable, 2) # column percentages 
# #########A     successes    failures    n
#          a         5           10      15
#          b        15           20      35
#          c        25           30      55
#          d        35           40      75
#          prop <- successes/n                # proportion of successes
#          z <- glm(prop ~ A, weights=n, family = binomial(link="logit"))
#          
# origin<-c("nat", "inv")
# No<-c(64,66)
# Yes<-c(70,28)
# tot<-c(134,94)
# boltsuccess<-data.frame(Origin=origin, No=No, Yes=Yes, tot=tot)
# prop<-boltsuccess$Yes/boltsuccess$tot
# boltmodel2<-glm(prop~boltsuccess$Origin-1, weights=boltsuccess$tot,family = binomial(link="logit"))
# summary(boltmodel2)
# anova(boltmodel2, test="Chi")
#class(boltsuccess$Yes)
#prop.table(boltsuccess)
#boltsuccess<-as.table(addmargins(boltsuccess, FUN = list(tot = sum), quiet = TRUE))
#margin.table(boltsuccess,1)
#boltsuccess$tot<-rowSums(boltsuccess, na.rm=TRUE, dims=1)      
#prop$boltsuccess<-boltsuccess$Yes/boltsuccess$tot
#class(boltsuccess)         
        
         A      freq    outcome
         a         5    success
         b        15    success
         c        25    success
         d        35    success
         a        10    failure
         b        20    failure
         c        30    failure
         d        40    failure
         z <- glm(freq ~ A * outcome, family = poisson(link="log"))
         summary(z)
         anova(z, test="Chi")

boltsuccess<-as.data.frame(table(h$Origin, h$BoltedatH))
boltsuccess         
boltmodel<-glm(Freq ~ Var1*Var2, data=boltsuccess, family = poisson(link="log"))
#family = quasipoisson(link = "log"))
summary(boltmodel)
anova(boltmodel, test="Chi")

#linear models...
# model<-lm(FrmHDKdatdes$LfCountH~FrmHDKdatdes$Origin+FrmHDKdatdes$Trt+FrmHDKdatdes$Origin*FrmHDKdatdes$Trt, na.rm=TRUE)
# anova(model)
# shapiro.test(resid(model))
# qqnorm(resid(model))
# qqline(resid(model))
# hist(resid(model), breaks="FD")
# 
# z2<-lm(LfCount1~Origin-1, data=d)
# plot(d$Origin, d$LfCount1, pch=as.numeric(d$Origin))
# plot(d$Origin, d$LfCount, main="Week 5", ylab = "leaf number", xlab="range and treatment")
#legend(locator(1), as.character(levels(Frm1DKdatdes$Trt)), pch=1:length(levels(Frm1DKdatdes$Trt)))
#groups<-levels(d$group)
#  for(i in 1:length(groups)){
#   +     xi<-Frm1DKdatdes$Trt[Frm1DKdatdes$Origin==groups[i]]            # grabs x-values for group i
#   +     yhati<-fitted(z2)[Frm1DKdatdes$Origin==groups[i]] # grabs yhat's for group i
#   +     lines(xi[order(xi)],yhati[order(xi)]) # connects the dots
#   + }
# plot(z2)

###linear mixed effects model, harvest
library(nlme)
z <- lme(y ~ 1, random= ~ 1 | B, data = mydata)

hmodel<-lme(LfLgth~Origin*Trt+BoltedatH, random= ~ 1| Pop, data = h)
summary(hmodel)
plot(hmodel)
intervals(hmodel)
fitted(hmodel)
VarCorr(hmodel)
anova(hmodel)



#plots w/ m1 lf count
plot(d$Origin, d$LfCount, main="Leaf No., Week 5", ylab = "Leaf number", xlab="Range", 
     varwidth=TRUE, lwd=3)

barplot(table(d$LfCount1,d$group), beside=TRUE, space=c(.1, .3)) #wtf?

boxplot(d$LfCount1 ~ d$group, varwidth=TRUE)

stripchart(d$LfCount1 ~ d$group, vertical=TRUE, method="jitter", jitter=0.2, pch=1)
points(c(1,2,3,4)+0.2, tapply(d$LfCount1, d$group, mean, na.rm=TRUE), pch=2, col="red")

interaction.plot(Frm1DKdatdes$Origin, Frm1DKdatdes$Trt, Frm1DKdatdes$LfCount1)

#m2 plots
plot(Frm2DKdatdes$Origin, Frm2DKdatdes$MaxLfWdth2, main="Leaf Width, Week 8", 
     ylab = "Leaf width (cm)", xlab="Range", varwidth=TRUE, lwd=3)

#harvest interaction plots
interaction.plot(h$BoltedatH, h$Origin, h$Rose.AreaH.m2, col=1:length(unique(h$Origin)), 
                 main="Rosette area interaction plot, harvest")
#col=1:length(unique(h$Origin), fun = function(x) mean(x, na.rm=TRUE), fun = mean, ylim=c(0,10), main="Root crown diameter interaction plot, harvest"
# summary(h)
# class(h$BoltedatH)
# h$Origin<-factor(h$Origin, levels=c("nat","inv"))
# h<-h[h$Origin!="sk",]
# levels(h$BoltedatH)
# unique(h$Origin)
# unique(Frm1DKdatdes$Trt)
# mean(h$CrownDiam.mm)

#interaction.plot(h$Trt, h$Origin, h$CrownDiam.mm, col=1:length(unique(h$Origin)), 
                 main="Root crown diameter, harvest", cex.main=2.5,cex.lab=1.5)
h$Trt<-factor(h$Trt, levels=c("control", "drought"))  
         
#tapply(h$CrownDiam.mm, h$Trt,mean,na.rm=TRUE)
#tapply(h$CrownDiam.mm, h$Trt,se)
#interaction.plot(interaction.plot(h$Trt, h$Origin, h$CrownDiam.mm, 
                  col=1:length(unique(h$Origin)), main="Root crown diameter, harvest"))

naty.se <- tapply(h[h$Origin=="nat",]$CrownDiam.mm, h[h$Origin=="nat",]$Trt,se)
naty.mean <- tapply(h[h$Origin=="nat",]$CrownDiam.mm, h[h$Origin=="nat",]$Trt, mean, na.rm=TRUE)
naty.mean + naty.se

invy.mean<- tapply(h[h$Origin=="inv",]$CrownDiam.mm, h[h$Origin=="inv",]$Trt,mean)
invy.se<- tapply(h[h$Origin=="inv",]$CrownDiam.mm, h[h$Origin=="inv",]$Trt,se)
invy.mean+invy.se
c(0, max(invy.mean+invy.se))
ylim <- c(0, max(invy.mean+invy.se))
interaction.plot(h$Trt, h$Origin, h$CrownDiam.mm, col=1:length(unique(h$Origin)), 
                main="Root crown, harvest", ylim=ylim, xlab="Treatment",
                 ylab="Mean root crown diameter (mm)", lwd=4, cex.main=2.5,cex.lab=1.5)
x <- 1:2
arrows(x, naty.mean - naty.se, x, naty.mean + naty.se,code=3, length=0.03, angle=90, lwd=4)
arrows(x, invy.mean - invy.se, x, invy.mean + invy.se,code=3, length=0.03, angle=90,col=2, lwd=4)

# stripchart(y ~ A, vertical=TRUE, method="jitter", pch=1)
# points( c(1,2,3,4), tapply(y, A, mean), pch=16))

# plot(h$Trt, h$CrownDiam.mm, type="n")
# lines(x[order(x)],y[order(x)])

# source("http://bioconductor.org/biocLite.R")
# biocLite("HH")
# library(HH)
# intxplot(CrownDiam.mm ~ Trt, groups=Origin, data=h, ylim=c(0,35), offset.scale=100, 
#          se=sqrt(var(CrownDiam.mm, na.rm=TRUE)/(length(na.omit(x))-1)), 
#          main.title=("Root crown diameter at harvest"), main.cex=2, xlab="Treatment", 
#          ylab="Root Crown Diameter (mm)", cex.lab =3, lwd=4, 
#          col.line=1:length(unique(h$Origin)), rug.use=FALSE)
# 
# panel.intxpot(CrownDiam.mm, Trt, groups=)
# max(h$CrownDiam.mm)
# sqrt(var(h$CrownDiam.mm, na.rm=TRUE)/(length(na.omit(x))-1))

#proportion bolted at harvest
mosaicplot(table(A,B),col=TRUE,las=2,cex.axis=0.8)

mosaicplot(h$Origin~h$BoltedatH, col=TRUE,
           las=2, cex.axis=1, main="Proportion bolted by harvest", cex.main=2.5, 
           ylab="Bolted at harvest", xlab="Range", cex.lab=1.5)
#1:length(unique(h$Origin))

##########################
testM<-as.matrix(cbind(c(7, 8, 9), c(12, 13,14), c(1,2,3)))
testM2<-mvrnorm(10, c(1,2,3,4,5), diag(5))
head(testM2)


#linear model
model<-lm(Frm1DKdatdes$LfCount1~Frm1DKdatdes$Origin+Frm1DKdatdes$Trt+Frm1DKdatdes$Origin*Frm1DKdatdes$Trt)
#model<-lm(Frm2datTag$lf.width~Frm2datTag$Origin-1) #removing intercept... not sure when/why to do this
summary(model)
plot(Frm1DKdatdes$Origin,Frm1DKdatdes$LfCount1, xlab="Origin",ylab="Leaf count", varwidth=TRUE )

abline(model, lty=2)

model<-lm(Frm1DKcont$LfCount1~Frm1DKcont$Origin)
shapiro.test(resid(model))
qqnorm(resid(model))
qqline(resid(model))
hist(resid(model), breaks="FD")
anova(model)

#interaction.plot(Frm1DKdatdes$Origin, Frm1DKdatdes$Trt, Frm1DKdatdes$LfCount1)
library(lattice)
barchart(~table(Frm1DKcont$LfCount1)|Frm1DKcont$Origin, data=Frm1DKcont)

####
#Transform datas!
#log transform continuous
#sqrt transform count data

#m1
d<-cbind(d,log(d$MaxLfLgth1),log(d$MaxLfWdth1))
colnames(d)[12]<-"LfLgth1.log"
colnames(d)[13]<-"LfWdth1.log"
d<-cbind(d,sqrt(d$LfCount1))
colnames(d)[14]<-"LfCount1.sq"

#m2
d2<-cbind(d2,log(d2$Rose.diam2),log(d2$MaxLfLgth2),log(d2$MaxLfWdth2))
colnames(d2)[11]<-"RoseD2.log"
colnames(d2)[12]<-"LfLgth2.log"
colnames(d2)[13]<-"LfWdth2.log"
d2<-cbind(d2,sqrt(d2$LfCount2))
colnames(d2)[14]<-"LfCount2.sq"

#mh - lf count not reliably collected
h<-cbind(h,log(h$Max.Rose.diamH),log(h$Min.Rose.diamH),log(h$Rose.AreaH.m2), log(h$LfLgth),log(h$Lf.Width),
         log(h$CrownDiam.mm), log(h$Shoot.mass.gH))
colnames(h)[19]<-"MaxRoseDh.log"
colnames(h)[20]<-"MinRoseDh.log"
colnames(h)[21]<-"RoseAh.log"
colnames(h)[22]<-"LfLgthH.log"
colnames(h)[23]<-"LfWdthH.log"
colnames(h)[24]<-"Crown.log"
colnames(h)[25]<-"Mass.log"



#write
write.table(d, file="Frm1DKdatdes.txt", sep="\t", quote=F)
write.table(d2, file="Frm2DKdatdes.txt", sep="\t", quote=F)
write.table(h, file="FrmHDKdatdes.txt", sep="\t", quote=F)








###################################################
#comparing continuous distributions
Frm2datTag$Trt[Frm2datTag$Trt==""]<-"control"
summary(Frm2datTag)
by(Frm2datTag$Trt, Frm2datTag$Origin, summary)
Frm2datTagnoSk<-Frm2datTag[!Frm2datTag$Origin=="sk",]
summary(Frm2datTagnoSk)
t.test(Frm2datTagnoSk$MaxLfWdth2 ~ Frm2datTagnoSk$Origin, 
       data=Frm2datTagnoSk$Trt[Frm2datTagnoSk$Trt=="control"])
wilcox.test(Frm2datTagnoSk$Rose.diam2 ~ Frm2datTagnoSk$Origin, 
            data=Frm2datTagnoSk$Trt[Frm2datTagnoSk$Trt=="control"])
library(ggplot2)
#ggplot(as.data.frame(Frm2datTagnoSk$Trt[Frm2datTagnoSk$Trt=="control"]), aes(Frm2datTagnoSk$Origin, Frm2datTagnoSk$Rose.diam2)) + geom_boxplot()
class(Frm2datTagnoSk$Rose.diam2)

# inspect the fake data
summary(df)
# generate five number summary and mean for each group
> by(df$outcome, df$group, summary)
# perform t-test for the difference of means
> t.test(outcome ~ group, data=df)
# perform two-sample Wilcoxon test
> wilcox.test(outcome ~ group, data=df)
# draw a box plot
> ggplot(df, aes(group, outcome)) + geom_boxplot()
# draw a histogram
> ggplot(df, aes(outcome)) + geom_histogram(binwidth=10) +
  + facet_wrap(~group, nrow=2, ncol=1)
# draw a density plot colored by group
> ggplot(df, aes(outcome,color=group)) + geom_density() 
# draw a frequency plot colored by group
> ggplot(df, aes(outcome,color=group)) + geom_freqpoly(binwidth=10)

#load required packages
library(ggplot2)
source("http://bioconductor.org/biocLite.R")
biocLite("tabplot")
library(tabplot)
# import data set
data(diamonds)
# make the plot
tableplot(Frm2datTag, sortCol = 3) #nBins = 100, 



###linear regression diagnostics
library(car)
outlierTest(model) # Bonferonni p-value for most extreme obs
#test for normality of residuals
model_resid<-studres(model)
hist(model_resid, freq=FALSE)
#test for homoscedasticity (non-constant error variance test)
ncvTest(model)

#produce Box-Cox power transformation parameters using log-likelihood values???? 
#Arguments: lambda, the power parameters and length of parameter; plotit, logical argument for whether to produce a graph. To 
boxcox(model, lambda= seq(-2, 2, 1/10), plotit=TRUE)
#confirm highest point on Box-Cox log-likelihood graph????
powerTransform(model, start=NULL)
#transform variables???
lf.width_trans<-(1/Frm2datTag$lf.width^1.5)

#####
#from semniar 5
#fitting linear model using limma
#include two factors in model (development stage and genotype)
mm <- model.matrix(~developmentStage + genotypeOrVariation, pData(eset))
fit <- lmFit(eset, mm)
#empirical bayes to borrow strength
fit <- eBayes(fit)

#examining results of model fit
#to get names limma gave the contrasts see:
#top results ranked by F-stat for model fit
topTable(fit)
#top signif diff expressed probe stes for genotype effect (i.e. one contrast)
topTable(fit, coef = "genotypeOrVariationNrl_deficient")
#top genes for with devolopmentStage factor account for most variance
topTable(fit, coef=c("developmentStageP2", "developmentStageP6", "developmentStageP10","developmentStage4_weeks"))
#top genes for one contrast out of many (btw named stage and reference/baseline)
topTable(fit, coef=c("developmentStageP2"))

#anova of a single probe... maybe
s5anova<-as(eset[1:10,], "data.frame")
head(s5anova)
is.factor(s5anova$genotypeOrVariation)
interaction.plot(s5anova$genotypeOrVariation, s5anova$developmentStage, s5anova$X1415670_at)
ResFullMod<-lm(s5anova$X1415670_at~s5anova$genotypeOrVariation+s5anova$developmentStage+s5anova$genotypeOrVariation:s5anova$developmentStage)
ResFullMod
anova(ResFullMod)
layout(matrix(c(1,2,3,4),2,2))
plot(ResFullMod)

#model selection - interaction effect important?
models <- list(devStageGenotype = model.matrix(~developmentStage + genotypeOrVariation,pData(eset)), devStageXGenotype = model.matrix(~developmentStage * genotypeOrVariation,pData(eset)))
sm <- selectModel(eset, models)
table(sm$pref) #sm is big! don't do head'

#setting contrasts
contrast.matrix <- contr.helmert(colnames(fit))[, -5]
colnames(contrast.matrix) <- levels(pData(eset)$developmentStage)[-1]
cfit <- contrasts.fit(fit, contrast.matrix)
cfit <- eBayes(cfit)
topTable(cfit)