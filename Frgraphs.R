#graphing

d <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1) #measure1 Frm1DKdatdes.txt
d2<-read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1)#measure 2 Frm2DKdatdes.txt
h <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1) #measure harvest FrmHDKdatdes.txt

#plots of pop means from means table
head(PopMeansMh)
Hcont<-PopMeansMh[PopMeansMh$Trt=="control",]
plot(Hcont$Pop, Hcont$MassH)
title(main="Shoot mass at harvest", sub="Control treatment",
      xlab="Population", ylab="mass(g)") 
text(Hcont$Pop, Hcont$MassH, Hcont$Pop, cex=0.6, pos=4, col="red")

#plots of pop means from data, grouped by pop, trt
library("gplot")
library("ggplot2")

str(h)
unique(h$Pop)
h$Pop<-factor(h$Pop, c("CA001","CA008","CA009","CA010", "US001", "US002","US003", "BG001","GR001","GR002","GR003","HU001","RO001", "RO005","RU008","TR001","UA004"))
print(levels(h$Pop))

png(filename="FrmassMeans.png", width=800, bg="white")
p <- ggplot(data=h, aes(Pop, Shoot.mass.gH, fill=Trt)) + 
  geom_boxplot()  
plot(p)
dev.off()

png(filename="FrcrownMeans.png", width=800, bg="white")
p <- ggplot(data=h, aes(Pop, CrownDiam.mm, fill=Trt)) + 
  geom_boxplot()  
plot(p)
dev.off()

str(d)
unique(d$Pop)
d$Pop<-factor(d$Pop, c("CA001","CA008","CA009","CA010", "US001", "US002","US003", "BG001","GR001","GR002","GR003","HU001","RO001", "RO005","RU008","TR001","UA004"))
print(levels(d$Pop))

png(filename="FrDlfMeans.png", width=800, bg="white")
p <- ggplot(data=d, aes(Pop, MaxLfLgth1)) + 
  geom_boxplot()  
plot(p)
dev.off()

#barplot with se bars
#harvest control shoot mass
se <- function(x) sqrt(var(x, na.rm=TRUE)/(length(na.omit(x))-1))

Hcont2<-h[h$Trt=="control",]
tapply(Hcont2$Shoot.mass.gH, Hcont2$Pop,mean,na.rm=TRUE)
tapply(Hcont2$Shoot.mass.gH, Hcont2$Pop,se)
plt <- barplot(tapply(Hcont2$Shoot.mass.gH, Hcont2$Pop,mean,na.rm=TRUE), ylim=c(0, 30))
y.se <- tapply(Hcont2$Shoot.mass.gH, Hcont2$Pop,se)
y.mean <- tapply(Hcont2$Shoot.mass.gH, Hcont2$Pop, mean, na.rm=TRUE)
# y.mean + y.se
# max(y.mean + y.se)
# c(0, max(y.mean + y.se, na.rm=TRUE))
ylim <- c(0, max(y.mean + y.se, na.rm=TRUE))

png(filename="Frmassbar.png", width=800, bg="white")
x<- barplot(y.mean,ylim=ylim, main="Shoot mass at harvest, control", col="blue")
arrows(x, y.mean - y.se, x, y.mean + y.se,code=3, length=0.03, angle=90)
dev.off()
#axis(1, at=1:17, lab=Hcont$Pop)

#overall
tapply(h$Shoot.mass.gH, h$Pop,mean,na.rm=TRUE)
tapply(h$Shoot.mass.gH, h$Pop,se)
plt <- barplot(tapply(h$Shoot.mass.gH, h$Pop,mean,na.rm=TRUE), ylim=c(0, 30))
y.se <- tapply(h$Shoot.mass.gH, h$Pop,se)
y.mean <- tapply(h$Shoot.mass.gH, h$Pop, mean, na.rm=TRUE)
# y.mean + y.se
# max(y.mean + y.se)
c(0, max(y.mean + y.se, na.rm=TRUE))
ylim <- c(0, max(y.mean + y.se, na.rm=TRUE))
x<- barplot(y.mean,ylim=ylim, main="Shoot mass at harvest, control", col="blue", beside=TRUE)
arrows(x, y.mean - y.se, x, y.mean + y.se,code=3, length=0.03, angle=90)


#########

#summary

summary(d)
dpop<-as.data.frame(d)
dpop<-dpop[order(dpop$Origin, decreasing=FALSE),]

dpop$Pop <- factor(dpop$Pop, c("", "", "","", ""))

plot(dpop$Pop)

plot(sort(PopMeansM1$Latitude))
#axis(1, at=1:17, lab=as.vector(PopMeansM1$Pop))

plot(PopMeansM1$Latitude)
plot(PopMeansM1$Pop,PopMeansM1$Latitude,col=ifelse(PopMeansM1$Latitude==3,"red", "black"))
#col=ifelse(PopMeansM1$Origin=="inv", "red", "black")

plot(PopMeansM1$Latitude)
# > axis(1, at=1:17, lab=as.vector(PopMeansM1$Pop))

# > PopMeansM1$Origin<-factor(PopMeansM1$Origin)
# > PopMeansM1$col[PopMeansM1$Origin=="inv"]<-"red"
#PopMeansM1$col[PopMeansM1$Origin=="nat"]<-"black"
# > dotchart(PopMeansM1$Latitude, labels=PopMeansM1$Pop, groups=PopMeansM1$Origin, color=PopMeansM1$col)
# > dotchart(PopMeansM1$Latitude, labels=PopMeansM1$Pop, color=PopMeansM1$col)
# > dotchart(sort(PopMeansM1$Latitude), labels=PopMeansM1$Pop, color=PopMeansM1$col)
# > dotchart(order(PopMeansM1$Latitude), labels=PopMeansM1$Pop, color=PopMeansM1$col)


# summary(Frm1DKdatdes[Frm1DKdatdes$Origin=="nat"])
# 
# source("http://bioconductor.org/biocLite.R")
# biocLite("psych")
# library(psych)
# describe.by(Frm1DKdatdes$LfCount1, Frm1DKdatdes$Origin)

#library(doBy)
#summaryBy(mpg + wt ~ cyl + vs, data = mtcars,FUN = function(x) { c(m = mean(x), s = sd(x)) } )
# produces mpg.m wt.m mpg.s wt.s for each
# combination of the levels of cyl and vs 

tapply(Frm1DKcont$LfCount1, INDEX = Frm1DKcont$Origin, FUN = mean, na.rm=TRUE)
tapply(Frm1DKcont$LfCount1, Frm1DKcont$Origin, sd, na.rm = TRUE)
tapply(Frm1DKdatdes$LfCount1, INDEX = list(Frm1DKdatdes$Origin,Frm1DKdatdes$Trt), 
       FUN = mean, na.rm=TRUE)
# #barplots
barplot(agdatm1$x, main="Leaf Count- m 1",names.arg=paste(agdatm1$Group.1,agdatm1$Group.2),
        col="blue", axis.lty=1, xlab="groups", ylab="lf count") 

# aggregate data frame returning means
# for numeric variables

agdatm1 <-aggregate(Frm1DKdatdes$LfCount1, by=list(Frm1DKdatdes$Origin,Frm1DKdatdes$Trt) ,FUN=mean, na.rm=TRUE)
print(agdatm1)

#barplot with se bars
#harvest root crown
h <- FrmHDKdatdes
head(h)
h$group <- paste(h$Origin, h$Trt)
class(h$group)
h$group <- factor(h$group, levels=c("nat control","inv control","nat drought","inv drought"))
tapply(h$CrownDiam.mm, h$group,mean,na.rm=TRUE)
se <- function(x) sqrt(var(x, na.rm=TRUE)/(length(na.omit(x))-1))
tapply(h$CrownDiam.mm, h$group,se)
plt <- barplot(tapply(h$CrownDiam.mm, h$group,mean,na.rm=TRUE), ylim=c(0, 30))
plt
y.se <- tapply(h$CrownDiam.mm, h$group,se)
y.mean <- tapply(h$CrownDiam.mm, h$group, mean, na.rm=TRUE)
y.mean + y.se
c(0, max(y.mean + y.se))
ylim <- c(0, max(y.mean + y.se))
x<- barplot(y.mean,ylim=ylim, main="Root crown diameter at harvest", col="blue")
arrows(x, y.mean - y.se, x, y.mean + y.se,code=3, length=0.03, angle=90)

#m1 lf count
d <- Frm1DKdatdes
d$Origin<-factor(d$Origin, levels=c("nat","inv"))
tapply(d$LfCount1, d$Origin,mean,na.rm=TRUE)
se <- function(x) sqrt(var(x, na.rm=TRUE)/(length(na.omit(x))-1))
tapply(d$LfCount1, d$Origin,se)
barplot(tapply(d$LfCount1, d$Origin,mean,na.rm=TRUE), ylim=c(0, 10))
plt <- barplot(tapply(d$LfCount1, d$Origin,mean,na.rm=TRUE), ylim=c(0, 10))
plt
y.se <- tapply(d$LfCount1, d$Origin,se)
y.mean <- tapply(d$LfCount1, d$Origin, mean, na.rm=TRUE)
y.mean + y.se
c(0, max(y.mean + y.se))
ylim <- c(0, max(y.mean + y.se))
plt<- barplot(y.mean,ylim=ylim, main="Leaf No., week 5",cex.main=2.5, 
              col=1:length(unique(Frm2DKcont$Origin)),xlab="Range", ylab="Leaf number",
              cex.lab=1.5)
arrows(plt, y.mean - y.se, plt, y.mean + y.se,code=3, length=0.03, angle=90)

#m2 lf width
Frm2DKcont$Origin<-factor(Frm2DKcont$Origin,levels=c("nat", "inv"))

tapply(Frm2DKcont$MaxLfWdth2, Frm2DKcont$Origin,mean,na.rm=TRUE)
se <- function(x) sqrt(var(x, na.rm=TRUE)/(length(na.omit(x))-1))
tapply(Frm2DKcont$MaxLfWdth2, Frm2DKcont$Origin,se)
barplot(tapply(Frm2DKcont$MaxLfWdth2, Frm2DKcont$Origin,mean,na.rm=TRUE), ylim=c(0, 10))
plt <- barplot(tapply(Frm2DKcont$MaxLfWdth2, Frm2DKcont$Origin,mean,na.rm=TRUE), ylim=c(0, 10))
plt
y.se <- tapply(Frm2DKcont$MaxLfWdth2, Frm2DKcont$Origin,se)
y.mean <- tapply(Frm2DKcont$MaxLfWdth2, Frm2DKcont$Origin, mean, na.rm=TRUE)
y.mean + y.se
c(0, max(y.mean + y.se))
ylim <- c(0, 5)
# Frm2DKcont$color[Frm2DKcont$Origin=="inv"]<-"red"
# Frm2DKcont$color[Frm2DKcont$Origin=="nat"]<-"black"
plt<- barplot(y.mean,ylim=ylim, main="Leaf width, week 8 ",  
              col=1:length(unique(Frm2DKcont$Origin)), xlab="Range", ylab="Leaf width (cm)",
              cex.main=2.5,cex.lab=1.5)
arrows(plt, y.mean - y.se, plt, y.mean + y.se,code=3, length=0.03, angle=90)


#Grouped and colored dot plot
#Group and color data by genotype
Frm1DKdatdes<-Frm1DKdatdes[order(Frm1DKdatdes$Origin),]
Frm1DKdatdes$Origin<-factor(Frm1DKdatdes$Origin)
Frm1DKdatdes$color[Frm1DKdatdes$Origin=="inv"]<-"red"
Frm1DKdatdes$color[Frm1DKdatdes$Origin=="nat"]<-"black"
# Frm2datTag$color[Frm2datTag$Origin=="sk"]<-"blue"
# 
par(mar=c(5,6,4,2)+0.1,mgp=c(7,1,0))
dotchart(Frm1DKdatdes$LfCount1, ylab="indiv", xlab="lf number",cex=.7,labels=row.names(Frm1DKdatdes),groups= Frm1DKdatdes$Origin,main="lf number by origin", gcolor="black", color=Frm1DKdatdes$color)
mtext("lf number", side=1,line=4)

# #lf length
# 
# par(mar=c(5,6,4,2)+0.1,mgp=c(7,1,0))
# dotchart(Frm2datTag$lf.length, ylab="indiv", xlab="lf number",cex=.7,labels=row.names(Frm2datTag),groups= Frm2datTag$Origin,main="lf number by origin", gcolor="black", color=Frm2datTag$color)
# mtext("lf length", side=1,line=4)
# 
# #lf width
# class(Frm2datTag$lf.width)
# Frm2datTag$lf.width<-as.numeric(Frm2datTag$lf.width)
# dotchart(Frm2datTag$lf.width, ylab="indiv", xlab="lf number",cex=.7,labels=row.names(Frm2datTag),groups= Frm2datTag$Origin,main="lf number by origin", gcolor="black", color=Frm2datTag$color)
# mtext("lf width", side=1,line=4)
# 
# #rosette diameter
# class(Frm2datTag$rosette.diam)
# Frm2datTag$rosette.diam<-as.numeric(Frm2datTag$rosette.diam)
# dotchart(Frm2datTag$rosette.diam, ylab="indiv", xlab="lf number",cex=.7,labels=row.names(Frm2datTag),groups= Frm2datTag$Origin,main="lf number by origin", gcolor="black", color=Frm2datTag$color)
# mtext("rosette diam", side=1,line=4)
# 

# #avg
# class()
# m2means<-as.data.frame(aggregate(Frm2Imp$lf.number, list(Frm2Imp$Origin) , mean))
# m2means$lf.number <- aggregate(Frm2Imp$lf.number, list(Frm2Imp$Origin) , mean)
# m2means$lf.width <- aggregate(Frm2Imp$lf.width, list(Frm2Imp$Origin) , mean)
# m2means$lf.length <- aggregate(Frm2Imp$lf.length, list(Frm2Imp$Origin) , mean)
# m2means$rosette.diam <- aggregate(Frm2Imp$rosette.diam, list(Frm2Imp$Origin) , mean)
# m2means
# #names(m2means) <- c('dnase.conc', 'dens.avg')
# 
# 
# plot(m2means$Group.1, m2means$x)
