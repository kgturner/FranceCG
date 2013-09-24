#mixed-effects models of univariate traits
#lots of climate variables! Including climate PCA
#Sept 2013

#make big ol' data table
#france data
d <- read.table("Frm1DKdatdes.txt", header=T, sep="\t",quote='"', row.names=1) #measure1 
d2<-read.table("Frm2DKdatdes.txt", header=T, sep="\t",quote='"', row.names=1)#measure 2 
h <- read.table("FrmHDKdatdes.txt", header=T, sep="\t",quote='"', row.names=1) #measure harvest 

# #remove small pops (<3)
# summary(d$Pop)
# d<-d[d$Pop!="CA008",]
# d<-d[d$Pop!="GR003",]
# d<-d[d$Pop!="UA004",]
# summary(d2$Pop)
# d2<-d2[d2$Pop!="CA008",]
# d2<-d2[d2$Pop!="GR003",]
# d2<-d2[d2$Pop!="UA004",]
# summary(h$Pop)
# h<-h[h$Pop!="CA008",]
# h<-h[h$Pop!="GR003",]
# h<-h[h$Pop!="UA004",]
# 
# #write
# write.table(d, file="Frm1DKdatdes.txt", sep="\t", quote=F)
# write.table(d2, file="Frm2DKdatdes.txt", sep="\t", quote=F)
# write.table(h, file="FrmHDKdatdes.txt", sep="\t", quote=F)

#load climate table
Frclimdat2 <- read.table("FrbioclimPCAdat.txt", header=TRUE)

#get rid of traitPC1
d <- d[,-15]
d2 <- d2[,-15]
h <- h[,-26]

#merge all the things!
frdat <- merge(d, d2,h,Frclimdat2[,c(1,13,16,19,22:27)])