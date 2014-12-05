#France data format

#update PCA data for cline version of ms, use DK only 12/3/14

library(plyr)


#for long data formating, see FrSKdata_format.R
#read
Frdatsk.l<- read.table("FrTraitClimDat_SK_long.txt", header=T, sep="\t",quote='"', row.names=1)
#read
FrdatSK<- read.table("FrTraitClimDat_SK.txt", header=T, sep="\t",quote='"', row.names=1)
#read
frend<- read.table("FrEnd.txt", header=T, sep="\t",quote='"', row.names=1)

Frclimdat.dk <- read.table("FrbioclimPCA_DKdat.txt", header=TRUE)


frend2 <- frend[,1:25]
frend2 <- subset(frend2, Origin%in%c("inv", "nat"))
frend2 <- merge(frend2, Frclimdat.dk[,c(1,2,22:27)], all.x=TRUE)
#write
write.table(frend2, file="FrEnd_cline.txt",sep="\t", quote=F)

Frdatsk.l2 <- Frdatsk.l[,c(1:13,21:27)]
Frdatsk.l2 <- subset(Frdatsk.l2, Origin%in%c("inv", "nat"))
Frdatsk.l2 <- merge(Frdatsk.l2, Frclimdat.dk[,c(1,2,22:27)], all.x=TRUE)
#write
write.table(Frdatsk.l2, file="FrTraitClimDat_cline_long.txt",sep="\t", quote=F)

FrdatSK2 <- FrdatSK[,c(1:7,11,16:22,25:26, 50)]
FrdatSK2 <- subset(FrdatSK2, Origin%in%c("inv", "nat"))
FrdatSK2 <- merge(FrdatSK2, Frclimdat.dk[,c(1,2,22:27)], all.x=TRUE)
#write
write.table(FrdatSK2, file="FrTraitClimDat_cline.txt",sep="\t", quote=F)
