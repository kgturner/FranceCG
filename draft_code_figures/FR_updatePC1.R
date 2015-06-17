#France data format

#update PCA data 5/22/14

library(plyr)

#with SK
#for long data formating, see FrSKdata_format.R
#read
Frdatsk.l<- read.table("FrTraitClimDat_SK_long.txt", header=T, sep="\t",quote='"', row.names=1)
#read
FrdatSK<- read.table("FrTraitClimDat_SK.txt", header=T, sep="\t",quote='"', row.names=1)
#read
frend<- read.table("FrEnd.txt", header=T, sep="\t",quote='"', row.names=1)

Frclimdat <- read.table("FrbioclimPCAdat.txt", header=TRUE)


frend2 <- frend[,1:32]
frend2 <- merge(frend2, Frclimdat[,c(1,25:28)], all.x=TRUE)
#write
write.table(frend2, file="FrEnd.txt",sep="\t", quote=F)

Frdatsk.l2 <- Frdatsk.l[,c(1:20,25:31)]
Frdatsk.l2 <- merge(Frdatsk.l2, Frclimdat[,c(1,25:28)], all.x=TRUE)
#write
write.table(Frdatsk.l2, file="FrTraitClimDat_SK_long.txt",sep="\t", quote=F)

FrdatSK2 <- FrdatSK[,c(1:34, 39:56)]
FrdatSK2 <- merge(FrdatSK2, Frclimdat[,c(1,25:28)], all.x=TRUE)
#write
write.table(FrdatSK2, file="FrTraitClimDat_SK.txt",sep="\t", quote=F)
