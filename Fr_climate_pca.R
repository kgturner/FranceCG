##France CG - climate and lat/long PCA###

#add lat/long
#get population coordinates
allpop <- read.table("Popcoord.txt", header=T, sep="\t") #Popcoord.txt !not wordlclim approximations
Frdes <- read.table("Frdes.txt", header=T, sep="\t")
Frpop <- allpop[allpop$Pop %in% Frdes$Pop,]
rownames(Frpop) <- Frpop$Pop
Frpop$Pop <- droplevels(Frpop$Pop)
Frpop <- Frpop[,1:3]

Frclim$Pop <- row.names(Frclim)
Frclimdat <- merge(Frclim, Frpop,all.x=TRUE)
Frclimdat <- merge(Frclimdat, Frdes[,c(5,7)],all.x=TRUE)
row.names(Frclimdat) <- Frclimdat$Pop

#PCA fun times
Frclim.pca <- prcomp(Frclimdat[2:23], center=TRUE, scale=TRUE)
summary(Frclim.pca)
# Importance of components:
#                           PC1    PC2    PC3    PC4     PC5     PC6     PC7
# Standard deviation     2.5058 2.3797 2.1050 1.5462 1.14469 0.93389 0.77103
# Proportion of Variance 0.2854 0.2574 0.2014 0.1087 0.05956 0.03964 0.02702
# Cumulative Proportion  0.2854 0.5428 0.7442 0.8529 0.91247 0.95211 0.97914

#visualize components
plot(Frclim.pca)
biplot(Frclim.pca, var.axes=FALSE, main="PCA analysis of climate data")
biplot(Frclim.pca, var.axes=TRUE, main="PCA analysis of climate data", cex=c(1,2), col=c(Frclimdat$Origin,"red"))
# biplot(Frclim.pca, var.axes=FALSE, main="PCA analysis of climate data", choices=1:2, scale=1)

#see bottom for figure

#get top 4 PCs
PC1 <- as.matrix(Frclim.pca$x[,1])
PC2 <- as.matrix(Frclim.pca$x[,2])
PC3 <- as.matrix(Frclim.pca$x[,3])
PC4 <- as.matrix(Frclim.pca$x[,4])
Frclimdat2 <- cbind(Frclimdat, PC1, PC2, PC3, PC4)

#find top loadings (for PC1)
loadings <- Frclim.pca$rotation[,1]
sort(abs(loadings), decreasing=TRUE)
#       bio11        bio9        bio6        bio4        bio1        bio3       bio19        bio7 
# 0.340912515 0.332758344 0.326315353 0.322945390 0.266800012 0.262227179 0.254227563 0.227854928 
#   Latitude       bio18       bio15        bio8       bio16       bio13       bio14       bio12 
# 0.220288560 0.213953960 0.186201310 0.184740915 0.176494679 0.164047194 0.138930963 0.135513808 
#       bio5   Longitude       bio17       bio10        bio2         alt 
# 0.115751459 0.112019618 0.105026831 0.090877697 0.041357958 0.001561833 

BIO4 = Temperature Seasonality (standard deviation *100)
BIO6 = Min Temperature of Coldest Month
BIO9 = Mean Temperature of Driest Quarter
BIO11 = Mean Temperature of Coldest Quarter
#profound or mundane? bio7, bio19, bio4 also top three loadings for kay's ragweed.
#hahaha or bug in code!

# Frclimdat2 <- cbind(Frclimdat2, Frclimdat$bio4, Frclimdat$bio19, Frclimdat$bio7)

#find top loadings (for PC2)
loadings2 <- Frclim.pca$rotation[,2]
sort(abs(loadings2), decreasing=TRUE)
#       bio5      bio17      bio14      bio12       bio7       bio2      bio13      bio10      bio16 
# 0.35997025 0.35814999 0.34097909 0.33161588 0.27124791 0.25704538 0.24785737 0.24481185 0.24102295 
#     bio19      bio18       bio4   Latitude      bio15       bio1       bio9       bio6       bio3 
# 0.23807320 0.19295129 0.18111880 0.15672227 0.13026160 0.09110723 0.07874105 0.07353062 0.06975585 
#  Longitude        alt       bio8      bio11 
# 0.03920001 0.03870989 0.02445978 0.01259052

BIO5 = Max Temperature of Warmest Month
BIO12 = Annual Precipitation
BIO14 = Precipitation of Driest Month
BIO17 = Precipitation of Driest Quarter

#proportional contributions of each bioclim to each PC
#If you want this as a relative contribution then sum up the loadings per column and 
#express each loading as a proportion of the column (loading) sum, taking care to use 
#the absolute values to account for negative loadings.

sweep(abs(Frclim.pca$rotation),2, colSums(abs(Frclim.pca$rotation)),"/")
#                   PC1         PC2         PC3          PC4         PC5          PC6          PC7
# alt       0.0003700317 0.009726293 0.080241106 0.0002862402 0.136541025 0.1278128350 0.0113478423
# bio1      0.0632106477 0.022891707 0.078607769 0.0197688472 0.032485009 0.0207426481 0.0080142155
# bio10     0.0215308765 0.061511709 0.077904121 0.0529325300 0.025750881 0.0107554990 0.0560709160
# bio11     0.0807694899 0.003163509 0.055810108 0.0076301154 0.025825855 0.0367513497 0.0256461059
# bio12     0.0321061289 0.083322189 0.030246598 0.0639083589 0.007402877 0.0009263328 0.0640894387
# bio13     0.0388663002 0.062276928 0.039953199 0.0994800838 0.004540169 0.0237631698 0.0149642109
# bio14     0.0329157262 0.085674801 0.020899014 0.0355625489 0.044405426 0.0796467270 0.0594965680
# bio15     0.0441150857 0.032729680 0.054284938 0.0994125720 0.002882290 0.0278806752 0.0889688938
# bio16     0.0418153767 0.060559705 0.042324740 0.0961729617 0.013779842 0.0241376604 0.0136096546
# bio17     0.0248831098 0.089989180 0.021229573 0.0353264673 0.045935055 0.0533400401 0.0720805482
# bio18     0.0506902840 0.048481164 0.009960104 0.0752581763 0.084748060 0.1095400238 0.0154371401
# bio19     0.0602319648 0.059818548 0.035355612 0.0047388288 0.022094921 0.1048296893 0.0640415598
# bio2      0.0097985876 0.064585518 0.075106530 0.0114835372 0.085156190 0.0652519646 0.0349710045
# bio3      0.0621272455 0.017526937 0.065607410 0.0164990538 0.087410183 0.0535887000 0.0008687706
# bio4      0.0765126926 0.045508118 0.008321502 0.0448817539 0.016522566 0.0553113710 0.0684137402
# bio5      0.0274240044 0.090446541 0.015102487 0.0265584306 0.062781020 0.0087702754 0.0852951457
# bio6      0.0773111090 0.018475389 0.059906567 0.0213585749 0.013082718 0.0112977951 0.0275192990
# bio7      0.0539837218 0.068154063 0.045332597 0.0343578629 0.023982701 0.0051759384 0.0733621660
# bio8      0.0437690870 0.006145793 0.063810874 0.1041215792 0.041087821 0.0086414085 0.0403542468
# bio9      0.0788375918 0.019784568 0.007171714 0.0427338314 0.015725496 0.0292312529 0.0986024888
# Latitude  0.0521910868 0.039378219 0.030363654 0.0489836923 0.101534551 0.1362725193 0.0162156998
# Longitude 0.0265398512 0.009849439 0.082459782 0.0585439532 0.106325346 0.0063321248 0.0606303448

#....


#write table
write.table(Frclimdat, file="FrbioclimPCAdat.txt")
# 
Frclimdat <- read.table("FrbioclimPCAdat.txt", header=TRUE)

##########figure#######
#nat and inv and sk colors:"#F8766D","#00BFC4"
library(ggplot2)
n <- 3 #number of variables or colors
hcl(h=seq(15, 375-360/n, length=n)%%360, c=100, l=65)
# "#F8766D" "#00BA38" "#619CFF"
origincol <- c("#F8766D", "#00BA38", "#619CFF")
# Frclimdat$colCode <- "#F8766D"
# Frclimdat[Frclimdat$Origin %in% "nat",]$colCode <- "#00BA38"
# Frclimdat[Frclimdat$Origin %in% "sk",]$colCode <- "#619CFF"
Frdes$colCode <- "F8766D"
Frdes[Frdes$Origin %in% "nat",]$colCode <- "00BA38"
Frdes[Frdes$Origin %in% "sk",]$colCode <- "619CFF"

#ggplot2 version
#with pops labeled
library("ggplot2")
library("grid") 
# data <- data.frame(obsnames=row.names(Frclim.pca$x), Frclim.pca$x)
# data <- merge(data, unique(Frdes[,c(5,7,12)]),by.x="obsnames", by.y="Pop")
# 
# plot <- ggplot(data, aes_string(x="PC1", y="PC2")) + 
#   geom_text(size=5, aes(label=obsnames,color=colCode,fontface=2))+
#   scale_x_continuous(expand = c(0,1))
# 
# plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
# datapc <- data.frame(varnames=rownames(Frclim.pca$rotation), Frclim.pca$rotation)
# mult <- min(
#   (max(data[,"PC2"]) - min(data[,"PC2"])/(max(datapc[,"PC2"])-min(datapc[,"PC2"]))),
#   (max(data[,"PC1"]) - min(data[,"PC1"])/(max(datapc[,"PC1"])-min(datapc[,"PC1"])))
# )
# datapc <- transform(datapc,
#                     v1 = .7 * mult * (get("PC1")),
#                     v2 = .7 * mult * (get("PC2"))
# )
# # plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), 
# #                                          size = 5, vjust=1, color="gray47", alpha=0.6)
# plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), 
#                             arrow=arrow(length=unit(0.2,"cm")), alpha=0.4, color="gray47")
# plot

#pts instead of labels for pops
data <- data.frame(obsnames=row.names(Frclim.pca$x), Frclim.pca$x)
data <- merge(data, unique(Frdes[,c(5,7,12)]),by.x="obsnames", by.y="Pop")
levels(data$Origin)[levels(data$Origin)=="inv"] <- "Invasive C. diffusa"
levels(data$Origin)[levels(data$Origin)=="nat"] <- "Native C. diffusa"
levels(data$Origin)[levels(data$Origin)=="sk"] <- "Native C. stoebe"
# data$pch <- 15
# data[data$Origin %in% "nat",]$pch <- 16
# data[data$Origin %in% "sk",]$pch <- 17

# pdf("KTurnerFig2.pdf", useDingbats=FALSE, width=13.38)
png("FrClimatePCA.png",width=800, height = 600, pointsize = 16)
# postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)

plot <- ggplot(data, aes_string(x="PC1", y="PC2")) + 
  geom_point(aes(shape=Origin, color=Origin), size=5) +
  #   scale_x_continuous(expand = c(0,1)) #+
  theme(legend.justification=c(1,0), legend.position=c(1,0))

# plot

plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
datapc <- data.frame(varnames=rownames(Frclim.pca$rotation), Frclim.pca$rotation)
mult <- min(
  (max(data[,"PC2"]) - min(data[,"PC2"])/(max(datapc[,"PC2"])-min(datapc[,"PC2"]))),
  (max(data[,"PC1"]) - min(data[,"PC1"])/(max(datapc[,"PC1"])-min(datapc[,"PC1"])))
)
datapc <- transform(datapc,
                    v1 = .7 * mult * (get("PC1")),
                    v2 = .7 * mult * (get("PC2"))
)

plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), 
                                         size = 6, vjust=1, color="gray47", alpha=0.75)
plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), 
                            arrow=arrow(length=unit(0.2,"cm")), alpha=0.4, color="gray47")
plot
dev.off()





################
#remove PC1....?
# #Problem 3
# 
# #make eSet from imputed data
# #Relevel factors
# hw2des$TumorType <- factor(hw2des$TumorType, c("GIST", "LEIO", "LIPO","MFH", "Schwannoma","Synovial"))
# hw2des$ArrayType <- relevel(hw2des$ArrayType, "22K")
# #Make both files have the same ordering
# hw2datImp <- hw2datImp[,row.names(hw2des)]
# all(colnames(hw2datImp)==row.names(hw2des))
# # make eSet
# hw2esetImp <- new("ExpressionSet", phenoData = as(hw2des, "AnnotatedDataFrame"),exprs = as.matrix(hw2datImp))
# #order eSet by array and tumor type
# hw2esetImp<-hw2esetImp[,order( hw2des$ArrayType, hw2des$TumorType )]
# 
# #PCA
# hw2pcs<-prcomp(exprs(hw2esetImp), center=F, scale=F)
# 
# # append the rotations for the first 10 PCs to the phenodata
# pData(hw2esetImp)<- cbind(pData(hw2esetImp), hw2pcs$rotation[sampleNames(hw2esetImp),1:10])
# 
# 
# # plot data on first two PCs, colored by array type
# plot(pData(hw2esetImp)[,c("PC1","PC2")], bg=pData(hw2esetImp)$ArrayType, pch=21, cex=2, main= "PC1 and PC2 by Array Type" )
# legend(  list(x=0.2,y=0.3), as.character(levels(pData(hw2esetImp)$ArrayType)), x="topright", pch =21, pt.bg=c(1,2,3,4,5))
# 
# #Remove PC1
# #Remove the first component as described in lecture. use the svd command. 
# hw2svd<-svd(hw2datImp)
# summary(hw2svd)
# hw2svdD <- diag(hw2svd$d)
# hw2svdcorr<-hw2svd$u[,-1] %*% hw2svd$d[-1,-1] %*% t(hw2svd$v[,-1])
# dim(hw2svdcorr)
# dim(hw2datImp)
# 
# #change column names
# colnames(hw2svdcorr)<-row.names(hw2des)
# #change rownames
# rownames(hw2svdcorr)<-row.names(hw2datImp)
# 
# #replace NAs
# hw2svdcorrNA<-hw2svdcorr
# namatrix<-is.na(hw2dat)
# class(namatrix)
# dim(namatrix)
# head(namatrix)
# hw2svdcorrNA[namatrix==TRUE]<-NA
# 
# sum(is.na(hw2dat))
# sum(is.na(hw2svdcorrNA))
# 
# #filter probes
# #retain only if at least two values of log2(3)=1.584962500721156 or more in the row. How many probes are left? (2 points)
# hw2corrF<-as.data.frame(hw2svdcorrNA)
# #hw2corrF<-hw2corrF[-which(rowSums(hw2corrF>=log2(3))>=2),]
# hw2corrF<-hw2corrF[which(rowSums(abs(hw2corrF)>=log2(3), na.rm=TRUE)>=2),]
# 
# dim(hw2corrF)
# hw2corrF <- hw2corrF[,row.names(hw2des)]
# all(colnames(hw2corrF)==row.names(hw2des))