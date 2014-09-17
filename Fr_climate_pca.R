##France CG - climate and lat/long PCA###

#add lat/long
#get population coordinates
# allpop <- read.table("Popcoord.txt", header=T, sep="\t") #Popcoord.txt !not wordlclim approximations
Frdes <- read.table("Frdes.txt", header=T, sep="\t")
# Frclim <- read.table("Frbioclimdata.txt", header=TRUE)
# Frpop <- allpop[allpop$Pop %in% Frdes$Pop,]
# rownames(Frpop) <- Frpop$Pop
# Frpop$Pop <- droplevels(Frpop$Pop)
# Frpop <- Frpop[,1:3]
# 
# Frclim$Pop <- row.names(Frclim)
# Frclimdat <- merge(Frclim, Frpop,all.x=TRUE)
# Frclimdat <- merge(Frclimdat, unique(Frdes[,c(5,7)]),all.x=TRUE)
# row.names(Frclimdat) <- Frclimdat$Pop
#OR
Frclimdat <- read.table("FrbioclimPCAdat.txt", header=TRUE) #climate table with PC1-4 included



#PCA fun times
#Origin and longitude artificially separates groups...
Frclim.pca <- prcomp(Frclimdat[2:22], center=TRUE, scale=TRUE)
summary(Frclim.pca)
# Importance of components:
#                          PC1    PC2    PC3    PC4    PC5     PC6     PC7
# Standard deviation     2.4945 2.3784 1.9951 1.5061 1.0420 0.93344 0.73651
# Proportion of Variance 0.2963 0.2694 0.1895 0.1080 0.0517 0.04149 0.02583
# Cumulative Proportion  0.2963 0.5657 0.7552 0.8632 0.9150 0.95644 0.98227

#visualize components
plot(Frclim.pca, main="Variances of each principle component of climate", xlab="Principal component", ylim=c(0,7))
# screeplot(Frclim.pca, type="lines")
# biplot(Frclim.pca)
#see bottom for figure

# variances of the principal components:
apply(Frclim.pca$x, 2, var)
# PC1          PC2          PC3          PC4          PC5          PC6          PC7 
# 6.222319e+00 5.656794e+00 3.980600e+00 2.268456e+00 1.085800e+00 8.713113e-01 5.424501e-01 
# PC8          PC9         PC10         PC11         PC12         PC13         PC14 
# 1.986467e-01 8.162214e-02 4.683574e-02 1.714412e-02 1.113305e-02 7.202734e-03 4.776770e-03 
# PC15         PC16         PC17         PC18         PC19         PC20         PC21 
# 2.367863e-03 1.469710e-03 5.929469e-04 3.777926e-04 5.993845e-05 4.056082e-05 2.955439e-32

biplot(Frclim.pca, var.axes=FALSE, main="PCA analysis of climate data")
biplot(Frclim.pca, var.axes=TRUE, main="PCA analysis of climate data", cex=c(1,2), col=c(Frclimdat$Origin,"red"))
biplot(Frclim.pca, var.axes=FALSE, main="PCA analysis of climate data", choices=c(1,3))

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
# bio11       bio6       bio4       bio9       bio1      bio19       bio7       bio3 
# 0.35561348 0.34405154 0.32821929 0.32780965 0.28576342 0.25315144 0.24798843 0.23885621 
# Latitude      bio18      bio16      bio15      bio13       bio8      bio12      bio14 
# 0.22882643 0.20459253 0.17627689 0.17079849 0.16539931 0.16311250 0.14120526 0.12468296 
# bio5      bio10      bio17        alt       bio2 
# 0.10843923 0.10654302 0.08999619 0.02125970 0.01044607 

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
# bio5       bio17       bio14       bio12       bio10        bio7        bio2 
# 0.365558016 0.359169391 0.343189009 0.329961839 0.259354158 0.257856811 0.246805130 
# bio13       bio16       bio19       bio18        bio4    Latitude       bio15 
# 0.246297501 0.239684519 0.234413896 0.197982641 0.170699151 0.169973427 0.129965360 
# bio1        bio9        bio3        bio6         alt        bio8       bio11 
# 0.110706298 0.088031679 0.067558509 0.055121674 0.028756031 0.019952580 0.005689587 

BIO5 = Max Temperature of Warmest Month
BIO12 = Annual Precipitation
BIO14 = Precipitation of Driest Month
BIO17 = Precipitation of Driest Quarter

#find top loadings (for PC3)
loadings3 <- Frclim.pca$rotation[,3]
sort(abs(loadings3), decreasing=TRUE)
# alt       bio2       bio1      bio10      bio15       bio3       bio8       bio6 
# 0.37124460 0.32319500 0.30504092 0.29604980 0.29138159 0.28473454 0.25856074 0.23958625 
# bio16      bio13      bio11       bio7      bio19      bio12      bio14      bio17 
# 0.22324272 0.21688301 0.21638487 0.19477620 0.17172262 0.15830856 0.13734286 0.13543037 
# Latitude      bio18       bio9       bio4       bio5 
# 0.06189877 0.06002142 0.05691771 0.03715797 0.03674185 

altitude
BIO1 = Annual Mean Temperature
BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
BIO10 = Mean Temperature of Warmest Quarter

#proportional contributions of each bioclim to each PC
#If you want this as a relative contribution then sum up the loadings per column and 
#express each loading as a proportion of the column (loading) sum, taking care to use 
#the absolute values to account for negative loadings.

sweep(abs(Frclim.pca$rotation),2, colSums(abs(Frclim.pca$rotation)),"/")
#                 PC1         PC2         PC3         PC4         PC5         PC6        PC7
# alt      0.00519412 0.007323155 0.091066713 0.029883229 0.145611854 0.121636450 0.02838832
# bio1     0.06981705 0.028193020 0.074826878 0.034655793 0.017984088 0.020759359 0.01243912
# bio10    0.02603034 0.066048428 0.072621344 0.068393274 0.007563910 0.010547462 0.07054704
# bio11    0.08688265 0.001448939 0.053079449 0.002045350 0.017351499 0.036492360 0.02752563
# bio12    0.03449894 0.084029733 0.038833266 0.058248313 0.005133899 0.001666514 0.06916862
# bio13    0.04040997 0.062723354 0.053201643 0.093821351 0.013798304 0.024226537 0.02190727
# bio14    0.03046225 0.087398230 0.033690357 0.025904070 0.056664936 0.082407192 0.05091982
# bio15    0.04172909 0.033097629 0.071476228 0.091262334 0.019079958 0.029936551 0.09121042
# bio16    0.04306756 0.061039259 0.054761687 0.091969855 0.009708941 0.023551224 0.01535396
# bio17    0.02198766 0.091467874 0.033221220 0.026128391 0.062620053 0.056803028 0.06296393
# bio18    0.04998557 0.050419250 0.014723322 0.092632060 0.067088013 0.110514000 0.02291785
# bio19    0.06184937 0.059697016 0.042123749 0.016953202 0.003330594 0.102314065 0.06358563
# bio2     0.00255216 0.062852629 0.079280094 0.006279525 0.126497539 0.071675834 0.00128328
# bio3     0.05835679 0.017204788 0.069845700 0.022253103 0.134974256 0.060320773 0.04237009
# bio4     0.08018977 0.043471100 0.009114891 0.044098529 0.019914523 0.055037966 0.08137944
# bio5     0.02649362 0.093094834 0.009012817 0.032745977 0.083665579 0.013665928 0.07373292
# bio6     0.08405787 0.014037561 0.058770774 0.012768158 0.006009599 0.010830548 0.02885084
# bio7     0.06058795 0.065667106 0.047778819 0.030148264 0.042247076 0.001966599 0.06797490
# bio8     0.03985126 0.005081224 0.063425238 0.125497736 0.001549413 0.010745086 0.02912205
# bio9     0.08008969 0.022418588 0.013961977 0.053000116 0.018582731 0.025835189 0.09321769
# Latitude 0.05590634 0.043286283 0.015183836 0.041311370 0.140623236 0.129067335 0.04514117
#....


#write table
write.table(Frclimdat2, file="FrbioclimPCAdat.txt")
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

#see top for loading all data

Frdes$colCode <- "F8766D"
Frdes[Frdes$Origin %in% "nat",]$colCode <- "00BA38"
Frdes[Frdes$Origin %in% "sk",]$colCode <- "619CFF"

# #ggplot2 version PC1 vs PC2
# #with pops labeled
# library("ggplot2")
# library("grid") 
# data <- data.frame(obsnames=row.names(Frclim.pca$x), Frclim.pca$x)
# data <- merge(data, unique(Frdes[,c(5,7,12)]),by.x="obsnames", by.y="Pop")
# 
# plot <- ggplot(data, aes_string(x="PC1", y="PC2")) + 
#   geom_text(size=5, aes(label=obsnames,color=colCode,fontface=2))+
#   scale_x_continuous(expand = c(0,1))
# plot
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

###PC1 vs PC2
#pts instead of labels for pops
# library("ggplot2")
library("grid") 
data <- data.frame(obsnames=row.names(Frclim.pca$x), Frclim.pca$x)
data <- merge(data, unique(Frdes[,c(5,7,12)]),by.x="obsnames", by.y="Pop")
levels(data$Origin)[levels(data$Origin)=="inv"] <- "Invasive C. diffusa"
levels(data$Origin)[levels(data$Origin)=="nat"] <- "Native C. diffusa"
levels(data$Origin)[levels(data$Origin)=="sk"] <- "Native C. stoebe"
# data$pch <- 15
# data[data$Origin %in% "nat",]$pch <- 16
# data[data$Origin %in% "sk",]$pch <- 17

pdf("KTurnerFig2.pdf", useDingbats=FALSE, width=4.4, height=4.8, pointsize = 12) #3.149, 4.4 or 6.65
# png("FrClimatePCA.png",width=800, height = 600, pointsize = 16)
# postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)

plot <- ggplot(data, aes_string(x="PC1", y="PC2")) + 
  geom_point(aes(shape=Origin, color=Origin), size=3) +
  #   scale_x_continuous(expand = c(0,1)) #+
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(1,0), 
        legend.title = element_text(size=7, face="bold"), 
        legend.text = element_text(size = 7),
        axis.title = element_text( size=7),
        axis.text  = element_text(size=5), axis.text.y= element_text(angle=0))

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
                                         size = 4, vjust=1, color="gray47", alpha=0.75)
plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), 
                            arrow=arrow(length=unit(0.2,"cm")), alpha=0.4, color="gray47")
plot
dev.off()

####PC1 vs PC3####
png("FrClimatePCA1v3.png",width=800, height = 600, pointsize = 16)
# postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)

plot <- ggplot(data, aes_string(x="PC1", y="PC3")) + 
  geom_point(aes(shape=Origin, color=Origin), size=5) +
  #   scale_x_continuous(expand = c(0,1)) #+
  theme(legend.justification=c(1,0), legend.position=c(1,0))

# plot

plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)

datapc <- data.frame(varnames=rownames(Frclim.pca$rotation), Frclim.pca$rotation)
mult <- min(
  (max(data[,"PC3"]) - min(data[,"PC3"])/(max(datapc[,"PC3"])-min(datapc[,"PC3"]))),
  (max(data[,"PC1"]) - min(data[,"PC1"])/(max(datapc[,"PC1"])-min(datapc[,"PC1"])))
)
datapc <- transform(datapc,
                    v1 = .7 * mult * (get("PC1")),
                    v2 = .7 * mult * (get("PC3"))
)

plot <- plot  +coord_equal(ratio=6.1/4)+ geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), 
                                         size = 6, vjust=1, color="gray47", alpha=0.75)

plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), 
                            arrow=arrow(length=unit(0.2,"cm")), alpha=0.4, color="gray47")
plot
dev.off()


##component variances supp. fig####
# pdf("KTurnerFig2.pdf", useDingbats=FALSE, width=13.38)
png("FrClimatePCA_var.png",width=800, height = 600, pointsize = 16)
# postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)

plot(Frclim.pca, main="Variances of each principle component of climate", xlab="Principal component", ylim=c(0,7))
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