
#for just B3 (Mf)
#BG3 42.1153  23.3203
BG3<-SpatialPoints(as.matrix(t(c(23.3203,42.1153)))) #long, lat

tile16tiffvector<-c(alt_16.tif, bio1_16.tif,bio2_16.tif,bio3_16.tif,bio4_16.tif,bio5_16.tif,bio6_16.tif,bio7_16.tif,bio8_16.tif,bio9_16.tif,bio10_16.tif,bio11_16.tif,bio12_16.tif,bio13_16.tif,bio14_16.tif,bio15_16.tif,bio16_16.tif,bio17_16.tif,bio18_16.tif,bio19_16.tif)

valuevectorBG3<-NULL

#for loops, tile16
for(map in tile16tiffvector){
  myValue<-extract(map, BG3)
  valuevectorBG3<-c(valuevectorBG3, myValue)
}


#check length of all vectors
length(valuevectorBG3)

BG3bioclim<-as.data.frame(rbind(valuevectorBG3))
#change col names
colnames(BG3bioclim)<-c("alt", "bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19" )

#change rownames
rownames(BG3bioclim)<-"BG3"

BG3bioclim$Pop <- "BG3"
BG3bioclim$Latitude <- "42.1153"
BG3bioclim$Longitude <- "23.3203"
BG3bioclim$Origin <- "sk"

Mfbioclim <- merge(Frclimdat[1:24],BG3bioclim, all=TRUE)
Mfbioclim$Latitude <- as.numeric(Mfbioclim$Latitude)
Mfbioclim$Longitude <- as.numeric(Mfbioclim$Longitude)
row.names(Mfbioclim) <- Mfbioclim$Pop

#only pops used in Mf
Mfbioclim <- subset(Mfbioclim, Pop%in%c(levels(Mf$Pop)))
Mfbioclim <- droplevels(Mfbioclim)

##Mf - climate and lat/long PCA###

#PCA fun times
#Origin and longitude artificially separates groups...
Mfclim.pca <- prcomp(Mfbioclim[c(2:22)], center=TRUE, scale=TRUE)
summary(Mfclim.pca)
# Importance of components:
#                           PC1    PC2    PC3     PC4     PC5     PC6    PC7     PC8
# Standard deviation     2.8660 2.4800 1.8483 1.36766 0.78224 0.69456 0.4072 0.25846
# Proportion of Variance 0.3911 0.2929 0.1627 0.08907 0.02914 0.02297 0.0079 0.00318
# Cumulative Proportion  0.3911 0.6840 0.8467 0.93577 0.96490 0.98788 0.9958 0.99895

#visualize components
plot(Mfclim.pca, main="Variances of each principle component of climate", xlab="Principal component", ylim=c(0,7))
# screeplot(Frclim.pca, type="lines")
# biplot(Frclim.pca)
#see bottom for figure

# variances of the principal components:
apply(Mfclim.pca$x, 2, var)
# PC1          PC2          PC3          PC4          PC5          PC6 
# 8.213882e+00 6.150429e+00 3.416264e+00 1.870493e+00 6.119004e-01 4.824200e-01 
# PC7          PC8          PC9         PC10         PC11 
# 1.658040e-01 6.680188e-02 1.263456e-02 9.371928e-03 3.868380e-32

biplot(Frclim.pca, var.axes=FALSE, main="PCA analysis of climate data")
biplot(Frclim.pca, var.axes=TRUE, main="PCA analysis of climate data", cex=c(1,2), col=c(Frclimdat$Origin,"red"))
biplot(Frclim.pca, var.axes=FALSE, main="PCA analysis of climate data", choices=c(1,3))

#see bottom for figure

#get top 4 PCs
PC1 <- as.matrix(Mfclim.pca$x[,1])
PC2 <- as.matrix(Mfclim.pca$x[,2])
PC3 <- as.matrix(Mfclim.pca$x[,3])
PC4 <- as.matrix(Mfclim.pca$x[,4])
Mfclim <- cbind(Mfbioclim, PC1, PC2, PC3, PC4)

#find top loadings (for PC1)
loadings <- Mfclim.pca$rotation[,1]
sort(abs(loadings), decreasing=TRUE)
# bio5      bio17      bio14      bio12       bio2      bio13       bio7 
# 0.33121802 0.31525621 0.30470940 0.29909050 0.26485660 0.25916822 0.25761070 
# bio10      bio18      bio16       bio9       bio8       bio1      bio19 
# 0.25345748 0.25295116 0.24956682 0.21593836 0.20185968 0.18546257 0.16153033 
# bio4      bio15       bio3      bio11        alt   Latitude       bio6 
# 0.15919497 0.11096349 0.10382910 0.09112818 0.07137618 0.05846632 0.03601961 
BIO5 = Max Temperature of Warmest Month
BIO12 = Annual Precipitation
BIO14 = Precipitation of Driest Month
BIO17 = Precipitation of Driest Quarter
#loadings for Pc1 and PC2 flipped, compared to FR

#find top loadings (for PC2)
loadings2 <- Mfclim.pca$rotation[,2]
sort(abs(loadings2), decreasing=TRUE)
# bio19        bio6        bio4       bio11        bio8        bio9 
# 0.342910812 0.339626194 0.325802302 0.325424528 0.251580425 0.248940239 
# bio3         alt       bio16        bio7       bio18       bio13 
# 0.245064801 0.238996085 0.224511346 0.221696831 0.221599960 0.208641304 
# bio1       bio12       bio15    Latitude       bio14       bio17 
# 0.176903551 0.169882472 0.164804817 0.159598365 0.073273773 0.043920551 
# bio5        bio2       bio10 
# 0.015076371 0.008386781 0.001635389 

BIO4 = Temperature Seasonality (standard deviation *100)
BIO6 = Min Temperature of Coldest Month
BIO11 = Mean Temperature of Coldest Quarter
BIO19 = Precipitation of Coldest Quarter

#find top loadings (for PC3)
loadings3 <- Mfclim.pca$rotation[,3]
sort(abs(loadings3), decreasing=TRUE)
# bio1       bio3      bio15       bio2      bio10   Latitude      bio11 
# 0.37549371 0.36215219 0.33376735 0.32534754 0.31962771 0.30454162 0.28376845 
# bio6      bio14        alt      bio17       bio7      bio16      bio13 
# 0.28349296 0.18174275 0.16586009 0.15465993 0.15349145 0.12880758 0.11176569 
# bio19       bio8       bio5       bio4      bio12      bio18       bio9 
# 0.08214716 0.07257525 0.04959296 0.03685157 0.03360935 0.02484793 0.01078104 

BIO15 = Precipitation Seasonality (Coefficient of Variation)
BIO1 = Annual Mean Temperature
BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
BIO3 = Isothermality (BIO2/BIO7) (* 100)

#proportional contributions of each bioclim to each PC
#If you want this as a relative contribution then sum up the loadings per column and 
#express each loading as a proportion of the column (loading) sum, taking care to use 
#the absolute values to account for negative loadings.

sweep(abs(Mfclim.pca$rotation),2, colSums(abs(Mfclim.pca$rotation)),"/")
#                 PC1          PC2         PC3         PC4         PC5         PC6
# alt      0.017060729 0.0596256425 0.043705771 0.129921183 0.079351996 0.015718344
# bio1     0.044330285 0.0441345634 0.098946299 0.033399334 0.029511189 0.015644329
# bio10    0.060582803 0.0004080029 0.084225056 0.056862883 0.055756875 0.037188802
# bio11    0.021781960 0.0811881356 0.074775787 0.001806665 0.005166473 0.007700607
# bio12    0.071490258 0.0423829182 0.008856394 0.008903772 0.076541094 0.067221505
# bio13    0.061947816 0.0520526174 0.029451362 0.047037452 0.094893720 0.006461296
# bio14    0.072833318 0.0182806165 0.047891008 0.038651936 0.032791633 0.067449694
# bio15    0.026523106 0.0411161258 0.087950993 0.089753375 0.053117352 0.120859847
# bio16    0.059652836 0.0560119352 0.033942068 0.046260434 0.084372897 0.022007184
# bio17    0.075354277 0.0109574644 0.040754417 0.043583368 0.014965214 0.066560354
# bio18    0.060461780 0.0552855917 0.006547674 0.063356558 0.047007638 0.059127915
# bio19    0.038609870 0.0855506795 0.021646586 0.022755104 0.035941329 0.030092552
# bio2     0.063307484 0.0020923656 0.085732287 0.018081212 0.012394451 0.078198795
# bio3     0.024817804 0.0611396885 0.095430678 0.042946800 0.002345059 0.027158417
# bio4     0.038051657 0.0812823841 0.009710751 0.054538775 0.065704402 0.035973643
# bio5     0.079169555 0.0037613097 0.013068233 0.024480231 0.037012069 0.092120276
# bio6     0.008609605 0.0847312207 0.074703195 0.002409863 0.012738184 0.003934198
# bio7     0.061575529 0.0553097594 0.040446511 0.019214205 0.040363920 0.081283936
# bio8     0.048249613 0.0627652310 0.019124295 0.083126401 0.018399604 0.030782824
# bio9     0.051614775 0.0621065474 0.002840909 0.079138335 0.050205987 0.037798651
# Latitude 0.013974942 0.0398172005 0.080249723 0.093772111 0.151418916 0.096716832#....
# ...

#write table
write.table(Mfclim, file="MfbioclimPCAdat.txt")
# 
Mfclim <- read.table("MfbioclimPCAdat.txt", header=TRUE)

##########figure#######
#nat and inv and sk colors:"#F8766D","#00BFC4"
library("ggplot2")
library("grid") 

###PC1 vs PC2
#pts instead of labels for pops
data <- data.frame(obsnames=row.names(Mfclim.pca$x), Mfclim.pca$x)
data <- merge(data, Mfclim[c(1,24)],by.x="obsnames", by.y="Pop")
levels(data$Origin)[levels(data$Origin)=="inv"] <- "Invasive C. diffusa"
levels(data$Origin)[levels(data$Origin)=="nat"] <- "Native C. diffusa"
levels(data$Origin)[levels(data$Origin)=="sk"] <- "Native C. stoebe"

# pdf("KTurnerFig2.pdf", useDingbats=FALSE, width=13.38)
png("MfClimatePC1vPC2.png",width=800, height = 600, pointsize = 16)
# postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)

plot <- ggplot(data, aes_string(x="PC1", y="PC2")) + 
  geom_point(aes(shape=Origin, color=Origin), size=5) +
  #   scale_x_continuous(expand = c(0,1)) #+
  theme(legend.justification=c(1,0), legend.position=c(1,0))

# plot

plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
datapc <- data.frame(varnames=rownames(Mfclim.pca$rotation), Mfclim.pca$rotation)
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

##PC1 vs PC3
png("MfClimatePCA1v3.png",width=800, height = 600, pointsize = 16)
# postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)

plot <- ggplot(data, aes_string(x="PC1", y="PC3")) + 
  geom_point(aes(shape=Origin, color=Origin), size=5) +
  #   scale_x_continuous(expand = c(0,1)) #+
  theme(legend.justification=c(1,0), legend.position=c(1,0))

# plot

plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)

datapc <- data.frame(varnames=rownames(Mfclim.pca$rotation), Mfclim.pca$rotation)
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
png("MfClimatePCA_var.png",width=800, height = 600, pointsize = 16)
# postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)

plot(Mfclim.pca, main="Variances of each principle component of climate", xlab="Principal component", ylim=c(0,7))
dev.off()
