##France CG - climate and lat/long PCA###
#DK only

library("ggplot2")
library("grid") 
library("gridBase")

Frclimdat <- read.table("FrbioclimPCAdat.txt", header=TRUE) #climate table with PC1-4 included
#DK only
Frclimdat.dk <- droplevels(subset(Frclimdat, Origin%in%c("inv", "nat"), select=1:24))

#OH GOD DAMN IT. REMOVE EXTRANEOUS POPS WITH INSUFFICIENT GERMINATION
# #subset from larger occurrence dataset
# #load table
# allclim <- read.table("Cdif_allocc_bioclimdata.txt", header=TRUE)

#match to pops used in trait data
#read
Frdatcline<- read.table("FrTraitClimDat_cline.txt", header=T)
expops <- levels(Frdatcline$Pop)
Frclimdat.dk <- droplevels(subset(Frclimdat.dk, Pop%in%expops))


####PCA fun times####
#Origin and longitude artificially separates groups...
FrclimDK.pca <- prcomp(Frclimdat.dk[c(2:22)], center=TRUE, retx=T, scale.=TRUE)
summary(FrclimDK.pca)
# Importance of components:
#                           PC1    PC2    PC3     PC4     PC5     PC6
# Standard deviation     2.6436 2.4092 2.2165 1.34140 0.98280 0.60484
# Proportion of Variance 0.3328 0.2764 0.2339 0.08568 0.04599 0.01742
# Cumulative Proportion  0.3328 0.6092 0.8431 0.92880 0.97479 0.99221

#visualize components
plot(FrclimDK.pca, main="(a) Screeplot, Montpellier Experiment", xlab="Principal component", ylim=c(0,7))
# screeplot(Frclim.pca, type="lines")
biplot(FrclimDK.pca)
biplot(FrclimDK.pca,  main="PCA analysis of climate data", choices=c(1,3))
#see bottom for figure

# variances of the principal components:
apply(FrclimDK.pca$x, 2, var)
# PC1          PC2          PC3          PC4          PC5          PC6          
# 6.988432e+00 5.804037e+00 4.912901e+00 1.799352e+00 9.658928e-01 3.658346e-01

#find top loadings (for PC1)
loadings <- FrclimDK.pca$rotation[,1]
sort(abs(loadings), decreasing=TRUE)
# bio5      bio12      bio13      bio16      bio17      bio14      bio10      bio19       bio7   Latitude      bio18 
# 0.34715140 0.33894826 0.31602877 0.30443769 0.29706295 0.27626843 0.27582810 0.26788752 0.22105836 0.21433073 0.20231115 
# bio1       bio2       bio4       bio9      bio11       bio6        alt       bio3      bio15       bio8 
# 0.18573689 0.17691133 0.16815355 0.13576150 0.09138876 0.04103954 0.02731012 0.01983365 0.01398861 0.00447920 
BIO5 = Max Temperature of Warmest Month
BIO12 = Annual precipitation
BIO13 = Precipitation of wettest month
BIO16 = Precipitation of wettest quarter

#find top loadings (for PC2)
loadings2 <- FrclimDK.pca$rotation[,2]
sort(abs(loadings2), decreasing=TRUE)
# bio6      bio11       bio1       bio4       bio7        alt   Latitude      bio19      bio18      bio10       bio9 
# 0.40641183 0.39805675 0.32475260 0.31860577 0.30349130 0.30058036 0.20786868 0.20192831 0.18392141 0.18123486 0.17107571 
# bio16       bio2      bio13      bio12      bio15       bio3      bio14       bio8       bio5      bio17 
# 0.15336282 0.13948588 0.13433600 0.12627875 0.09409075 0.08752913 0.06974677 0.06082398 0.05495018 0.04426379 
BIO6 = Min Temperature of Coldest Month
BIO11 = Mean Temperature of Coldest Quarter
BIO1 = Annual Mean Temperature
BIO4 = Temperature seasonality (standard deviation*100)

#find top loadings (for PC3)
loadings3 <- FrclimDK.pca$rotation[,3]
sort(abs(loadings3), decreasing=TRUE)
# bio3       bio2       bio8       bio9      bio15      bio18      bio19      bio10      bio14        alt   Latitude 
# 0.43297960 0.34960493 0.33207007 0.30599987 0.28300816 0.25050285 0.21843834 0.21439030 0.20783338 0.19816714 0.18500539 
# bio4      bio17       bio1      bio16      bio13       bio7       bio5       bio6      bio12      bio11 
# 0.17083761 0.16936803 0.16119704 0.13246742 0.10038198 0.09605245 0.06812094 0.05418166 0.05274364 0.04603406 
BIO3 = Isothermality (BIO2/BIO7)*100
BIO2 = Mean diurnal temperature range (mean of monthly (max temp – min temp))
BIO8 = Mean temperature of wettest quarter
BIO9 = Mean Temperature of Driest Quarter

#proportional contributions of each bioclim to each PC
#If you want this as a relative contribution then sum up the loadings per column and 
#express each loading as a proportion of the column (loading) sum, taking care to use 
#the absolute values to account for negative loadings.

sweep(abs(FrclimDK.pca$rotation),2, colSums(abs(FrclimDK.pca$rotation)),"/")
#                 PC1        PC2        PC3         PC4         PC5
# alt      0.006956369 0.07585058 0.04918049 0.050342676 0.088122132
# bio1     0.047310453 0.08195038 0.04000537 0.005967265 0.029940322
# bio10    0.070258270 0.04573409 0.05320671 0.004423015 0.053536304
# bio11    0.023278324 0.10044847 0.01142459 0.020211745 0.005678002
# bio12    0.086336085 0.03186608 0.01308975 0.009611622 0.072143592
# bio13    0.080498088 0.03389930 0.02491248 0.068362865 0.055263973
# bio14    0.070370429 0.01760040 0.05157943 0.096718855 0.032714681
# bio15    0.003563146 0.02374353 0.07023607 0.159624616 0.015387999
# bio16    0.077545635 0.03870066 0.03287534 0.062928207 0.053257458
# bio17    0.075667159 0.01116984 0.04203322 0.092322963 0.049331928
# bio18    0.051532210 0.04641204 0.06216901 0.071521823 0.078905133
# bio19    0.068235663 0.05095602 0.05421134 0.026158372 0.017089107
# bio2     0.045062428 0.03519886 0.08676385 0.012919352 0.046033689
# bio3     0.005051980 0.02208772 0.10745551 0.013383919 0.022076871
# bio4     0.042831666 0.08039924 0.04239794 0.032880890 0.047404794
# bio5     0.088425568 0.01386652 0.01690604 0.006133363 0.073819010
# bio6     0.010453492 0.10255685 0.01344663 0.022583372 0.005542243
# bio7     0.056307452 0.07658515 0.02383799 0.014594928 0.059083355
# bio8     0.001140931 0.01534875 0.08241210 0.134011517 0.036352873
# bio9     0.034580843 0.04317046 0.07594208 0.089586187 0.063218344
# Latitude 0.054593808 0.05245506 0.04591405 0.005712448 0.095098190

#get top 4 PCs
PC1 <- as.matrix(FrclimDK.pca$x[,1])
PC2 <- as.matrix(FrclimDK.pca$x[,2])
PC3 <- as.matrix(FrclimDK.pca$x[,3])
# PC4 <- as.matrix(Frclim.pca$x[,4])
Frclimdat.dk2 <- cbind(Frclimdat.dk, PC1, PC2, PC3)

#orienting
head(subset(Frclimdat.dk2, PC1< -2))
head(subset(Frclimdat.dk2, PC1> 2))

head(subset(Frclimdat.dk2, PC2< 2))
head(subset(Frclimdat.dk2, PC2> 2))

#write table
write.table(Frclimdat.dk2, file="FrbioclimPCA_DKdat.txt")
# 
Frclimdat.dk <- read.table("FrbioclimPCA_DKdat.txt", header=TRUE)

####95% conf limits of clusters####
scores <- FrclimDK.pca$x[,1:3]                        # scores for first three PC's

# k-means clustering [assume 2 clusters]
km     <- kmeans(scores, centers=2, nstart=10)
ggdata <- data.frame(scores, Cluster=km$cluster, Origin=Frclimdat.dk$Origin, alt=Frclimdat.dk$alt,Pop=Frclimdat.dk$Pop,Latitude=Frclimdat.dk$Latitude)

# stat_ellipse is not part of the base ggplot package
source("https://raw.github.com/low-decarie/FAAV/master/r/stat-ellipse.R") 

#PC1 vs PC2
plot <- ggplot(ggdata, aes_string(x="PC1", y="PC2")) +
  geom_point(aes(color=factor(Origin), shape=Origin), size=5) +  #,
  stat_ellipse(aes(x=PC1,y=PC2),  #,fill=factor(Cluster)
               geom="polygon", level=0.95, alpha=0.2) +
  guides(color=guide_legend("Origin"),fill=guide_legend("Origin"))
plot

# plot <- ggplot(data, aes_string(x="PC1", y="PC2")) + 
  #   geom_point(aes(shape=Origin, color=Origin), size=3) +
  #   #   scale_x_continuous(expand = c(0,1)) #+
  #   theme_bw() +
  #   theme(legend.justification=c(1,0), legend.position=c(1,0), 
  #         legend.title = element_text(size=7, face="bold"), 
  #         legend.text = element_text(size = 7),
  #         axis.title = element_text( size=7),
  #         axis.text  = element_text(size=5), axis.text.y= element_text(angle=0))
  # 
  # # plot
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
# 
# plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), 
#                                          size = 4, vjust=1, color="gray47", alpha=0.75)
# plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), 
#                             arrow=arrow(length=unit(0.2,"cm")), alpha=0.4, color="gray47")
# plot

#PC1 vs PC3
plot <- ggplot(ggdata, aes_string(x="PC1", y="PC3")) +
  geom_point(aes(color=factor(Cluster),shape=Origin), size=5) +
  stat_ellipse(aes(x=PC1,y=PC3,fill=factor(Cluster)),
               geom="polygon", level=0.95, alpha=0.2) +
  guides(color=guide_legend("Cluster"),fill=guide_legend("Cluster"))
plot

#pc1 vs altitude
plot <- ggplot(ggdata, aes_string(x="PC1", y="alt")) +
  geom_point(aes(color=factor(Cluster),shape=Origin), size=5) +
  stat_ellipse(aes(x=PC1,y=alt,fill=factor(Cluster)),
               geom="polygon", level=0.95, alpha=0.2) +
  guides(color=guide_legend("Cluster"),fill=guide_legend("Cluster"))
plot

#pc1 vs latitude
plot <- ggplot(ggdata, aes_string(x="PC1", y="Latitude")) +
  geom_point(aes(color=factor(Cluster),shape=Origin), size=5) +
  stat_ellipse(aes(x=PC1,y=Latitude,fill=factor(Cluster)),
               geom="polygon", level=0.95, alpha=0.2) +
  guides(color=guide_legend("Cluster"),fill=guide_legend("Cluster"))
plot

####exploratory clustering####
library(devtools)
install_github("dgrtwo/broom")
library(broom)

library(dplyr)

kclusts <- data.frame(k=1:9) %>% group_by(k) %>% do(km=kmeans(scores, .$k))
clusters <- kclusts %>% group_by(k) %>% do(tidy(.$km[[1]]))
assignments <- kclusts %>% group_by(k) %>% do(augment(.$km[[1]], scores))
clusterings <- kclusts %>% group_by(k) %>% do(glance(.$km[[1]]))

p1 <- ggplot(assignments, aes(PC1, PC2)) + geom_point(aes(color=.cluster)) + facet_wrap(~ k)
p1
p2 <- ggplot(assignments, aes(PC1, PC3)) + geom_point(aes(color=.cluster)) + facet_wrap(~ k)
p2
p3 <- p1 + geom_point(data=clusters, size=10, shape="x")
p3


# ##########figure#######
# #nat and inv and sk colors:"#F8766D","#00BFC4"
# library(ggplot2)
# n <- 3 #number of variables or colors
# hcl(h=seq(15, 375-360/n, length=n)%%360, c=100, l=65)
# # "#F8766D" "#00BA38" "#619CFF"
# origincol <- c("#F8766D", "#00BA38", "#619CFF")
# # Frclimdat$colCode <- "#F8766D"
# # Frclimdat[Frclimdat$Origin %in% "nat",]$colCode <- "#00BA38"
# # Frclimdat[Frclimdat$Origin %in% "sk",]$colCode <- "#619CFF"
# 
# #see top for loading all data
# 
# Frdes$colCode <- "F8766D"
# Frdes[Frdes$Origin %in% "nat",]$colCode <- "00BA38"
# Frdes[Frdes$Origin %in% "sk",]$colCode <- "619CFF"
# 
# # #ggplot2 version PC1 vs PC2
# # #with pops labeled
# # library("ggplot2")
# # library("grid") 
# # data <- data.frame(obsnames=row.names(Frclim.pca$x), Frclim.pca$x)
# # data <- merge(data, unique(Frdes[,c(5,7,12)]),by.x="obsnames", by.y="Pop")
# # 
# # plot <- ggplot(data, aes_string(x="PC1", y="PC2")) + 
# #   geom_text(size=5, aes(label=obsnames,color=colCode,fontface=2))+
# #   scale_x_continuous(expand = c(0,1))
# # plot
# # 
# # plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
# # datapc <- data.frame(varnames=rownames(Frclim.pca$rotation), Frclim.pca$rotation)
# # mult <- min(
# #   (max(data[,"PC2"]) - min(data[,"PC2"])/(max(datapc[,"PC2"])-min(datapc[,"PC2"]))),
# #   (max(data[,"PC1"]) - min(data[,"PC1"])/(max(datapc[,"PC1"])-min(datapc[,"PC1"])))
# # )
# # datapc <- transform(datapc,
# #                     v1 = .7 * mult * (get("PC1")),
# #                     v2 = .7 * mult * (get("PC2"))
# # )
# # # plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), 
# # #                                          size = 5, vjust=1, color="gray47", alpha=0.6)
# # plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), 
# #                             arrow=arrow(length=unit(0.2,"cm")), alpha=0.4, color="gray47")
# # plot
# 
####Main fig 2: PC1 vs PC2 fig####
#pts instead of labels for pops
# library("ggplot2")
library("grid") 
data <- data.frame(obsnames=row.names(FrclimDK.pca$x), FrclimDK.pca$x)
data <- merge(data, Frclimdat.dk[,c(1,24)],by.x="obsnames", by.y="Pop")
levels(data$Origin)[levels(data$Origin)=="inv"] <- "Invasive C. diffusa"
levels(data$Origin)[levels(data$Origin)=="nat"] <- "Native C. diffusa"
# levels(data$Origin)[levels(data$Origin)=="sk"] <- "Native C. stoebe"
# data$pch <- 15
# data[data$Origin %in% "nat",]$pch <- 16
# data[data$Origin %in% "sk",]$pch <- 17

# pdf("KTurnerFig2.pdf", useDingbats=FALSE, width=4.4, height=4.8, pointsize = 12) #3.149, 4.4 or 6.65
# png("FrClimatePCA.png",width=800, height = 600, pointsize = 16)
# postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)

plot <- ggplot(data, aes_string(x="PC1", y="PC2")) + 
  geom_point(aes(shape=Origin, color=Origin), size=3) +
  #   scale_x_continuous(expand = c(0,1)) #+
  theme_bw() +
  theme(legend.justification=c(1.05,0), legend.position=c(1.05,0), 
        legend.title = element_text(size=9, face="bold"), 
        legend.text = element_text(size = 9),
        axis.title = element_text( size=9),
        axis.text  = element_text(size=7), axis.text.y= element_text(angle=0))

# plot

plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
datapc <- data.frame(varnames=rownames(FrclimDK.pca$rotation), FrclimDK.pca$rotation)
mult <- min(
  (max(data[,"PC2"]) - min(data[,"PC2"])/(max(datapc[,"PC2"])-min(datapc[,"PC2"]))),
  (max(data[,"PC1"]) - min(data[,"PC1"])/(max(datapc[,"PC1"])-min(datapc[,"PC1"])))
)
datapc <- transform(datapc,
                    v1 = .7 * mult * (get("PC1")),
                    v2 = .7 * mult * (get("PC2"))
)

plot <- plot + coord_equal(ratio=5/4) #+ 
#   geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 4, vjust=1, color="gray47", alpha=0.75)
plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), 
                            arrow=arrow(length=unit(0.2,"cm")), alpha=0.4, color="gray47")
plot
# dev.off()
ggsave("KTurnerFig2.pdf", width=4.4, height=4.8, pointsize = 12)
ggsave("KTurnerFig2.png", width=4.4, height=4.8, pointsize = 12)

svg("KTurnerFig2.svg", width=4.4, height=4.8, pointsize = 12)
plot
dev.off()

####sup fig S1b: PC1 vs PC2 more details####
#clustering
scores <- FrclimDK.pca$x[,1:3]                        # scores for first three PC's

# k-means clustering [assume 2 clusters]
km     <- kmeans(scores, centers=2, nstart=10)
ggdata <- data.frame(scores, Cluster=km$cluster, Frclimdat.dk)

# stat_ellipse is not part of the base ggplot package
source("https://raw.github.com/low-decarie/FAAV/master/r/stat-ellipse.R") 

## pdf("KTurnerFig2.pdf", useDingbats=FALSE, width=4.4, height=4.8, pointsize = 12) #3.149, 4.4 or 6.65
# png("FrClimatePCA.png",width=800, height = 600, pointsize = 16)
# postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)

S1plot <- ggplot(ggdata, aes_string(x="PC1", y="PC2")) + 
  geom_point(aes(color=factor(Origin), shape=Origin), size=3) +
  stat_ellipse(aes(x=PC1,y=PC2),geom="polygon", level=0.95, alpha=0.2) +
  guides(color=guide_legend("Origin"),fill=guide_legend("Origin"))+
  theme_bw() + 
  theme(legend.position="none",
                 axis.title = element_text( size=9),
                 axis.text  = element_text(size=7), axis.text.y= element_text(angle=0))

# S1plot

S1plot <- S1plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
datapc <- data.frame(varnames=rownames(FrclimDK.pca$rotation), FrclimDK.pca$rotation)
mult <- min(
  (max(data[,"PC2"]) - min(data[,"PC2"])/(max(datapc[,"PC2"])-min(datapc[,"PC2"]))),
  (max(data[,"PC1"]) - min(data[,"PC1"])/(max(datapc[,"PC1"])-min(datapc[,"PC1"])))
)
datapc <- transform(datapc,
                    v1 = .7 * mult * (get("PC1")),
                    v2 = .7 * mult * (get("PC2"))
)

S1plot <- S1plot + coord_equal(ratio=4.4/4) + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), 
                                                  size = 6, vjust=1, color="black", alpha=0.75)
S1plot <- S1plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), 
                            arrow=arrow(length=unit(0.2,"cm")), alpha=0.4, color="black")+
  ggtitle("(b)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
S1plot
# dev.off()
# ggsave("KTurnerFig2.pdf", width=4.4, height=4.8, pointsize = 12)
# ggsave("KTurnerFig2.png", width=4.4, height=4.8, pointsize = 12)
# 
# svg("KTurnerFig2.svg", width=4.4, height=4.8, pointsize = 12)
# plot
# dev.off()
# 
# ####PC1 vs PC3 fig for ppt####
# png("FrClimatePCA_forppt.png",width=800, height = 600, pointsize = 26)
# 
# plot <- ggplot(data, aes_string(x="PC1", y="PC2")) + 
#   geom_point(aes(shape=Origin, color=Origin), size=7) +
#   #   scale_x_continuous(expand = c(0,1)) #+
#   theme_bw() +
#   theme(legend.justification=c(1,0), legend.position=c(1,0), 
#         legend.title = element_text(size=12, face="bold"), 
#         legend.text = element_text(size = 12),
#         axis.title = element_text( size=18),
#         axis.text  = element_text(size=12), axis.text.y= element_text(angle=0))
# 
# # plot
# 
# plot <- plot + geom_hline(aes(0), size=1) + geom_vline(aes(0), size=1)
# datapc <- data.frame(varnames=rownames(Frclim.pca$rotation), Frclim.pca$rotation)
# mult <- min(
#   (max(data[,"PC2"]) - min(data[,"PC2"])/(max(datapc[,"PC2"])-min(datapc[,"PC2"]))),
#   (max(data[,"PC1"]) - min(data[,"PC1"])/(max(datapc[,"PC1"])-min(datapc[,"PC1"])))
# )
# datapc <- transform(datapc,
#                     v1 = .7 * mult * (get("PC1")),
#                     v2 = .7 * mult * (get("PC2"))
# )
# 
# plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), 
#                                          size = 8, vjust=1, color="gray47", alpha=0.75)
# plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), size=2,
#                             arrow=arrow(length=unit(0.2,"cm")), alpha=0.4, color="gray47")
# plot
# dev.off()
# 
####PC1 vs PC3 fig####
# png("FrClimatePCA1v3.png",width=800, height = 600, pointsize = 16)
# postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)

pPC3 <- ggplot(data, aes_string(x="PC1", y="PC3")) + 
  geom_point(aes(shape=Origin, color=Origin), size=3) +
  #   scale_x_continuous(expand = c(0,1)) #+
  theme_bw()+
  theme(legend.justification=c(1,0), legend.position=c(1,0), 
        legend.title = element_text(size=9, face="bold"), 
        legend.text = element_text(size = 9),
        axis.title = element_text( size=9),
        axis.text  = element_text(size=7), axis.text.y= element_text(angle=0))
#   theme(legend.justification=c(1,0), legend.position=c(1,0))

# plot

pPC3 <- pPC3 + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)

datapc <- data.frame(varnames=rownames(FrclimDK.pca$rotation), FrclimDK.pca$rotation)
mult <- min(
  (max(data[,"PC3"]) - min(data[,"PC3"])/(max(datapc[,"PC3"])-min(datapc[,"PC3"]))),
  (max(data[,"PC1"]) - min(data[,"PC1"])/(max(datapc[,"PC1"])-min(datapc[,"PC1"])))
)
datapc <- transform(datapc,
                    v1 = .7 * mult * (get("PC1")),
                    v2 = .7 * mult * (get("PC3"))
)

pPC3 <- pPC3  +coord_equal(ratio=6.5/4)+ geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), 
                                                   size = 6, vjust=1, color="black", alpha=0.75)

pPC3 <- pPC3 + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), 
                            arrow=arrow(length=unit(0.2,"cm")), alpha=0.4, color="black")+
  ggtitle("(c)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pPC3

# dev.off()
ggsave("FrClimatePCA1v3.png") #width=800, height = 600, pointsize = 16

####supp. fig S1 all togther####
library(gridBase) #necessary to plot ggplots and base plots together

# pdf("KTurnerSup_MontPCA.pdf", useDingbats=FALSE, width=13.38)
png("KTurnerSup_MontPCA.png",width=800, height = 800)   #, pointsize = 12
# postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)

plot.new()
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))

pushViewport(viewport(layout.pos.col = 1, layout.pos.row=1))
par(fig = gridFIG(), new = TRUE, adj=0)
plot(FrclimDK.pca, main="(a)", xlab="Principal component", ylim=c(0,8), cex.main=1.3)
popViewport()

pushViewport(viewport(layout.pos.col = 2, layout.pos.row=1))
print(S1plot, newpage = FALSE)
popViewport()

pushViewport(viewport(layout.pos.col = 1, layout.pos.row=2))
print(pPC3, newpage = FALSE)
popViewport()

dev.off()



# ####multiplot####
# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   require(grid)
#   
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#   
#   numPlots = length(plots)
#   
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#   
#   if (numPlots==1) {
#     print(plots[[1]])
#     
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#     
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#       
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
}



