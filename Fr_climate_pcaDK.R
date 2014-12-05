##France CG - climate and lat/long PCA###

library("ggplot2")
library("grid") 
library("gridBase")

Frclimdat <- read.table("FrbioclimPCAdat.txt", header=TRUE) #climate table with PC1-4 included
#DK only
Frclimdat.dk <- droplevels(subset(Frclimdat, Origin%in%c("inv", "nat"), select=1:24))

####PCA fun times####
#Origin and longitude artificially separates groups...
FrclimDK.pca <- prcomp(Frclimdat.dk[2:22], center=TRUE, retx=T, scale.=TRUE)
summary(FrclimDK.pca)
# Importance of components:
#                           PC1    PC2    PC3    PC4     PC5     PC6     PC7     PC8 
# Standard deviation     2.5324 2.3291 2.0339 1.5321 1.04559 0.85042 0.73721 0.44243 
# Proportion of Variance 0.3054 0.2583 0.1970 0.1118 0.05206 0.03444 0.02588 0.00932
# Cumulative Proportion  0.3054 0.5637 0.7607 0.8725 0.92452 0.95896 0.98484 0.99416


#visualize components
plot(FrclimDK.pca, main="(a) Screeplot, Montpellier Experiment", xlab="Principal component", ylim=c(0,7))
# screeplot(Frclim.pca, type="lines")
biplot(FrclimDK.pca)
biplot(FrclimDK.pca,  main="PCA analysis of climate data", choices=c(1,3))
#see bottom for figure

# variances of the principal components:
apply(FrclimDK.pca$x, 2, var)
PC1          PC2          PC3          PC4          PC5          PC6          PC7          PC8          
6.413035e+00 5.424698e+00 4.136683e+00 2.347227e+00 1.093252e+00 7.232118e-01 5.434783e-01 1.957406e-01


#find top loadings (for PC1)
loadings <- FrclimDK.pca$rotation[,1]
sort(abs(loadings), decreasing=TRUE)
# bio11         bio4         bio6         bio9         bio1         bio7        bio19         bio3     Latitude        bio18        bio16        bio13 
# 0.3477634412 0.3415443495 0.3396753681 0.3227947573 0.2734575490 0.2646601273 0.2622142510 0.2372984061 0.2234071480 0.2026011484 0.1928539203 0.1808279853 
# bio12        bio15         bio8        bio14        bio10         bio5        bio17          alt         bio2 
# 0.1681997984 0.1627035834 0.1618949110 0.1077019953 0.0858056376 0.0852779938 0.0686258731 0.0131679736 0.0009167083 
BIO11 = Mean Temperature of Coldest Quarter
BIO4 = Temperature Seasonality (standard deviation *100)
BIO6 = Min Temperature of Coldest Month
BIO9 = Mean Temperature of Driest Quarter

#find top loadings (for PC2)
loadings2 <- FrclimDK.pca$rotation[,2]
sort(abs(loadings2), decreasing=TRUE)
# bio5       bio17       bio14       bio12        bio2       bio10        bio7       bio19       bio13    Latitude       bio16       bio18       bio15 
# 0.374747216 0.371415358 0.354202953 0.306940244 0.268473933 0.246119137 0.241468103 0.234550843 0.222581033 0.217225448 0.211710814 0.182753097 0.151095453 
# bio4        bio3        bio1        bio9         alt        bio6       bio11        bio8 
# 0.133955131 0.113901566 0.112861695 0.082237770 0.073618541 0.043044836 0.021681001 0.005394978 
BIO5 = Max Temperature of Warmest Month
BIO17 = Precipitation of Driest Quarter
BIO14 = Precipitation of Driest Month
BIO12 = Annual Precipitation

#find top loadings (for PC3)
loadings3 <- FrclimDK.pca$rotation[,3]
sort(abs(loadings3), decreasing=TRUE)
# alt      bio10       bio1       bio2      bio15       bio3       bio8       bio6      bio16      bio13      bio11       bio7      bio19      bio12      bio14 
# 0.36654119 0.31909538 0.31237598 0.30242551 0.28402020 0.26738636 0.25362066 0.23730987 0.23315531 0.22609464 0.21886604 0.18635341 0.17574859 0.17461695 0.13646132 
# bio17   Latitude       bio5      bio18       bio9       bio4 
# 0.13181951 0.08586004 0.05688603 0.04774380 0.04719642 0.03717003 
altitude
BIO10 = Mean Temperature of Warmest Quarter
BIO1 = Annual Mean Temperature
BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))


#proportional contributions of each bioclim to each PC
#If you want this as a relative contribution then sum up the loadings per column and 
#express each loading as a proportion of the column (loading) sum, taking care to use 
#the absolute values to account for negative loadings.

sweep(abs(FrclimDK.pca$rotation),2, colSums(abs(FrclimDK.pca$rotation)),"/")
#                   PC1         PC2         PC3          PC4          PC5          PC6         PC7         PC8
# alt      0.0032566644 0.018543810 0.089384000 0.0261236303 0.1286202616 0.1476962282 0.001339065 0.007880284
# bio1     0.0676307136 0.028428788 0.076175380 0.0329832587 0.0206778537 0.0209867327 0.002501644 0.034834596
# bio10    0.0212211969 0.061995070 0.077813960 0.0692460164 0.0146350015 0.0184793263 0.070201954 0.018238789
# bio11    0.0860078275 0.005461238 0.053372234 0.0010476343 0.0170430995 0.0237253067 0.041776025 0.018488781
# bio12    0.0415986775 0.077315329 0.042581740 0.0631192418 0.0008009791 0.0308461161 0.066495498 0.056198119
# bio13    0.0447218434 0.056066046 0.055134985 0.0969637622 0.0085305767 0.0041111848 0.021444919 0.003847089
# bio14    0.0266365395 0.089220356 0.033277183 0.0264133058 0.0678903701 0.0944978549 0.023560609 0.029487890
# bio15    0.0402393698 0.038059508 0.069260597 0.0892038234 0.0205142125 0.0568640256 0.078725826 0.115892881
# bio16    0.0476960622 0.053327941 0.056856787 0.0940081743 0.0123244071 0.0139913270 0.018431924 0.022226168
# bio17    0.0169723483 0.093555997 0.032145242 0.0243751323 0.0745874706 0.0712373798 0.040905168 0.041961413
# bio18    0.0501067178 0.046033767 0.011642706 0.0969667142 0.0744412245 0.1115038556 0.076334608 0.110965426
# bio19    0.0648500543 0.059081127 0.042857698 0.0119836294 0.0019638103 0.0756627558 0.093926989 0.058351348
# bio2     0.0002267176 0.067626031 0.073748879 0.0006043175 0.1311551819 0.0475902659 0.022167858 0.041619008
# bio3     0.0586879412 0.028690722 0.065204303 0.0276584304 0.1329923384 0.0083256916 0.053221605 0.080601063
# bio4     0.0844697401 0.033742024 0.009064208 0.0440401998 0.0153923275 0.0203901213 0.098172604 0.010249894
# bio5     0.0210907016 0.094395261 0.013872113 0.0274640281 0.0938031222 0.0321406072 0.068906774 0.001264315
# bio6     0.0840075091 0.010842585 0.057869908 0.0130125094 0.0016049478 0.0001208292 0.032739910 0.018816482
# bio7     0.0654549612 0.060823519 0.045443770 0.0267119752 0.0491202696 0.0172229432 0.067103794 0.016526795
# bio8     0.0400393714 0.001358944 0.061847426 0.1261614229 0.0017959918 0.0513916858 0.008968933 0.004672688
# bio9     0.0798326463 0.020714912 0.011509224 0.0545300906 0.0216367020 0.0171288165 0.097346339 0.239017090
# Latitude 0.0552523962 0.054717025 0.020937658 0.0473827031 0.1104698515 0.1360869460 0.015727953 0.068859880

#get top 4 PCs
PC1 <- as.matrix(FrclimDK.pca$x[,1])
PC2 <- as.matrix(FrclimDK.pca$x[,2])
PC3 <- as.matrix(FrclimDK.pca$x[,3])
# PC4 <- as.matrix(Frclim.pca$x[,4])
Frclimdat.dk2 <- cbind(Frclimdat.dk, PC1, PC2, PC3)

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
  geom_point(aes(color=factor(Cluster),shape=Origin), size=5) +
  stat_ellipse(aes(x=PC1,y=PC2,fill=factor(Cluster)),
               geom="polygon", level=0.95, alpha=0.2) +
  guides(color=guide_legend("Cluster"),fill=guide_legend("Cluster"))
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
# ####PC1 vs PC2 fig####
# #pts instead of labels for pops
# # library("ggplot2")
# library("grid") 
# data <- data.frame(obsnames=row.names(Frclim.pca$x), Frclim.pca$x)
# data <- merge(data, unique(Frdes[,c(5,7,12)]),by.x="obsnames", by.y="Pop")
# levels(data$Origin)[levels(data$Origin)=="inv"] <- "Invasive C. diffusa"
# levels(data$Origin)[levels(data$Origin)=="nat"] <- "Native C. diffusa"
# levels(data$Origin)[levels(data$Origin)=="sk"] <- "Native C. stoebe"
# # data$pch <- 15
# # data[data$Origin %in% "nat",]$pch <- 16
# # data[data$Origin %in% "sk",]$pch <- 17
# 
# pdf("KTurnerFig2.pdf", useDingbats=FALSE, width=4.4, height=4.8, pointsize = 12) #3.149, 4.4 or 6.65
# # png("FrClimatePCA.png",width=800, height = 600, pointsize = 16)
# # postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)
# 
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
# ####PC1 vs PC3 fig####
# png("FrClimatePCA1v3.png",width=800, height = 600, pointsize = 16)
# # postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)
# 
# pPC3 <- ggplot(data, aes_string(x="PC1", y="PC3")) + 
#   geom_point(aes(shape=Origin, color=Origin), size=3) +
#   #   scale_x_continuous(expand = c(0,1)) #+
#   theme_bw()+
#   theme(legend.position="bottom")
# #   theme(legend.justification=c(1,0), legend.position=c(1,0))
# 
# # plot
# 
# pPC3 <- pPC3 + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
# 
# datapc <- data.frame(varnames=rownames(Frclim.pca$rotation), Frclim.pca$rotation)
# mult <- min(
#   (max(data[,"PC3"]) - min(data[,"PC3"])/(max(datapc[,"PC3"])-min(datapc[,"PC3"]))),
#   (max(data[,"PC1"]) - min(data[,"PC1"])/(max(datapc[,"PC1"])-min(datapc[,"PC1"])))
# )
# datapc <- transform(datapc,
#                     v1 = .7 * mult * (get("PC1")),
#                     v2 = .7 * mult * (get("PC3"))
# )
# 
# pPC3 <- pPC3  +coord_equal(ratio=6.1/4)+ geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), 
#                                                    size = 6, vjust=1, color="gray47", alpha=0.75)
# 
# pPC3 <- pPC3 + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), 
#                             arrow=arrow(length=unit(0.2,"cm")), alpha=0.4, color="gray47")+
#   ggtitle("(b)")+theme(plot.title = element_text(lineheight=2, face="bold"))
# pPC3
# 
# dev.off()
# 
# 
# ####supp. fig####
# library(gridBase) #necessary to plot ggplots and base plots together
# 
# # pdf("KTurnerSup_MontPCA.pdf", useDingbats=FALSE, width=13.38)
# png("KTurnerSup_MontPCA.png",width=800, height = 500, pointsize = 12)
# # postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)
# 
# par(mfcol=c(1,2))
# 
# plot(Frclim.pca, main="(a)", xlab="Principal component", ylim=c(0,7), cex.main=1.3)
# plot.new()
# vps <- baseViewports()
# pushViewport(vps$figure)
# vp1 <- plotViewport(c(0,0,0,0))
# print(pPC3, vp=vp1)
# 
# 
# 
# dev.off()
# 
# 
# 
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



