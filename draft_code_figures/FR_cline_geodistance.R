#Test for oversampling problem in niche shift
#4/28/2015


library(dplyr)
library(geosphere)

#load lat long data
allclim2 <- read.table("Cdif_allocc_bioclimPCA.txt", header=TRUE)

####determine subset of more dispersed data points####
## first, average distances among points

avg_dist <- . %>%
  select(Longitude, Latitude) %>%
  distm %>%
  .[upper.tri(.,diag = FALSE)]
  
#avg_dist(allclim2)

distances <- allclim2 %>%
  group_by(Origin) %>%
  do(distlist = avg_dist(.))

sum(1:375)

## the distribution of pairwise distances

pdf("AllOccDist_hist.pdf", useDingbats=FALSE,width=6.65, height=9, pointsize = 12)
par(mfrow=c(1,2))
hist(filter(distances, Origin == "inv")$distlist[[1]])
hist(filter(distances, Origin == "nat")$distlist[[1]])
dev.off()

#sampling random subset 1/3 of data, keeping Origin groups proportional
?sample_frac

distances_frac <- allclim2 %>%
  group_by(Origin) %>%
  sample_frac(0.3) %>%
  do(distlist = avg_dist(.)) %>%
  mutate(meandist = mean(distlist),
         sddist = sd(distlist))

#summing all pairwise distances across rows
totaldistmaker <- function(df){
  
  fn_distance <- df %>%  
    select(Longitude, Latitude) %>%
    distm %>%
    rowSums %>%
    data.frame(totaldist = .)
  
  data.frame(Pop = df$Pop,distance = fn_distance)
}

tot_distances <- allclim2 %>%
  group_by(Origin) %>%
  do(totaldistmaker(.))


#sampling data to include only half of the points, which are farthest apart
samp_dist <- tot_distances %>%
  sample_frac(size = 0.5, replace = FALSE, weight = totaldist^2)

samp_dist %>%
  tally

tot_distances %>%
  tally

## the distribution of distances

pdf("SampOccDist_hist.pdf", useDingbats=FALSE,width=6.65, height=9, pointsize = 12)
par(mfrow=c(1,2))
hist(filter(samp_dist, Origin == "inv")$totaldist)
hist(filter(samp_dist, Origin == "nat")$totaldist)
dev.off()

####subset climate data and rerun PCA####
sampclim <- subset(allclim, Pop%in%samp_dist$Pop)

library("ggplot2")
library("grid") 
library("gridBase")

# allclim$Pop <- as.factor(allclim$Pop)
# row.names(allclim) <- allclim$Pop

sampclim.pca <- prcomp(sampclim[c(2,7:26)], center=TRUE, retx=T, scale.=TRUE)
summary(sampclim.pca)
# Importance of components:
#                           PC1    PC2    PC3     PC4     PC5     PC6
# Standard deviation     2.5642 2.4290 1.9408 1.20236 1.11040 0.90443
# Proportion of Variance 0.3131 0.2809 0.1794 0.06884 0.05871 0.03895
# Cumulative Proportion  0.3131 0.5940 0.7734 0.84225 0.90097 0.93992

#visualize components
plot(sampclim.pca, main="(a) Screeplot, Sampled Occurrences", xlab="Principal component", ylim=c(0,7))
# screeplot(Frclim.pca, type="lines")
biplot(sampclim.pca)
biplot(sampclim.pca,  main="PCA analysis of climate data", choices=c(1,3))

#see bottom for figure
# 
# variances of the principal components:
apply(sampclim.pca$x, 2, var)
#         PC1          PC2          PC3          PC4          PC5 
# 6.574957e+00 5.899809e+00 3.766883e+00 1.445675e+00 1.232983e+00

#find top loadings (for PC1)
loadings <- sampclim.pca$rotation[,1]
sort(abs(loadings), decreasing=TRUE)
# bio14      bio17      bio18       bio2       bio5      bio15       bio3 
# 0.35742133 0.34775857 0.33716713 0.32735937 0.31258350 0.30551061 0.30196904 
# bio9       bio7        alt      bio10       bio8   Latitude       bio1 
# 0.23863730 0.22616024 0.18926053 0.15438761 0.15070391 0.11601836 0.11591473 
# bio12      bio11      bio19      bio13      bio16       bio4       bio6 
# 0.09530510 0.09453894 0.08606827 0.04857905 0.04126005 0.03088462 0.02082351
# BIO14 = Precipitation of driest month
# BIO17 = Precipitation of driest quarter
# BIO2 = Mean diurnal temperature range (mean of monthly (max temp – min temp))
# BIO18 = Precipitation of Warmest Quarter

#find top loadings (for PC2)
loadings2 <- sampclim.pca$rotation[,2]
sort(abs(loadings2), decreasing=TRUE)
# bio6        bio4       bio11       bio16       bio19       bio13 
# 0.344941379 0.329284352 0.326526140 0.312239006 0.310067513 0.308223985 
# bio12        bio7        bio1        bio9         alt       bio15 
# 0.307652098 0.292859055 0.245938116 0.191092609 0.163668665 0.150124976 
# bio2    Latitude       bio10        bio3       bio17       bio14 
# 0.121783907 0.101605936 0.092995007 0.086469094 0.078543477 0.051517893 
# bio8        bio5       bio18 
# 0.018540260 0.010870753 0.004752517  
# BIO19 = Precipitation of coldest quarter
# BIO16 = Precipitation of wettest quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO13 = Precipitation of wettest month

#find top loadings (for PC3)
loadings3 <- sampclim.pca$rotation[,3]
sort(abs(loadings3), decreasing=TRUE)
# bio10        bio1       bio16       bio13       bio19         alt 
# 0.403572879 0.371510907 0.292712715 0.289829690 0.283176219 0.280411374 
# bio12       bio11        bio6        bio5        bio8       bio15 
# 0.268721014 0.262376956 0.260369769 0.239638612 0.236088617 0.131968347 
# bio3        bio2    Latitude        bio7       bio17        bio9 
# 0.098755016 0.090600830 0.070155324 0.055406820 0.043679389 0.043085202 
# bio4       bio14       bio18 
# 0.032316707 0.020433470 0.001790556  
# BIO10 = Mean Temperature of Warmest Quarter
# BIO1 = Annual Mean Temperature
# BIO11 = Mean Temperature of Coldest Quarter
# BIO6 = Min temperature of coldest month


#proportional contributions of each bioclim to each PC
#If you want this as a relative contribution then sum up the loadings per column and 
#express each loading as a proportion of the column (loading) sum, taking care to use 
#the absolute values to account for negative loadings.

sweep(abs(sampclim.pca$rotation),2, colSums(abs(sampclim.pca$rotation)),"/")
#                 PC1         PC2          PC3         PC4         PC5
# Latitude 0.029761179 0.026393231 0.0185763164 0.024722072 0.093596065
# alt      0.048549358 0.042514690 0.0742496805 0.053647767 0.074473707
# bio1     0.029734598 0.063885062 0.0983717805 0.027478920 0.017045572
# bio10    0.039603710 0.024156450 0.1068614188 0.048989788 0.078707957
# bio11    0.024251252 0.084818666 0.0694743756 0.011619832 0.039330257
# bio12    0.024447788 0.079915931 0.0711542088 0.047448553 0.050894981
# bio13    0.012461560 0.080064485 0.0767435413 0.051876641 0.049614271
# bio14    0.091686184 0.013382325 0.0054105460 0.006847254 0.017757979
# bio15    0.078369978 0.038996572 0.0349436882 0.041097242 0.006493681
# bio16    0.010584082 0.081107429 0.0775069329 0.046639787 0.047804097
# bio17    0.089207479 0.020402510 0.0115657957 0.005750593 0.021269687
# bio18    0.086490549 0.001234517 0.0004741185 0.096253554 0.005422446
# bio19    0.022078346 0.080543361 0.0749817793 0.009354334 0.047702889
# bio2     0.083974651 0.031634675 0.0239900493 0.059593331 0.039205856
# bio3     0.077461491 0.022461274 0.0261491832 0.041216842 0.102184463
# bio4     0.007922562 0.085535141 0.0085570894 0.032234119 0.126331946
# bio5     0.080184326 0.002823795 0.0634535259 0.040799087 0.050101266
# bio6     0.005341674 0.089602221 0.0689428959 0.027816495 0.024738770
# bio7     0.058014918 0.076073279 0.0146710835 0.050299232 0.053968276
# bio8     0.038658762 0.004816031 0.0625135283 0.163489337 0.025065517
# bio9     0.061215550 0.049638354 0.0114084620 0.112825222 0.028290316

# #get top 4 PCs
PC1 <- as.matrix(sampclim.pca$x[,1])
PC2 <- as.matrix(sampclim.pca$x[,2])
PC3 <- as.matrix(sampclim.pca$x[,3])
# # PC4 <- as.matrix(Frclim.pca$x[,4])

sampclim2 <- cbind(sampclim, PC1, PC2, PC3)
# 
# #write table
write.table(sampclim2, file="Cdif_distributedSampOcc_bioclimPCA.txt")
# # 
sampclim2 <- read.table("Cdif_distributedSampOcc_bioclimPCA.txt", header=TRUE)

####all occ main fig; 95% conf limits of clusters####
# http://stackoverflow.com/questions/20260434/test-significance-of-clusters-on-a-pca-plot
# draw 95% confidence ellipses around clusters. Note that stat_ellipse(...) uses the bivariate t-distribution.
scores <- sampclim.pca$x[,1:3]                        # scores for first three PC's

# k-means clustering [assume 2 clusters]
km     <- kmeans(scores, centers=2, nstart=5)
ggdata <- data.frame(scores, Cluster=km$cluster, Origin=sampclim2$Origin, alt=sampclim2$alt,Pop=sampclim2$Pop)
levels(ggdata$Origin)[levels(ggdata$Origin)=="inv"] <- "Invasive C. diffusa"
levels(ggdata$Origin)[levels(ggdata$Origin)=="nat"] <- "Native C. diffusa"

# stat_ellipse is not part of the base ggplot package
source("https://raw.github.com/low-decarie/FAAV/master/r/stat-ellipse.R") 

#centroid based on origin
centroids <- aggregate(cbind(PC1,PC2)~Origin,data=ggdata,mean)

#95% plot
sampplot <- ggplot(ggdata, aes_string(x="PC1", y="PC2")) +
  geom_point(aes(color=factor(Origin),shape=Origin), size=3) +
  guides(color=guide_legend("Origin"),fill=guide_legend("Origin"))+
  stat_ellipse(aes(x=PC1,y=PC2,fill=factor(Origin)),
               geom="polygon", level=0.95, alpha=0.2) +
  geom_point(data=centroids, aes(x=PC1, y=PC2, color=Origin, shape=Origin), size=8)+
  #coord_cartesian(ylim = c(-6.5, 8.5)) +
  theme_bw() + 
  theme(legend.justification=c(1,0), legend.position=c(1,0),
        legend.title = element_text(size=10, face="bold"),
        legend.text = element_text(size = 10))

sampplot
ggsave("SampOcc.pdf", width=6.65, height = 5)
ggsave("SampOcc.png", width=6.65, height = 5)

####ade4 to quantify centroid shift####
library(ade4)

sampclim.dudi <- dudi.pca(sampclim[c(2,7:26)], center = TRUE, scale = TRUE,scannf = TRUE, nf = 2)
2
sampclim.bca <- bca(sampclim.dudi, fac=sampclim$Origin, scannf=TRUE, nf=2) #p36
2
summary(sampclim.bca)
print(sampclim.bca)
sampclim.bca$ratio
[1] 0.03973598
randtest(sampclim.bca, nrept=999)
plot(randtest(sampclim.bca, nrept=999))

####sampling 1/3 of the data...?####
#sampling data to include only 1/3 of the points, which are farthest apart
samp_dist3 <- tot_distances %>%
  sample_frac(size = 0.3, replace = FALSE, weight = totaldist^2)

samp_dist3 %>%
  tally

tot_distances %>%
  tally

## the distribution of distances
# pdf("SampOccDist_hist.pdf", useDingbats=FALSE,width=6.65, height=9, pointsize = 12)
# par(mfrow=c(1,2))
hist(filter(samp_dist3, Origin == "inv")$totaldist)
hist(filter(samp_dist3, Origin == "nat")$totaldist)
# dev.off()

sampclim3 <- subset(allclim, Pop%in%samp_dist3$Pop)

sampclim3.pca <- prcomp(sampclim3[c(2,7:26)], center=TRUE, retx=T, scale.=TRUE)

#find top loadings (for PC1)
loadings <- sampclim3.pca$rotation[,1]
sort(abs(loadings), decreasing=TRUE)
# bio15       bio3      bio18      bio14       bio9      bio17      bio19       bio5      bio13      bio16 
# 0.34132791 0.29761310 0.29045834 0.28873821
# BIO14 = Precipitation of driest month
# BIO17 = Precipitation of driest quarter
# BIO2 = Mean diurnal temperature range (mean of monthly (max temp – min temp))
# BIO18 = Precipitation of Warmest Quarter

#find top loadings (for PC2)
loadings2 <- sampclim3.pca$rotation[,2]
sort(abs(loadings2), decreasing=TRUE)
# bio7       bio6       bio2      bio12       bio4        alt      bio11      bio17      bio16       bio1 
# 0.37047415 0.33651734 0.28434619 0.28122544 0.27602894 0.26809621 0.26709900 0.22201619 0.21581369 0.21357875 

# BIO19 = Precipitation of coldest quarter
# BIO16 = Precipitation of wettest quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO13 = Precipitation of wettest month

PC1 <- as.matrix(sampclim3.pca$x[,1])
PC2 <- as.matrix(sampclim3.pca$x[,2])
PC3 <- as.matrix(sampclim3.pca$x[,3])
# # PC4 <- as.matrix(Frclim.pca$x[,4])

sampclim4 <- cbind(sampclim3, PC1, PC2, PC3)
# 
# #write table
write.table(sampclim4, file="Cdif_distributedSampOcc3_bioclimPCA.txt")
# # 
sampclim4 <- read.table("Cdif_distributedSampOcc3_bioclimPCA.txt", header=TRUE)

#all occ main fig; 95% conf limits of clusters
# http://stackoverflow.com/questions/20260434/test-significance-of-clusters-on-a-pca-plot
# draw 95% confidence ellipses around clusters. Note that stat_ellipse(...) uses the bivariate t-distribution.
scores <- sampclim3.pca$x[,1:3]                        # scores for first three PC's

# k-means clustering [assume 2 clusters]
km     <- kmeans(scores, centers=2, nstart=5)
ggdata <- data.frame(scores, Cluster=km$cluster, Origin=sampclim3$Origin, alt=sampclim3$alt,Pop=sampclim3$Pop)
levels(ggdata$Origin)[levels(ggdata$Origin)=="inv"] <- "Invasive C. diffusa"
levels(ggdata$Origin)[levels(ggdata$Origin)=="nat"] <- "Native C. diffusa"

# stat_ellipse is not part of the base ggplot package
source("https://raw.github.com/low-decarie/FAAV/master/r/stat-ellipse.R") 

#centroid based on origin
centroids <- aggregate(cbind(PC1,PC2)~Origin,data=ggdata,mean)

#95% plot
samp3plot <- ggplot(ggdata, aes_string(x="PC1", y="PC2")) +
  geom_point(aes(color=factor(Origin),shape=Origin), size=3) +
  guides(color=guide_legend("Origin"),fill=guide_legend("Origin"))+
  stat_ellipse(aes(x=PC1,y=PC2,fill=factor(Origin)),
               geom="polygon", level=0.95, alpha=0.2) +
  geom_point(data=centroids, aes(x=PC1, y=PC2, color=Origin, shape=Origin), size=8)+
  #coord_cartesian(ylim = c(-6.5, 8.5)) +
  theme_bw() + 
  theme(legend.justification=c(1,0), legend.position=c(1,0),
        legend.title = element_text(size=10, face="bold"),
        legend.text = element_text(size = 10))

samp3plot
ggsave("SampOcc3.pdf", width=6.65, height = 5)
ggsave("SampOcc3.png", width=6.65, height = 5)

#ade4 to quantify centroid shift
library(ade4)

sampclim3.dudi <- dudi.pca(sampclim3[c(2,7:26)], center = TRUE, scale = TRUE,scannf = TRUE, nf = 2)
2
sampclim3.bca <- bca(sampclim3.dudi, fac=sampclim3$Origin, scannf=TRUE, nf=2) #p36
2
summary(sampclim3.bca)
print(sampclim3.bca)
sampclim3.bca$ratio
[1] 0.04003375
randtest(sampclim3.bca, nrept=999)
plot(randtest(sampclim3.bca, nrept=999))

####sampling 1/2 of the data, using dist^3...?####
#sampling data to include only 1/3 of the points, which are farthest apart
samp_dist_cube <- tot_distances %>%
  sample_frac(size = 0.5, replace = FALSE, weight = totaldist^3)

samp_dist_cube %>%
  tally

tot_distances %>%
  tally

## the distribution of distances
pdf("SampOccCubeDist_hist.pdf", useDingbats=FALSE,width=6.65, height=9, pointsize = 12)
par(mfrow=c(1,2))
hist(filter(samp_dist_cube, Origin == "inv")$totaldist)
hist(filter(samp_dist_cube, Origin == "nat")$totaldist)
dev.off()

sampclim_cube <- subset(allclim, Pop%in%samp_dist_cube$Pop)

sampclim_cube.pca <- prcomp(sampclim_cube[c(2,7:26)], center=TRUE, retx=T, scale.=TRUE)

#find top loadings (for PC1)
loadings <- sampclim_cube.pca$rotation[,1]
sort(abs(loadings), decreasing=TRUE)
# bio2       bio17       bio14        bio7       bio18        bio5         alt        bio3       bio15 
# 0.351191497 0.349431045 0.349406986 0.326859756 0.300242819 0.284158447 0.264576011 0.243893109 0.223891469 
# BIO14 = Precipitation of driest month
# BIO17 = Precipitation of driest quarter
# BIO2 = Mean diurnal temperature range (mean of monthly (max temp – min temp))
# BIO18 = Precipitation of Warmest Quarter

#find top loadings (for PC2)
loadings2 <- sampclim_cube.pca$rotation[,2]
sort(abs(loadings2), decreasing=TRUE)
# bio19      bio16      bio13      bio11       bio6       bio4      bio12      bio15       bio9       bio1 
# 0.34345310 0.32924311 0.32789163 0.32178118 0.28534962 0.28328437 0.27947574 0.26029179 0.25475244 0.24847785 

# BIO19 = Precipitation of coldest quarter
# BIO16 = Precipitation of wettest quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO13 = Precipitation of wettest month

PC1 <- as.matrix(sampclim_cube.pca$x[,1])
PC2 <- as.matrix(sampclim_cube.pca$x[,2])
PC3 <- as.matrix(sampclim_cube.pca$x[,3])
# # PC4 <- as.matrix(Frclim.pca$x[,4])

sampclim_cube2 <- cbind(sampclim_cube, PC1, PC2, PC3)
# 
# #write table
write.table(sampclim_cube2, file="Cdif_distributedSampOccCube_bioclimPCA.txt")
# # 
sampclim_cube2 <- read.table("Cdif_distributedSampOccCube_bioclimPCA.txt", header=TRUE)

#all occ main fig; 95% conf limits of clusters
# http://stackoverflow.com/questions/20260434/test-significance-of-clusters-on-a-pca-plot
# draw 95% confidence ellipses around clusters. Note that stat_ellipse(...) uses the bivariate t-distribution.
scores <- sampclim_cube.pca$x[,1:3]                        # scores for first three PC's

# k-means clustering [assume 2 clusters]
km     <- kmeans(scores, centers=2, nstart=5)
ggdata <- data.frame(scores, Cluster=km$cluster, Origin=sampclim_cube$Origin, alt=sampclim_cube$alt,Pop=sampclim_cube2$Pop)
levels(ggdata$Origin)[levels(ggdata$Origin)=="inv"] <- "Invasive C. diffusa"
levels(ggdata$Origin)[levels(ggdata$Origin)=="nat"] <- "Native C. diffusa"

# stat_ellipse is not part of the base ggplot package
source("https://raw.github.com/low-decarie/FAAV/master/r/stat-ellipse.R") 

#centroid based on origin
centroids <- aggregate(cbind(PC1,PC2)~Origin,data=ggdata,mean)

#95% plot
samp_cubeplot <- ggplot(ggdata, aes_string(x="PC1", y="PC2")) +
  geom_point(aes(color=factor(Origin),shape=Origin), size=3) +
  guides(color=guide_legend("Origin"),fill=guide_legend("Origin"))+
  stat_ellipse(aes(x=PC1,y=PC2,fill=factor(Origin)),
               geom="polygon", level=0.95, alpha=0.2) +
  geom_point(data=centroids, aes(x=PC1, y=PC2, color=Origin, shape=Origin), size=8)+
  #coord_cartesian(ylim = c(-6.5, 8.5)) +
  theme_bw() + 
  theme(legend.justification=c(1,0), legend.position=c(1,0),
        legend.title = element_text(size=10, face="bold"),
        legend.text = element_text(size = 10))

samp_cubeplot
ggsave("SampOccCube.pdf", width=6.65, height = 5)
ggsave("SampOccCube.png", width=6.65, height = 5)

#ade4 to quantify centroid shift
library(ade4)

sampclim_cube.dudi <- dudi.pca(sampclim_cube[c(2,7:26)], center = TRUE, scale = TRUE,scannf = TRUE, nf = 2)
2
sampclim_cube.bca <- bca(sampclim_cube.dudi, fac=sampclim_cube$Origin, scannf=TRUE, nf=2) #p36
2
summary(sampclim_cube.bca)
print(sampclim_cube.bca)
sampclim_cube.bca$ratio
1] 0.04148833
randtest(sampclim_cube.bca, nrept=999)
plot(randtest(sampclim_cube.bca, nrept=999))
