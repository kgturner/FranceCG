#specimen and occurrence
#12/16/14

library (ggplot2)
library (plyr)

####all occ for Cdif, North America####
library("devtools")

install_github("ropensci/rgbif")
library("rgbif")

Cdif_alloccNA <- occ_search(scientificName="Centaurea diffusa", country=c("US", "CA"),limit=999999,
                          fields=c("gbifID","species", "basisOfRecord","year","eventDate","countryCode",
                                   "decimalLatitude", "decimalLongitude",  "genus","specificEpithet","collectionCode",
                                   "institutionCode","locality", "datasetKey"),
                          return="data")
Cdif_NAoccdf <- ldfast(Cdif_alloccNA)
Cdif_NAoccdf$year <- as.integer(Cdif_NAoccdf$year)
write.table(Cdif_NAoccdf, "Cdif_GBIF_allocc_NorthAm.txt", col.names=TRUE, sep="\t", quote=F)
#get rid of NA lat/long, but double check eventDates, move col around
Cdif_NAoccdf <-read.delim("Cdif_GBIF_allocc_NorthAm.txt", sep="\t")
# levels(Cdif_NAoccdf$species)

# #occ count per yr
# qplot(Cdif_alloccdf$year,binwidth = 1)
# #cumulative occ count
# png("GBIF_allocc_cumcount_Cdif.png") #width, height
# ggplot(Cdif_alloccdf, aes(x=year))+geom_histogram(aes(y=cumsum(..count..)), binwidth = 1)+
#   scale_y_continuous(name="Cumulative North American Centaurea diffusa all occurences")+
#   theme_bw()
# #+stat_bin(aes(y=cumsum(..count..)), geom="line", color="green")
# dev.off()

####all occ for Cdif, Europe####
library("devtools")

install_github("ropensci/rgbif")
library("rgbif")


Cdif_alloccEU <- occ_search(scientificName="Centaurea diffusa", geometry='POLYGON((70.3125 69.92426677626425,70.3125 30.811684101174777,-13.0078125 30.811684101174777,-13.0078125 69.92426677626425,70.3125 69.92426677626425))',
                            limit=999999,fields=c("gbifID","species", "basisOfRecord","year","eventDate","countryCode",
                                                  "decimalLatitude", "decimalLongitude", "locality", "genus","specificEpithet","collectionCode",
                                                  "institutionCode", "datasetKey"),
                            return="data")
Cdif_EUoccdf <- as.data.frame(Cdif_alloccEU)
Cdif_EUoccdf$year <- as.integer(Cdif_EUoccdf$year)
write.table(Cdif_EUoccdf, "Cdif_GBIF_allocc_EU.txt", col.names=TRUE, sep="\t", quote=F)
#get rid of NA lat/long, but double check eventDates, move col around
Cdif_EUoccdf <-read.delim("Cdif_GBIF_allocc_EU.txt", sep="\t")
# Cdif_EUoccdf$species <- paste(Cdif_EUoccdf$genus, Cdif_EUoccdf$specificEpithet)

# #occ count per yr
# qplot(Cdif_alloccdf$year,binwidth = 1)
# #cumulative occ count
# png("GBIF_allocc_cumcount_Cdif.png") #width, height
# ggplot(Cdif_alloccdf, aes(x=year))+geom_histogram(aes(y=cumsum(..count..)), binwidth = 1)+
#   scale_y_continuous(name="Cumulative North American Centaurea diffusa all occurences")+
#   theme_bw()
# #+stat_bin(aes(y=cumsum(..count..)), geom="line", color="green")
# dev.off()

####get worldclim data for all occurences####
Cdif_alloccdf <- rbind(Cdif_NAoccdf, Cdif_EUoccdf)
#occurence locations
pop <- subset(Cdif_alloccdf, select=c(decimalLatitude, decimalLongitude,gbifID))
colnames(pop) <- c("Latitude","Longitude","Pop")
row.names(pop) <- pop$Pop
pop$Pop <- as.factor(pop$Pop)

#download and unzip all relevant WorldClim geoTIFF files into a single directory.
#I used the highest resolution (~1km2), but should work for other resolutions too.

#load packages: raster, rgdal, foreach
library(rgdal)
library(raster)
library(foreach)
#Read names of all files in directory into a list
#from http://stackoverflow.com/questions/5319839/read-multiple-csv-files-into-separate-data-frames
filenames <- list.files(path="~/grad work/Centaurea diffusa/WorldClim_2013/")
#Load all geoTIFF files
for(i in filenames){
  filepath <- file.path("~/grad work/Centaurea diffusa/WorldClim_2013/",i)
  assign(i, raster(filepath))
}
#check that all files loaded properly by raster
#from http://stackoverflow.com/questions/15387727/use-object-names-as-list-names-in-r
list <- mget(filenames, envir=globalenv())
for(i in list){
  if (hasValues(i)==FALSE){
    print(i,"hasValues error")
  }
  if (inMemory(i)==TRUE){
    print(i, "inMemory error")
  }
  else{
    print("All checked out!")
  }
}

#load location coordinates as SpatialPoints
for(i in pop$Pop){
  assign(i,SpatialPoints(as.matrix(t(c(pop[i,2], pop[i,1])))))
}
#check that SpatialPoints load correctly from geoTIFFs
#no column should be entirely NAs (if they are, see note above)
poplist <- mget(levels(pop$Pop), envir=globalenv())
tiffvector <- unlist(list)
foreach(p=poplist, .combine='rbind') %:%
  foreach(t=tiffvector, .combine='cbind') %do%{
    is.na(extract(t,p))
  }
#make climate data table
climate <- foreach(p=poplist, .combine='rbind') %:%
  foreach(t=tiffvector, .combine='cbind') %do%{
    myValue<-extract(t, p)
  } #may take a while 

popnames <- sort(as.character(pop$Pop))
clim <- as.data.frame(climate, row.names=popnames)
colnames(clim) <- filenames
head(clim)

#find rows that are all NAs, these are likely populations too close to large bodies of water
movepops <- clim[rowSums(is.na(clim)) == ncol(clim),]
#if time, adjust coordinates for these occurences (below is gbifID): 
> row.names(movepops)
[1] "1020763351" "224897636"  "466334255"  "466334257"  "695807221"  "695807222"  "695807231"  "695807232"  "695807234"  "695807259" 
[11] "695807262"  "918939137"  "918939142"  "918957675"  "918957703"  "921023893" 

#write table
write.table(clim, file="bioclimdata_withNAs.txt")
#load table
clim <- read.table("bioclimdata_withNAs.txt", header=TRUE)

#remove movepops, fix later if possible
clim <- clim[rowSums(is.na(clim)) != ncol(clim),]

#squish table
squish <- function(dat=dat, cols=cols, newcol="newcol"){
  dat$temp <- NA
  
  for (i in dat[,cols]){
    ## where is temp NA?
    ss <- which(is.na(dat$temp))
    ## what are the values of i there? put them in temp
    dat$temp[ss] <- i[ss]
  }
  names(dat)[names(dat)=="temp"] <- newcol
  return(subset(dat,select=ncol(dat)))
} 

squishsplit <- function(dat, split="_"){
  varnames <- colnames(dat)
  splitvars <- strsplit(varnames, split)
  squishvars <- sapply(splitvars,"[[",i=1)
  return(squishvars)
} 

squishr <- function(dat, squishvar){
  return(squish(dat=dat, cols=grep(squishvar,names(dat)),newcol=squishvar))
}

vars <- squishsplit(clim) 

#lapply to each unique value of vars, and cbind into a pretty dataframe
test1 <- do.call(cbind,lapply(unique(vars),squishr,dat=clim)) 
test1$Pop <- row.names(test1)
head(test1)

test2 <- merge(pop, test1)
test3 <- merge(test2, subset(Cdif_alloccdf, select=c(gbifID, basisOfRecord, year, countryCode)), by.x="Pop", by.y="gbifID")
test3$Origin <- "nat"
test3[test3$countryCode%in%c("CA","US"),]$Origin <- "inv"
test3$Origin <- as.factor(test3$Origin)

#write table
write.table(test3, file="Cdif_allocc_bioclimdata.txt")
#load table
allclim <- read.table("Cdif_allocc_bioclimdata.txt", header=TRUE)


####PCA funtimes for all occurences####
library("ggplot2")
library("grid") 
library("gridBase")

allclim$Pop <- as.factor(allclim$Pop)
row.names(allclim) <- allclim$Pop

allclim.pca <- prcomp(allclim[c(2,4:23)], center=TRUE, retx=T, scale.=TRUE)
summary(allclim.pca)
# Importance of components:
#                           PC1    PC2    PC3     PC4     PC5     PC6
# Standard deviation     2.7963 2.4232 1.8006 1.11633 0.97366 0.92571
# Proportion of Variance 0.3724 0.2796 0.1544 0.05934 0.04514 0.04081
# Cumulative Proportion  0.3724 0.6520 0.8064 0.86571 0.91086 0.95166

#visualize components
plot(allclim.pca, main="(a) Screeplot, All Occurrences", xlab="Principal component", ylim=c(0,7))
# screeplot(Frclim.pca, type="lines")
biplot(allclim.pca)
biplot(allclim.pca,  main="PCA analysis of climate data", choices=c(1,3))
#see bottom for figure
# 
# variances of the principal components:
apply(allclim.pca$x, 2, var)
PC1          PC2          PC3          PC4          PC5          PC6          PC7          PC8 
7.819447e+00 5.872006e+00 3.242318e+00 1.246197e+00 9.480185e-01 8.569455e-01 5.354649e-01 1.520626e-01 

#find top loadings (for PC1)
loadings <- allclim.pca$rotation[,1]
sort(abs(loadings), decreasing=TRUE)
#       bio7       bio12        bio4       bio16        bio6        bio2       bio13       bio19         alt 
# 0.333013763 0.313259146 0.302423974 0.282839578 0.279421534 0.279381445 0.279073669 0.267543783 0.236690156 
#       bio5       bio17       bio11       bio14        bio1       bio18        bio3        bio9       bio10 
# 0.233364876 0.231512205 0.216454494 0.205944599 0.109898642 0.105642888 0.104492940 0.096468877 0.082710925 
#   Latitude       bio15        bio8 
# 0.081558293 0.060131929 0.001734011 # BIO11 = Mean Temperature of Coldest Quarter
# BIO7 = Temperature Annual Range (BIO5-BIO6)
# BIO12 = Annual Precipitation
# BIO4 = Temperature Seasonality (standard deviation *100)

#find top loadings (for PC2)
loadings2 <- allclim.pca$rotation[,2]
sort(abs(loadings2), decreasing=TRUE)
#     bio15      bio18       bio9       bio3      bio14      bio17       bio5      bio11      bio19       bio2 
# 0.35879856 0.33916451 0.32639274 0.31506113 0.29478421 0.26127598 0.22570740 0.21321138 0.20666012 0.20503080 
#     bio1       bio8      bio13      bio16      bio10       bio4        alt       bio6   Latitude      bio12 
# 0.19571145 0.18684775 0.17673752 0.17281110 0.14933267 0.11779438 0.11583033 0.10884562 0.09764864 0.07810987 
#     bio7 
# 0.05227214 
BIO15 = Precipitation Seasonality (Coefficient of Variation)
BIO18 = Precipitation of Warmest Quarter
BIO9 = Mean Temperature of Driest Quarter

#find top loadings (for PC3)
loadings3 <- allclim.pca$rotation[,3]
sort(abs(loadings3), decreasing=TRUE)
#     bio10        bio1        bio8       bio11        bio6         alt        bio5       bio19       bio16 
# 0.453763544 0.450578366 0.323889620 0.315207218 0.291956045 0.255917994 0.224572921 0.207207559 0.195297598 
#     bio13       bio12       bio15        bio2        bio7       bio18        bio3       bio17        bio4 
# 0.194191189 0.173388156 0.123884676 0.079183800 0.078704498 0.062663009 0.055750157 0.033023212 0.019684845 
#   Latitude       bio14        bio9 
# 0.016414918 0.014159582 0.002824124  
# BIO10 = Mean Temperature of Warmest Quarter
# BIO1 = Annual Mean Temperature
# BIO8 = Mean Temperature of Wettest Quarter

#proportional contributions of each bioclim to each PC
#If you want this as a relative contribution then sum up the loadings per column and 
#express each loading as a proportion of the column (loading) sum, taking care to use 
#the absolute values to account for negative loadings.

sweep(abs(allclim.pca$rotation),2, colSums(abs(allclim.pca$rotation)),"/")
#                   PC1        PC2         PC3          PC4        PC5        PC6         PC7          PC8
# Latitude 0.0198750009 0.02326060 0.004595103 0.0928603504 0.13647532 0.21998023 0.055075602 0.0003892476
# alt      0.0576791997 0.02759160 0.071640300 0.0960223575 0.01097815 0.02913851 0.057996807 0.3287888506
# bio1     0.0267812816 0.04661985 0.126132472 0.0089436802 0.02551324 0.01947541 0.012511727 0.0438266787
# bio10    0.0201558868 0.03557210 0.127024113 0.0193258506 0.08028150 0.05846026 0.028734589 0.0706972086
# bio11    0.0527479561 0.05078846 0.088237404 0.0353177493 0.02047941 0.01841027 0.035614747 0.0608397995
# bio12    0.0763383536 0.01860632 0.048537343 0.0349685892 0.06302813 0.04120384 0.009684897 0.0046485507
# bio13    0.0680076694 0.04210013 0.054360832 0.0330436342 0.05522977 0.01642622 0.048008590 0.0007676209
# bio14    0.0501867921 0.07021968 0.003963757 0.0078187612 0.04432909 0.07647134 0.086232392 0.0053654336
# bio15    0.0146535944 0.08546835 0.034679606 0.0323716679 0.02017214 0.02974585 0.092543183 0.0096026780
# bio16    0.0689253866 0.04116482 0.054670554 0.0307635454 0.05262426 0.01619288 0.044866624 0.0047433316
# bio17    0.0564173808 0.06223778 0.009244340 0.0096787724 0.04846335 0.08508695 0.088418561 0.0056608208
# bio18    0.0257441937 0.08079138 0.017541544 0.0999272394 0.04397413 0.02476781 0.009158929 0.0031243504
# bio19    0.0651979428 0.04922790 0.058004564 0.0045428849 0.04220270 0.02169049 0.014469567 0.0347610935
# bio2     0.0680826714 0.04883978 0.022166285 0.0632130696 0.04005674 0.01080221 0.061588411 0.0807202641
# bio3     0.0254639619 0.07504979 0.015606398 0.1093595272 0.01119659 0.03728527 0.112056588 0.1793179983
# bio4     0.0736979224 0.02805945 0.005510469 0.0501634420 0.08059253 0.06202635 0.061622276 0.0005039894
# bio5     0.0568688597 0.05376510 0.062865729 0.0077161978 0.06929826 0.05354481 0.023477684 0.0383435754
# bio6     0.0680924408 0.02592780 0.081728597 0.0049151001 0.04378742 0.02152253 0.020583543 0.0192795657
# bio7     0.0811523708 0.01245159 0.022032112 0.0009371949 0.07099848 0.04609662 0.001154480 0.0357623125
# bio8     0.0004225624 0.04450845 0.090667909 0.1492244264 0.02671340 0.05761720 0.067350860 0.0283080180
# bio9     0.0235085723 0.07774906 0.000790570 0.1088859593 0.01360537 0.05405494 0.068849945 0.0445486119

# #get top 4 PCs
# PC1 <- as.matrix(allclim.pca$x[,1])
# PC2 <- as.matrix(allclim.pca$x[,2])
# PC3 <- as.matrix(allclim.pca$x[,3])
# # PC4 <- as.matrix(Frclim.pca$x[,4])
# allclim2 <- cbind(allclim, PC1, PC2, PC3)
# 
# #write table
# write.table(allclim2, file="Cdif_allocc_bioclimPCA.txt")
# # 
# allclim <- read.table("Cdif_allocc_bioclimPCA.txt", header=TRUE)

####95% conf limits of clusters####
# draw 95% confidence ellipses around clusters. Note that stat_ellipse(...) uses the bivariate t-distribution.
scores <- allclim.pca$x[,1:3]                        # scores for first three PC's

# k-means clustering [assume 2 clusters]
km     <- kmeans(scores, centers=2, nstart=5)
ggdata <- data.frame(scores, Cluster=km$cluster, Origin=allclim$Origin, alt=allclim$alt,Pop=allclim$Pop)

# stat_ellipse is not part of the base ggplot package
source("https://raw.github.com/low-decarie/FAAV/master/r/stat-ellipse.R") 

#centroid based on origin
centroids <- aggregate(cbind(PC1,PC2)~Origin,data=ggdata,mean)
#PC1 vs PC2
# plot <- ggplot(ggdata, aes_string(x="PC1", y="PC2")) +
#   geom_point(aes(color=factor(Cluster),shape=Origin), size=5) +
#   stat_ellipse(aes(x=PC1,y=PC2,fill=factor(Cluster)),
#                geom="polygon", level=0.95, alpha=0.2) +
#   guides(color=guide_legend("Cluster"),fill=guide_legend("Cluster"))
# plot

Oplot <- ggplot(ggdata, aes_string(x="PC1", y="PC2")) +
  geom_point(aes(color=factor(Origin),shape=Origin), size=3) +
  guides(color=guide_legend("Origin"),fill=guide_legend("Origin"))+
  stat_ellipse(aes(x=PC1,y=PC2,fill=factor(Origin)),
               geom="polygon", level=0.95, alpha=0.2) +
  geom_point(data=centroids, aes(x=PC1, y=PC2, color=Origin, shape=Origin), size=8)
  
Oplot
ggsave("Cdiff_nicheoverlap_95.png")

#PC1 vs PC3
plot <- ggplot(ggdata, aes_string(x="PC1", y="PC3")) +
  geom_point(aes(color=factor(Origin),shape=Origin), size=3) +
  stat_ellipse(aes(x=PC1,y=PC3,fill=factor(Origin)),
               geom="polygon", level=0.95, alpha=0.2) +
  guides(color=guide_legend("Origin"),fill=guide_legend("Origin"))
plot

####exploratory clustering####
# library(devtools)
# install_github("dgrtwo/broom")
# library(broom)
# 
# library(dplyr)
# 
# kclusts <- data.frame(k=1:9) %>% group_by(k) %>% do(km=kmeans(scores, .$k))
# clusters <- kclusts %>% group_by(k) %>% do(tidy(.$km[[1]]))
# assignments <- kclusts %>% group_by(k) %>% do(augment(.$km[[1]], scores))
# clusterings <- kclusts %>% group_by(k) %>% do(glance(.$km[[1]]))
# 
# p1 <- ggplot(assignments, aes(PC1, PC2)) + geom_point(aes(color=.cluster)) + facet_wrap(~ k)
# p1
# p2 <- ggplot(assignments, aes(PC1, PC3)) + geom_point(aes(color=.cluster)) + facet_wrap(~ k)
# p2
# p3 <- p1 + geom_point(data=clusters, size=10, shape="x")
# p3

####trying ade4####
library(ade4)

allclim.dudi <- dudi.pca(allclim[c(2,4:23)], center = TRUE, scale = TRUE,scannf = TRUE, nf = 2)
allclim.bca <- bca(allclim.dudi, fac=allclim$Origin, scannf=TRUE, nf=2) #p36

summary(allclim.bca)
print(allclim.bca)
allclim.bca$ratio
[1] 0.05677683
randtest(allclim.bca, nrept=999)
plot(randtest(allclim.bca, nrept=999))

####PC1 vs PC2 fig####
#pts instead of labels for pops
# library("ggplot2")
# library("grid") 
data <- data.frame(obsnames=row.names(allclim.pca$x), allclim.pca$x)
data <- merge(data, allclim[,c(1,24:27)],by.x="obsnames", by.y="Pop")
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
plot <- ggplot(data, aes_string(x="PC1", y="PC2")) + 
  geom_point(aes(shape=Origin, color=Origin), size=3) +
  #   scale_x_continuous(expand = c(0,1)) #+
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(1,0), 
        legend.title = element_text(size=7, face="bold"), 
        legend.text = element_text(size = 7),
        axis.title = element_text( size=7),
        axis.text  = element_text(size=5), axis.text.y= element_text(angle=0))

plot

plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
datapc <- data.frame(varnames=rownames(allclim.pca$rotation), allclim.pca$rotation)
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
ggsave("allClimPCA.png")
# dev.off()

#PC1 vs PC3
# pdf("KTurnerFig2.pdf", useDingbats=FALSE, width=4.4, height=4.8, pointsize = 12) #3.149, 4.4 or 6.65
# # png("FrClimatePCA.png",width=800, height = 600, pointsize = 16)
# # postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)
# 
plot <- ggplot(data, aes_string(x="PC1", y="PC3")) + 
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
datapc <- data.frame(varnames=rownames(allclim.pca$rotation), allclim.pca$rotation)
mult <- min(
  (max(data[,"PC3"]) - min(data[,"PC3"])/(max(datapc[,"PC3"])-min(datapc[,"PC3"]))),
  (max(data[,"PC1"]) - min(data[,"PC1"])/(max(datapc[,"PC1"])-min(datapc[,"PC1"])))
)
datapc <- transform(datapc,
                    v1 = .7 * mult * (get("PC1")),
                    v2 = .7 * mult * (get("PC3"))
)

plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), 
                                         size = 4, vjust=1, color="gray47", alpha=0.75)
plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), 
                            arrow=arrow(length=unit(0.2,"cm")), alpha=0.4, color="gray47")
plot
# dev.off()

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