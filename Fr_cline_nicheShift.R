#specimen and occurrence
#12/16/14

library (ggplot2)
library (plyr)

####all occ for Cdif, North America####
library("devtools")

install_github("ropensci/rgbif")
library("rgbif")
citation(package = 'rgbif')

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

####fix up all occurences####

Cdif_alloccdf <- rbind(Cdif_NAoccdf, Cdif_EUoccdf)
write.table(Cdif_alloccdf, "Cdif_GBIF_allocc.txt", col.names=TRUE, sep="\t", quote=F)
#get rid of NA lat/long, but double check eventDates, move col around
Cdif_alloccdf <-read.delim("Cdif_GBIF_allocc.txt", sep="\t")
#remove my greenhouse populations, add in my collection info
ghouse <- as.numeric(as.vector(Cdif_alloccdf[225:348,"gbifID"]))
test <- Cdif_alloccdf[-(225:348),]
###ugh, they don't all have their remarks which say seed collection location???
# occ_get(918957709, fields = c("recordNumber","occurrenceRemarks"))
# 
# library("plyr")
# test2 <- occ_get(ghouse, fields = c("gbifID","species", "basisOfRecord","year","eventDate","countryCode",
#                                     "decimalLatitude", "decimalLongitude", "genus","specificEpithet","collectionCode",
#                                     "institutionCode","locality", "datasetKey","recordNumber","occurrenceRemarks"))
# ghousedat <- rbind.fill(lapply(test2, "[[", "data"))
# ghousedat$year <- as.integer(ghousedat$year)
pop <- read.table("Popcoord_worldclim.txt", header=TRUE, stringsAsFactor=FALSE) #some population coordinates adjusted to compensate for worldclim structure
alloc <- test[,c(1,4,6:8)]
colnames(alloc) <- c("Pop", "year", "countryCode","Longitude","Latitude")
alloc$Origin <- "nat"
alloc[alloc$countryCode%in%c("US","CA"),]$Origin <- "inv"
pop <- subset(pop, !Origin=="sk")
pop$Pop <- as.factor(pop$Pop)
pop$Origin <- as.factor(pop$Origin)
pop$countryCode <- as.factor(as.character(pop$countryCode))
alloc$Pop <- as.factor(alloc$Pop)
alloc$Origin <- as.factor(alloc$Origin)
pop <- rbind(pop,alloc)

#occurence locations
# pop <- subset(Cdif_alloccdf, select=c(decimalLatitude, decimalLongitude,gbifID))
# colnames(pop) <- c("Latitude","Longitude","Pop")
row.names(pop) <- pop$Pop
# pop$Pop <- as.factor(pop$Pop)

####get worldclim data for all occurences####
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
# head(clim)

#find rows that are all NAs, these are likely populations too close to large bodies of water
movepops <- clim[rowSums(is.na(clim)) == ncol(clim),]
#if time, adjust coordinates for these occurences (below is gbifID): 
row.names(movepops)
[1] "1022548282" "164347431"  "466089620"  "695807120"  "695807121"  "725174681"  "725174968"  "727819202"  "767974977"  "788462754"  "918835735" 
[12] "918836019"  "918965870"  "920994041"  "920994819"  "921023893"  "US023"

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
# test3 <- merge(test2, subset(Cdif_alloccdf, select=c(gbifID, basisOfRecord, year, countryCode)), by.x="Pop", by.y="gbifID")
# test3$Origin <- "nat"
# test3[test3$countryCode%in%c("CA","US"),]$Origin <- "inv"
# test3$Origin <- as.factor(test3$Origin)

#####replace worldclim adjusted coordinates with real ones!######
#load table
popcoord <- read.table("Popcoord.txt", header=TRUE, stringsAsFactor=FALSE) #true pop coordinates

test <- subset(allclim, Pop%in%popcoord$Pop)
setdiff(test$Latitude,popcoord$Latitude)
test2 <- test[,-c(2:3)]
test3 <- merge(test2, popcoord, all.x=TRUE)
actest <- subset(allclim, !Pop%in%popcoord$Pop)
actest2 <- rbind(actest, test3)

count(is.na(actest2$Latitude))
row.names(actest2) <- actest2$Pop
setdiff(allclim,actest2)
subset(allclim, Pop=="CA001")#longitude should be -122.8821
subset(actest2, Pop=="CA001")
subset(allclim, Pop=="GR003")#latitude shoudl be 40.85
subset(actest2, Pop=="GR003")

subset(actest2, Pop%in%popcoord$Pop, select=Latitude)
subset(allclim, Pop%in%popcoord$Pop, select=Latitude)

#write table
write.table(actest2, file="Cdif_allocc_bioclimdata.txt")
#load table
allclim <- read.table("Cdif_allocc_bioclimdata.txt", header=TRUE)


####PCA funtimes for all occurences####
library("ggplot2")
library("grid") 
library("gridBase")

# allclim$Pop <- as.factor(allclim$Pop)
row.names(allclim) <- allclim$Pop

allclim.pca <- prcomp(allclim[c(2,7:26)], center=TRUE, retx=T, scale.=TRUE)
summary(allclim.pca)
# Importance of components:
#                           PC1    PC2    PC3     PC4     PC5     PC6
# Standard deviation     2.6024 2.3915 1.9350 1.18925 1.12557 0.90419
# Proportion of Variance 0.3225 0.2723 0.1783 0.06735 0.06033 0.03893
# Cumulative Proportion  0.3225 0.5948 0.7731 0.84048 0.90081 0.93974

#visualize components
plot(allclim.pca, main="(a) Screeplot, All Occurrences", xlab="Principal component", ylim=c(0,7))
# screeplot(Frclim.pca, type="lines")
biplot(allclim.pca)
biplot(allclim.pca,  main="PCA analysis of climate data", choices=c(1,3))

#see bottom for figure
# 
# variances of the principal components:
apply(allclim.pca$x, 2, var)
#          PC1          PC2          PC3          PC4          PC5 
# 6.772415e+00 5.719035e+00 3.744403e+00 1.414322e+00 1.266899e+00 #find top loadings (for PC1)

loadings <- allclim.pca$rotation[,1]
sort(abs(loadings), decreasing=TRUE)
# bio14       bio17        bio2       bio18        bio5        bio7 
# 0.347298252 0.343228791 0.342039986 0.323926091 0.308432614 0.285987931 
# bio3       bio15         alt        bio9        bio8       bio12 
# 0.267371488 0.261377698 0.238453572 0.201530432 0.165092828 0.155354386 
# Latitude       bio10        bio4        bio6        bio1       bio16 
# 0.152931672 0.135872618 0.119721291 0.118325791 0.050999650 0.024876838 
# bio19       bio13       bio11 
# 0.019259059 0.016668389 0.003925343
# BIO14 = Precipitation of driest month
# BIO17 = Precipitation of driest quarter
# BIO2 = Mean diurnal temperature range (mean of monthly (max temp – min temp))
# BIO18 = Precipitation of Warmest Quarter

#find top loadings (for PC2)
loadings2 <- allclim.pca$rotation[,2]
sort(abs(loadings2), decreasing=TRUE)
# bio19      bio16      bio11      bio13       bio4       bio6 
# 0.33691497 0.33227594 0.33045001 0.32990608 0.31819499 0.31369436 
# bio12       bio1       bio9      bio15       bio7       bio3 
# 0.28758959 0.24161543 0.23271851 0.22874373 0.22423683 0.17648578 
# alt      bio18      bio10   Latitude      bio14       bio2 
# 0.08512139 0.08129536 0.07755328 0.06949477 0.04346524 0.03793857 
# bio5       bio8      bio17 
# 0.02490116 0.02483213 0.01152872 
# BIO19 = Precipitation of coldest quarter
# BIO16 = Precipitation of wettest quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO13 = Precipitation of wettest month

#find top loadings (for PC3)
loadings3 <- allclim.pca$rotation[,3]
sort(abs(loadings3), decreasing=TRUE)
# bio10        bio1       bio11        bio6       bio16       bio13 
# 0.408911794 0.401713309 0.295388960 0.283648170 0.270833872 0.268965076 
# alt       bio19       bio12        bio8        bio5       bio15 
# 0.264317664 0.263442954 0.253582517 0.238366600 0.230121509 0.109256221 
# bio2        bio3        bio7        bio9       bio17    Latitude 
# 0.086360297 0.083438299 0.071199688 0.063846616 0.061261903 0.059453406 
# bio14       bio18        bio4 
# 0.041433713 0.012519479 0.002193115   
# BIO10 = Mean Temperature of Warmest Quarter
# BIO1 = Annual Mean Temperature
# BIO11 = Mean Temperature of Coldest Quarter
# BIO6 = Min temperature of coldest month


#proportional contributions of each bioclim to each PC
#If you want this as a relative contribution then sum up the loadings per column and 
#express each loading as a proportion of the column (loading) sum, taking care to use 
#the absolute values to account for negative loadings.

sweep(abs(allclim.pca$rotation),2, colSums(abs(allclim.pca$rotation)),"/")
#                  PC1         PC2          PC3          PC4        PC5
# Latitude 0.039388227 0.018245091 0.0157690670 0.0067645986 0.06947236
# alt      0.061414769 0.022347690 0.0701060414 0.0403798347 0.07822222
# bio1     0.013135185 0.063433490 0.1065480429 0.0345006000 0.01844354
# bio10    0.034994592 0.020360766 0.1084573262 0.0662075079 0.07185225
# bio11    0.001010989 0.086756040 0.0783472066 0.0061601924 0.03518617
# bio12    0.040012207 0.075503504 0.0672587150 0.0575747438 0.04343001
# bio13    0.004293017 0.086613236 0.0713386931 0.0555832091 0.03270647
# bio14    0.089448197 0.011411324 0.0109896310 0.0211192996 0.03369259
# bio15    0.067318979 0.060054168 0.0289784687 0.0315499737 0.01332905
# bio16    0.006407139 0.087235418 0.0718343614 0.0505288082 0.03261296
# bio17    0.088400089 0.003026740 0.0162487420 0.0225588499 0.03950082
# bio18    0.083428593 0.021343208 0.0033205919 0.0935437814 0.01920913
# bio19    0.004960256 0.088453345 0.0698740383 0.0004096737 0.05056459
# bio2     0.088093907 0.009960358 0.0229056902 0.0551883105 0.03615709
# bio3     0.068862706 0.046334413 0.0221306771 0.0274814418 0.09103151
# bio4     0.030834747 0.083538618 0.0005816888 0.0511544915 0.10466534
# bio5     0.079438180 0.006537527 0.0610360571 0.0527114022 0.04782633
# bio6     0.030475329 0.082357028 0.0752331493 0.0327692614 0.01794677
# bio7     0.073657453 0.058870930 0.0188845807 0.0584758753 0.04403909
# bio8     0.042520386 0.006519403 0.0632229358 0.1384801062 0.06550357
# bio9     0.051905052 0.061097702 0.0169342957 0.0968580380 0.05460814

# #get top 4 PCs
PC1 <- as.matrix(allclim.pca$x[,1])
PC2 <- as.matrix(allclim.pca$x[,2])
PC3 <- as.matrix(allclim.pca$x[,3])
# # PC4 <- as.matrix(Frclim.pca$x[,4])

allclim2 <- cbind(allclim, PC1, PC2, PC3)
# 
# #write table
write.table(allclim2, file="Cdif_allocc_bioclimPCA.txt")
# # 
# allclim <- read.table("Cdif_allocc_bioclimPCA.txt", header=TRUE)

####all occ main fig; 95% conf limits of clusters####
# http://stackoverflow.com/questions/20260434/test-significance-of-clusters-on-a-pca-plot
# draw 95% confidence ellipses around clusters. Note that stat_ellipse(...) uses the bivariate t-distribution.
scores <- allclim.pca$x[,1:3]                        # scores for first three PC's

# k-means clustering [assume 2 clusters]
km     <- kmeans(scores, centers=2, nstart=5)
ggdata <- data.frame(scores, Cluster=km$cluster, Origin=allclim$Origin, alt=allclim$alt,Pop=allclim$Pop)
levels(ggdata$Origin)[levels(ggdata$Origin)=="inv"] <- "Invasive C. diffusa"
levels(ggdata$Origin)[levels(ggdata$Origin)=="nat"] <- "Native C. diffusa"

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
  geom_point(data=centroids, aes(x=PC1, y=PC2, color=Origin, shape=Origin), size=8)+
  #coord_cartesian(ylim = c(-6.5, 8.5)) +
  theme_bw() + 
  theme(legend.justification=c(1,0), legend.position=c(1,0),
        legend.title = element_text(size=10, face="bold"),
        legend.text = element_text(size = 10))
  
Oplot
ggsave("KTurnerFig4.pdf", width=6.65, height = 5)

svg("KTurnerFig4.svg", width=6.65, height=5, pointsize = 12)
Oplot
dev.off()

#PC1 vs PC3
plot <- ggplot(ggdata, aes_string(x="PC1", y="PC3")) +
  geom_point(aes(color=factor(Origin),shape=Origin), size=3) +
  stat_ellipse(aes(x=PC1,y=PC3,fill=factor(Origin)),
               geom="polygon", level=0.95, alpha=0.2) +
  guides(color=guide_legend("Origin"),fill=guide_legend("Origin"))
plot

#orienting
head(subset(allclim2, PC1< -4))
head(subset(allclim2, PC1> 4))

head(subset(allclim2, PC2< -4))
head(subset(allclim2, PC2> 4))

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

####ade4 to quantify centroid shift####
library(ade4)

allclim.dudi <- dudi.pca(allclim[c(2,7:26)], center = TRUE, scale = TRUE,scannf = TRUE, nf = 2)
2
allclim.bca <- bca(allclim.dudi, fac=allclim$Origin, scannf=TRUE, nf=2) #p36
2
summary(allclim.bca)
print(allclim.bca)
allclim.bca$ratio
[1] 0.0684579
randtest(allclim.bca, nrept=999)
plot(randtest(allclim.bca, nrept=999))

####inherent clusters?####
scores <- allclim.pca$x[,1:3]                        # scores for first three PC's

# k-means clustering [assume 2 clusters]
km     <- kmeans(scores, centers=2, nstart=10)
ggdata <- data.frame(scores, Cluster=km$cluster, Origin=allclim$Origin, alt=allclim$alt,Pop=allclim$Pop,Latitude=allclim$Latitude)

# stat_ellipse is not part of the base ggplot package
source("https://raw.github.com/low-decarie/FAAV/master/r/stat-ellipse.R") 

#PC1 vs PC2
plot <- ggplot(ggdata, aes_string(x="PC1", y="PC2")) +
  geom_point(aes(color=factor(Cluster),shape=Origin), size=5) +
  stat_ellipse(aes(x=PC1,y=PC2,fill=factor(Cluster)),
               geom="polygon", level=0.95, alpha=0.2) +
  guides(color=guide_legend("Cluster"),fill=guide_legend("Cluster"))
plot

####subset pops, same pattern?####
#remove pops that are more than 2 std dev away from mean in either pc1 or 2
mean(allclim2$PC1)
[1] 1.79859e-16
sd(allclim2$PC1)
[1] 2.602386
PC1up <- mean(allclim2$PC1)+2*sd(allclim2$PC1)
PC1low <- mean(allclim2$PC1)-2*sd(allclim2$PC1)
mean(allclim2$PC2)
[1] -2.037455e-16
sd(allclim2$PC2)
[1] 2.39145
PC2up <- mean(allclim2$PC2)+2*sd(allclim2$PC2)
PC2low <- mean(allclim2$PC2)-2*sd(allclim2$PC2)

#pca on subset
acsubset <- subset(allclim2, PC1>PC1low&PC1<PC1up&PC2>PC2low&PC2<PC2up, select=-c(27:29))

acsubset.pca <- prcomp(acsubset[c(2,7:26)], center=TRUE, retx=T, scale.=TRUE)
summary(acsubset.pca)

loadings <- acsubset.pca$rotation[,1]
sort(abs(loadings), decreasing=TRUE)
#      bio17      bio14       bio2       bio7      bio18       bio5 
# 0.32713176 0.32685789 0.32023825 0.30460886 0.29634896 0.28816759 
# BIO14 = Precipitation of driest month
# BIO17 = Precipitation of driest quarter
# BIO2 = Mean diurnal temperature range (mean of monthly (max temp – min temp))
# BIO18 = Precipitation of Warmest Quarter

#find top loadings (for PC2)
loadings2 <- acsubset.pca$rotation[,2]
sort(abs(loadings2), decreasing=TRUE)
#      bio11       bio1       bio6      bio10       bio9       bio4 
# 0.45600558 0.44321086 0.41396779 0.33865248 0.24396497 0.24200044 
# BIO19 = Precipitation of coldest quarter
# BIO16 = Precipitation of wettest quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO13 = Precipitation of wettest month

#find top loadings (for PC3)
loadings3 <- acsubset.pca$rotation[,3]
sort(abs(loadings3), decreasing=TRUE)
#       bio19       bio16       bio13       bio12       bio15 
# 0.446496036 0.431596200 0.429458611 0.293021594 0.290832118 
# BIO10 = Mean Temperature of Warmest Quarter
# BIO1 = Annual Mean Temperature
# BIO11 = Mean Temperature of Coldest Quarter
# BIO6 = Min temperature of coldest month

#ellipses
# http://stackoverflow.com/questions/20260434/test-significance-of-clusters-on-a-pca-plot
# draw 95% confidence ellipses around clusters. Note that stat_ellipse(...) uses the bivariate t-distribution.
scores <- acsubset.pca$x[,1:3]                        # scores for first three PC's

# k-means clustering [assume 2 clusters]
km     <- kmeans(scores, centers=2, nstart=5)
ggdata <- data.frame(scores, Cluster=km$cluster, Origin=acsubset$Origin, alt=acsubset$alt,Pop=acsubset$Pop)
levels(ggdata$Origin)[levels(ggdata$Origin)=="inv"] <- "Invasive C. diffusa"
levels(ggdata$Origin)[levels(ggdata$Origin)=="nat"] <- "Native C. diffusa"

# stat_ellipse is not part of the base ggplot package
source("https://raw.github.com/low-decarie/FAAV/master/r/stat-ellipse.R") 

#centroid based on origin
centroids <- aggregate(cbind(PC1,PC2)~Origin,data=ggdata,mean)
pacsub <- ggplot(ggdata, aes_string(x="PC1", y="PC2")) +
  geom_point(aes(color=factor(Origin),shape=Origin), size=3) +
  guides(color=guide_legend("Origin"),fill=guide_legend("Origin"))+
  stat_ellipse(aes(x=PC1,y=PC2,fill=factor(Origin)),
               geom="polygon", level=0.95, alpha=0.2) +
  geom_point(data=centroids, aes(x=PC1, y=PC2, color=Origin, shape=Origin), size=8)+
  #coord_cartesian(ylim = c(-6.5, 8.5)) +
  theme_bw() + 
  theme(legend.position="none")+
  ggtitle("(b)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))

pacsub
ggsave("Fr_subsetocc_niche.png")

#centroid based on origin PC1 vs PC3
centroids <- aggregate(cbind(PC1,PC3)~Origin,data=ggdata,mean)
pacsub_PC3 <- ggplot(ggdata, aes_string(x="PC1", y="PC3")) +
  geom_point(aes(color=factor(Origin),shape=Origin), size=3) +
  guides(color=guide_legend("Origin"),fill=guide_legend("Origin"))+
  stat_ellipse(aes(x=PC1,y=PC3,fill=factor(Origin)),
               geom="polygon", level=0.95, alpha=0.2) +
  geom_point(data=centroids, aes(x=PC1, y=PC3, color=Origin, shape=Origin), size=8)+
  #coord_cartesian(ylim = c(-6.5, 8.5)) +
  theme_bw() + 
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.title = element_text(size=10, face="bold"),
        legend.text = element_text(size = 10))+
  ggtitle("(c)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))

pacsub_PC3
# ggsave("Fr_subsetocc_niche.png")


#centroid shift
library(ade4)

acsubset.dudi <- dudi.pca(acsubset[c(2,7:26)], center = TRUE, scale = TRUE,scannf = TRUE, nf = 2)
2
acsubset.bca <- bca(acsubset.dudi, fac=acsubset$Origin, scannf=TRUE, nf=2) #p36
2
summary(acsubset.bca)
print(acsubset.bca)
acsubset.bca$ratio
[1] 0.09314062
randtest(acsubset.bca, nrept=999)
plot(randtest(acsubset.bca, nrept=999))

####sup mat fig for subset####

library(gridBase) #necessary to plot ggplots and base plots together

# pdf("KTurnerSup_MontPCA.pdf", useDingbats=FALSE, width=13.38)
png("KTurnerSup_subsetoccPCA.png", width=800, height = 800) #,,pointsize = 12
# postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)

plot.new()
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))

pushViewport(viewport(layout.pos.col = 1, layout.pos.row=1))
par(fig = gridFIG(), new = TRUE, adj=0)
plot(acsubset.pca, main="(a)", xlab="Principal component", ylim=c(0,8), cex.main=1.3)
popViewport()

pushViewport(viewport(layout.pos.col = 2, layout.pos.row=1))
print(pacsub, newpage = FALSE)
popViewport()

pushViewport(viewport(layout.pos.col = 1, layout.pos.row=2))
print(pacsub_PC3, newpage = FALSE)
popViewport()

dev.off()
# par(mfrow=c(2,2),adj=0)
# 
# plot(acsubset.pca, main="(a)", xlab="Principal component", ylim=c(0,8), cex.main=1.3)
# plot.new()
# vps <- baseViewports()
# pushViewport(vps$figure)
# vp1 <- plotViewport(c(0,0,0,0))
# print(pacsub, vp=vp1)
# plot.new()
# pushViewport(vps$figure)
# # vps <- baseViewports()
# # pushViewport(vps$figure)
# 
# vp2 <- plotViewport(c(0,0,0,0))
# print(pacsub_PC3, vp=vp2)




####all occ PC1 vs PC2 fig####
# library("ggplot2")
# library("grid") 
data <- data.frame(obsnames=row.names(allclim.pca$x), allclim.pca$x)
data <- merge(data, allclim[,c(1,3:5)],by.x="obsnames", by.y="Pop")
levels(data$Origin)[levels(data$Origin)=="inv"] <- "Invasive C. diffusa"
levels(data$Origin)[levels(data$Origin)=="nat"] <- "Native C. diffusa"

# pdf("KTurnerFig2.pdf", useDingbats=FALSE, width=4.4, height=4.8, pointsize = 12) #3.149, 4.4 or 6.65
# # png("FrClimatePCA.png",width=800, height = 600, pointsize = 16)
# # postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)
# 
pall <- ggplot(data, aes_string(x="PC1", y="PC2")) + 
  geom_point(aes(shape=Origin, color=Origin), size=3) +
  #   scale_x_continuous(expand = c(0,1)) #+
  theme_bw() +
  theme(legend.position="none")

pall

pall <- pall + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
datapc <- data.frame(varnames=rownames(allclim.pca$rotation), allclim.pca$rotation)
mult <- min(
  (max(data[,"PC2"]) - min(data[,"PC2"])/(max(datapc[,"PC2"])-min(datapc[,"PC2"]))),
  (max(data[,"PC1"]) - min(data[,"PC1"])/(max(datapc[,"PC1"])-min(datapc[,"PC1"])))
)
datapc <- transform(datapc,
                    v1 = .7 * mult * (get("PC1")),
                    v2 = .7 * mult * (get("PC2"))
)

pall <- pall + coord_equal(ratio=4/7) + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), 
                                         size = 6, vjust=1, color="gray47", alpha=0.75)
pall <- pall + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), 
                            arrow=arrow(length=unit(0.2,"cm")), alpha=0.4, color="gray47")+
  ggtitle("(b)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pall
ggsave("allClimPCA.png")
# dev.off()

####PC1 vs PC3####
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

#pc1 vs pc3 overlap
scores <- allclim.pca$x[,1:3]                        # scores for first three PC's
km     <- kmeans(scores, centers=2, nstart=5)
ggdata <- data.frame(scores, Cluster=km$cluster, Origin=allclim$Origin, alt=allclim$alt,Pop=allclim$Pop)
levels(ggdata$Origin)[levels(ggdata$Origin)=="inv"] <- "Invasive C. diffusa"
levels(ggdata$Origin)[levels(ggdata$Origin)=="nat"] <- "Native C. diffusa"

centroids <- aggregate(cbind(PC1,PC3)~Origin,data=ggdata,mean)
OpPC3 <- ggplot(ggdata, aes_string(x="PC1", y="PC3")) +
  geom_point(aes(color=factor(Origin),shape=Origin), size=3) +
  guides(color=guide_legend("Origin"),fill=guide_legend("Origin"))+
  stat_ellipse(aes(x=PC1,y=PC3,fill=factor(Origin)),
               geom="polygon", level=0.95, alpha=0.2) +
  geom_point(data=centroids, aes(x=PC1, y=PC3, color=Origin, shape=Origin), size=8)+
  #coord_cartesian(ylim = c(-6.5, 8.5)) +
  theme_bw() + 
  theme(legend.justification=c(1,0), legend.position=c(1,0),
        legend.title = element_text(size=10, face="bold"),
        legend.text = element_text(size = 10))+
  ggtitle("(c)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))

OpPC3

####all occ sup mat fig####
library(gridBase) #necessary to plot ggplots and base plots together

# pdf("KTurnerSup_MontPCA.pdf", useDingbats=FALSE, width=13.38)
png("KTurnerSup_alloccPCA.png", width=800, height = 800) #,,pointsize = 12
# postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)

plot.new()
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))

pushViewport(viewport(layout.pos.col = 1, layout.pos.row=1))
par(fig = gridFIG(), new = TRUE, adj=0)
plot(allclim.pca, main="(a)", xlab="Principal component", ylim=c(0,8), cex.main=1.1)
popViewport()

pushViewport(viewport(layout.pos.col = 2, layout.pos.row=1))
print(pall, newpage = FALSE)
popViewport()

pushViewport(viewport(layout.pos.col = 1, layout.pos.row=2))
print(OpPC3, newpage = FALSE)
popViewport()

dev.off()

# par(mfrow=c(2,2),adj=0)
# 
# plot(acsubset.pca, main="(a)", xlab="Principal component", ylim=c(0,8), cex.main=1.3)
# plot.new()
# vps <- baseViewports()
# pushViewport(vps$figure)
# vp1 <- plotViewport(c(0,0,0,0))
# print(pacsub, vp=vp1)
# plot.new()
# pushViewport(vps$figure)
# # vps <- baseViewports()
# # pushViewport(vps$figure)
# 
# vp2 <- plotViewport(c(0,0,0,0))
# print(pacsub_PC3, vp=vp2)


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