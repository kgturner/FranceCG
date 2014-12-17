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
test4 <- read.table("Cdif_allocc_bioclimdata.txt", header=TRUE)


####PCA funtimes for all occurences####
library("ggplot2")
library("grid") 
library("gridBase")

FrclimDK.pca <- prcomp(Frclimdat.dk[2:22], center=TRUE, retx=T, scale.=TRUE)
# summary(FrclimDK.pca)
# # Importance of components:
# #                           PC1    PC2    PC3    PC4     PC5     PC6     PC7     PC8 
# # Standard deviation     2.5324 2.3291 2.0339 1.5321 1.04559 0.85042 0.73721 0.44243 
# # Proportion of Variance 0.3054 0.2583 0.1970 0.1118 0.05206 0.03444 0.02588 0.00932
# # Cumulative Proportion  0.3054 0.5637 0.7607 0.8725 0.92452 0.95896 0.98484 0.99416
# 
# 
# #visualize components
# plot(FrclimDK.pca, main="(a) Screeplot, Montpellier Experiment", xlab="Principal component", ylim=c(0,7))
# # screeplot(Frclim.pca, type="lines")
# biplot(FrclimDK.pca)
# biplot(FrclimDK.pca,  main="PCA analysis of climate data", choices=c(1,3))
# #see bottom for figure
# 
# # variances of the principal components:
# apply(FrclimDK.pca$x, 2, var)
# PC1          PC2          PC3          PC4          PC5          PC6          PC7          PC8          
# 6.413035e+00 5.424698e+00 4.136683e+00 2.347227e+00 1.093252e+00 7.232118e-01 5.434783e-01 1.957406e-01
# 
# 
# #find top loadings (for PC1)
# loadings <- FrclimDK.pca$rotation[,1]
# sort(abs(loadings), decreasing=TRUE)
# # bio11         bio4         bio6         bio9         bio1         bio7        bio19         bio3     Latitude        bio18        bio16        bio13 
# # 0.3477634412 0.3415443495 0.3396753681 0.3227947573 0.2734575490 0.2646601273 0.2622142510 0.2372984061 0.2234071480 0.2026011484 0.1928539203 0.1808279853 
# # bio12        bio15         bio8        bio14        bio10         bio5        bio17          alt         bio2 
# # 0.1681997984 0.1627035834 0.1618949110 0.1077019953 0.0858056376 0.0852779938 0.0686258731 0.0131679736 0.0009167083 
# BIO11 = Mean Temperature of Coldest Quarter
# BIO4 = Temperature Seasonality (standard deviation *100)
# BIO6 = Min Temperature of Coldest Month
# BIO9 = Mean Temperature of Driest Quarter
# 
# #find top loadings (for PC2)
# loadings2 <- FrclimDK.pca$rotation[,2]
# sort(abs(loadings2), decreasing=TRUE)
# # bio5       bio17       bio14       bio12        bio2       bio10        bio7       bio19       bio13    Latitude       bio16       bio18       bio15 
# # 0.374747216 0.371415358 0.354202953 0.306940244 0.268473933 0.246119137 0.241468103 0.234550843 0.222581033 0.217225448 0.211710814 0.182753097 0.151095453 
# # bio4        bio3        bio1        bio9         alt        bio6       bio11        bio8 
# # 0.133955131 0.113901566 0.112861695 0.082237770 0.073618541 0.043044836 0.021681001 0.005394978 
# BIO5 = Max Temperature of Warmest Month
# BIO17 = Precipitation of Driest Quarter
# BIO14 = Precipitation of Driest Month
# BIO12 = Annual Precipitation
# 
# #find top loadings (for PC3)
# loadings3 <- FrclimDK.pca$rotation[,3]
# sort(abs(loadings3), decreasing=TRUE)
# # alt      bio10       bio1       bio2      bio15       bio3       bio8       bio6      bio16      bio13      bio11       bio7      bio19      bio12      bio14 
# # 0.36654119 0.31909538 0.31237598 0.30242551 0.28402020 0.26738636 0.25362066 0.23730987 0.23315531 0.22609464 0.21886604 0.18635341 0.17574859 0.17461695 0.13646132 
# # bio17   Latitude       bio5      bio18       bio9       bio4 
# # 0.13181951 0.08586004 0.05688603 0.04774380 0.04719642 0.03717003 
# altitude
# BIO10 = Mean Temperature of Warmest Quarter
# BIO1 = Annual Mean Temperature
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# 
# 
# #proportional contributions of each bioclim to each PC
# #If you want this as a relative contribution then sum up the loadings per column and 
# #express each loading as a proportion of the column (loading) sum, taking care to use 
# #the absolute values to account for negative loadings.
# 
# sweep(abs(FrclimDK.pca$rotation),2, colSums(abs(FrclimDK.pca$rotation)),"/")
# #                   PC1         PC2         PC3          PC4          PC5          PC6         PC7         PC8
# # alt      0.0032566644 0.018543810 0.089384000 0.0261236303 0.1286202616 0.1476962282 0.001339065 0.007880284
# # bio1     0.0676307136 0.028428788 0.076175380 0.0329832587 0.0206778537 0.0209867327 0.002501644 0.034834596
# # bio10    0.0212211969 0.061995070 0.077813960 0.0692460164 0.0146350015 0.0184793263 0.070201954 0.018238789
# # bio11    0.0860078275 0.005461238 0.053372234 0.0010476343 0.0170430995 0.0237253067 0.041776025 0.018488781
# # bio12    0.0415986775 0.077315329 0.042581740 0.0631192418 0.0008009791 0.0308461161 0.066495498 0.056198119
# # bio13    0.0447218434 0.056066046 0.055134985 0.0969637622 0.0085305767 0.0041111848 0.021444919 0.003847089
# # bio14    0.0266365395 0.089220356 0.033277183 0.0264133058 0.0678903701 0.0944978549 0.023560609 0.029487890
# # bio15    0.0402393698 0.038059508 0.069260597 0.0892038234 0.0205142125 0.0568640256 0.078725826 0.115892881
# # bio16    0.0476960622 0.053327941 0.056856787 0.0940081743 0.0123244071 0.0139913270 0.018431924 0.022226168
# # bio17    0.0169723483 0.093555997 0.032145242 0.0243751323 0.0745874706 0.0712373798 0.040905168 0.041961413
# # bio18    0.0501067178 0.046033767 0.011642706 0.0969667142 0.0744412245 0.1115038556 0.076334608 0.110965426
# # bio19    0.0648500543 0.059081127 0.042857698 0.0119836294 0.0019638103 0.0756627558 0.093926989 0.058351348
# # bio2     0.0002267176 0.067626031 0.073748879 0.0006043175 0.1311551819 0.0475902659 0.022167858 0.041619008
# # bio3     0.0586879412 0.028690722 0.065204303 0.0276584304 0.1329923384 0.0083256916 0.053221605 0.080601063
# # bio4     0.0844697401 0.033742024 0.009064208 0.0440401998 0.0153923275 0.0203901213 0.098172604 0.010249894
# # bio5     0.0210907016 0.094395261 0.013872113 0.0274640281 0.0938031222 0.0321406072 0.068906774 0.001264315
# # bio6     0.0840075091 0.010842585 0.057869908 0.0130125094 0.0016049478 0.0001208292 0.032739910 0.018816482
# # bio7     0.0654549612 0.060823519 0.045443770 0.0267119752 0.0491202696 0.0172229432 0.067103794 0.016526795
# # bio8     0.0400393714 0.001358944 0.061847426 0.1261614229 0.0017959918 0.0513916858 0.008968933 0.004672688
# # bio9     0.0798326463 0.020714912 0.011509224 0.0545300906 0.0216367020 0.0171288165 0.097346339 0.239017090
# # Latitude 0.0552523962 0.054717025 0.020937658 0.0473827031 0.1104698515 0.1360869460 0.015727953 0.068859880
# 
# #get top 4 PCs
# PC1 <- as.matrix(FrclimDK.pca$x[,1])
# PC2 <- as.matrix(FrclimDK.pca$x[,2])
# PC3 <- as.matrix(FrclimDK.pca$x[,3])
# # PC4 <- as.matrix(Frclim.pca$x[,4])
# Frclimdat.dk2 <- cbind(Frclimdat.dk, PC1, PC2, PC3)
# 
# #write table
# write.table(Frclimdat.dk2, file="FrbioclimPCA_DKdat.txt")
# # 
# Frclimdat.dk <- read.table("FrbioclimPCA_DKdat.txt", header=TRUE)

