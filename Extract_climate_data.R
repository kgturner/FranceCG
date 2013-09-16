#Extract climate data from WorldClim.org tiles for several locations and make data table
#Kathryn Turner Sept 16, 2013

#load packages: raster, rgdal, sp
library(rgdal)
library(raster)
library(foreach)

#Read names of all files in directory into a list
#from xyz1111.csv, xyz2222.csv, etc. from http://stackoverflow.com/questions/5319839/read-multiple-csv-files-into-separate-data-frames
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

#get population coordinates
allpop <- read.table(file.choose(), header=T, sep="\t") #Popcoord_worldclim.txt
Frdes <- read.table("Frdes.txt", header=T, sep="\t")
Frpop <- allpop[allpop$Pop %in% Frdes$Pop,]
rownames(Frpop) <- Frpop$Pop
Frpop$Pop <- droplevels(Frpop$Pop)

#load pop coord at SpatialPoints
for(i in Frpop$Pop){
  assign(i,SpatialPoints(as.matrix(t(c(Frpop[i,2], Frpop[i,1])))))
}

#check that spatial points load correctly from geoTIFFs
#no column should be entirely NAs
poplist <- mget(levels(Frpop$Pop), envir=globalenv())

tiffvector <- unlist(list)

foreach(p=poplist, .combine='cbind') %:%
  foreach(t=tiffvector, .combine='rbind') %do%{
    is.na(extract(t,p))
  }

#make climate data table
climate <- foreach(t=tiffvector, .combine='cbind') %:%    
  foreach(p=poplist, .combine='rbind') %do%{
    myValue<-extract(t, p)
  } #may take a while

#tidy table
Frclim <- as.data.frame(climate)
row.names(Frclim) <- Frpop$Pop
colnames(Frclim) <- filenames

#need to merge columns so that all bio1 columns are merged, etc. 
#Should have total of 20 columns (19 bioclim variables + altitude)
Frclim.1 <- Frclim

#do this 20 times?
for (i in Frclim.1[,116:120]){
  Frclim.1[,115][is.na(Frclim.1[,115])] <- i[is.na(Frclim.1[,115])]
  which(is.na(Frclim.1[,115]))
  which(Frclim.1[,115] != i)
}

head(Frclim.1)
tail(Frclim.1)

for (col in Frclim.1[,seq(from=1, to=120, by=6)]) {
  for (i in Frclim.1[,c(col+seq(from=1, to=5, by=1))]) {
    Frclim.1[,col][is.na(Frclim.1[,col])] <- i[is.na(Frclim.1[,col])]
    which(is.na(Frclim.1[,col]))
    which(Frclim.1[,col] != i)
  }
    
}

col <-Frclim.1[,seq(from=1, to=120, by=6)]
head(col)

i <- Frclim.1[,c(col+seq(from=1, to=5, by=1)]

check <- c(col+seq(from=1, to=5, by=1))
#

foreach(col=Frclim.1[,seq(from=1, to=120, by=6)], .combine="cbind") %:%
  foreach(i=Frclim.1[,c(col+seq(from=1, to=5, by=1))], .combine="c") %do% {
    Frclim.1[,col][is.na(Frclim.1[,col])] <- i[is.na(Frclim.1[,col])]
    which(is.na(Frclim.1[,col]))
    which(Frclim.1[,col] != i)
  }
  

#foreach(p=poplist, .combine='cbind') %:%
foreach(t=tiffvector, .combine='rbind') %do%{
  is.na(extract(t,p))
}
# extract(bio9_11.tif, CA001)
# CA001

#merge two columns
# ## Copy BNR.y if BNR.x is missing
# d$BNR.x[is.na(d$BNR.x)] <- d$BNR.y[is.na(d$BNR.x)]
# ## List the indices of BNR.x that are still missing
# which(is.na(d$BNR.x))
# ## List the indices where BNR.x is different from BNR.y
# which(d$BNR.x != d$BNR.y)

# library(reshape2)
# Frclim.1 <- melt(Frclim, id.vars=rownames(Frclim), measure.vars=115:120, na.rm=TRUE)

#write table
write.csv(Frclim, file="Frbioclimdata.csv")


# ####

#join value vectors into table
KHbioclim<-rbind(valuevectorPop1,valuevectorPop2,valuevectorPop3,valuevectorPop4,valuevectorPop5,valuevectorPop6,valuevectorPop7,valuevectorPop8,valuevectorPop9,valuevectorPop10,valuevectorPop11,valuevectorPop12)
KHbioclim<-as.data.frame(KHbioclim)
#change col names
colnames(KHbioclim)<-c("BIO1", "BIO2", "BIO3", "BIO4","BIO5","BIO6","BIO7","BIO8","BIO9","BIO10","BIO11","BIO12","BIO13","BIO14","BIO15","BIO16","BIO17","BIO18","BIO19")
#add pop names
KHbioclim<-data.frame(KHbioclim, popdata$Population)
names(KHbioclim)[20]<-"Population"
#change rownames
rownames(KHbioclim)<-NULL

