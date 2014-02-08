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

# tiffvector <- NULL
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

#tidy table
popnames <- sort(as.character(Frpop$Pop))
Frclim <- as.data.frame(climate, row.names=popnames)

colnames(Frclim) <- filenames

#write table
write.table(Frclim, file="Frbioclimdata.txt")

#load table
Frclim <- read.table("Frbioclimdata.txt", header=TRUE)

##################################
# #Problem with CA008 - should be in tile 12, data coming from tile 11?
# 
# #load files for tile 12
# tile12alt <- raster("~/grad work/Centaurea diffusa/WorldClim_2013/alt_12.tif")
# hasValues(tile12alt)
# #[1] TRUE
# inMemory(tile12alt)
# #[1] FALSE
# 
# #extracting cell values by coordinates
# CA008<-SpatialPoints(as.matrix(t(c(-118.64571,49.01208))))
# CA008
# # SpatialPoints:
# #   coords.x1 coords.x2
# # [1,]      -100        49
# # Coordinate Reference System (CRS) arguments: NA 
# extract(tile12alt, CA008)
# #      1308 
# 
# #load files for tile 11
# tile11alt <- raster("~/grad work/Centaurea diffusa/WorldClim_2013/alt_11.tif")
# hasValues(tile11alt)
# #[1] TRUE
# inMemory(tile11alt)
# #[1] FALSE
# extract(tile11alt, CA008)
# #[1] NA
# 
# #ummmmmmmmmmmmmmmm
# 
# #problem here with either combining commands or possibly order of nesting of loops
# 
# #check that spatial points load correctly from geoTIFFs
# #no column should be entirely NAs
# poplist <- mget(levels(Frpop$Pop), envir=globalenv())
# subpoplist <- head(poplist)
# 
# # tiffvector <- NULL
# tiffvector <- unlist(list)
# subtiffvector <- head(tiffvector)
# 
# foreach(p=subpoplist, .combine='rbind') %:%
#   foreach(t=subtiffvector, .combine='cbind') %do%{
#     is.na(extract(t,p))
#   }
# 
# #make climate data table
# subclimate <- foreach(p=subpoplist, .combine='rbind') %:%    
#   foreach(t=subtiffvector, .combine='cbind') %do%{
#     myValue<-extract(t, p)
#   } #may take a while
# subclimate
# 
# #tidy table
# subname <- #unlist(subpoplist)
# 
# subpop <- sort(as.character(head(Frpop$Pop)))
# 
# 
# subclim <- as.data.frame(subclimate, row.names=subpop)
# # row.names(subclim) <- levels(subpop$Pop)
# colnames(subclim) <- filenames


##################Squishr##################
#need to merge columns so that all bio1 columns are merged, etc. 
#Should have total of 20 columns (19 bioclim variables + altitude)
Frclim.1 <- Frclim

#function to squish group of columns together

squish <- function(dat=dat, cols=cols, newcol="newcol"){
  
  dat$temp <- NA
#   cols <- 1:6
  for (i in dat[,cols]){
    ## where is temp NA?
    ss <- which(is.na(dat$temp))
    ## what are the values of i there? put them in temp
    dat$temp[ss] <- i[ss] 
    
    #dat$temp <- i[is.na(dat[["temp"]])]
    #which(is.na(dat[,dat$temp]))
    #which(dat[,dat$temp] != i)
  }
  #names(dat["temp"]) <- newcol
  #newcol <- "testalt"
#   names(dat)[which(names(dat)=="temp")] <- newcol
#   colnames(dat$temp) <- newcol
  names(dat)[names(dat)=="temp"] <- newcol
  

#   return(as.data.frame(cbind(rownames(dat),dat$newcol)))
  return(subset(dat,select=ncol(dat)))
}

#for one group of columns
Frclim.2 <- squish(dat=Frclim.1, cols=1:6, newcol="alt")
Frclim.2


# varnames <- colnames(Frclim.1)
# 
# #get column #s for the same variables:
# splitvars <- strsplit(varnames,"_")
# vars <- sapply(splitvars,"[[",i=1)
# unique(vars)
# 
# # outs <- list(NULL)
# # for (k in unique(vars)){
# # squishables <- which(vars==k)
# # outs[[k]] <- squish(dat=Frclim.1, cols=squishables,newcol=k)
# # }
# # outs <- outs[-1]
# # fuckyeah <- do.call(cbind,outs)
# 
# #squishing func w/Andrew
# squishr <- function(tosquish){
#   squishables <- which(vars==tosquish)
#   outs[[k]] <- squish(dat=Frclim.1, cols=squishables,newcol=tosquish)
# }
# 
# do.call(cbind,lapply(unique(vars),squishr))
# #take unique vars and put them in squishr

####rewrite

squishsplit <- function(dat, split="_"){
  varnames <- colnames(dat)
  splitvars <- strsplit(varnames, split)
  squishvars <- sapply(splitvars,"[[",i=1)
  return(squishvars)
  #   tosquish <- lapply(unique(vars))
}

squishr <- function(dat, squishvar){
#   outs <- list(NULL)
#   squishables <- which(squishvars==unique(squishvars))
  return(squish(dat=dat, cols=grep(squishvar,names(dat)),newcol=squishvar))
}

Frvars <- squishsplit(Frclim.1)
test1 <- do.call(cbind,lapply(unique(Frvars),squishr,dat=Frclim.1))

#write table
write.table(test1, file="Frbioclimdata.txt")

#load table
Frclim <- read.table("Frbioclimdata.txt", header=TRUE)

#merge two columns
# ## Copy BNR.y if BNR.x is missing
# d$BNR.x[is.na(d$BNR.x)] <- d$BNR.y[is.na(d$BNR.x)]
# ## List the indices of BNR.x that are still missing
# which(is.na(d$BNR.x))
# ## List the indices where BNR.x is different from BNR.y
# which(d$BNR.x != d$BNR.y)

