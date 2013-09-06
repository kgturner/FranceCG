#raster notes

#open file
r <- raster(filename)
filename(r)

#[1] "c:\\temp\\RtmpGQO8Nk\\Rinst14fc1dbd23d3\\raster\\external\\test.grd"
hasValues(r)
#[1] TRUE
inMemory(r)
#[1] FALSE
plot(r, main='RasterLayer from file')

#Multi-layer objects can be created in memory (fromRasterLayerobjects)or from files.
# create three identical RasterLayer objects
r1 <- r2 <- r3 <- raster(nrow=10, ncol=10)
# Assign random cell values
values(r1) <- runif(ncell(r1))
values(r2) <- runif(ncell(r2))
values(r3) <- runif(ncell(r3))
# combine three RasterLayer objects into a RasterStack
s <- stack(r1, r2, r3)
#get class, dimensions,resolution, extent, coord. ref., min values, max values 

nlayers(s)
#[1] 3
# combine three RasterLayer objects into a RasterBrick
b1 <- brick(r1, r2, r3)
# equivalent to:
b2 <- brick(s)
# create a RasterBrick from file
filename <- system.file("external/rlogo.grd", package="raster")
filename
#[1] "c:/temp/RtmpGQO8Nk/Rinst14fc1dbd23d3/raster/external/rlogo.grd"
b <- brick(filename)
b
nlayers(b)
#[1] 3
# extract a single RasterLayer
r <- raster(b, layer=2)
# equivalent to creating it from disk
r <- raster(filename, band=2)

#Raster algebra
#Summary functions(min, max, mean, prod, sum, Median, cv, range,
#any, all)always return aRasterLayerobject. Perhaps this is not obvious when
#using functions likemin, sum or mean.

a <- mean(r,s,10)
b <- sum(r,s)
st <- stack(r, s, a, b)
sst <- sum(st)
sst
#class : RasterLayer, etc.

#Use cellStats if instead of a RasterLayeryou want a single number sum-marizing the cell values of each layer.
cellStats(st, 'sum')
#layer.1 layer.2 layer.3 layer.4 layer.5
#25.0 25.0 50.0 87.5 100.0
cellStats(sst, 'sum')
#[1] 287.5

#Accessing cell values

#getValues to get all values or a single row
r <- raster(system.file("external/test.grd", package="raster"))
getValues(r, 50)[35:39]
#[1] 456.878 485.538 550.788 580.339 590.029
#getValuesBlock to read a block(rectangle) of cell values
getValuesBlock(r, 50, 1, 35, 5)
#[1] 456.878 485.538 550.788 580.339 590.029
#extract using cell numbers/coordinates
cells <- cellFromRowCol(r, 50, 35:39)
cells
#[1] 3955 3956 3957 3958 3959
extract(r, cells)
#[1] 456.878 485.538 550.788 580.339 590.029
xy = xyFromCell(r, cells)
xy
# x y
# [1,] 179780 332020
# [2,] 179820 332020
# [3,] 179860 332020
# [4,] 179900 332020
# [5,] 179940 332020
extract(r, xy)
#[1] 456.878 485.538 550.788 580.339 590.02


#extracting cell values by coordinates
point<-SpatialPoints(as.matrix(t(c(-100,49))
+ )
+ )
> point
SpatialPoints:
     coords.x1 coords.x2
[1,]      -100        49
Coordinate Reference System (CRS) arguments: NA 
> extract(bioclim12.1, point)
[1] 19
> point<-SpatialPoints(as.matrix(t(c(-100,30))))
> extract(bioclim12.1, point)
[1] 178

#to make my table!
#load packages: raster, rgdal, sp

#load files for one tile
tile12bioclim1 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio1_12.tif")
hasValues(tile12bioclim1)
#[1] TRUE
inMemory(tile12bioclim1)
#[1] FALSE
#repeat for all other tiffs
#tile12
tile12bioclim2 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio2_12.tif")
hasValues(tile12bioclim2)
inMemory(tile12bioclim2)
tile12bioclim3 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio3_12.tif")
hasValues(tile12bioclim3)
inMemory(tile12bioclim3)
tile12bioclim4 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio4_12.tif")
hasValues(tile12bioclim4)
inMemory(tile12bioclim4)
tile12bioclim5 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio5_12.tif")
hasValues(tile12bioclim5)
inMemory(tile12bioclim5)
tile12bioclim6 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio6_12.tif")
hasValues(tile12bioclim6)
inMemory(tile12bioclim6)
tile12bioclim7 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio7_12.tif")
hasValues(tile12bioclim7)
inMemory(tile12bioclim7)
tile12bioclim8 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio8_12.tif")
hasValues(tile12bioclim8)
inMemory(tile12bioclim8)
tile12bioclim9 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio9_12.tif")
hasValues(tile12bioclim9)
inMemory(tile12bioclim9)
tile12bioclim10 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio10_12.tif")
hasValues(tile12bioclim10)
inMemory(tile12bioclim10)
tile12bioclim11 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio11_12.tif")
hasValues(tile12bioclim11)
inMemory(tile12bioclim11)
tile12bioclim12 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio12_12.tif")
hasValues(tile12bioclim12)
inMemory(tile12bioclim12)
tile12bioclim13 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio13_12.tif")
hasValues(tile12bioclim13)
inMemory(tile12bioclim13)
tile12bioclim14 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio14_12.tif")
hasValues(tile12bioclim14)
inMemory(tile12bioclim14)
tile12bioclim15 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio15_12.tif")
hasValues(tile12bioclim15)
inMemory(tile12bioclim15)
tile12bioclim16 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio16_12.tif")
hasValues(tile12bioclim16)
inMemory(tile12bioclim16)
tile12bioclim17 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio17_12.tif")
hasValues(tile12bioclim17)
inMemory(tile12bioclim17)
tile12bioclim18 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio18_12.tif")
hasValues(tile12bioclim18)
inMemory(tile12bioclim18)
tile12bioclim19 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_12\\bio19_12.tif")
hasValues(tile12bioclim19)
inMemory(tile12bioclim19)
#tile13
tile13bioclim1 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio1_13.tif")
hasValues(tile13bioclim1)
inMemory(tile13bioclim1)
tile13bioclim2 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio2_13.tif")
hasValues(tile13bioclim2)
inMemory(tile13bioclim2)
tile13bioclim3 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio3_13.tif")
hasValues(tile13bioclim3)
inMemory(tile13bioclim3)
tile13bioclim4 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio4_13.tif")
hasValues(tile13bioclim4)
inMemory(tile13bioclim4)
tile13bioclim5 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio5_13.tif")
hasValues(tile13bioclim5)
inMemory(tile13bioclim5)
tile13bioclim6 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio6_13.tif")
hasValues(tile13bioclim6)
inMemory(tile13bioclim6)
tile13bioclim7 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio7_13.tif")
hasValues(tile13bioclim7)
inMemory(tile13bioclim7)
tile13bioclim8 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio8_13.tif")
hasValues(tile13bioclim8)
inMemory(tile13bioclim8)
tile13bioclim9 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio9_13.tif")
hasValues(tile13bioclim9)
inMemory(tile13bioclim9)
tile13bioclim10 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio10_13.tif")
hasValues(tile13bioclim10)
inMemory(tile13bioclim10)
tile13bioclim11 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio11_13.tif")
hasValues(tile13bioclim11)
inMemory(tile13bioclim11)
tile13bioclim12 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio12_13.tif")
hasValues(tile13bioclim12)
inMemory(tile13bioclim12)
tile13bioclim13 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio13_13.tif")
hasValues(tile13bioclim13)
inMemory(tile13bioclim13)
tile13bioclim14 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio14_13.tif")
hasValues(tile13bioclim14)
inMemory(tile13bioclim14)
tile13bioclim15 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio15_13.tif")
hasValues(tile13bioclim15)
inMemory(tile13bioclim15)
tile13bioclim16 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio16_13.tif")
hasValues(tile13bioclim16)
inMemory(tile13bioclim16)
tile13bioclim17 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio17_13.tif")
hasValues(tile13bioclim17)
inMemory(tile13bioclim17)
tile13bioclim18 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio18_13.tif")
hasValues(tile13bioclim18)
inMemory(tile13bioclim18)
tile13bioclim19 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_13\\bio19_13.tif")
hasValues(tile13bioclim19)
inMemory(tile13bioclim19)
#tile16
tile16bioclim1 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio1_16.tif")
hasValues(tile16bioclim1)
inMemory(tile16bioclim1)
tile16bioclim2 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio2_16.tif")
hasValues(tile16bioclim2)
inMemory(tile16bioclim2)
tile16bioclim3 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio3_16.tif")
hasValues(tile16bioclim3)
inMemory(tile16bioclim3)
tile16bioclim4 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio4_16.tif")
hasValues(tile16bioclim4)
inMemory(tile16bioclim4)
tile16bioclim5 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio5_16.tif")
hasValues(tile16bioclim5)
inMemory(tile16bioclim5)
tile16bioclim6 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio6_16.tif")
hasValues(tile16bioclim6)
inMemory(tile16bioclim6)
tile16bioclim7 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio7_16.tif")
hasValues(tile16bioclim7)
inMemory(tile16bioclim7)
tile16bioclim8 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio8_16.tif")
hasValues(tile16bioclim8)
inMemory(tile16bioclim8)
tile16bioclim9 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio9_16.tif")
hasValues(tile16bioclim9)
inMemory(tile16bioclim9)
tile16bioclim10 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio10_16.tif")
hasValues(tile16bioclim10)
inMemory(tile16bioclim10)
tile16bioclim11 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio11_16.tif")
hasValues(tile16bioclim11)
inMemory(tile16bioclim11)
tile16bioclim12 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio12_16.tif")
hasValues(tile16bioclim12)
inMemory(tile16bioclim12)
tile16bioclim13 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio13_16.tif")
hasValues(tile16bioclim13)
inMemory(tile16bioclim13)
tile16bioclim14 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio14_16.tif")
hasValues(tile16bioclim14)
inMemory(tile16bioclim14)
tile16bioclim15 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio15_16.tif")
hasValues(tile16bioclim15)
inMemory(tile16bioclim15)
tile16bioclim16 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio16_16.tif")
hasValues(tile16bioclim16)
inMemory(tile16bioclim16)
tile16bioclim17 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio17_16.tif")
hasValues(tile16bioclim17)
inMemory(tile16bioclim17)
tile16bioclim18 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio18_16.tif")
hasValues(tile16bioclim18)
inMemory(tile16bioclim18)
tile16bioclim19 <- raster("~\\grad work\\Courses\\KH_microarray\\bio_16\\bio19_16.tif")
hasValues(tile16bioclim19)
inMemory(tile16bioclim19)

#make a vector for each tile
tile12tiffvector<-c(tile12bioclim1, tile12bioclim2, tile12bioclim3, tile12bioclim4,tile12bioclim5,tile12bioclim6,tile12bioclim7,tile12bioclim8,tile12bioclim9,tile12bioclim10,tile12bioclim11,tile12bioclim12,tile12bioclim13,tile12bioclim14,tile12bioclim15,tile12bioclim16,tile12bioclim17,tile12bioclim18,tile12bioclim19)
tile13tiffvector<-c(tile13bioclim1, tile13bioclim2, tile13bioclim3, tile13bioclim4,tile13bioclim5,tile13bioclim6,tile13bioclim7,tile13bioclim8,tile13bioclim9,tile13bioclim10,tile13bioclim11,tile13bioclim12,tile13bioclim13,tile13bioclim14,tile13bioclim15,tile13bioclim16,tile13bioclim17,tile13bioclim18,tile13bioclim19)

#tile15tiffvector<-c(tile12bioclim1, tile12bioclim2, tile12bioclim3, tile12bioclim4,tile12bioclim5,tile12bioclim6,tile12bioclim7,tile12bioclim8,tile12bioclim9,tile12bioclim10,tile12bioclim11,tile12bioclim12,tile12bioclim13,tile12bioclim14,tile12bioclim15,tile12bioclim16,tile12bioclim17,tile12bioclim18,tile12bioclim19)
tile16tiffvector<-c(tile16bioclim1, tile16bioclim2, tile16bioclim3, tile16bioclim4,tile16bioclim5,tile16bioclim6,tile16bioclim7,tile16bioclim8,tile16bioclim9,tile16bioclim10,tile16bioclim11,tile16bioclim12,tile16bioclim13,tile16bioclim14,tile16bioclim15,tile16bioclim16,tile16bioclim17,tile16bioclim18,tile16bioclim19)

#make empty vector for values
valuevectorPop1<-NULL
valuevectorPop2<-NULL
valuevectorPop3<-NULL
valuevectorPop4<-NULL
valuevectorPop5<-NULL
valuevectorPop6<-NULL
valuevectorPop7<-NULL
valuevectorPop8<-NULL
valuevectorPop9<-NULL
valuevectorPop10<-NULL
valuevectorPop11<-NULL
valuevectorPop12<-NULL

#load each collection site as SpatialPoints
#check for weird columns?
popdata<-read.table(file.choose(), header=T, sep="")
#longitude, latitude!!
Pop1<-SpatialPoints(as.matrix(t(c(-95.958333,44.32536))))
Pop1<-SpatialPoints(as.matrix(t(c(popdata[1,4], popdata[1,3]))))
#rest of the pops
Pop2<-SpatialPoints(as.matrix(t(c(popdata[2,4], popdata[2,3]))))
Pop3<-SpatialPoints(as.matrix(t(c(popdata[3,4], popdata[3,3]))))
Pop4<-SpatialPoints(as.matrix(t(c(popdata[4,4], popdata[4,3]))))
Pop5<-SpatialPoints(as.matrix(t(c(popdata[5,4], popdata[5,3]))))
Pop6<-SpatialPoints(as.matrix(t(c(popdata[6,4], popdata[6,3]))))
Pop7<-SpatialPoints(as.matrix(t(c(popdata[7,4], popdata[7,3]))))
Pop8<-SpatialPoints(as.matrix(t(c(popdata[8,4], popdata[8,3]))))
Pop9<-SpatialPoints(as.matrix(t(c(popdata[9,4], popdata[9,3]))))
Pop10<-SpatialPoints(as.matrix(t(c(popdata[10,4], popdata[10,3]))))
Pop11<-SpatialPoints(as.matrix(t(c(popdata[11,4], popdata[11,3]))))
Pop12<-SpatialPoints(as.matrix(t(c(popdata[12,4], popdata[12,3]))))

#which tile? check pops 5 and 6 for tile13 and pop 8 for tile15
extract(tile16bioclim1, Pop12)

#for loop for one collection site, creating value vector
for(map in tile12tiffvector){
  myValue<-extract(map, Pop1)
  valuevectorPop1<-c(valuevectorPop1, myValue)
}
#repeat for all pops, tile12
for(map in tile12tiffvector){
  myValue<-extract(map, Pop2)
  valuevectorPop2<-c(valuevectorPop2, myValue)
}
for(map in tile12tiffvector){
  myValue<-extract(map, Pop3)
  valuevectorPop3<-c(valuevectorPop3, myValue)
}
for(map in tile12tiffvector){
  myValue<-extract(map, Pop4)
  valuevectorPop4<-c(valuevectorPop4, myValue)
}
#for loops, tile13
for(map in tile13tiffvector){
  myValue<-extract(map, Pop5)
  valuevectorPop5<-c(valuevectorPop5, myValue)
}
for(map in tile13tiffvector){
  myValue<-extract(map, Pop6)
  valuevectorPop6<-c(valuevectorPop6, myValue)
}
#for loops, tile16
for(map in tile16tiffvector){
  myValue<-extract(map, Pop7)
  valuevectorPop7<-c(valuevectorPop7, myValue)
}
for(map in tile16tiffvector){
  myValue<-extract(map, Pop8)
  valuevectorPop8<-c(valuevectorPop8, myValue)
}
for(map in tile16tiffvector){
  myValue<-extract(map, Pop9)
  valuevectorPop9<-c(valuevectorPop9, myValue)
}
for(map in tile16tiffvector){
  myValue<-extract(map, Pop10)
  valuevectorPop10<-c(valuevectorPop10, myValue)
}
for(map in tile16tiffvector){
  myValue<-extract(map, Pop11)
  valuevectorPop11<-c(valuevectorPop11, myValue)
}
for(map in tile16tiffvector){
  myValue<-extract(map, Pop12)
  valuevectorPop12<-c(valuevectorPop12, myValue)
}


#check length of all vectors
length(valuevectorPop4)
#join value vectors into table
KHbioclim<-rbind(valuevectorPop1,valuevectorPop2,valuevectorPop3,valuevectorPop4,valuevectorPop5,valuevectorPop6,valuevectorPop7,valuevectorPop8,valuevectorPop9,valuevectorPop10,valuevectorPop11,valuevectorPop12)
KHbioclim<-as.data.frame(KHbioclim)
#change col names
colnames(KHbioclim)<-c("BIO1", "BIO2", "BIO3", "BIO4","BIO5","BIO6","BIO7","BIO8","BIO9","BIO10","BIO11","BIO12","BIO13","BIO14","BIO15","BIO16","BIO17","BIO18","BIO19")
#add pop names
KHbioclim<-data.frame(KHbioclim, popdata$Population)
#change rownames
rownames(KHbioclim)<-NULL
names(KHbioclim)[20]<-"Population"

#write table
write.csv(KHbioclim, file="KHbioclimdata.csv")

######
a <- c(1,2,3,4)
b <- c(2,4,6,8)
levels <- factor(c("A","B","A","B"))
bubba <- rbind(a, b,levels)
bioclimvector<-c("bioclim1", "bioclim2", "bioclim3", "bioclim4")
tbubba<-t(bubba)
colnames(tbubba)<-bioclimvector
pop<-c("pop1", "pop2", "pop3")
tbubba<-data.frame(tbubba, pop)


popdata<-data.frame(popdata)
class(popdata)

popdata$X<-NULL
popdata$X.1<-NULL
popdata$X.2<-NULL
popdata$X.3<-NULL
View(popdata)
write.table(popdata, file="popdataG.txt", quote=T, sep="")
popdata<- read.delim(file.choose(), header = T, )
popdataG<-read.delim(file.choose(), header=T)