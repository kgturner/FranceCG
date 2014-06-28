#map for France data
#5/9/2014


library(rgdal) # Commands for reprojecting the vector data.
library(rworldmap) # Recently updated mapping program.
library(rworldxtra) # Add-ons for rworldmap.

####set up map####
projectionCRS <- CRS("+proj=laea +lon_0=0.001 +lat_0=89.999 +ellps=sphere") #the ellps 'sphere' has a radius of 6370997.0m
par(mai=c(0,0,0.2,0)) #,xaxs="i",yaxs="i"
sPDF <- getMap()[-which(getMap()$ADMIN=='Antarctica')] 
sPDF <- spTransform(sPDF, CRS=projectionCRS)
setLims <- TRUE #FALSE back to whole world
#setLims <- FALSE
if ( !setLims )
{
  xlim <- ylim <- NA
} else
{
  ### TRY FIDDLING WITH THESE LIMITS ###
  xlimUnproj <- c(-52,120)
  ylimUnproj <- c(10,30)
  sPointsLims <- data.frame(x=xlimUnproj, y=ylimUnproj)
  coordinates(sPointsLims) = c("x", "y")
  proj4string(sPointsLims) <- CRS("+proj=longlat +ellps=WGS84")
  sPointsLims <- spTransform(sPointsLims, CRS=projectionCRS)
  xlim <- coordinates(sPointsLims)[,"x"]
  ylim <- coordinates(sPointsLims)[,"y"]  
}

# sPDF <- getMap()
# #list of country names
# sPDF$ADMIN
#setup a color code column filled with numbers
sPDF$colCode <- 4

#set codes for specified countries
sPDF$colCode[ which(sPDF$ADMIN %in% c("Canada","United States of America"))] <- 1
sPDF$colCode[ which(sPDF$ADMIN %in% c("Armenia","Azerbaijan", "Bulgaria", "Georgia", 
                                      "Greece", "Moldova", "Romania","Russia", "Turkey",
                                      "Ukraine", "Serbia"))] <- 2
sPDF$colCode[ which(sPDF$ADMIN %in% c("Poland", "Belarus", "Italy", "Syria", "Czech Republic",
                                      "Estonia", "Switzerland","Latvia","Lithuania", 
                                      "Slovenia", "Serbia","Austria","Belgium", "France",
                                      "Germany","Hungary","Luxembourg","Norway","Slovakia",
                                      "Spain", "United Kingdom", "Kazakhstan", "Turkmenistan", "China"))] <- 3

#create a colour palette - note for each value not for each country
colourPalette <- c("#F8766D","#00BA38", "palegreen","lightgray") #inv, nat, present/naturalized, extra countries

# spName <- plotmath(italic("Centaurea diffusa"))

#points
pop <- read.table("Popcoord.txt", header=TRUE, stringsAsFactor=FALSE)
frend<- read.table("FrEnd.txt", header=T, sep="\t",quote='"', row.names=1)

pop <- merge(pop, unique(frend[,c(1,7)], all.y=TRUE))
#add Montpellier lat:  43.638814°, long:   3.864083°
#and add BG3  Latitude Longitude Pop
#         64  42.1153   23.3203 BG3
pop$Origin <- as.character(pop$Origin)
cnrs <- c("CNRS",43.638814,3.864083, "CNRS")
BG3 <- c("BG3", 42.1153, 23.3203, "sk")
pop <- rbind(pop, cnrs, BG3) #blurgh, factors didn't transfer...
pop$Pop <- as.factor(pop$Pop)
pop$Origin <- as.factor(pop$Origin)
pop$Latitude <- as.numeric(pop$Latitude)
pop$Longitude <- as.numeric(pop$Longitude)

pop$pch <- 1 #for invasives
pop[pop$Origin %in% "nat",]$pch <- 17
pop[pop$Origin %in% "sk",]$pch <- 0
pop[pop$Origin %in% "CNRS",]$pch <- 8

coordinates(pop) = c("Longitude", "Latitude")
proj4string(pop) <- CRS("+proj=longlat +ellps=WGS84")
sPointsDF <- spTransform(pop, CRS=projectionCRS)

#lat markings...
markings <- data.frame(Latitude=as.numeric(c(75,60,45,30,15,85,85)), Longitude=as.numeric(c(-45,-45,-45,-45,-45,0,180)),name=c("75", "60","45","30","15","0","180"))
coordinates(markings) = c("Longitude", "Latitude")
proj4string(markings) <- CRS("+proj=longlat +ellps=WGS84")
sPointsDFmark <- spTransform(markings, CRS=projectionCRS)

####plot map####
# pdf("KTurnerFig1.pdf", useDingbats=FALSE, width=13.38)
png("FrMap.png", width=600, height = 600, pointsize = 16)
#svg("collectionMap_bw.svg", pointsize = 12)
# setEPS( horizontal = FALSE, onefile = FALSE, paper = "special") #340 mm is c. 13 in, height and width in in
# postscript("colMap_bw.eps")
# postscript("KTurnerFig1.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)

# eps <- function(file, onefile=FALSE, width=5.5, height = 5.5, horizontal=FALSE,paper="special", ...){
#   postscript(file=file,width=width,height=height,onefile=onefile,horizontal=horizontal,paper=paper,title=file,...)
#   par(bty='l')
# }

par(mar=c(0,0,0,0))
mapCountryData(sPDF, nameColumnToPlot="colCode", mapTitle=NA,
               colourPalette=colourPalette, borderCol ='gray24', addLegend = FALSE,
               xlim=xlim, ylim=ylim, catMethod=c(0,1,2,3,4))
#note that catMethod defines the breaks and values go in a category if they are <= upper end
#mapTitle=bquote(Global~range~of~italic(Centaurea)~italic(diffusa)) 


points(sPointsDF, pch=pop$pch, cex=1, lwd=2)

llgridlines(sPDF, easts=c(-90,-180,0,90,180), norths=seq(0,90,by=15), 
            plotLabels=FALSE, ndiscr=1000) #ndiscr=num points in lines
text(sPointsDFmark, labels = sPointsDFmark$name, cex=1) #pch2 for triangles

legend("topright", c("Invasive C. diffusa","Native C. diffusa", "Native C. stoebe", "Experimental field"), 
       pch=c(1,17,0,8),  bg="white", title = "Sampled populations", cex=1.2)
legend("bottomleft", c("Invasive", "Native","Naturalized"), fill=colourPalette,
       title="Ranges of C. diffusa", bg="white", cex=1.2)
box(lty="solid", col = "black")
# #shameless plug !
# mtext("map made using rworldmap", line=-1, side=1, adj=1, cex=0.6)

# 
dev.off()
