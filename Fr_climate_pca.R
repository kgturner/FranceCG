##France CG - climate and lat/long PCA###

#add lat/long
#get population coordinates
allpop <- read.table("Popcoord.txt", header=T, sep="\t") #Popcoord.txt !not wordlclim approximations
Frdes <- read.table("Frdes.txt", header=T, sep="\t")
Frpop <- allpop[allpop$Pop %in% Frdes$Pop,]
rownames(Frpop) <- Frpop$Pop
Frpop$Pop <- droplevels(Frpop$Pop)
Frpop <- Frpop[,1:3]

Frclim$Pop <- row.names(Frclim)
Frclimdat <- merge(Frclim, Frpop,all.x=TRUE)
row.names(Frclimdat) <- Frclimdat$Pop

#PCA fun times
Frclim.pca <- prcomp(Frclimdat[2:23], center=TRUE, scale=TRUE)
summary(Frclim.pca)
# Importance of components:
#                           PC1    PC2    PC3    PC4     PC5     PC6     PC7
# Standard deviation     2.5058 2.3797 2.1050 1.5462 1.14469 0.93389 0.77103
# Proportion of Variance 0.2854 0.2574 0.2014 0.1087 0.05956 0.03964 0.02702
# Cumulative Proportion  0.2854 0.5428 0.7442 0.8529 0.91247 0.95211 0.97914

#visualize components
plot(Frclim.pca)
biplot(Frclim.pca, var.axes=FALSE, main="PCA analysis of climate data")
# biplot(Frclim.pca, var.axes=FALSE, main="PCA analysis of climate data", choices=1:2, scale=1)

#get top 4 PCs
PC1 <- as.matrix(Frclim.pca$x[,1])
PC2 <- as.matrix(Frclim.pca$x[,2])
PC3 <- as.matrix(Frclim.pca$x[,3])
PC4 <- as.matrix(Frclim.pca$x[,4])
Frclimdat2 <- cbind(Frclimdat, PC1, PC2, PC3, PC4)

#find top loadings
loadings <- Frclim.pca$rotation[,1]
sort(abs(loadings))
# alt        bio2       bio10       bio17   Longitude        bio5       bio12 
# 0.001561833 0.041357958 0.090877697 0.105026831 0.112019618 0.115751459 0.135513808 
# bio14       bio13       bio16        bio8       bio15       bio18    Latitude 
# 0.138930963 0.164047194 0.176494679 0.184740915 0.186201310 0.213953960 0.220288560 
# bio7       bio19        bio3        bio1        bio4        bio6        bio9 
# 0.227854928 0.254227563 0.262227179 0.266800012 0.322945390 0.326315353 0.332758344 
# bio11 
# 0.340912515 
#reverse order, top ones at the end. Lat 13th.
#profound or mundane? bio7, bio19, bio4 also top three loadings for kay's ragweed.
#hahaha or bug in code!

# Frclimdat2 <- cbind(Frclimdat2, Frclimdat$bio4, Frclimdat$bio19, Frclimdat$bio7)


#write table
write.table(Frclimdat2, file="FrbioclimPCAdat.txt")
#      