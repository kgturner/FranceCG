##France CG - climate and lat/long PCA###

#add lat/long
#get population coordinates
allpop <- read.table(file.choose(), header=T, sep="\t") #Popcoord.txt !not wordlclim approximations
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
#                         PC1    PC2    PC3    PC4    PC5     PC6    
# Standard deviation     2.4796 2.3822 2.0296 1.5433 1.0808 0.85425 
# Proportion of Variance 0.2795 0.2580 0.1872 0.1083 0.0531 0.03317 
# Cumulative Proportion  0.2795 0.5374 0.7247 0.8329 0.8861 0.91922 

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
# alt      bio18      bio15      bio10      bio14       bio1       bio2  Longitude       bio8   Latitude      bio17       bio5 
# 0.00995386 0.04658053 0.05829867 0.10981148 0.11420187 0.12110800 0.12526569 0.12956648 0.14417015 0.14941215 0.15001422 0.15302580 
# bio3       bio9      bio11       bio6      bio13      bio16      bio12       bio7      bio19       bio4 
# 0.15873725 0.20161735 0.24932707 0.27877950 0.28224989 0.28734473 0.31565663 0.33815842 0.35222244 0.35643172 
#reverse order, top ones at the end. Lat 13th.
#profound or mundane? bio7, bio19, bio4 also top three loadings for kay's ragweed

Frclimdat2 <- cbind(Frclimdat2, Frclimdat$bio4, Frclimdat$bio19, Frclimdat$bio7)


#write table
write.table(Frclimdat2, file="FrbioclimPCAdat.txt")
#      