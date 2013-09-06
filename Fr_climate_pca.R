##France CG - climate and lat/long PCA###



#####example######
# #set working directory
# setwd('D:/UBC/STAT540/gproject')
# #load climate data
# cdata <- read.table("KHbioclimdata.txt", sep = ",", header = T, row.names = 21)
# #pca!
# cdata.pca <- prcomp(cdata[2:20], center=T, scale=T)
# summary(cdata.pca)
# Importance of components:
#                           PC1    PC2    PC3    PC4     PC5     PC6     PC7
# Standard deviation     2.9562 2.2462 1.5226 1.0366 0.70475 0.40331 0.34354
# Proportion of Variance 0.4855 0.2803 0.1288 0.0597 0.02759 0.00904 0.00656
# Cumulative Proportion  0.4855 0.7658 0.8946 0.9543 0.98190 0.99094 0.99749
#                            PC8     PC9    PC10    PC11      PC12
# Standard deviation     0.17247 0.09100 0.07708 0.03382 8.342e-16
# Proportion of Variance 0.00165 0.00046 0.00033 0.00006 0.000e+00
# Cumulative Proportion  0.99915 0.99961 0.99994 1.00000 1.000e+00
# #visualize components
# plot(cdata.pca)
# #visualize components in biplot
# biplot(cdata.pca, choices=1:2, scale = 1, var.axes= F, main="PCA analysis of climate data")
# 
# PC1<-as.matrix(cdata.pca$x[,1])
# colnames(PC1)[1] <- "PC1"
# write.table(PC1, "PC1.txt", quote=F, sep='\t', na='')
# 
# #find top two loadings
# loadings <- cdata.pca$rotation[,1]
# sort(abs(loadings)
#      BIO13        BIO16         BIO8        BIO18         BIO3        BIO10         BIO1        BIO12 
#      0.0008545854 0.0335656739 0.0759163821 0.1342447214 0.1721681069 0.1738263552 0.2188846791 0.2279499025 
#      BIO9         BIO5        BIO14        BIO11         BIO2        BIO17         BIO6        BIO15 
#      0.2313146633 0.2388232401 0.2506778470 0.2566660234 0.2790224213 0.2793931895 0.2805858707 0.2833998460 
#      BIO19         BIO4         BIO7 
#      0.2867335067 0.2895502466 0.3083590288 
#      
# #pick BIO 7 and BIO 19
# bio7 <- as.matrix(cdata$BIO7)
# bio19 <- as.matrix(cdata$BIO19)
# newvector <- cbind(bio7, bio19)
# 
# colnames(newvector)[1]<- "BIO7"
# colnames(newvector)[2]<- "BIO19"  
#      
# row.names(newvector) <- row.names(cdata)
# write.table(newvector, "top2climate.txt", quote=F, sep='\t', na='')
#      
# top2climate <- read.table("top2climate.txt", sep = "\t", header = T)
#      
# meta <- read.table("meta.txt", sep = "\t", header = T)
#      
# #merge
# newmeta <- merge(meta, top2climate, by="Population")
# write.table(newmeta, "meta.txt", quote=F, sep='\t', na='', col.names = NA)
#      