###WorldClim notes###


#preferred citation:
#Hijmans, R.J., S.E. Cameron, J.L. Parra, P.G. Jones and A. Jarvis, 2005. Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology 25: 1965-1978. 

#datum is WGS84
#temperature data are in °C * 10. This means that a value of 231 represents 23.1 °C

#spatial resolution: 30 seconds (0.93 x 0.93 = 0.86 km2 at the equator)

#The ZIP files have names like X_R_F.ZIP, where X indicates the variable (TMEAN, TMIN, TMAX, PREC, BIO, or ALT); 
#R indicates the resolution (10m, 5m, 2_5m, 30s); 
#F indicates to format: 'BIL' for generic grids, and 'ESRI' for ESRI grids

#Bioclim coded as follows
BIO1 = Annual Mean Temperature
BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
BIO3 = Isothermality (BIO2/BIO7) (* 100)
BIO4 = Temperature Seasonality (standard deviation *100)
BIO5 = Max Temperature of Warmest Month
BIO6 = Min Temperature of Coldest Month
BIO7 = Temperature Annual Range (BIO5-BIO6)
BIO8 = Mean Temperature of Wettest Quarter
BIO9 = Mean Temperature of Driest Quarter
BIO10 = Mean Temperature of Warmest Quarter
BIO11 = Mean Temperature of Coldest Quarter
BIO12 = Annual Precipitation
BIO13 = Precipitation of Wettest Month
BIO14 = Precipitation of Driest Month
BIO15 = Precipitation Seasonality (Coefficient of Variation)
BIO16 = Precipitation of Wettest Quarter
BIO17 = Precipitation of Driest Quarter
BIO18 = Precipitation of Warmest Quarter
BIO19 = Precipitation of Coldest Quarter

#The WorldClim data can be easily imported into most GIS applications. Here is some help:
library(rgdal)
library(raster)
r = raster("tmin01")

#Or download directly from within R. For example:
  
library(raster)
w = getData('worldclim', var='tmin', res=0.5, lon=5, lat=45)

#See the raster package vignette for more info.