
library(raster)
library(sp)
library(rgdal)
library(sf)
library(maps)
library(mapdata)
library(ggplot2)
setwd('D:/ESE5023/data')



##windspeed----------------------------------------
path <- "D:/ESE5023/data/wc2.1_2.5m_wind"
fileNames <- dir(path) 
filePath <- sapply(fileNames, function(x){ 
    paste(path,x,sep='/')})   
wind <- lapply(filePath, function(x){
    raster(x)})  


wind_sum<-wind$wc2.1_2.5m_wind_01.tif
for(i in 2:length(fileNames)){
    wind_sum<-wind[[i]]+wind_sum
}
wind_avg<-wind_sum/length(fileNames)



#zoom to China
Crop_box <- c(65,145,15,55)
# Crop the raster
wind_avg_crop <- crop(wind_avg, Crop_box)

par(mar=c(4.5,3,2,1))


plot(wind_avg_crop,
     main="Wind speed in China for 1970-2000",
     horizontal=T,
     useRaster=T,
     legend.width=0.7, 
     legend.mar=2,
     legend.args=list(text="Wind speed [m s-1]",cex=1),
     col=heat.colors(100)
)

map("china", 
    add=T,
    col = "black", 
    lwd = 1.5,
    ylim = c(15, 54))

contour(wind_avg_crop, 
        levels=seq(from=4, to=8, by=1),
        add=T,
        col='blue',
        labcex=1,
        lty = 1)



##Precipitation---------------------------------------------
path_p <- "D:/ESE5023/data/wc2.1_2.5m_prec"
fileNames_p <- dir(path_p) 
filePath_p <- sapply(fileNames_p, function(x){ 
    paste(path_p,x,sep='/')})   
prec <- lapply(filePath_p, function(x){
    raster(x)})  


prec_sum<-prec$wc2.1_2.5m_prec_01.tif
for(i in 2:length(fileNames_p)){
    prec_sum<-prec[[i]]+prec_sum
}
prec_avg<-prec_sum/length(fileNames_p)




# Crop the raster
prec_avg_crop <- crop(prec_avg, Crop_box)

par(mar=c(4.5,3,2,1))
plot(prec_avg_crop,
     main="Precipitation in China for 1970-2000",
     horizontal=T,
     useRaster=T,
     legend.width=0.7, 
     legend.mar=2,
     legend.args=list(text="Precipitation [mm]",cex=1))


map("china", 
    add=T,
    col = "red4", 
    ylim = c(15, 54))

contour(prec_avg_crop, 
        levels=seq(from=0, to=20, by=5),
        add=T,
        col='blue',
        labcex=1,
        lty = 1)

##Solarradiation----------------------------------------
path_s <- "D:/ESE5023/data/wc2.1_2.5m_srad"
fileNames_s <- dir(path_s) 
filePath_s <- sapply(fileNames_s, function(x){ 
    paste(path_s,x,sep='/')})   
srad <- lapply(filePath_s, function(x){
    raster(x)})  


srad_sum<-srad$wc2.1_2.5m_srad_01.tif
for(i in 2:length(fileNames_s)){
    srad_sum<-srad[[i]]+srad_sum
}
srad_avg<-srad_sum/(length(fileNames_s)*1000)




# Crop the raster
srad_avg_crop <- crop(srad_avg, Crop_box)

par(mar=c(4.5,3,2,1))
plot(srad_avg_crop,
     main="Solar radiation in China for 1970-2000",
     horizontal=T,
     useRaster=T,
     legend.width=0.7, 
     legend.mar=2,
     legend.args=list(text="Solar Radiation [*10^3 kJ m-2 day-1]",cex=1),
     col=terrain.colors(100))

map("china", 
    add=T,
    col = "red4", 
    ylim = c(15, 54))

contour(srad_avg_crop,
        levels=seq(from=17,to=25, by=2),
        add=T,
        col='blue',
        labcex=1,
        lty = 1)