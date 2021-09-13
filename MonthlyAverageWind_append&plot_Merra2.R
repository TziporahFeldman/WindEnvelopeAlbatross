########################################################################
# Load Libraries
########################################################################
library(ggplot2) # plots 
library(tidyverse) # data cleanup
library(sf) # spatial
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial) # spatial ggplot
library(rgeos)
library(raster) # convert netcdf to raster
library(rworldmap)
library(shape)
library(ncmeta)
library(ncdf4)
library(RNetCDF)
library(nngeo)
library(lubridate) # Clean up dates
library(rgdal)
library(stars) 
library(lattice)
library(rasterVis) # vectorplot
library(colorRamps) # matlab.like
library(viridisLite)# color palette
library(DescTools) #closest
library(imputeTS) #na.interpolation
library(swfscMisc)
require(RColorBrewer) # color palette
library(viridis)# color palette
library(maps)# world maps for ggplot

###############################################################################
# The purpose of this code is to download and clean up monthly-averaged MERRA-2 
# data for plotting in ggpplot 
###############################################################################

# Code for plotting average wind data with tracks 
# https://renewable-analytics.netlify.app/2018/06/25/using-era-interim-in-r/

##########################################################################################
# AUXILIARY FUNCTIONS
# run before analyses to calculate wind speed 
##########################################################################################
# -----------------------------------------------------------------
# uv2ddff
# -----------------------------------------------------------------
uv2ddff <- function(u, v = NULL, rad = FALSE){
  # if input u is zoo or data.frame
  zoo_index <- NULL # Default
  if (inherits(u, c("zoo", "data.frame"))) {
    # If input 'u' is zoo: keep index
    if (inherits(u, "zoo")) zoo_index <- index(u)
    if (!all(c("u", "v") %in% names(u)))
      stop("necessary colums \"u\" and/or \"v\" missing")
    # If "v" is set in addition: warn
    if (!is.null(v)) {
      warning(sprintf("input \"u\" to uv2ddff is \"%s\":", class(u)),
              "\"v\" specified as well but will be ignored!")
    }
    v = as.numeric(u$v)
    u = as.numeric(u$u)
    # if u has 2 columns the second column is taken as v
  } else if (NCOL(u) == 2) {
    v <- u[,2]
    u <- u[,1]
  } else {
    if (is.null(v)) stop("input \"v\" missing")
    # If lenths do not match and none of them is of length 1: stop.
    if (!(length(u) == length(v)) & !any(c(length(u), length(v)) == 1L)) {
      stop("Length of \"u\" and \"v\" not identical")
      # Else recycle the one with length one to the length of the other one.
      # Damn it, so much one's in one sentence!
    } else if (length(u) == 1) {
      u <- rep(u, length(v))
    } else if (length(v) == 1) {
      v <- rep(v, length(u))
    }
  }
  # polar coordinates:
  ff <- sqrt(u^2 + v^2)
  dd <- atan(v/u) + (u < 0) * pi
  # Only non-na combis
  idx <- which(!is.na(dd) & !is.na(ff));   dd[idx] <- dd[idx] + 2 * pi
  # convert angle to meteorological convention
  dd <- 3 * pi / 2 - dd
  idx <- which(!is.na(dd) & !is.na(ff));   dd[idx] <- dd[idx] + 2 * pi
  # if rad (radiants) = F we have to convert to degrees.
  if (!rad) dd <- dd * 180 / pi
  res <- data.frame(dd, ff)
  if (is.null(zoo_index)) return(res) else return(zoo(res, zoo_index))
}

#################################################################################
# Download and Import data
#################################################################################
# -110, -70, 10, -30 Extent for Bird Island (-30N -70S -80W -20E)
# 165, 20, -170, 40 Extent for Bird Island (40N, 20S, 165W, -170E)
# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
#  Useful Links
# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
# File specification: https://gmao.gsfc.nasa.gov/pubs/docs/Bosilovich785.pdf
# How to download the data: https://daac.gsfc.nasa.gov/information/howto?title=How%20to%20Download%20MERRA-2%20Daily%20Mean%20Data
# How to calculate and plot wind speed using MERRA-2 wind component: https://disc.gsfc.nasa.gov/information/howto?title=How%20to%20calculate%20and%20plot%20wind%20speed%20using%20MERRA-2%20wind%20component%20data%20using%20Python
# Monthly averages: https://disc.gsfc.nasa.gov/datasets/M2IMNXASM_5.12.4/summary
########################################################################

setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/Merra-2Datasets/Monthly/L0/Midway/L/")
a <- list.files("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/Merra-2Datasets/Monthly/L0/Midway/L/", pattern = ".nc")
# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
#  Isolate U and V components for 2 and 10 m 
# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
for (i in 1:length(a)) {
  mi<-read_ncdf(a[i])
  times <- st_get_dimension_values(mi, "time")
  if (i==1) {
    times_all<-times
  }else{
    times_all<-c(times_all,times)
  }
}


# U-component, 2 m 
for (i in 1:length(a)) {
  mi<-read_ncdf(a[i])
  wind_u<-as(mi[2,,,], "Raster")
  wind_u.df= raster::as.data.frame(wind_u, xy = TRUE)
  if (i==1) {
    wind_U2M<-wind_u.df
  }else{
    wind_U2M<-cbind(wind_U2M,wind_u.df[, -c(1,2)])
  }
}

  
# V-component, 2 m 
for (i in 1:length(a)) {
  mi<-read_ncdf(a[i])
  wind_v<-as(mi[5,,,], "Raster")
  wind_v.df= raster::as.data.frame(wind_v, xy = TRUE)
  if (i==1) {
    wind_V2M<-wind_v.df
  }else{
    wind_V2M<-cbind(wind_V2M,wind_v.df[, -c(1,2)])
  }
}


# U-component, 10 m 
for (i in 1:length(a)) {
  mi<-read_ncdf(a[i])
  wind_u<-as(mi[1,,,], "Raster")
  wind_u.df= raster::as.data.frame(wind_u, xy = TRUE)
  if (i==1) {
    wind_U10M<-wind_u.df
  }else{
    wind_U10M<-cbind(wind_U10M,wind_u.df[, -c(1,2)])
  }
}


# V-component, 10 m 
for (i in 1:length(a)) {
  mi<-read_ncdf(a[i])
  wind_v<-as(mi[4,,,], "Raster")
  wind_v.df= raster::as.data.frame(wind_v, xy = TRUE)
  if (i==1) {
    wind_V10M<-wind_v.df
  }else{
    wind_V10M<-cbind(wind_V10M,wind_v.df[, -c(1,2)])
  }
}


# U-component, 50 m 
for (i in 1:length(a)) {
  mi<-read_ncdf(a[i])
  wind_u<-as(mi[3,,,], "Raster")
  wind_u.df= raster::as.data.frame(wind_u, xy = TRUE)
  if (i==1) {
    wind_U50M<-wind_u.df
  }else{
    wind_U50M<-cbind(wind_U50M,wind_u.df[, -c(1,2)])
  }
}

# V-component, 50 m 
for (i in 1:length(a)) {
  mi<-read_ncdf(a[i])
  wind_v<-as(mi[6,,,], "Raster")
  wind_v.df= raster::as.data.frame(wind_v, xy = TRUE)
  if (i==1) {
    wind_V50M<-wind_v.df
  }else{
    wind_V50M<-cbind(wind_V50M,wind_v.df[, -c(1,2)])
  }
}


# R side  
setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/Merra-2Datasets/Monthly/L0/Midway/R/")
b <- list.files("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/Merra-2Datasets/Monthly/L0/Midway/R/", pattern = ".nc")
# U-component, 2 m 
for (i in 1:length(b)) {
  mi<-read_ncdf(b[i])
  wind_u2<-as(mi[2,,,], "Raster")
  wind_u.df2= raster::as.data.frame(wind_u2, xy = TRUE)
  if (i==1) {
    wind_U2M2<-wind_u.df2
  }else{
    wind_U2M2<-cbind(wind_U2M2,wind_u.df2[, -c(1,2)])
  }
}

# V-component, 2 m 
for (i in 1:length(b)) {
  mi<-read_ncdf(b[i]) 
  wind_v2<-as(mi[5,,,], "Raster")
  wind_v.df2= raster::as.data.frame(wind_v2, xy = TRUE)
  if (i==1) {
    wind_V2M2<-wind_v.df2
  }else{
    wind_V2M2<-cbind(wind_V2M2,wind_v.df2[, -c(1,2)])
  }
}


# U-component, 10 m 
for (i in 1:length(b)) {
  mi<-read_ncdf(b[i])
  wind_u2<-as(mi[1,,,], "Raster")
  wind_u.df2= raster::as.data.frame(wind_u2, xy = TRUE)
  if (i==1) {
    wind_U10M2<-wind_u.df2
  }else{
    wind_U10M2<-cbind(wind_U10M2,wind_u.df2[, -c(1,2)])
  }
}

# V-component, 10 m 
for (i in 1:length(b)) {
  mi<-read_ncdf(b[i])
  wind_v2<-as(mi[4,,,], "Raster")
  wind_v.df2= raster::as.data.frame(wind_v2, xy = TRUE)
  if (i==1) {
    wind_V10M2<-wind_v.df2
  }else{
    wind_V10M2<-cbind(wind_V10M2,wind_v.df2[, -c(1,2)])
  }
}


# U-component, 50 m 
for (i in 1:length(b)) {
  mi<-read_ncdf(b[i])
  wind_u2<-as(mi[3,,,], "Raster")
  wind_u.df2= raster::as.data.frame(wind_u2, xy = TRUE)
  if (i==1) {
    wind_U50M2<-wind_u.df2
  }else{
    wind_U50M2<-cbind(wind_U50M2,wind_u.df2[, -c(1,2)])
  }
}

# V-component, 50 m 
for (i in 1:length(b)) {
  mi<-read_ncdf(b[i])
  wind_v2<-as(mi[6,,,], "Raster")
  wind_v.df2= raster::as.data.frame(wind_v2, xy = TRUE)
  if (i==1) {
    wind_V50M2<-wind_v.df2
  }else{
    wind_V50M2<-cbind(wind_V50M2,wind_v.df2[, -c(1,2)])
  }
}

names(wind_U2M2) <- names(wind_U2M)
names(wind_U10M) <- names(wind_U10M2)
names(wind_U50M) <- names(wind_U50M2)
names(wind_V2M) <- names(wind_V2M2)
names(wind_V10M) <- names(wind_V10M2)
names(wind_V50M) <- names(wind_V50M2)

wind_U2M <- rbind(wind_U2M, wind_U2M2)
wind_U10M <- rbind(wind_U10M, wind_U10M2)
wind_U50M <- rbind(wind_U50M, wind_U50M2)
wind_V2M <- rbind(wind_V2M, wind_V2M2)
wind_V10M <- rbind(wind_V10M, wind_V10M2)
wind_V50M <- rbind(wind_V50M, wind_V50M2)


########################################################################
# Calculate monthly wind speed and wind shear 
########################################################################
col <- c("lon", "lat", "U2M","U10M", "U50M", "V2M", "V10M", "V50M")
path_out <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/Merra-2Datasets/Monthly/L1/Midway/"

times_all

rm(month)
month <- cbind(wind_U2M[, c(1, 2,4)], wind_U10M[,4], wind_U50M[,4], wind_V2M[,4], wind_V10M[,4], wind_V50M[,4])
colnames(month) <- col

res.2M<-uv2ddff(month$U2M, month$V2M)
month$windspeed_2M <- res.2M$ff

res.10M<-uv2ddff(month$U10M, month$V10M)
month$windspeed_10M <- res.10M$ff

res.50M<-uv2ddff(month$U50M, month$V50M)
month$windspeed_50M <- res.50M$ff

month$windshear_10M <- month$windspeed_10M- month$windspeed_2M
month$windshear_50M <- month$windspeed_50M- month$windspeed_10M 

month$month_year <- "February_2019"
write_csv(month, paste0(path_out, 'February_2019.csv'))


########################################################################
# Append  
########################################################################

setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/Merra-2Datasets/Monthly/L1/Midway/")
a <- list.files("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/Merra-2Datasets/Monthly/L1/Midway/", pattern = ".csv")

for (i in 1: length(a)) {
  m <- read.csv(a[i])
  if (i==1) {
    mat <- m 
  }else{
    mat<- rbind(mat,m)
  }
}


mat <- mat[, c(1, 2, 9:14)]
mat$location <- "Midway"
write.csv(mat, "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/Merra-2Datasets/Monthly/L1_2/monthlyaveragewind_MIAT.csv", row.names = F)

########################################################################
# Plotting wind maps 
########################################################################
# download world data for map  
world<- map_data("world")
world2<- map_data("world2")
wind.BI <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/Merra-2Datasets/Monthly/L1_2/monthlyaveragewind_BI.csv")

# only wind maps
ggplot(wind.BI,aes(lon,lat))+
  geom_raster(aes(fill=windspeed_2M),interpolate = T,alpha=.75)+
  scale_fill_viridis(name="Wind speed")+
  geom_polygon(data=world,aes(x=long,y=lat, group=group),color='black',fill=NA)+
  coord_sf(xlim = c(-110, 10), ylim = c(-70, -30)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Bird Island Wind Speed")+
  facet_wrap(~month_year)+
  theme_bw()

ggplot(wind.BI,aes(lon,lat))+
  geom_raster(aes(fill=windshear_10M),interpolate = T,alpha=.75)+
  scale_fill_viridis(name="Wind shear")+
  geom_polygon(data=world,aes(x=long,y=lat, group=group),color='black',fill=NA)+
  coord_sf(xlim = c(-110, 10), ylim = c(-70, -30)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Bird Island Wind Shear")+
  facet_wrap(~month_year)+
  theme_bw()

ggplot(wind.MIAT,aes(lon,lat))+
  geom_raster(aes(fill=windspeed_2M),interpolate = T,alpha=.75)+
  scale_fill_viridis(name="Wind speed")+
  geom_polygon(data=world2,aes(x=long,y=lat, group=group),color='black',fill=NA)+
  coord_sf(xlim = c(165, 180), ylim = c(20, 40)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Midway Wind Speed")+
  facet_wrap(~month_year)+
  theme_bw()

ggplot(wind.MIAT,aes(lon,lat))+
  geom_raster(aes(fill=windshear_10M),interpolate = T,alpha=.75)+
  scale_fill_viridis(name="Wind shear")+
  geom_polygon(data=world2,aes(x=long,y=lat, group=group),color='black',fill=NA)+
  coord_sf(xlim = c(165, 180), ylim = c(20, 40)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Midway Wind shear")+
  facet_wrap(~month_year)+
  theme_bw()


