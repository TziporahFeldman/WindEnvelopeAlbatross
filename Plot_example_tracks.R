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


wrap360 = function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}
Lon360to180 <- function(lon){
  ((lon + 180) %% 360) - 180
}
###############################################################################
# Import specific day 
############################################################################### 
a <-read_ncdf("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/Merra-2Datasets/Hourly/Jan2019-Feb2019_hourly_MIDWAY/L/MERRA2_400.inst1_2d_asm_Nx.20190204.SUB.nc")
b <-read_ncdf("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/Merra-2Datasets/Hourly/Jan2019-Feb2019_hourly_MIDWAY/R/MERRA2_400.inst1_2d_asm_Nx.20190204.SUB_2.nc")


# Left
wind_u<-as(a[2,,,], "Raster")
wind_U2M <- raster::as.data.frame(wind_u, xy = TRUE)

wind_v<-as(a[5,,,], "Raster")
wind_V2M <- raster::as.data.frame(wind_u, xy = TRUE)

wind_u<-as(a[1,,,], "Raster")
wind_U10M <- raster::as.data.frame(wind_u, xy = TRUE)

wind_u<-as(a[4,,,], "Raster")
wind_V10M <- raster::as.data.frame(wind_u, xy = TRUE)


# Right
wind_u<-as(b[2,,,], "Raster")
wind_U2M2 <- raster::as.data.frame(wind_u, xy = TRUE)

wind_v<-as(b[5,,,], "Raster")
wind_V2M2 <- raster::as.data.frame(wind_u, xy = TRUE)

wind_u<-as(b[1,,,], "Raster")
wind_U10M2 <- raster::as.data.frame(wind_u, xy = TRUE)

wind_u<-as(b[4,,,], "Raster")
wind_V10M2 <- raster::as.data.frame(wind_u, xy = TRUE)

# combine L and R
wind_U2M <- rbind(wind_U2M, wind_U2M2)
wind_U10M <- rbind(wind_U10M, wind_U10M2)
wind_V2M <- rbind(wind_V2M, wind_V2M2)
wind_V10M <- rbind(wind_V10M, wind_V10M2)

# Average each component for the day 
wind_U2M$avg <- rowMeans(wind_U2M[,3:26])
wind_U10M$avg <- rowMeans(wind_U10M[,3:26])
wind_V2M$avg <- rowMeans(wind_V2M[,3:26])
wind_V10M$avg <- rowMeans(wind_V10M[,3:26])

# create dataframe with averages
df <- data.frame(cbind(wind_U2M[,1:2],wind_U2M$avg, wind_U10M$avg , wind_V2M$avg , wind_V10M$avg))
colnames(df) <- c("lon", "lat", "U2M","U10M", "V2M", "V10M")

# Find avg windspeed and windshear for the day 
res.2M<-uv2ddff(df$U2M, df$V2M)
df$windspeed_2M <- res.2M$ff

res.10M<-uv2ddff(df$U10M, df$V10M)
df$windspeed_10M <- res.10M$ff

df$windshear <- df$windspeed_10M- df$windspeed_2M

###############################################################################
# Specify specific bird 
############################################################################### 
GPS_G037 <- GPS %>% filter(id=="WAAL_G037")
GPS_G037$state <- as.factor(GPS_G037$state)

GPS_BF63 <- GPS %>% filter(id=="WAAL_BF63")
GPS_BF63$state <- as.factor(GPS_BF63$state)

GPS_YA75 <- GPS %>% filter(id=="WAAL_YA75")
GPS_YA75$state <- as.factor(GPS_YA75$state)

GPS_31 <- GPS %>% filter(id=="BBAL_31")
GPS_31$state <- as.factor(GPS_31$state)

GPS_36 <- GPS %>% filter(id=="BBAL_36")
GPS_36$state <- as.factor(GPS_36$state)

GPS_32 <- GPS %>% filter(id=="GHAL_32")
GPS_32$state <- as.factor(GPS_32$state)

GPS_34 <- GPS %>% filter(id=="GHAL_34")
GPS_34$state <- as.factor(GPS_34$state)

GPS_04 <- GPS %>% filter(id=="LAAL_04")
GPS_04$state <- as.factor(GPS_04$state)

GPS_05 <- GPS %>% filter(id=="LAAL_05")
GPS_05$state <- as.factor(GPS_05$state)

GPS_21 <- GPS %>% filter(id=="BFAL_21")
GPS_21$state <- as.factor(GPS_21$state)

GPS_26 <- GPS %>% filter(id=="BFAL_26")
GPS_26$state <- as.factor(GPS_26$state)

########################################################################
# Plotting wind map
########################################################################
# download world data for map  
world<- map_data("world") # for BI
world2<- map_data("world2") # for Midway

# Bird Island 
speed <- ggplot(df,aes(lon,lat))+
  geom_raster(aes(fill=windspeed_2M),interpolate = T,alpha=.75)+
  scale_fill_viridis(name="Wind speed")+
  geom_polygon(data=world,aes(x=long,y=lat, group=group),color='black',fill=NA)+
  coord_sf(xlim = c(-80,-20), ylim = c(-70, -30)) +
  geom_point(data = GPS_YA75, mapping = aes(x=Lon360to180(lon), y=lat, col = soaring), size = 1) +
  scale_color_grey()+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("WAAL_YA75")+
  theme_bw()

shear <- ggplot(df,aes(lon,lat))+
  geom_raster(aes(fill=windshear),interpolate = T,alpha=.75)+
  scale_fill_viridis(name="Wind shear")+
  geom_polygon(data=world,aes(x=long,y=lat, group=group),color='black',fill=NA)+
  coord_sf(xlim = c(-80,-20), ylim = c(-70, -30)) +
  geom_point(data = GPS_YA75, mapping = aes(x=Lon360to180(lon), y=lat, col = soaring), size = 1) +
  scale_color_grey()+
  xlab("Longitude")+
  ylab("Latitude")+ 
  theme_bw()

g <- arrangeGrob(speed, shear, ncol=1) #generates g
ggsave(file="/Users/tziporahserota/Desktop/WAAL_YA75.png", g) #saves g

# Midway
df$lon <- wrap360(df$lon)

speed <- ggplot(df,aes(lon,lat))+
  geom_raster(aes(fill=windspeed_2M),interpolate = T,alpha=.75)+
  scale_fill_viridis(name="Wind speed")+
  geom_polygon(data=world2,aes(x=long,y=lat, group=group),color='black',fill=NA)+
  coord_sf(xlim = c(165, 190), ylim = c(20, 40)) +
  geom_point(data = GPS_26, mapping = aes(x=lon, y=lat, col = soaring), size = 1) +
  scale_color_grey()+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("BFAL_26")+
  theme_bw()

shear <- ggplot(df,aes(lon,lat))+
  geom_raster(aes(fill=windshear),interpolate = T,alpha=.75)+
  scale_fill_viridis(name="Wind shear")+
  geom_polygon(data=world2,aes(x=long,y=lat, group=group),color='black',fill=NA)+
  coord_sf(xlim = c(165, 190), ylim = c(20, 40)) +
  geom_point(data = GPS_26, mapping = aes(x=lon, y=lat, col = soaring), size = 1) +
  scale_color_grey()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()

g <- arrangeGrob(speed, shear, ncol=1) #generates g
ggsave(file="/Users/tziporahserota/Desktop/BFAL_26.png", g) #saves g



unique(GPS$id)
