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
library(lubridate) # Clean up dates
library(lattice)
library(rasterVis) # vectorplot
library(colorRamps) # matlab.like
library(viridisLite)# color palette
require(RColorBrewer) # color palette
library(viridis)# color palette
library(maps)# world maps for ggplot
library(ggridges)
library(stringr)

###############################################################################
# This code uses the monthly averaged output of 
# MonthlyAverageWind_append&plot_MERRA-2.r to plot GPS tracks over 
# windspeed and windshear
###############################################################################

##########################################################################################
# AUXILIARY FUNCTIONS
##########################################################################################
# -----------------------------------------------------------------
# Lon systems
# -----------------------------------------------------------------
wrap360 = function(lon) {lon360<-ifelse(lon<0,lon+360,lon);return(lon360)}
Lon360to180 <- function(lon){
  ((lon + 180) %% 360) - 180
}

###############################################################################
# Import data  
###############################################################################
GPS <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/HMM/HMM_wind_match/AllData/HMM_wind_30s.csv")
wind <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/Merra-2Datasets/Monthly/L1_2/monthlyaveragewind.csv")

# -----------------------------------------------------------------
# Clean data 
# -----------------------------------------------------------------
GPS$datetime <- as.POSIXct(GPS$datetime, format="%Y-%m-%d %H:%M:%OS")
GPS$datetime2 <- parse_date_time(GPS$datetime, orders=c("%Y-%m-%d %H:%M:%OS"))
GPS$month_year <-str_c(month(GPS$datetime, label=T, abbr=F),"_" ,year(GPS$datetime))

# -----------------------------------------------------------------
# Subset data 
# -----------------------------------------------------------------
wind.MIAT <-  wind %>% dplyr::filter(location=="Midway")
wind.BI <- wind %>% dplyr::filter(location=="Bird Island", month_year!= "February_2019")

GPS_BI <- GPS %>% dplyr::filter(location=="Bird Island")
GPS_Midway<- GPS %>% dplyr::filter(location=="Midway")

###############################################################################
# Contour plots
###############################################################################
world<- map_data("world")

ggplot(BI_appended, aes(x=Lon360to180(lon), y=lat)) +
  geom_polygon(data=world,aes(x=long,y=lat, group=group),color='black',fill=NA)+
  stat_density_2d(aes(fill = ..level..), geom = "polygon")


ggplot(appended_sample, aes(x=Lon360to180(lon), y = lat, col)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  scale_color_viridis(discrete=TRUE) +
  xlab('Longitude') + 
  ylab('Latitude') + 
  geom_polygon(data=world,aes(x=long,y=lat, group=group),color='black',fill=NA)+
  theme(legend.position = "none")+
  theme_bw()

###############################################################################
# Lat/lon maps  
############################################################################### 
wind_latavg <- wind %>% group_by(lat,month_year,location) %>% summarise(avg_lat=mean(windspeed_2M))
wind_lonavg <- wind %>% group_by(lon,month_year,location) %>% summarise(avg_lon=mean(windspeed_2M))


wind_latavg.MIAT <- wind.MIAT %>% group_by(lat,month_year,location) %>% summarise(avg_lat=mean(windspeed_2M), avg_shear=mean(windshear_10M))
wind_lonavg.MIAT <- wind.MIAT %>% group_by(lon,month_year,location) %>% summarise(avg_lon=mean(windspeed_2M), avg_shear=mean(windshear_10M))


wind_latavg.BI <- wind.BI %>% group_by(lat,month_year,location) %>% summarise(avg_lat=mean(windspeed_2M), avg_shear=mean(windshear_10M))
wind_lonavg.BI <- wind.BI %>% group_by(lon,month_year,location) %>% summarise(avg_lon=mean(windspeed_2M), avg_shear=mean(windshear_10M))

# lat long occurrence 
ggplot(BI_appended, aes(x=lat))+
  geom_density()+
  facet_wrap(~month_year)+
  theme_bw()

ggplot(BI_appended, aes(x=lon))+
  geom_density()+
  facet_wrap(~month_year)+
  theme_bw()

# wind speed 
ggplot(wind_latavg.BI, aes(x=lat, y=avg_lat))+
  geom_line()+
  facet_wrap(location~month_year)

ggplot(wind_lonavg.BI, aes(x=lon, y=avg_lon))+
  geom_line()+
  facet_wrap(location~month_year)

# wind shear 
ggplot(wind_latavg.BI, aes(x=lat, y=avg_shear))+
  geom_line()+
  facet_wrap(location~month_year)

ggplot(wind_lonavg.BI, aes(x=lon, y=avg_shear))+
  geom_line()+
  facet_wrap(location~month_year)


###############################################################################
# Spatial maps  
############################################################################### 
# download world data for map  
world<- map_data("world")
world2<- map_data("world2")



# Plot tracks of each species over speed and shear by species 
GPS_BI$labels <- GPS_BI$species
GPS_BI$labels <- factor(GPS_BI$labels, levels = unique(GPS_BI$species))

GPS_BI.sample <- sample_n(GPS_BI, 50000)

ggplot(wind.BI,aes(lon,lat))+
  geom_raster(aes(fill=windspeed_2M),interpolate = T,alpha=.75)+
  scale_fill_viridis(name="Wind speed")+
  geom_polygon(data=world,aes(x=long,y=lat, group=group),color='black',fill=NA)+
  scale_color_grey(name= "Species")+
  geom_point(data = GPS_BI.sample, mapping = aes(x=Lon360to180(lon), y=lat, col = labels), size = .05) +
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
  geom_path(data = Midway, mapping = aes(color = labels), size = .1) +
  coord_sf(xlim = c(165, 180), ylim = c(20, 40)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Midway Wind shear")+
  facet_wrap(~month_year)+
  theme_bw()



