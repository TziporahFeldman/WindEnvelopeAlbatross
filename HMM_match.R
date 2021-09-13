library(stringr)
library(lubridate)
library(data.table)
library(dplyr)


###############################################################################
# The purpose of this code is to match HMM behavioral outputs with GPS 
# locations and wind characteristics
###############################################################################


setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/HMM/HMM_out/2021-04-01/state_dataframes/")
a <- list.files(pattern = ".txt")

# Append 30s HMM behavioral data for each bird into one dataframe  
for (i in 1: length(a)) {
  m <- read.csv(a[i])
  m$id <-  paste0(strsplit(a[i],"_")[[1]][1], "_", strsplit(a[i],"_")[[1]][2])
  if (i==1) {
    mat <- m 
  }else{
    mat <- rbind(mat,m)
  }
}

# clean up appended HMM data table 
colnames(mat) <- c("datetime", "state", "id")
mat$datetime <- ymd_hms(mat$datetime)
mat <- as.data.table(mat)
mat$datetime <- lubridate::round_date(mat$datetime, "30 sec") # round to nearest 30s to match with wind/GPS data 

# Read in GPS/wind data and clean up
GPS <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/MERRA-2/L1_appended/L1_3_30sAppended/Wind30sAppendedWithPhase.csv")
GPS$datetime <- ymd_hms(GPS$datetime)
GPS <- as.data.table(GPS)
GPS$datetime <- lubridate::round_date(GPS$datetime, "30 sec") # round to nearest 30s to match with HMM output 


# Match GPS/wind data to HMM data 
match <- na.omit(full_join(GPS, mat, by= c("id", "datetime")))
write.csv(match, "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/HMM/HMM_wind_match/HMM_wind_30s.csv", row.names = F) # save

