library(tidyr)
library(dplyr)
library(stringr)
###############################################################################
# The purpose of this code is to append 30s wind data for each individual bird
# into one file for each season, add breeding phase and season information to 
# those files, and then append into one document
###############################################################################


###############################################################################
# 1. Append Data for each dataset and Define breeding phase 
###############################################################################
# -----------------------------------
# a. 2020-2021 Bird Island
# -----------------------------------
# append 30s
setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/MERRA-2/L0_paired/2020-2021/indiv_birds/")
a <- list.files(pattern = ".csv")
path_out <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/MERRA-2/L1_appended/L1_1_30sAppended_byseason/"

for (i in 1: length(a)) {
  m <- read.csv(a[i])
  if (i==1) {
    mat <- m 
  }else{
    mat <- rbind(mat,m)
  }
}
mat$datetime <- as.POSIXct(mat$datetime, format="%Y-%m-%d %H:%M:%OS")
write.csv(mat, file = paste0(path_out, "Wind30sAppended_2020.csv"), row.names=FALSE)

# Define breeding phase, location, season
# m <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/L1_appended/L1_1_30sAppended_byseason/Wind30sAppended_2020.csv")
m <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/L1_appended/L1_1_30sAppended_byseason/Wind30sAppended_2020.csv")
meta <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Conners_Bird_Island/2020_2021/Metadata/Deployment Meta Data/BirdIsland2021_Deployment_Data.csv")
path_out <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/MERRA-2/L1_appended/L1_2_30sAppended_byseasonphase/"

m$season <- "2020-2021"
m$location <- "Bird Island"

n_rows <- m %>% count(id)
colnames(n_rows) <- c("Deployment_ID", "n")

meta_filter <- meta %>% dplyr::select(Deployment_ID, Capture_Nest_Contents)
match <- na.omit(left_join(meta_filter, n_rows))
match$phase <- NA

match$phase <-  ifelse(match$Capture_Nest_Contents== "C","Brood-guard", "Incubation" )


df <- uncount(match, n)
colnames(df) <- c("id", "Capture_Nest_Contents", "phase")
attach(df)
df <- df[order(id),]
phase <- df[, 3]

attach(m)
m <- m[order(id),]

m <- m[,1:29]
m <- data.frame(m,phase)
write.csv(m, file = paste0(path_out, "Wind30sAppendedWithPhase_2020.csv"), row.names=FALSE)



# -----------------------------------
# b. 2019-2020 Bird Island
# -----------------------------------
setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/L0_paired/2019-2020/indiv_birds/")
a <- list.files(pattern = ".csv")
path_out <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/L1_appended/L1_1_30sAppended_byseason/"

# Append 30s 
for (i in 1: length(a)) {
  m <- read.csv(a[i])
  if (i==1) {
    mat <- m 
  }else{
    mat <- rbind(mat,m)
  }
}

write.csv(mat, file = paste0(path_out, "Wind30sAppended_2019.csv"), row.names=FALSE)

# Define breeding phase, location, season
m <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/L1_appended/L1_1_30sAppended_byseason/Wind30sAppended_2019.csv")
meta <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Conners_Bird_Island/2019_2020/Metadata/Metadata_Deployment-Data/L1_1_deployment-data_PROOFED_with-masses/BirdIsland_2019-2020_BreedingPhase.csv")
path_out <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/L1_appended/L1_2_30sAppended_byseasonphase/"


m$season <- "2019-2020"
m$location <- "Bird Island"

n_rows <- m %>% count(id)
colnames(n_rows) <- c("Deployment_ID", "n")
n_rows$Num <- str_split_fixed(n_rows$Deployment_ID, "_", 2)[,2]
n_rows$Num = str_remove(n_rows$Num, "^0+")

match <- na.omit(left_join(meta, n_rows))

df <- uncount(match, n)
df <-data.frame( df[, 3:4])
colnames(df) <- c("phase", "id")
df <- arrange(df, df$id)

m <- arrange(m, m$id)
m <- data.frame(m,df)
m <- m[,1:30]

write.csv(m, file = paste0(path_out, "Wind30sAppendedWithPhase_2019.csv"), row.names=FALSE)

# -----------------------------------
# c. 2019-2020 Bird Island, WAAL, incubation
# -----------------------------------
# append 30s
setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/MERRA-2/L0_paired/2019-2020_WAAL_incubation/indiv_birds/")
pathout <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/MERRA-2/L1_appended/L1_1_30sAppended_byseason/"
a <- list.files(pattern = ".csv")

for (i in 1: length(a)) {
  m <- read.csv(a[i])
  if (i==1) {
    mat <- m 
  }else{
    mat <- rbind(mat,m)
  }
}

write.csv(mat, file = paste0(pathout, "Wind30sAppended_2019WAAL.csv"), row.names=FALSE)

# Define breeding phase, location, season
pathout <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/MERRA-2/L1_appended/L1_2_30sAppended_byseasonphase/"

mat$phase <- "Incubation"
mat$season <- "2019-2020"
mat$location <- "Bird Island"
write.csv(mat, file = paste0(pathout, "Wind30sAppendedWithPhase_2019WAAL.csv"), row.names=FALSE)

# -----------------------------------
# d. 2018-2019 Bird Island, WAAL, brood-guard
# -----------------------------------
# append 30s
setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/MERRA-2/L0_paired/2018-2019_WAAL_brood-guard/indiv_birds/")
pathout <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/MERRA-2/L1_appended/L1_1_30sAppended_byseason/"
a <- list.files(pattern = ".csv")

for (i in 1: length(a)) {
  m <- read.csv(a[i])
  if (i==1) {
    mat <- m 
  }else{
    mat <- rbind(mat,m)
  }
}

write.csv(mat, file = paste0(pathout, "Wind30sAppended_2018WAAL.csv"), row.names=FALSE)

# Define breeding phase, location, season
pathout <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/MERRA-2/L1_appended/L1_2_30sAppended_byseasonphase/"

mat$phase <- "Brood-guard"
mat$season <- "2018-2019"
mat$location <- "Bird Island"
write.csv(mat, file = paste0(pathout, "Wind30sAppendedWithPhase_2018WAAL.csv"), row.names=FALSE)

# -----------------------------------
# e. 2018-2019 Bird Island, BBAL brood-guard
# -----------------------------------
setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/MERRA-2/L0_paired/2018-2019_BBAL/indiv_birds/")
a <- list.files(pattern = ".csv")
path_out <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/MERRA-2/L1_appended/L1_1_30sAppended_byseason/"


for (i in 1: length(a)) {
  m <- read.csv(a[i])
  if (i==1) {
    mat <- m 
  }else{
    mat <- rbind(mat,m)
  }
}

mat$season <- "2018-2019"
mat$location <- "Bird Island"
write.csv(mat, file = paste0(path_out, "Wind30sAppended_2018BBAL.csv"), row.names=FALSE)

# Define breeding phase, location, season
pathout <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/MERRA-2/L1_appended/L1_2_30sAppended_byseasonphase/"
mat$phase <- "Brood-guard"
write.csv(mat, file = paste0(pathout, "Wind30sAppendedWithPhase_2018BBAL.csv"), row.names=FALSE)

# -----------------------------------
# f. 2018-2019 Midway
# -----------------------------------
# append 30s
setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/L0_paired/2018-2019_Midway/indiv_birds/")
a <- list.files(pattern = ".csv")
path_out <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/L1_appended/L1_1_30sAppended_byseason/"

# Append 30s 
for (i in 1: length(a)) {
  m <- read.csv(a[i])
  if (i==1) {
    mat <- m 
  }else{
    mat <- rbind(mat,m)
  }
}

mat <- mat[,1:27]
mat[mat=="BFAL_01b"]<-"BFAL_01"
mat[mat=="BFAL_01b_1"]<-"BFAL_01_2"

mat[mat=="BFAL_03b"]<-"BFAL_03"
mat[mat=="BFAL_03b_1"]<-"BFAL_03_2"

mat[mat=="BFAL_08b"]<-"BFAL_08"
mat[mat=="BFAL_08b_1"]<-"BFAL_08_2"

mat[mat=="BFAL_09b"]<-"BFAL_09"
mat[mat=="BFAL_09b_1"]<-"BFAL_09_1"

write.csv(mat, file = paste0(path_out, "Wind30sAppended_2018-2019Midway.csv"), row.names=FALSE)

# Define breeding phase, location, season
m <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/MERRA-2/L1_appended/L1_1_30sAppended_byseason/Wind30sAppended_2018-2019Midway.csv")
meta <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Conners_Midway/2018-2019/Metadata/Deployment_Data/Midway2019_AlbatrossDeployments_04172019_transposed.csv")
path_out <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/MERRA-2/L1_appended/L1_2_30sAppended_byseasonphase/"

m$season <- "2018-2019"
m$location <- "Midway"
m$phase <- "Brood-guard"
write.csv(m, file = paste0(path_out, "Wind30sAppendedWithPhase_2018-2019Midway.csv"), row.names=FALSE)


###############################################################################
# 2. Append all 30s data from all datasets into one file 
###############################################################################
setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/MERRA-2/L1_appended/L1_2_30sAppended_byseasonphase/")
a <- list.files(pattern = ".csv")
path_out <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/L1_appended/L1_3_30sAppended/"

# Append 30s 
for (i in 1: length(a)) {
  m <- read.csv(a[i])
  if (i==1) {
    appended <- m 
  }else{
    appended <- rbind(appended,m)
  }
}


write.csv(appended, file = "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/MERRA-2/L1_appended/L1_3_30sAppended/Wind30sAppendedWithPhase.csv", row.names=FALSE)
