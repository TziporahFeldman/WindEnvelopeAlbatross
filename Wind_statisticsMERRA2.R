########################################################################
# Load Libraries
########################################################################
library(ggplot2) # plots 
library(tidyverse) # data cleanup
library(lubridate) # Clean up dates
library(mgcv) # GAMs
library(MASS) # GLMMs
library(lme4) # GLMMs
library(sjPlot)
library(ggeffects)
library(tidymv)
library(gridExtra)
library(visreg)

#########################################################################
# This code uses GPS, behavioral, and wind data to create a GLMM and GAMM
#########################################################################

###############################################################################
# Import data  
###############################################################################
GPS <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/HMM/HMM_wind_match/AllData/HMM_wind_30s.csv")
# -----------------------------------------------------------------
# Clean data 
# -----------------------------------------------------------------
GPS$soaring <-  ifelse(GPS$state== "2","1", "0" )
GPS$soaring <- as.numeric(GPS$soaring)
GPS_sample <- sample_n(GPS, 5000) 

write.csv(GPS_sample, "/Users/tziporahserota/Desktop/Thorne Lab/GPS_sample.csv")
###############################################################################
# Exploratory Graphs 
###############################################################################
dropdir <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/HMM/HMM_wind_match/AllData/figures/Models/"
attach(GPS_sample)

# scatterplot
png(file=paste0(dropdir, 'WindshearSoaring_scatterplot.png'))
plot(wind_speed2M, soaring)
dev.off()

png(file=paste0(dropdir, 'Windspeed_shear_scatterplot.png'))
plot(wind_speed2M, windshear10m)
dev.off()
GPS_sample.pair <- GPS_sample[, c(15, 16, 17, 21, 22)]
pairs(GPS_sample.pair)

###############################################################################
# Mixed Models 
###############################################################################

# -----------------------------------
# GLMM
# -----------------------------------
attach(GPS_sample)
GPS_sample$fid <- factor(id)
GPS_sample$fspecies <- factor(species)
GPS_sample$fphase <- factor(phase)

# lme4
attach(GPS_sample)
fit_mixed2 <- glmer(soaring~ windshear10m * wind_speed2M + fspecies+ fphase+ (1|fid), nAGQ=0,
                    family = binomial) # https://stats.stackexchange.com/questions/304132/glmer-not-converging
summary(fit_mixed2)


plot(fit_mixed2)
order <- order(GPS_sample$wind_speed2M)
species <- unique(GPS_sample$species)

a <- ggpredict(fit_mixed2, c("windshear10m[all]", "fspecies", "fphase")) %>% plot()
b <- ggpredict(fit_mixed2, c("wind_speed2M[all]", "fspecies", "fphase")) %>% plot()
g <- arrangeGrob(a, b, ncol=1) #generates g
ggsave(file="/Users/tziporahserota/Desktop/GLMM_predict.png", g) #saves g
# -----------------------------------
# GAMM
# -----------------------------------
attach(GPS_sample)
fit_gammMix <- gamm(soaring~ s(windshear10m)+ s(wind_speed2M)+ fspecies+ fphase, random = list(fid=~1), family= binomial)


png(file=paste0(dropdir, 'gamm_output.png'))
par(mfrow=c(2,1))
plot(fit_gammMix$gam)
summary(fit_gammMix)


a <- ggplot(data = GPS_sample, aes(x = windshear10m, y = soaring)) + 
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) 

b <- ggplot(data = GPS_sample, aes(x = wind_speed2M, y = soaring)) + 
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) 

g <- arrangeGrob(a, b, ncol=1) #generates g
ggsave(file="/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/HMM/HMM_wind_match/AllData/figures/Models/GAMM.png", g) #saves g
  
png(file=paste0(dropdir, 'windspeed_hist.png'))
hist(GPS_sample$wind_speed2M)
dev.off()

