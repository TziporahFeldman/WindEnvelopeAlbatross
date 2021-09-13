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
GPS_sample <- sample_n(GPS, 500)

###############################################################################
# Exploratory Graphs 
###############################################################################
dropdir <- "/Users/tziporahserota/Desktop/Thorne Lab/Analyses/Wind/Wind_maps/Figures/Graphs/Exploratory/"
attach(GPS_sample)

# scatterplot
png(file=paste0(dropdir, 'WindshearSoaring_scatterplot.png'))
plot(windshear10m, soaring)
dev.off()

png(file=paste0(dropdir, 'WindspeedSoaring_scatterplot.png'))
plot(wind_speed2M, windshear10m)
dev.off()
GPS_sample.pair <- GPS_sample[, c(15, 16, 17, 21, 22)]
pairs(GPS_sample.pair)
###############################################################################
# Model
###############################################################################
dropdir <- "/Users/tziporahserota/Desktop/Thorne Lab/Analyses/Wind/Wind_maps/Figures/Graphs/Models/"

# -----------------------------------
# GLM
# -----------------------------------
attach(GPS)
# wind speed only
fit <- glm(soaring~wind_speed2M, family = "binomial")
summary(fit)
MyData<-data.frame(wind_speed2M=seq(min(wind_speed2M),max(wind_speed2M)))
Pred <- predict(fit, newdata = MyData, type = "response")

png(file=paste0(dropdir, 'WindspeedSoaring_model.png'))
plot(x = MERRA$wind_speed2M, y = MERRA$soaring, xlab="Wind Speed", ylab="Soaring")
lines(MyData$wind_speed2M, Pred)
dev.off()

# wind shear only
fit2 <- glm(soaring~windshear, family = "binomial")
summary(fit2)
MyData<-data.frame(windshear=seq(min(windshear),max(windshear)))
Pred <- predict(fit2, newdata = MyData, type = "response")

png(file=paste0(dropdir, 'WindshearSoaring_model.png'))
plot(x = MERRA$windshear, y = MERRA$soaring, xlab="Wind Shear", ylab="Soaring")
lines(MyData$windshear, Pred)
dev.off()

# bivariate, no interaction
fit3 <- glm(soaring~windshear+wind_speed2M+ fspecies, family = "binomial")
summary(fit3)

op <- par(mfrow=c(2,2))
plot(fit3)
par(op)

# bivariate, interaction
GPS$fspecies <- factor(species)
GPS$fphase <- factor(phase)

attach(GPS)
fit4 <- glm(soaring~windshear10m*wind_speed2M+ fspecies+ fphase, family = "binomial")
summary(fit4)
step(fit4)

op <- par(mfrow=c(2,2))
plot(fit4)
par(op)

# -----------------------------------
# GAM
# -----------------------------------
fit_gamm <- gamm(soaring~ s(windshear)+ s(wind_speed2M)+ fspecies, family = binomial)
summary(fit_gamm)
plot(fit_gamm$gam)

###############################################################################
# Mixed Models 
###############################################################################
# -----------------------------------
# center data 
# -----------------------------------


# -----------------------------------
# GLMM
# -----------------------------------
MERRA$fid <- factor(id)
MERRA$fspecies <- factor(species)
MERRA$fwinshear <- factor(windshear)
# fixed
fit_fixed <- glm(soaring~windshear*wind_speed2M+fid, 
                 family = "binomial")
summary(fit_fixed)

# MASS
fit_mixed <- glmmPQL(soaring~windshear*wind_speed2M, 
                 random=~1|fid,
                 family = "binomial")
summary(fit_mixed)

op <- par(mfrow=c(2,2))
plot(fit_mixed)
par(op)

# lme4
fit_mixed2 <- glmer(soaring~ windshear * wind_speed2M + fspecies+ (1|fid), nAGQ=0,
                     family = binomial) # https://stats.stackexchange.com/questions/304132/glmer-not-converging
summary(fit_mixed2)

plot_model(fit_mixed2)
# -----------------------------------
# GAMM
# -----------------------------------
fit_gammMix <- gamm(soaring~ s(windshear)+ s(wind_speed2M)+ fspecies, random = list(fid=~1), family= binomial)
plot(fit_gammMix$gam)

