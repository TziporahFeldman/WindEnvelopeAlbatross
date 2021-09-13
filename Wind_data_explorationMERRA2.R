library(dplyr)
library(hrbrthemes)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(stringr)
library(RColorBrewer)
library(gridExtra) 
library(ggiraphExtra)
library(ggplot2)

##################################################################################
# This code is used to create visualizations for behavior, wind, and location data
##################################################################################

########################################################################
# Set environment and load data
########################################################################
dropdir <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/HMM/HMM_wind_match/AllWAAL/figures/"
dropdir_sub <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/HMM/HMM_wind_match/WAAL_Subset/figures/"
match <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/HMM/HMM_wind_match/HMM_wind_30s.csv")
match_sub <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/HMM/HMM_wind_match/HMM_wind_30s_WAALSubset.csv")


###############################################################################
# Summary Statistics
summary <- match  %>% group_by(species, phase, state)%>% 
  summarise(avg_speed2M=mean(wind_speed2M),
            avg_shear10M=mean(windshear10m), 
            q1speed_2M = quantile(wind_speed2M, 0.25),
            q3speed_2M = quantile(wind_speed2M, 0.75),
            q1shear_2M = quantile(windshear10m, 0.25),
            q3shear_2M = quantile(windshear10m, 0.75),
            n=n())
summary<-summary %>% group_by(species, phase) %>% mutate(percent = n/sum(n))

summary_sub <- match_sub  %>% group_by(species, phase, state)%>% 
  summarise(avg_speed2M=mean(wind_speed2M),
            avg_shear10M=mean(windshear10m), 
            q1speed_2M = quantile(wind_speed2M, 0.25),
            q3speed_2M = quantile(wind_speed2M, 0.75),
            q1shear_2M = quantile(windshear10m, 0.25),
            q3shear_2M = quantile(windshear10m, 0.75),
            n=n())
summary_sub <-summary_sub %>% group_by(species, phase) %>% mutate(percent = n/sum(n))

write.csv(summary, "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/HMM/HMM_wind_match/summary_statistics_WAALSubset.csv")

###############################################################################
# Wind Envelope and state, phase
############################################################################### 
# -----------------------------------
# Bar plot
# -----------------------------------
summary_sub$spp_phase <-str_c(summary_sub$species,summary_sub$phase, sep = "_")
summary_sub$state <- as.character(summary_sub$state)

summary_soaring <- summary %>% filter(state=="2")
write.csv(summary_soaring_WAALSubset, "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/HMM/HMM_wind_match/summary_statistics_WAALSubset_SoaringOnly.csv", row.names = F)

ggplot(summary_sub, aes(x=spp_phase, y=q1speed_2M, fill=state))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.text = element_text(size = 6)
  )+
  ylab("25th Quantile Wind Speed m/s")+
  xlab("Species and Breeding Phase")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+
  theme(legend.position = "top")
ggsave("Wind_speed_state_phase.png", path = dropdir_sub)


ggplot(summary, aes(x=spp_phase, y=q1shear_2M, fill=state))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.text = element_text(size = 6)
  )+
  ylab("25th Quantile Wind Shear (m/s)")+
  xlab("Species phase")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+
  theme(legend.position = "top")

ggsave("Wind_shear_state_phase.png", path = dropdir)

# -----------------------------------
# Bar plot, soaring only
# ----------------------------------- 
ggplot(summary_soaring_WAALSubset, aes(x=spp_phase, y=q1speed_2M, fill=spp_phase))+
  geom_bar(stat="identity",position="dodge")+
  theme_bw()+
  theme(legend.position="none")+
  ylab("25th Quantile Wind Speed m/s")+
  xlab("Species phase")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+
  theme(legend.position = "top")

ggsave("Wind_speed_state_phase_Soaring.png", path = dropdir_sub)


ggplot(summary_soaring_WAALSubset, aes(x=spp_phase, y=q1shear_2M, fill=spp_phase))+
  geom_bar(stat="identity",position="dodge")+
  theme_bw()+
  theme(legend.position="none")+
  ylab("25th Quantile Wind Shear(m/s)")+
  xlab("Species phase")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+
  theme(legend.position = "top")

ggsave("Wind_shear_state_phase_Soaring.png", path = dropdir_sub)

# -----------------------------------
# Ridge Plots
# -----------------------------------
soaring <- match %>% filter(state=="2")
soaring$spp_phase <- str_c(soaring$species,soaring$phase, sep = "_")

soaring_sub <- match_sub %>% filter(state=="2")
soaring_sub$spp_phase <- str_c(soaring_sub$species,soaring_sub$phase, sep = "_")


ggplot(soaring_sub, aes(y=spp_phase, x=wind_speed2M, fill=spp_phase)) +
  geom_density_ridges(alpha=0.8, bandwidth=.5, quantile_lines=TRUE, rel_min_height = 0.01) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Soaring Wind Speed")+
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines")
  )

ggsave("Wind_speed_soaring_Ridge.png", path = dropdir_sub)

ggplot(soaring_sub, aes(y=spp_phase, x=windshear10m, fill=spp_phase)) +
  geom_density_ridges(alpha=0.8, bandwidth=.1, quantile_lines=TRUE, rel_min_height = 0.01) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Soaring Wind Shear(m/s)")+
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.2, "lines")
  )

ggsave("Wind_shear_soaring_Ridge.png", path = dropdir_sub)


# By species only
ggplot(soaring, aes(y=species, x=wind_speed2M, fill=species)) +
  geom_density_ridges(alpha=0.8, bandwidth=.5, quantile_lines=TRUE, rel_min_height = 0.01) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Soaring Wind Speed")+
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines")
  )

ggsave("Wind_speed_soaring_spp_Ridge.png", path = dropdir)

ggplot(soaring, aes(y=species, x=windshear10m, fill=species)) +
  geom_density_ridges(alpha=0.8, bandwidth=.1, quantile_lines=TRUE, rel_min_height = 0.01) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Soaring Wind Shear(m/s)")+
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.2, "lines")
  )

ggsave("Wind_shear_soaring_spp_Ridge.png", path = dropdir)

###############################################################################
# Soaring Wind Speed Stacked Bars
###############################################################################
breaks <- rep(0:24, 1)
names <- c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13",
           "13-14","14-15","15-16","16-17","17-18","18-19","19-20","20-21", "21-22", "22-23", "23-24")

# -----------------------------------
# All data
# ----------------------------------
match$spp_phase <- str_c(match$species,match$phase, sep = "_")
match$wind_speed_cat <- cut(match$wind_speed2M, breaks = breaks, labels = names)
match$soaring <-  ifelse(match$state== "2","Soaring", "Non-Soaring" ) 
m_bar <- match %>% group_by(species, phase)%>% count(wind_speed_cat, soaring)

# Proportion
ggplot(m_bar, aes(fill=soaring, y=n, x=wind_speed_cat)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Speed")+
  ylab("Proporion")+
  # theme(legend.position="none")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+
  facet_grid(phase~species)+
  # geom_hline(yintercept=.25,  linetype="dashed")+
  # geom_vline(data=summary_soaring,aes(xintercept= avg_speed2M))+
  # geom_vline(data=summary_soaring,aes(xintercept=q3speed_2M), linetype="dashed")+
  geom_vline(data=summary_soaring,aes(xintercept=q1speed_2M))

ggsave("Wind_speed_soaring_proportion.png", path = dropdir, width=14.5)

# -----------------------------------
# WAAL subset
# ----------------------------------
match_sub$spp_phase <- str_c(match_sub$species,match_sub$phase, sep = "_")
match_sub$wind_speed_cat <- cut(match_sub$wind_speed2M, breaks = breaks, labels = names)
match_sub$soaring <-  ifelse(match_sub$state== "2","Soaring", "Non-Soaring" ) 
m_bar_sub <- match_sub %>% group_by(species, phase)%>% count(wind_speed_cat, soaring)

ggplot(m_bar_sub , aes(fill=soaring, y=n, x=wind_speed_cat)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Speed")+
  ylab("Proportion")+
  # theme(legend.position="none")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+
  facet_grid(phase~species)+
  # geom_hline(yintercept=.25,  linetype="dashed")+
  # geom_vline(data=summary_soaring,aes(xintercept= avg_speed2M))+
  # geom_vline(data=summary_soaring,aes(xintercept=q3speed_2M), linetype="dashed")+
  geom_vline(data=summary_soaring_WAALSubset,aes(xintercept=q1speed_2M))

ggsave("Wind_speed_soaring_proportion.png", path = dropdir_sub)
###############################################################################
# Wind Shear Stacked Bars
###############################################################################
breaks <- seq(0, 5, by=.5)
names <- c("0-0.5","0.5-1","1-1.5","1.5-2","2-2.5","2.5-3","3-3.5","3.5-4","4-4.5","4.5-5")
match$wind_shear_cat <- cut(match$windshear10m, breaks = breaks, labels = names)
m_bar<- match%>% group_by(species, phase)%>% count(wind_shear_cat, soaring)

# -----------------------------------
# All data
# ----------------------------------
 ggplot(m_bar, aes(fill=soaring, y=n, x=wind_shear_cat)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Shear")+
  ylab("Proportion")+
  # theme(legend.position="none")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+
  facet_grid(phase~species)+
  geom_vline(data=summary_soaring,aes(xintercept=q1shear_2M))

ggsave("Wind_shear_soaring_proportion.png", path = dropdir, width=12)

# -----------------------------------
# WAAL subset
# -----------------------------------
match_sub$wind_shear_cat <- cut(match_sub$windshear10m, breaks = breaks, labels = names)
m_bar_sub<- match_sub%>% group_by(species, phase)%>% count(wind_shear_cat, soaring)

ggplot(m_bar_sub, aes(fill=soaring, y=n, x=wind_shear_cat)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Shear")+
  ylab("Proportion")+
  # theme(legend.position="none")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+
  facet_grid(phase~species)+
  geom_vline(data=summary_soaring,aes(xintercept=q1shear_2M))

ggsave("Wind_shear_soaring_proportion.png", path = dropdir_sub)

###############################################################################
# Contour plots
###############################################################################
df <- appended[sample(nrow(appended), 500000), ]
world<- map_data("world")

ggplot(df, aes(x=Lon360to180(lon), y=lat)) +
  geom_polygon(data=world,aes(x=long,y=lat, group=group),color='black',fill=NA)+
  stat_density_2d(aes(fill = ..level..), geom = "polygon")









