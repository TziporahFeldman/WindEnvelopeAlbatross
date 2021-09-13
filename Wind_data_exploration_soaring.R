library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(stringr)
library(RColorBrewer)
library(gridExtra) 
library(ggiraphExtra)



dropdir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data!/Conners_Analysis/Wind/wind_graphs/wind_HMM/"

# Download 2019-2020 GPS/wind/HMM state file 
m <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data!/Conners_Analysis/Wind/wind_paired-data/2019-2020/bird-latlon-with-wind-bwa-state/ERA5_SingleLevels-10m/Appended/allaccwindstate30s_2019_withphase.csv")
m$state <- as.character(m$state) 


###############################################################################
# Summary Statistics
############################################################################### 
summary <- m  %>% group_by(species, phase, state)%>% 
  summarise(avg_speed=mean(wind_speed), max_speed=max(wind_speed), min_speed=min(wind_speed), q1 = quantile(wind_speed, 0.25),
            q3 = quantile(wind_speed, 0.75),avg_bwa=mean(bwa, na.rm=TRUE), max_bwa=max(bwa, na.rm=TRUE), min_bwa=min(bwa, na.rm=TRUE))

m <- m[, c(1, 12, 14, 20, 22)]
write.csv(summary, "/Users/tziporahserota/Desktop/Thorne Lab/wind_summarystatistics.csv")

###############################################################################
# Graphs
############################################################################### 

# -----------------------------------
# Prep data for graphs
# -----------------------------------
# Divide into species
bbal <- m %>% filter(species=="BBAL")
ghal <- m %>% filter(species=="GHAL")

# Divide into HMM state
bbal_1 <-  bbal %>% filter(state=="1")
bbal_2 <-  bbal %>% filter(state=="2")
bbal_3 <-  bbal %>% filter(state=="3")



ghal_1 <- ghal %>% filter(state=="1")
ghal_2 <- ghal %>% filter(state=="2")
ghal_3 <- ghal %>% filter(state=="3")


# Divide by breeding phase 
ghal_2_inc<-  m %>% filter(phase=="Incubation")
ghal_2_brd<-  m %>% filter(phase=="Brood-guard")

bbal_2_inc<-  m %>% filter(phase=="Incubation")
bbal_2_brd<-  m %>% filter(phase=="Brood-guard")


hist(bbal_1$wind_speed)
hist(bbal_2$wind_speed)
hist(bbal_3$wind_speed)
hist(ghal_1$wind_speed)
hist(ghal_2$wind_speed)
hist(ghal_3$wind_speed)
hist(ghal_2_inc$wind_speed)
hist(ghal_2_brd$wind_speed)
hist(bbal_2_brd$wind_speed)
hist(bbal_2_inc$wind_speed)

m$spp_phase_state <- str_c(m$spp_phase, m$state, sep = "_")
m$spp_state <- str_c(m$species, m$state, sep = "_")
# -----------------------------------
# Comparing states 
# -----------------------------------
x <- ggplot(ghal, aes(y=state, x=wind_speed, fill=state)) +
  geom_violin(alpha=0.6)+
  geom_boxplot(alpha=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  ggtitle("GHAL Wind Speed and State")+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

y <- ggplot(bbal, aes(y=state, x=wind_speed, fill=state)) +
  geom_violin(alpha=0.6)+
  geom_boxplot(alpha=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  ggtitle("BBAL Wind Speed and State")+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

grid.arrange(x, y,  ncol=2)



ggplot(m, aes(y=spp_state, x=wind_speed, fill=spp_state)) +
  geom_density_ridges(alpha=0.6, bandwidth=.5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Wind Speed and State")+
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines")
  )
ggsave("Wind_speed_state.png", path = dropdir)


# -----------------------------------
# Comparing states and phase
# -----------------------------------
ggplot(m, aes(y=spp_phase_state, x=wind_speed, fill=spp_phase_state)) +
  geom_density_ridges(alpha=0.6, bandwidth=.5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Wind Speed during Breeding Phase and state")+
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines")
  )

ggsave("Wind_speed_state_phase.png", path = dropdir)

# -----------------------------------
# bar plot
# -----------------------------------
summary <- read.csv("/Users/tziporahserota/Desktop/Thorne Lab/wind_summarystatistics.csv")
summary$spp_phase <-str_c(summary$species,summary$phase, sep = "_")
summary$state <- as.character(summary$state)
summary_2 <- summary%>% filter(state=="2")

avg_windspeed <- ggplot(summary, aes(x=spp_phase, y=avg_speed, fill=state))+
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
  ylab("Average Wind Speed m/s")+
  xlab("Species phase")

b <- ggplot(summary, aes(x=spp_phase, y=max_speed, fill=state))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  theme(legend.position="none")+
  ylab("Maximum Wind Speed m/s")+
  xlab("Species phase")

c <- ggplot(summary, aes(x=spp_phase, y=min_speed, fill=state))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  theme(legend.position="none")+
  ylab("Minimum Wind Speed m/s")+
  xlab("Species phase")



# soaring only 
avg_windspeed <- ggplot(summary_2, aes(x=spp_phase, y=avg_speed, fill=spp_phase))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb', '#e78ac3'))+
  theme_bw()+
  theme(legend.position="none")+
  ylab("Average Wind Speed m/s")+
  xlab("Species phase")

b <- ggplot(summary_2, aes(x=spp_phase, y=max_speed, fill=spp_phase))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb', '#e78ac3'))+
  theme_bw()+
  theme(legend.position="none")+
  ylab("Maximum Wind Speed m/s")+
  xlab("Species phase")

c <- ggplot(summary_2, aes(x=spp_phase, y=min_speed, fill=spp_phase))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb', '#e78ac3'))+
  theme_bw()+
  theme(legend.position="none")+
  ylab("Minimum Wind Speed m/s")+
  xlab("Species phase")

###############################################################################
# Soaring flight 
###############################################################################
# -----------------------------------
# Comparing states and phase
# -----------------------------------
dropdir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data!/Conners_Analysis/Wind/wind_graphs/wind_HMM/2019/"
soaring <- rbind(ghal_2, bbal_2)

ggplot(soaring, aes(y=spp_phase, x=wind_speed, fill=spp_phase)) +
  geom_density_ridges(alpha=0.8, bandwidth=.5, quantile_lines=TRUE, rel_min_height = 0.01) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("2019 Soaring Wind Speed")+
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines")
  )

ggsave("Wind_speed_soaring.png", path = dropdir)

ggplot(soaring, aes(y=spp_phase, x=wind_speed, fill=spp_phase)) +
  geom_violin(alpha=0.6)+
  geom_boxplot(alpha=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# -----------------------------------
# Rose Plots
# -----------------------------------

plot.windrose(spd = bbal_2$wind_speed[bbal_2$phase=="Brood-guard"], dir = bbal_2$bwa[bbal_2$phase=="Brood-guard"])
plot.windrose(spd = bbal_2$wind_speed[bbal_2$phase=="Incubation"], dir = bbal_2$bwa[bbal_2$phase=="Incubation"])

plot.windrose(spd = ghal_2$wind_speed[ghal_2$phase=="Brood-guard"], dir = ghal_2$bwa[ghal_2$phase=="Brood-guard"])
plot.windrose(spd = ghal_2$wind_speed[ghal_2$phase=="Incubation"], dir = ghal_2$bwa[ghal_2$phase=="Incubation"])


###############################################################################
# Stacked Bars
###############################################################################
# -----------------------------------
# All States
# -----------------------------------
# Count
breaks <- rep(0:21, 1)
names <- c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18","18-19","19-20","20-21")

m$wind_speed_cat <- cut(m$wind_speed, breaks = breaks, labels = names)

bbal <- m %>% filter(species=="BBAL")
ghal <- m %>% filter(species=="GHAL")

m_bar_bbal <- bbal %>% count(wind_speed_cat, state)
m_bar_ghal <- ghal %>% count(wind_speed_cat, state)

stacked_bbal <- ggplot(m_bar_bbal, aes(fill=state, y=n, x=wind_speed_cat)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Wind Speed (m/s)")+
  ylab("Count")+
  ylim(0, 50000)+
  ggtitle("BBAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))


stacked_ghal <- ggplot(m_bar_ghal, aes(fill=state, y=n, x=wind_speed_cat)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Speed")+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.text = element_text(size = 6)
  )+
  theme(axis.title.y = element_blank())+
  ylim(0, 50000)+
  ggtitle("GHAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))

grid.arrange(stacked_bbal,stacked_ghal, ncol=2)


# Proportion
bbal_prop <- ggplot(m_bar_bbal, aes(fill=state, y=n, x=wind_speed_cat)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Speed")+
  ylab("Proporion")+
  ggtitle("BBAL")+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))

ghal_prop <- ggplot(m_bar_ghal, aes(fill=state, y=n, x=wind_speed_cat)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Speed")+
  theme(axis.title.y = element_blank())+
  ggtitle("GHAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))

grid.arrange(bbal_prop,ghal_prop, ncol=2)

# -----------------------------------
# Just Soaring
# ----------------------------------
# Count
m$soaring <-  ifelse(m$state== "2","Soaring", "Non-Soaring" ) 
bbal <- m %>% filter(species=="BBAL")
ghal <- m %>% filter(species=="GHAL")
m_bar_bbal <- bbal %>% count(wind_speed_cat, soaring)
m_bar_ghal <- ghal %>% count(wind_speed_cat, soaring)

stacked_bbal <- ggplot(m_bar_bbal, aes(fill=soaring, y=n, x=wind_speed_cat)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Wind Speed")+
  ylab("Count")+
  ylim(0, 50000)+
  ggtitle("BBAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))


stacked_ghal <- ggplot(m_bar_ghal, aes(fill=soaring, y=n, x=wind_speed_cat)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Speed")+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.text = element_text(size = 6)
  )+
  theme(axis.title.y = element_blank())+
  ylim(0, 50000)+
  ggtitle("GHAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))



grid.arrange(stacked_bbal,stacked_ghal, ncol=2)


# Proportion

bbal_prop <- ggplot(m_bar_bbal, aes(fill=soaring, y=n, x=wind_speed_cat)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Speed")+
  ylab("Proporion")+
  ggtitle("BBAL")+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))

ghal_prop <- ggplot(m_bar_ghal, aes(fill=soaring, y=n, x=wind_speed_cat)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Speed")+
  theme(axis.title.y = element_blank())+
  ggtitle("GHAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))

grid.arrange(bbal_prop,ghal_prop, ncol=2)

