
### Data Import
source("data_compiling/compiling_climate.R")

# Packages
library(tidyverse) #data wrangling
library(ggplot2) #plot
library(ggpubr) #combine plots
library(vegan) #nmds
library(corrplot) #correlation matrix
library(dplyr)
library(lubridate)
library(ggrepel) #add labels to pca

#PCA of climate variables from PRISM

climate_matrix <- as.matrix(climate[,2:ncol(climate)]) 
pca_climate = rda(climate_matrix, scale = TRUE) #run PCA on all traits
biplot(pca_climate, display = c("sites", "species"), type = c("text", "points")) #plot biplot
pca_climate_scores <- as.data.frame(scores(pca_climate, choices=c(1,2), display=c("sites"))) #extract pca1 and pca2 scores
pca_climate_scores_lab = as.data.frame(cbind(climate[,1],pca_climate_scores))  #add plot info back
pca_climate_scores_lab$Population <- ordered(as.factor(pca_climate_scores_lab$Population), levels = c("Norc","NGBER", "Vale", "Susa",  "Roar",
                                                                                                  "Elko", "Litt"))
envout<-as.data.frame(scores(pca_climate, choices=c(1,2), display=c("species")))
summary(pca_climate)
ggplot(pca_climate_scores_lab, aes(x = PC1, y = PC2))+
  geom_point(size = 4, aes(colour = Population), alpha = 0.5)+
  theme(text = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15))+
  geom_segment(data = envout, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               alpha = 0.5, size = 1, colour = "grey30") +
  geom_text(data = envout, aes(x = PC1, y = PC2), colour = "grey30",
            fontface = "bold", label = row.names(envout), size = 5)+
  xlim(-2, 2.3)+
  xlab("PC1 (50.7%)")+
  ylab("PC2 (29.5%)")

## Monthly precipitation at Riley Weather Station
rain_monthly <- rain %>% 
  mutate(Time = lubridate::mdy(rain$Time)) 
rain_monthly$Type <- ordered(rain_monthly$Type, levels = c("actual", "30 yr average"))
ggplot(rain_monthly, aes(x = Time, y = Precipitation_mm, col = Type, lty = Type))+
  geom_rect(aes(xmin = as.Date("2020-09-01", "%Y-%m-%d"), 
                xmax = as.Date("2021-09-01", "%Y-%m-%d"), 
                ymin = -2, ymax = 45), fill = "#add8e6", alpha = 0.02)+
  geom_rect(aes(xmin = as.Date("2021-09-01", "%Y-%m-%d"), 
                xmax = as.Date("2022-08-01", "%Y-%m-%d"), 
                ymin = -2, ymax = 45), fill = "#d6ecf3", alpha = 0.03)+
  geom_point()+
  geom_line(size = 1)+
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "right")+
  ylab(bquote(Monthly~Precipitation~(mm)))+
  labs(col = "", lty = "")+
  scale_color_manual(values=c("#000000",  "#808080"))

#Link weather and treatment to help us hypothesize which seed source should do better in ambient vs drought
#remove NGBER_2021 and 2022
weather_simple <- weather %>%
  filter(Site != "NGBER_2021")%>%
  filter(Site != "NGBER_2022")
#monthly averages
ggplot(weather_simple, aes(x =Month, y=PRCP_mm, col=Site))+
  geom_point()+
  geom_line()
ggplot(weather_simple, aes(x =Month, y=TAVG_C, col=Site))+
  geom_point()+
  geom_line()
ggplot(weather_simple, aes(x =Month, y=TMAX_C, col=Site))+
  geom_point()+
  geom_line()
ggplot(weather_simple, aes(x =Month, y=TMIN_C, col=Site))+
  geom_point()+
  geom_line()

total_ppt <- weather%>%
  group_by(Site)%>%
  summarise(total_ppt = sum(PRCP_mm))

July_temp <- weather %>%
  filter(Month == 7) %>%
  dplyr::select(Site, TAVG_C, TMAX_C)

March_temp <- weather %>%
  filter(Month == 3) %>%
  dplyr::select(Site, TAVG_C, TMAX_C, TMIN_C)

#PCA of climate variables v2 with NOAA data

climate_all_matrix <- as.matrix(climate_all[1:7,2:8]) 
pca_climate_all = rda(climate_all_matrix, scale = TRUE) #run PCA on all traits
biplot(pca_climate_all, display = c("sites", "species"), type = c("text", "points")) #plot biplot
pca_climate_scores_all <- as.data.frame(scores(pca_climate_all, choices=c(1,2), display=c("sites"))) #extract pca1 and pca2 scores
pca_climate_scores_lab_all = as.data.frame(cbind(climate_all[1:7,1],pca_climate_scores_all))  #add plot info back
pca_climate_scores_lab_all$Site <- ordered(as.factor(pca_climate_scores_lab_all$Site), levels = c("Norc","NGBER","Vale", "Susa",  "Roar",
                                                                                                      "Elko", "Litt"))
envout_all<-as.data.frame(scores(pca_climate_all, choices=c(1,2), display=c("species")))
summary(pca_climate_all)
ggplot(pca_climate_scores_lab_all, aes(x = PC1, y = PC2))+
  geom_point(size = 4, aes(colour = Site), alpha = 0.5)+
  theme(text = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15))+
  geom_segment(data = envout_all, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               alpha = 0.5, size = 1, colour = "grey30") +
  geom_text(data = envout_all, aes(x = PC1, y = PC2), colour = "grey30",
            fontface = "bold", label = row.names(envout_all), size = 5)+
  xlim(-2, 2.3)+
  #xlab("PC1 (49.4%)")+
  #ylab("PC2 (24.0%)")+
  geom_text(aes(label=Site),vjust = 1.6)+
  labs(x=expression(atop("Cool" %<->% "Warm","PC1 (49.4%)")), y = expression(atop("Dry" %<->% "Wet","PC2 (24.0%)")))

#Bar graphs with treatments
bar_1 <- ggplot(climate_all, aes(x = reorder(Site, Tmax_03), y = Tmax_03))+
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue")+
  theme(text = element_text(size=18),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust=1))+
  labs(y = expression("March Maximum Air Temperature " (degree*C)), x = expression(atop("Cool" %<->% "Warm","Site")))

bar_2 <- ggplot(climate_all, aes(x = reorder(Site, Days_above_90), y = Days_above_90))+
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue")+
  theme(text = element_text(size=18),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust=1))+
  labs(y = expression("Number of days above "*32~degree*C), x = expression(atop("Cool" %<->% "Warm","Site")))

bar_3 <- ggplot(climate_all, aes(x = reorder(Site, PPT_annual), y = PPT_annual))+
  geom_bar(stat = "identity", width =0.5, fill = "steelblue")+
  theme(text = element_text(size=18),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust=1))+
  labs(y = "Annual Precipitation (mm)", x = expression(atop("Dry" %<->% "Wet","Site")))

bar_4 <- ggplot(climate_all, aes(x = reorder(Site, -Vpdmax_07), y = Vpdmax_07))+
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue")+
  theme(text = element_text(size=18),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust=1))+
  labs(y = "July Maximum Vapor Pressure Decifit (kPa)", x = expression(atop("Dry" %<->% "Wet","Site")))

