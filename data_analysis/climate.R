
### Data Import
source("data_compiling/compiling_climate.R")

# Packages
library(tidyverse) #data wrangling
library(ggplot2) #plot
library(ggpubr) #combine plots
library(vegan) #pca
library(stats) #pca
library(corrplot) #correlation matrix
library(dplyr)
library(lubridate)
library(ggrepel) #add labels to pca
library(ggfortify) #alternative pca package

#PCA of climate variables from PRISM

# climate_matrix <- as.matrix(climate[,2:ncol(climate)]) 
# pca_climate = rda(climate_matrix, scale = TRUE) #run PCA on all traits
# biplot(pca_climate, display = c("sites", "species"), type = c("text", "points")) #plot biplot
# pca_climate_scores <- as.data.frame(scores(pca_climate, choices=c(1,2), display=c("sites"))) #extract pca1 and pca2 scores
# pca_climate_scores_lab = as.data.frame(cbind(climate[,1],pca_climate_scores))  #add plot info back
# pca_climate_scores_lab$Population <- ordered(as.factor(pca_climate_scores_lab$Population), levels = c("Norc","NGBER", "Vale", "Susa",  "Roar",
#                                                                                                   "Elko", "Litt"))
# envout<-as.data.frame(scores(pca_climate, choices=c(1,2), display=c("species")))
# summary(pca_climate)
# ggplot(pca_climate_scores_lab, aes(x = PC1, y = PC2))+
#   geom_point(size = 4, aes(colour = Population), alpha = 0.5)+
#   theme(text = element_text(size=18),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
#         axis.title = element_text(size = 15))+
#   geom_segment(data = envout, aes(x = 0, y = 0, xend = PC1, yend = PC2),
#                alpha = 0.5, size = 1, colour = "grey30") +
#   geom_text(data = envout, aes(x = PC1, y = PC2), colour = "grey30",
#             fontface = "bold", label = row.names(envout), size = 5)+
#   xlim(-2, 2.3)+
#   xlab("PC1 (50.7%)")+
#   ylab("PC2 (29.5%)")

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
f_monthly_ppt <- ggplot(weather_simple, aes(x =as.factor(Month), y=PRCP_mm, col=Site))+
  geom_point()+
  geom_line(aes(x = as.numeric(Month)))+
  labs(x = "Month", y = "Monthly Mean Precipitation (mm)")+
  theme_bw()
f_monthly_avgtemp <- ggplot(weather_simple, aes(x =as.factor(Month), y=TAVG_C, col=Site))+
  geom_point()+
  geom_line(aes(x = as.numeric(Month)))+
  labs(x = "Month", y = expression("Monthly Mean Air Temperature "~(degree*C)))+
  theme_bw()

ggarrange(f_monthly_ppt, f_monthly_avgtemp, ncol =1, nrow =2, labels = c("(a)", "(b)"),
          align = "v", common.legend = TRUE, legend = "right")

ggplot(weather_simple, aes(x =Month, y=TMAX_C, col=Site))+
  geom_point()+
  geom_line()
ggplot(weather_simple, aes(x =Month, y=TMIN_C, col=Site))+
  geom_point()+
  geom_line()

NOAA_weather$Site <- ordered(NOAA_weather$Site, levels = c("Norc","NGBER", "NGBER_moderate", "NGBER_severe", "Roar", "Susa","Litt"  ,"Elko" , "Vale"))

f_PPT_30YR <- ggplot(NOAA_weather %>% filter(WaterYear == "30-YR"), aes(x = as.factor(Month), y = PRCP_mm, col = Site))+
  geom_point()+
  geom_line(aes(x = as.numeric(Month)))+
  labs(x = "Month", y = "Monthly Mean Precipitation (mm)")+
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 12))+
  scale_color_manual(name = "Site", 
                       labels = c("Norcross", "NGBER (common garden)", "NGBER 36% reduction", "NGBER 62% reduction", 
                                                 "Roaring Spring", "Susanville", "Little Sahara" ,"Elko", "Vale" ),
                       values = c("#4f4f4f", "#34cfeb", "#ebcf34", "#eb6734", 
                                  "#5f5f5f", "#6f6f6f", "#7e7e7e" , "#8e8e8e", "#9e9e9e"))+
  ylim(0, 110)+
  annotate("text", x = 3.4, y = 106, label = "a) 30 Year Mean", size = 5)

f_PPT_2021 <- ggplot(NOAA_weather %>% filter(WaterYear == 2021) , aes(x = as.factor(Month), y = PRCP_mm, col = Site))+
  geom_point()+
  geom_line(aes(x = as.numeric(Month)))+
  labs(x = "Month", y = "Monthly Precipitation (mm)")+
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 12))+
  scale_color_manual(name = "Site", 
                     labels = c("Norcross", "NGBER (common garden)", "NGBER 36% reduction", "NGBER 62% reduction", 
                                "Roaring Spring", "Susanville", "Little Sahara" ,"Elko", "Vale" ),
                     values = c("#4f4f4f", "#34cfeb", "#ebcf34", "#eb6734", 
                                "#5f5f5f", "#6f6f6f", "#7e7e7e" , "#8e8e8e", "#9e9e9e"))+
  ylim(0, 110)+
  annotate("text", x = 3, y = 106, label = "c) 2020-2021", size = 5)

f_PPT_2022 <- ggplot(NOAA_weather %>% filter(WaterYear == 2022) , aes(x = as.factor(Month), y = PRCP_mm, col = Site))+
  geom_point()+
  geom_line(aes(x = as.numeric(Month)))+
  labs(x = "Month", y = "Monthly Precipitation (mm)")+
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 12))+
  scale_color_manual(name = "Site", 
                     labels = c("Norcross", "NGBER (common garden)", "NGBER 36% reduction", "NGBER 62% reduction", 
                                "Roaring Spring", "Susanville", "Little Sahara" ,"Elko", "Vale" ),
                     values = c("#4f4f4f", "#34cfeb", "#ebcf34", "#eb6734", 
                                "#5f5f5f", "#6f6f6f", "#7e7e7e" , "#8e8e8e", "#9e9e9e"))+
  ylim(0, 110)+
  annotate("text", x = 3, y = 106, label = "e) 2021-2022", size = 5)

f_TAVG_30YR <- ggplot(NOAA_weather %>% filter(WaterYear == "30-YR") %>% filter(Site != "NGBER_moderate") %>% filter(Site != "NGBER_severe"), aes(x = as.factor(Month), y = as.numeric(TAVG_C), col = Site))+
  geom_point()+
  geom_line(aes(x = as.numeric(Month)))+
  labs(x = "Month", y = expression("Monthly Mean Air Temperature " (degree*C)))+
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 12))+
  scale_color_manual(name = "Site", 
                     labels = c("Norcross", "NGBER (common garden)", 
                                "Roaring Spring", "Susanville", "Little Sahara" ,"Elko", "Vale" ),
                     values = c("#4f4f4f", "#34cfeb", 
                                "#5f5f5f", "#6f6f6f", "#7e7e7e" , "#8e8e8e", "#9e9e9e"))+
  ylim(-5, 28)+
  annotate("text", x = 3.4, y = 27, label = "b) 30 Year Mean", size = 5)

f_TAVG_2021 <- ggplot(NOAA_weather %>% filter(WaterYear == 2021) %>% filter(Site != "NGBER_moderate") %>% filter(Site != "NGBER_severe"), aes(x = as.factor(Month), y = as.numeric(TAVG_C), col = Site))+
  geom_point()+
  geom_line(aes(x = as.numeric(Month)))+
  labs(x = "Month", y = expression("Monthly Air Temperature " (degree*C)))+
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 12))+
  scale_color_manual(name = "Site", 
                     labels = c("Norcross", "NGBER (common garden)", 
                                "Roaring Spring", "Susanville", "Little Sahara" ,"Elko", "Vale" ),
                     values = c("#4f4f4f", "#34cfeb", 
                                "#5f5f5f", "#6f6f6f", "#7e7e7e" , "#8e8e8e", "#9e9e9e"))+
  ylim(-5, 28)+
  annotate("text", x = 3, y = 27, label = "d) 2020-2021", size = 5)

f_TAVG_2022 <- ggplot(NOAA_weather %>% filter(WaterYear == 2022) %>% filter(Site != "NGBER_moderate") %>% filter(Site != "NGBER_severe"), aes(x = as.factor(Month), y = as.numeric(TAVG_C), col = Site))+
  geom_point()+
  geom_line(aes(x = as.numeric(Month)))+
  labs(x = "Month", y = expression("Monthly Air Temperature " (degree*C)))+
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 12))+
  scale_color_manual(name = "Site", 
                     labels = c("Norcross", "NGBER (common garden)", 
                                "Roaring Spring", "Susanville", "Little Sahara" ,"Elko", "Vale" ),
                     values = c("#4f4f4f", "#34cfeb", 
                                "#5f5f5f", "#6f6f6f", "#7e7e7e" , "#8e8e8e", "#9e9e9e"))+
  ylim(-5, 28)+
  annotate("text", x = 3, y = 27, label = "f) 2021-2022", size = 5)

ggarrange(f_PPT_30YR, f_TAVG_30YR, f_PPT_2021, f_TAVG_2021, f_PPT_2022, f_TAVG_2022, 
          ncol = 2, nrow = 3, common.legend = TRUE, legend= "right", align = "h")


#PCA of climate variables v2 with NOAA data
#check for multicollinearity
pairs(~Days_below_32+Days_above_90+Tmin_03+Tmax_03+Tmax_07+PPT_annual+Vpdmax_07, climate_all)
climate_all_matrix <- as.matrix(climate_all_redo[,2:7]) 
corr_mat=cor(climate_all_matrix, method = "s")
corrplot(corr_mat, method = 'number') #Tmax_07 and Tmax_03 corr 0.75; Days_below_32 and Tmin_03 corr -0.96 
climate_all_simple <- subset(decostand(climate_all_matrix, "standardize"), select = -c( Days_above_90)) #remove Tmax-03 and Days-below-32
#run PCA on reduced matrix
pca_climate_all = rda(climate_all_simple[1:7,], scale = TRUE) 
biplot(pca_climate_all, display = c("sites", "species"), type = c("text", "points")) #plot biplot
pca_climate_scores_all <- as.data.frame(scores(pca_climate_all, choices=c(1,2), display=c("sites"))) #extract pca1 and pca2 scores
pca_climate_scores_lab_all = as.data.frame(cbind(climate_all_redo[1:7,1],pca_climate_scores_all))  #add plot info back
pca_climate_scores_lab_all$Site <- ordered(as.factor(pca_climate_scores_lab_all$Site), levels = c("Norcross","NGBER","NGBER moderate", "NGBER severe", "Roaring Springs",  "Susanville", "Little Sahara", "Elko","Vale"))
envout_all<-as.data.frame(scores(pca_climate_all, choices=c(1,2), display=c("species")))
summary(pca_climate_all)
#visualize PCA plot
ggplot(pca_climate_scores_lab_all, aes(x = PC1, y = PC2))+
  theme(text = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15))+
  geom_segment(data = envout_all, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               alpha = 0.5, size = 1, colour = "#a9a9a9") +
  geom_text(data = envout_all, aes(x = PC1, y = PC2), colour = "#3C486B",
            fontface = "bold", label = c("Annual Precip", "Late Season Avg Temp","July Max VPD", "Early Season Avg Temp", "Days below 0C"), size = 4)+
  geom_point(size = 7, aes(colour = Site), alpha = 0.5)+
  #scale_color_discrete(name = "Site", labels = c("Norcross", "NGBER (common garden)", "Vale", "Susanville", "Roaring Spring", "Elko", "Little Sahara"))+
  xlim(-1.8, 1.8)+
  ylim(-1.5, 1.7)+
  geom_text(aes(label=Site),vjust = 1.6, size = 4)+
  labs(x=expression(atop("Cool" %<->% "Warm","PC1 (52.0%)")), y = expression(atop("Dry" %<->% "Wet","PC2 (26.2%)")))+
  scale_color_manual( values = c("#01665E", "#FFC0CB", "#5AB4AC" , "#C7EAE5","#F6E8C3","#D8B365", "#8C510A"  ))

#could I add treatment points to pca???
pca_alt <- prcomp(climate_all_simple[1:7,], scale = TRUE)
summary(pca_alt)

autoplot(pca_alt, x = 1, y = 2, data = pca_climate_scores_lab_all, frame = F, loadings = T, loadings.label = T, label = F, col = "Site", size = 2, loadings.colour = "black",
         loadings.label.colour="black",
         loadings.label.repel=TRUE) +
  theme_classic() +
  #stat_ellipse(aes(group = group)) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
    legend.title = element_blank())

df <- cbind(pca_alt$x[,1:2], pca_climate_scores_lab_all[,1]) %>% as.data.frame()
df$PC1 <- as.numeric(df$PC1)/(pca_alt$sdev[1] * sqrt(nrow(pca_climate_scores_lab)))
df$PC2 <- as.numeric(df$PC2)/(pca_alt$sdev[2] * sqrt(nrow(pca_climate_scores_lab)))
df$V3 <- as.factor(df$V3)

ggplot(df, aes(PC1, PC2, color = V3))+
  geom_point(size = 3)+
  geom_text(aes(label=V3),vjust = 1.6, size = 4)



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

ggarrange(bar_1, bar_2, bar_3, bar_4, ncol = 2, nrow = 2, labels = c("(a)", "(b)", "(c)", "(d)"))

f_climate1 <- ggplot(climate_all, aes(x = Days_above_90, y = Tmax_03))+
  geom_point()+
  theme_bw()+
  geom_text(aes(label=Site),vjust = 1.6)+
  ylim(8, 16.5)+
  xlim(9, 60)+
  labs(x = expression("Number of days above "*32~degree*C), y = expression("March Maximum Air Temperature "~(degree*C)))
  
f_climate2 <- ggplot(climate_all, aes(x = Vpdmax_07, y = PPT_annual))+
  geom_point()+
  theme_bw()+
  geom_text(aes(label=Site),vjust = 1.6)+
  ylim(100, 375)+
  xlim(31, 44)+
  labs(x = "July Maximum Vapor Pressure Deficit (kPa)", 
       y = "Annual Precipitation (mm)")

ggarrange(f_climate1, f_climate2, ncol = 2, labels = c("(a)", "(b)"), align = "h", common.legend = TRUE)

#Climate data from seedlot selection tool (USFS)
#run PCA on USFS climate matrix
USFS_climate_matrix <- as.matrix(USFS_climate[,2:12]) 
pca_USFS_climate = rda(USFS_climate_matrix, scale = TRUE) 
biplot(pca_USFS_climate, display = c("sites", "species"), type = c("text", "points")) #plot biplot
pca_USFS_climate_scores <- as.data.frame(scores(pca_USFS_climate, choices=c(1,2), display=c("sites"))) #extract pca1 and pca2 scores
pca_USFS_climate_scores_lab = as.data.frame(cbind(USFS_climate[,1],pca_USFS_climate_scores))  #add plot info back
pca_USFS_climate_scores_lab$Site <- ordered(as.factor(pca_USFS_climate_scores_lab$Site), levels = c("Norc","NGBER", "Vale", "Susa", "Roar", "Elko", "Litt"))
envout_USFS<-as.data.frame(scores(pca_USFS_climate, choices=c(1,2), display=c("species")))
summary(pca_USFS_climate)
#visualize PCA plot
ggplot(pca_USFS_climate_scores_lab, aes(x = PC1, y = PC2))+
  theme(text = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15))+
  geom_segment(data = envout_USFS, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               alpha = 0.5, size = 1, colour = "#a9a9a9") +
  geom_text(data = envout_USFS, aes(x = PC1, y = PC2), colour = "#3C486B",
            fontface = "bold", label = row.names(envout_USFS), size = 4)+
  geom_point(size = 4, aes(colour = Site), alpha = 0.5)+
  scale_color_discrete(name = "Site", labels = c("Norcross", "NGBER (common garden)", "Vale", "Susanville", "Roaring Springs", "Elko", "Little Sahara"))+
  xlim(-2, 2.2)+
  ylim(-2.3, 1.3)+
  geom_text(aes(label=Site),vjust = 1.6, size = 4)+
  labs(x=expression(atop("Dry, warm " %<->% "Wet, cool","PC1 (61.8%)")), y = expression(atop("Longer growing season" %<->% "Shorter growing season","PC2 (22.6%)")))

#Early and Late growing season PCA plots with NOAA data
#run PCA on Early season climate matrix
Early_climate <- Grw_climate %>% filter(Growing_Season == "Early")
Early_climate_matrix <- as.matrix(Early_climate[3:9,3:6]) 
pca_Early_climate = rda(Early_climate_matrix, scale = TRUE) 
biplot(pca_Early_climate, display = c("sites", "species"), type = c("text", "points")) #plot biplot
pca_Early_climate_scores <- as.data.frame(scores(pca_Early_climate, choices=c(1,2), display=c("sites"))) #extract pca1 and pca2 scores
pca_Early_climate_scores_lab = as.data.frame(cbind(Early_climate[3:9,1],pca_Early_climate_scores))  #add plot info back
pca_Early_climate_scores_lab$Site <- ordered(as.factor(pca_Early_climate_scores_lab$Site), levels = c("Norc","NGBER", "Vale", "Susa", "Roar", "Elko", "Litt"))
envout_Early<-as.data.frame(scores(pca_Early_climate, choices=c(1,2), display=c("species")))
summary(pca_Early_climate)
#visualize PCA plot
f_Early <- ggplot(pca_Early_climate_scores_lab, aes(x = PC1, y = PC2))+
  theme(text = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15))+
  geom_segment(data = envout_Early, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               alpha = 0.5, size = 1, colour = "#a9a9a9") +
  geom_text(data = envout_Early, aes(x = PC1, y = PC2), colour = "#3C486B",
            fontface = "bold", label = row.names(envout_Early), size = 4)+
  geom_point(size = 4, aes(colour = Site), alpha = 0.5)+
  scale_color_discrete(name = "Site", labels = c("Norcross", "NGBER (common garden)", "Vale", "Susanville", "Roaring Springs", "Elko", "Little Sahara"))+
  xlim(-2, 2.2)+
  ylim(-2.3, 1.3)+
  geom_text(aes(label=Site),vjust = 1.6, size = 4)+
  labs(x=expression("PC1 (73.0%)"), y = expression("PC2 (16.7%)"),
       title = "Early Growing Season (September-March)")

#run PCA on Late season climate matrix
Late_climate <- Grw_climate %>% filter(Growing_Season == "Late")
Late_climate_matrix <- as.matrix(Late_climate[3:9,3:6]) 
pca_Late_climate = rda(Late_climate_matrix, scale = TRUE) 
biplot(pca_Late_climate, display = c("sites", "species"), type = c("text", "points")) #plot biplot
pca_Late_climate_scores <- as.data.frame(scores(pca_Late_climate, choices=c(1,2), display=c("sites"))) #extract pca1 and pca2 scores
pca_Late_climate_scores_lab = as.data.frame(cbind(Late_climate[3:9,1],pca_Late_climate_scores))  #add plot info back
pca_Late_climate_scores_lab$Site <- ordered(as.factor(pca_Late_climate_scores_lab$Site), levels = c("Norc","NGBER", "Vale", "Susa", "Roar", "Elko", "Litt"))
envout_Late<-as.data.frame(scores(pca_Late_climate, choices=c(1,2), display=c("species")))
summary(pca_Late_climate)
#visualize PCA plot
f_Late <- ggplot(pca_Late_climate_scores_lab, aes(x = PC1, y = PC2))+
  theme(text = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15))+
  geom_segment(data = envout_Late, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               alpha = 0.5, size = 1, colour = "#a9a9a9") +
  geom_text(data = envout_Late, aes(x = PC1, y = PC2), colour = "#3C486B",
            fontface = "bold", label = row.names(envout_Late), size = 4)+
  geom_point(size = 4, aes(colour = Site), alpha = 0.5)+
  scale_color_discrete(name = "Site", labels = c("Norcross", "NGBER (common garden)", "Vale", "Susanville", "Roaring Springs", "Elko", "Little Sahara"))+
  xlim(-1.8, 1.5)+
  ylim(-1.5, 1.8)+
  geom_text(aes(label=Site),vjust = 1.6, size = 4)+
  labs(x=expression("PC1 (69.2%)"), y = expression("PC2 (26.8%)"), 
       title = "Late Growing Season (April-July)")

ggarrange(f_Early, f_Late, common.legend = TRUE)


#Climate data from NOAA and PRISM mixed dataset
#run PCA on mixed climate matrix
climate_mix_matrix <- as.matrix(mix_climate[,2:8]) 
pca_mix_climate = rda(climate_mix_matrix, scale = TRUE) 
biplot(pca_mix_climate, display = c("sites", "species"), type = c("text", "points")) #plot biplot
pca_mix_climate_scores <- as.data.frame(scores(pca_mix_climate, choices=c(1,2), display=c("sites"))) #extract pca1 and pca2 scores
pca_mix_climate_scores_lab = as.data.frame(cbind(mix_climate[,1],pca_mix_climate_scores))  #add plot info back
pca_mix_climate_scores_lab$Site <- ordered(as.factor(pca_mix_climate_scores_lab$Site), levels = c("Norc","NGBER", "Vale", "Susa", "Roar", "Elko", "Litt"))
envout_mix<-as.data.frame(scores(pca_mix_climate, choices=c(1,2), display=c("species")))
summary(pca_mix_climate)
#visualize PCA plot
ggplot(pca_mix_climate_scores_lab, aes(x = PC1, y = PC2))+
  theme(text = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15))+
  geom_segment(data = envout_mix, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               alpha = 0.5, size = 1, colour = "#a9a9a9") +
  geom_text(data = envout_mix, aes(x = PC1, y = PC2), colour = "#3C486B",
            fontface = "bold", label = row.names(envout_mix), size = 4)+
  geom_point(size = 4, aes(colour = Site), alpha = 0.5)+
  scale_color_discrete(name = "Site", labels = c("Norcross", "NGBER (common garden)", "Vale", "Susanville", "Roaring Springs", "Elko", "Little Sahara"))+
  #xlim(-2, 2.2)+
  #ylim(-2.3, 1.3)+
  geom_text(aes(label=Site),vjust = 1.6, size = 4)+
  labs(x=expression(atop("Dry, warm " %<->% "Wet, cool","PC1 (48.1%)")), y = expression(atop("Longer growing season" %<->% "Shorter growing season","PC2 (26.8%)")))

