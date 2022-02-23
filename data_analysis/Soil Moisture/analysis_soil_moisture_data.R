### Packages
library(ggplot2)
require(lubridate)
library(tidyverse)

### Data Import ###
source("data_compiling/compiling_soil_moisture_data.R")

### Tidy data ###
#Make a column for time 
names(soil_mois)[1] <- "Time"

#Tidy data into a long dataframe
soil_mois_long <- soil_mois %>%
  pivot_longer(c("Port 1", "Port 2", "Port 3", "Port 4", "Port 5", "Port 6"), names_to = "Port", values_to = "VWC" ) %>%
  #select(Time, Port, VWC) %>%
  mutate(Depth = ifelse(Port %in% c("Port 1", "Port 3", "Port 5"), "5 cm", "15 cm")) %>% #Make a column for soil depth
  mutate(Treatment = ifelse(Port %in% c("Port 1", "Port 2"), "50% cover", ifelse(Port %in% c("Port 3", "Port 4"), "80% cover", "ambient")))

#Reorder factor levels
soil_mois_long$Depth <- factor(soil_mois_long$Depth, levels = c("5 cm", "15 cm"))
soil_mois_long$Treatment <- factor(soil_mois_long$Treatment, levels = c("ambient", "50% cover", "80% cover"))

#Specify the Time column by month, date, year, and time
soil_mois_long$Time <- mdy_hm(soil_mois_long$Time)

### Visualize data
#Timeseries
ggplot(soil_mois_long, aes(Time, as.numeric(VWC))) +
  geom_line(aes(color = Treatment)) +
  facet_wrap(~Depth, ncol = 1)+
  ylim(0, 0.35)+
  ylab(bquote(Soil~Volumetric~Water~Content~(m^3/m^3)))+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        #legend.position = "none",
        axis.title = element_text(size = 14))+
  scale_color_manual(name = "Treatment", values = c("#34cfeb", "#ebcf34", "#eb6734"))
#averages
ggplot(soil_mois_long, aes(Treatment, as.numeric(VWC)))+
  geom_boxplot()+
  facet_wrap(~Depth, ncol = 1)
summary(aov(VWC~Treatment, soil_mois_long%>%filter(Depth == "5 cm")))
summary(aov(VWC~Treatment, soil_mois_long%>%filter(Depth == "15 cm")))
