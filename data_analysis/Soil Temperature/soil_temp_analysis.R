### Packages
library(ggplot2)
library(tidyverse)
library(lubridate)
library(multcomp) #tukey

### Data Import
source("data_compiling/compiling_soil_temp.R")

### Tidy data ###
#Reorder factor levels
soil_temp$Depth <- factor(soil_temp$Depth, levels = c("5 cm", "15 cm"))
soil_temp$Treatment <- factor(soil_temp$Treatment, levels = c("ambient", "50% cover", "80% cover"))

#Specify the Time column by month, date, year, and time
soil_temp$Time <- mdy_hm(soil_temp$Time)

### Visualize data
#Timeseries
ggplot(soil_temp, aes(Time, as.numeric(Value))) +
  geom_line(aes(color = Treatment)) +
  facet_wrap(Depth~Treatment, ncol = 3)+
  ylab(bquote(Soil~Temperature~(C^o)))+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        #legend.position = "none",
        axis.title = element_text(size = 14))+
  scale_color_manual(name = "Treatment", values = c("#34cfeb", "#ebcf34", "#eb6734"))
#averages
ggplot(soil_temp, aes(Treatment, as.numeric(Value)))+
  geom_boxplot()+
  facet_wrap(~Depth, ncol = 1)
TukeyHSD(aov(Value~Treatment, soil_temp%>%filter(Depth == "5 cm")))
TukeyHSD(aov(Value~Treatment, soil_temp%>%filter(Depth == "15 cm")))
