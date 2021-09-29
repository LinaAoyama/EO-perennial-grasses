### Packages
library(ggplot2)
library(tidyverse)
library(lubridate)
library(multcomp) #tukey

### Data Import
source("data_compiling/compiling_soil_temp.R")

### Tidy data ###
#Specify the Time column by month, date, year, and time
soil_temp <- soil_temp_full %>% 
  mutate(Date.Time = mdy_hm(soil_temp$Time)) %>%
  separate(Date.Time, into = c('date', "time"), sep =' ', remove = FALSE)

#Daily max, min, and mean
soil_temp_summary <- soil_temp %>%
  group_by(date, Depth, Treatment) %>%
  summarize(maxtemp = as.numeric(max(Value)), mintemp = as.numeric(min(Value)), meantemp = as.numeric(mean(Value)))

#Reorder factor levels
soil_temp_summary$Depth <- factor(soil_temp_summary$Depth, levels = c("5 cm", "15 cm"))
soil_temp_summary$Treatment <- factor(soil_temp_summary$Treatment, levels = c("ambient", "50% cover", "80% cover"))

#Set date as class Date
soil_temp_summary$date <- as.Date(soil_temp_summary$date)

### Visualize data
#Timeseries
ggplot(soil_temp_summary, aes(date, meantemp)) +
  geom_line(aes(color = Treatment)) +
  #facet_wrap(~Depth, ncol = 1)+
  ylab(bquote(Daily~Mean~Soil~Temperature~(C^o)))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_x_date(date_breaks = "month", date_labels = "%b")+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        #legend.position = "none",
        axis.title = element_text(size = 14))+
  scale_color_manual(name = "Treatment", values = c("#58CCED", "#F39C12", "#D35400"))



