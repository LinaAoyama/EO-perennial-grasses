### Packages
library(ggplot2)
library(tidyverse)
library(lubridate)
library(multcomp) #tukey
library(ggpubr)

### Data Import
source("data_compiling/compiling_air_temp.R")

### Tidy data ###
#Specify the Time column by month, date, year, and time
air_temp <- air_temp_full %>% 
  mutate(Date_Time = lubridate::mdy_hm(soil_temp_full$Time)) %>%
  separate(Date_Time, into = c('date', "time"), sep =' ', remove = FALSE)

### Visualize raw data
ggplot(soil_temp%>%filter(Depth == "5 cm"), aes(x = Date_Time, y = Value ))+
  geom_line(aes(color = Treatment))+
  facet_wrap(~ID)
ggplot(soil_temp%>%filter(Depth == "15 cm"), aes(x = Date_Time, y = Value ))+
  geom_line(aes(color = Treatment))+
  facet_wrap(~ID)

### Summarize Daily max, min, and mean
soil_temp_summary <- soil_temp %>%
  group_by(date, Depth, Treatment) %>%
  summarize(maxtemp = as.numeric(max(Value)), mintemp = as.numeric(min(Value)), meantemp = as.numeric(mean(Value)))

#Reorder factor levels
soil_temp_summary$Depth <- factor(soil_temp_summary$Depth, levels = c("5 cm", "15 cm"))
soil_temp_summary$Treatment <- factor(soil_temp_summary$Treatment, levels = c("ambient", "50% cover", "80% cover"))

#Set date as class Date
soil_temp_summary$date <- as.Date(soil_temp_summary$date)

### Visualize summarized data
#2021
#daily min
ggplot(soil_temp_summary, aes(date, mintemp)) +
  geom_line(aes(color = Treatment), size = 1) +
  facet_wrap(~Depth, ncol = 1)+
  ylab(bquote(Daily~Minimum~Soil~Temperature~(C^o)))+
  xlab(bquote(2021))+
  geom_hline(yintercept = 12, linetype ="dashed")+
  geom_hline(yintercept = 15, linetype ="dashed")+
  geom_hline(yintercept = 0, linetype ="solid")+
  scale_x_date(date_breaks = "month", date_labels = "%b", limits = c(as.Date("2021-02-01"), as.Date("2021-07-15")))+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        #legend.position = "none",
        axis.title = element_text(size = 14))+
  scale_color_manual(name = "Treatment", values = c("#34cfeb", "#ebcf34", "#eb6734"))
