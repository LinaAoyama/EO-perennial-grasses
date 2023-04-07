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
  mutate(Date_Time = lubridate::mdy_hm(air_temp_full$Time)) %>%
  separate(Date_Time, into = c('date', "time"), sep =' ', remove = FALSE)

### Visualize raw data
ggplot(air_temp, aes(x = Date_Time, y = Value ))+
  geom_line(aes(color = Treatment))+
  facet_wrap(~ID)

### Summarize Daily max, min, and mean
air_temp_summary <- air_temp %>%
  group_by(date, Treatment) %>%
  summarize(maxtemp = as.numeric(max(Value)), mintemp = as.numeric(min(Value)), meantemp = as.numeric(mean(Value)))

#Reorder factor levels
air_temp_summary$Treatment <- factor(air_temp_summary$Treatment, levels = c("ambient", "moderate", "severe"))

#Set date as class Date
air_temp_summary$date <- as.Date(air_temp_summary$date)

### Visualize summarized daily max, min, mean data

#daily min
ggplot(air_temp_summary, aes(date, mintemp)) +
  geom_point(aes(color = Treatment), size = 1) +
  ylab(bquote(Daily~Minimum~Air~Temperature~(C^o)))+
  xlab(bquote(2021-2022))+
  #geom_hline(yintercept = 12, linetype ="dashed")+
  #geom_hline(yintercept = 15, linetype ="dashed")+
  geom_hline(yintercept = 0, linetype ="solid")+
  scale_x_date(date_breaks = "month", date_labels = "%b", limits = c(as.Date("2021-03-23"), as.Date("2022-07-13")))+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        #legend.position = "none",
        axis.title = element_text(size = 14))+
  scale_color_manual(name = "Treatment", values = c("#34cfeb", "#ebcf34", "#eb6734"))

#daily max
ggplot(air_temp_summary, aes(date, maxtemp)) +
  geom_point(aes(color = Treatment), size = 1) +
  ylab(bquote(Daily~Maximum~Air~Temperature~(C^o)))+
  geom_hline(yintercept = 12, linetype ="dashed")+
  geom_hline(yintercept = 15, linetype ="dashed")+
  geom_hline(yintercept = 0, linetype ="solid")+
  xlab(bquote(2021-2022))+
  scale_x_date(date_breaks = "month", date_labels = "%b", limits = c(as.Date("2021-03-23"), as.Date("2022-07-13")))+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        #legend.position = "none",
        axis.title = element_text(size = 14))+
  scale_color_manual(name = "Treatment", values = c("#34cfeb", "#ebcf34", "#eb6734"))

#daily mean
ggplot(air_temp_summary, aes(date, meantemp)) +
  geom_point(aes(color = Treatment), size = 1) +
  ylab(bquote(Daily~Mean~Air~Temperature~(C^o)))+
  geom_hline(yintercept = 0, linetype ="solid")+
  #geom_hline(yintercept = 12, linetype ="dashed")+
  #geom_hline(yintercept = 15, linetype ="dashed")+
  xlab(bquote(2021-2022))+
  scale_x_date(date_breaks = "month", date_labels = "%b", limits = c(as.Date("2021-03-23"), as.Date("2022-07-13")))+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        #legend.position = "none",
        axis.title = element_text(size = 14))+
  scale_color_manual(name = "Treatment", values = c("#34cfeb", "#ebcf34", "#eb6734"))

###Summarize monthly max, min, mean
air_temp_monthly_summary <- air_temp%>%
  mutate(month = lubridate::month(ymd(air_temp$date)), year = lubridate::year(ymd(air_temp$date))) %>%
  group_by(date, month, year, Treatment) %>%
  summarize(maxtemp = as.numeric(max(Value)), mintemp = as.numeric(min(Value)), meantemp = as.numeric(mean(Value))) %>%
  group_by(month, year, Treatment) %>%
  summarize(maxtemp = mean(maxtemp), mintemp = mean(mintemp), meantemp = mean(meantemp))

#write.csv(air_temp_monthly_summary, "C:/Users/Lina/Dropbox/Academics/Projects/Perennial Grasses Eastern Oregon/Data/Weather station/Treatment_monthly_air_temp.csv", row.names=FALSE)

#Reorder factor levels
air_temp_monthly_summary$Treatment <- factor(air_temp_monthly_summary$Treatment, levels = c("ambient", "moderate", "severe"))

#Visualize monthly air temp
#monthly min
ggplot(air_temp_monthly_summary, aes(month, mintemp))+
  geom_point(aes(color = Treatment), size = 1) +
  ylab(bquote(Monthly~Minimum~Air~Temperature~(C^o)))+
  xlab(bquote(2021-2022))+
  #geom_hline(yintercept = 12, linetype ="dashed")+
  #geom_hline(yintercept = 15, linetype ="dashed")+
  geom_hline(yintercept = 0, linetype ="solid")+
  #scale_x_date(date_breaks = "month", date_labels = "%b", limits = c(as.Date("2021-03-23"), as.Date("2022-07-13")))+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        #legend.position = "none",
        axis.title = element_text(size = 14))+
  scale_color_manual(name = "Treatment", values = c("#34cfeb", "#ebcf34", "#eb6734"))

#monthly max
ggplot(air_temp_monthly_summary, aes(month, maxtemp)) +
  geom_point(aes(color = Treatment), size = 1) +
  ylab(bquote(Monthly~Maximum~Air~Temperature~(C^o)))+
  #geom_hline(yintercept = 12, linetype ="dashed")+
  #geom_hline(yintercept = 15, linetype ="dashed")+
  geom_hline(yintercept = 0, linetype ="solid")+
  xlab(bquote(2021-2022))+
  #scale_x_date(date_breaks = "month", date_labels = "%b", limits = c(as.Date("2021-03-23"), as.Date("2022-07-13")))+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        #legend.position = "none",
        axis.title = element_text(size = 14))+
  scale_color_manual(name = "Treatment", values = c("#34cfeb", "#ebcf34", "#eb6734"))

