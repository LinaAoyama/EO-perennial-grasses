### Packages
library(ggplot2)
library(tidyverse)
library(lubridate)
library(multcomp) #tukey
library(ggpubr)

### Data Import
source("data_compiling/compiling_soil_temp.R")

### Tidy data ###
#Specify the Time column by month, date, year, and time
soil_temp <- soil_temp_full %>% 
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
levels(soil_temp_summary$Treatment) <- c("ambient", "moderate", "severe")
#Set date as class Date
soil_temp_summary$date <- as.Date(soil_temp_summary$date)

### Visualize summarized data
#daily min
min2021 <- ggplot(soil_temp_summary %>% filter(Depth == "5 cm"), aes(date, mintemp)) +
              geom_line(aes(color = Treatment), size = 1) +
              #facet_wrap(~Depth, ncol = 1)+
              ylab(bquote(Daily~Min~Soil~Temp~(C^o)))+
              xlab("")+
              #geom_hline(yintercept = 12, linetype ="dashed")+
              #geom_hline(yintercept = 15, linetype ="dashed")+
              geom_hline(yintercept = 0, linetype ="dashed")+
              scale_x_date(date_breaks = "month", date_labels = "%b", limits = c(as.Date("2021-02-01"), as.Date("2021-07-15")))+
              theme(text = element_text(size=15),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    #legend.position = "none",
                    axis.title = element_text(size = 13))+
              scale_color_manual(name = "Treatment", values = c("#34cfeb", "#ebcf34", "#eb6734"))
min2022 <- ggplot(soil_temp_summary %>% filter(Depth == "5 cm"), aes(date, mintemp)) +
            geom_line(aes(color = Treatment), size = 1) +
            #facet_wrap(~Depth, ncol = 1)+
            ylab("")+
            xlab("")+
            #geom_hline(yintercept = 12, linetype ="dashed")+
            #geom_hline(yintercept = 15, linetype ="dashed")+
            geom_hline(yintercept = 0, linetype ="dashed")+
            scale_x_date(date_breaks = "month", date_labels = "%b", limits = c(as.Date("2022-02-01"), as.Date("2022-07-15")))+
            theme(text = element_text(size=15),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  #legend.position = "none",
                  axis.title = element_text(size = 13))+
            scale_color_manual(name = "Treatment", values = c("#34cfeb", "#ebcf34", "#eb6734"))

#daily max
max2021 <- ggplot(soil_temp_summary %>% filter(Depth == "5 cm"), aes(date, maxtemp)) +
  geom_line(aes(color = Treatment), size = 1) +
  #facet_wrap(~Depth, ncol = 1)+
  ylab(bquote(Daily~Max~Soil~Temp~(C^o)))+
  xlab("")+
  #geom_hline(yintercept = 12, linetype ="dashed")+
  #geom_hline(yintercept = 15, linetype ="dashed")+
  geom_hline(yintercept = 0, linetype ="dashed")+
  scale_x_date(date_breaks = "month", date_labels = "%b", limits = c(as.Date("2021-02-01"), as.Date("2021-07-15")))+
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        #legend.position = "none",
        axis.title = element_text(size = 13))+
  scale_color_manual(name = "Treatment", values = c("#34cfeb", "#ebcf34", "#eb6734"))
max2022 <- ggplot(soil_temp_summary %>% filter(Depth == "5 cm"), aes(date, maxtemp)) +
  geom_line(aes(color = Treatment), size = 1) +
  #facet_wrap(~Depth, ncol = 1)+
  ylab("")+
  xlab("")+
  #geom_hline(yintercept = 12, linetype ="dashed")+
  #geom_hline(yintercept = 15, linetype ="dashed")+
  geom_hline(yintercept = 0, linetype ="dashed")+
  scale_x_date(date_breaks = "month", date_labels = "%b", limits = c(as.Date("2022-02-01"), as.Date("2022-07-15")))+
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        #legend.position = "none",
        axis.title = element_text(size = 13))+
  scale_color_manual(name = "Treatment", values = c("#34cfeb", "#ebcf34", "#eb6734"))

# #daily mean
# mean2021 <- ggplot(soil_temp_summary, aes(date, meantemp)) +
#   geom_line(aes(color = Treatment), size = 1) +
#   facet_wrap(~Depth, ncol = 1)+
#   ylab(bquote(Daily~Mean~Soil~Temperature~(C^o)))+
#   xlab(bquote(2021))+
#   #geom_hline(yintercept = 12, linetype ="dashed")+
#   #geom_hline(yintercept = 15, linetype ="dashed")+
#   geom_hline(yintercept = 0, linetype ="solid")+
#   scale_x_date(date_breaks = "month", date_labels = "%b", limits = c(as.Date("2021-02-01"), as.Date("2021-07-15")))+
#   theme(text = element_text(size=16),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         #legend.position = "none",
#         axis.title = element_text(size = 14))+
#   scale_color_manual(name = "Treatment", values = c("#F1C40F", "#F39C12", "#D35400"))
# mean2022 <- ggplot(soil_temp_summary, aes(date, meantemp)) +
#   geom_line(aes(color = Treatment), size = 1) +
#   facet_wrap(~Depth, ncol = 1)+
#   ylab("")+
#   xlab(bquote(2022))+
#   #geom_hline(yintercept = 12, linetype ="dashed")+
#   #geom_hline(yintercept = 15, linetype ="dashed")+
#   geom_hline(yintercept = 0, linetype ="solid")+
#   scale_x_date(date_breaks = "month", date_labels = "%b", limits = c(as.Date("2022-02-01"), as.Date("2022-07-15")))+
#   theme(text = element_text(size=15),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         #legend.position = "none",
#         axis.title = element_text(size = 12))+
#   scale_color_manual(name = "Treatment", values = c("#F1C40F", "#F39C12", "#D35400"))
# ggarrange(mean2021, mean2022, common.legend = TRUE)

### FIG2: COMBINE SOIL MOISTURE AND TEMP FIG
source("data_analysis/analysis_soil_moisture_data.R")

soilfig_output <- ggarrange(mois2021, mois2022, max2021, max2022, min2021, min2022, common.legend = TRUE, ncol = 2, nrow = 3, legend = "right")


