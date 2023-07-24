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
  mutate(Treatment = ifelse(Port %in% c("Port 1", "Port 2"), "50% cover", ifelse(Port %in% c("Port 3", "Port 4"), 
                                                                                 "80% cover", "ambient"))) %>%
  mutate(Date_Time = lubridate::mdy_hm(soil_mois_long$Time))%>%
  separate(Date_Time, into = c('date', "time"), sep =' ', remove = FALSE)

### Summarize Daily max, min, and mean
soil_mois_summary <- soil_mois_long %>%
  group_by(date, Depth, Treatment) %>%
  summarize(maxmois = as.numeric(max(VWC)), minmois = as.numeric(min(VWC)), meanmois = as.numeric(mean(as.numeric(VWC))))

#Reorder factor levels
soil_mois_summary$Depth <- factor(soil_mois_summary$Depth, levels = c("5 cm", "15 cm"))
soil_mois_summary$Treatment <- factor(soil_mois_summary$Treatment, levels = c("ambient", "50% cover", "80% cover"))
levels(soil_mois_summary$Treatment) <- c("ambient", "moderate", "severe")

#Set date as class Date
soil_mois_summary$date <- as.Date(soil_mois_summary$date)

### Visualize data
#Top 5 cm 2021
mois2021 <- ggplot(soil_mois_summary %>% filter(Depth == "5 cm"), aes(date, maxmois)) +
                geom_line(aes(color = Treatment), size = 1) +
                #facet_wrap(~Depth, ncol = 1)+
                ylim(0, 0.35)+
                ylab(bquote(Mean~Soil~VWC~(m^3/m^3)))+
                xlab("")+
                ggtitle("2021")+
                theme(text = element_text(size=15),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "black"),
                      legend.position = "top",
                      axis.title = element_text(size = 13),
                      plot.title = element_text(hjust = 0.5))+
                scale_color_manual(name = "Treatment", values = c("#34cfeb", "#ebcf34", "#eb6734"))+
                scale_x_date(date_breaks = "month", date_labels = "%b", limits = c(as.Date("2021-02-01"), as.Date("2021-07-15")))

#Top 5 cm 2022
mois2022 <- ggplot(soil_mois_summary %>% filter(Depth == "5 cm"), aes(date, maxmois)) +
              geom_line(aes(color = Treatment), size = 1) +
              #facet_wrap(~Depth, ncol = 1)+
              ylim(0, 0.35)+
              xlab("")+
              ylab("")+
              ggtitle("2022")+
              theme(text = element_text(size=15),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    legend.position = "top",
                    axis.title = element_text(size = 13),
                    plot.title = element_text(hjust = 0.5))+
              scale_color_manual(name = "Treatment", values = c("#34cfeb", "#ebcf34", "#eb6734"))+
              scale_x_date(date_breaks = "month", date_labels = "%b", limits = c(as.Date("2022-02-01"), as.Date("2022-07-15")))
