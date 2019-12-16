#First, load data from compiling_soil_moisture_data. We'll start with data visualization. 
library(ggplot2)

#Take out row 1 and 2 and rename column 1 
soil_mois1 <- soil_mois[-(1:2),]
soil_mois1$Time <- soil_mois1$`z6-05212` 

#Gather columns to consolidate the data
soil_mois1 <- soil_mois1 %>%
  gather("Port 1", "Port 2", "Port 3", "Port 4", "Port 5", "Port 6", key = "Port", value = "SM" )

#Make a column for soil depth
soil_mois1 <- soil_mois1 %>%
  mutate(Depth = ifelse(Port %in% c("Port 1", "Port 3", "Port 5"), "5 cm", "15 cm"))
soil_mois1$Depth <- factor(soil_mois1$Depth, levels = c("5 cm", "15 cm"))

require(lubridate)
#Specify the Time column by month, date, year, and time
soil_mois1$Time <- mdy_hm(soil_mois1$Time)

#Soil moisture vs time
ggplot(soil_mois1, aes(Time, as.numeric(SM))) +
  geom_point(aes(color = Port)) +
  facet_wrap(~Depth, ncol = 1)+
  ylim(0, 0.35)+
  ylab(bquote('Soil Moisture ('~m^3/m^3 ~")")) +
  scale_color_discrete(name = "Treatment and depth", labels = c("80% cover, 5cm", "80% cover, 15cm",
                                                                    "50% cover, 5cm", "50% cover, 15cm",
                                                                    "ambient, 5cm", "ambient, 15 cm"))
