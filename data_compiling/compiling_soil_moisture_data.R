#set data pathway
datpath <- "C:/Users/Lina/Dropbox/Academics/Projects/Perennial Grasses Eastern Oregon/Data"

library(tidyverse)

#read in soil moisture data
Oct2020 <- read_csv(paste(datpath, "/Soil_moisture/clean_data/z6-05212 27Oct20-1415.csv", sep=""))
Mar2021 <- read_csv(paste(datpath, "/Soil_moisture/clean_data/z6-05212 23Mar21-1327.csv", sep=""))
May2021 <- read_csv(paste(datpath, "/Soil_moisture/clean_data/z6-05212 07May21-0952.csv", sep=""))
Jul2021 <- read_csv(paste(datpath, "/Soil_moisture/clean_data/z6-05212 15Jul21-0630.csv", sep=""))

#remove the first two rows 
Oct2020 <- Oct2020[-(1:2),]
Mar2021 <- Mar2021[-(1:2),]
May2021 <- May2021[-(1:2),]
Jul2021 <- Jul2021[-(1:2),]
soil_mois <- full_join(Oct2020, Mar2021) %>%
  full_join(., May2021) %>%
  full_join(., Jul2021)
