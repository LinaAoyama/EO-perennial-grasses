#set data pathway
datpath <- "C:/Users/Lina/Dropbox/Academics/Projects/Perennial Grasses Eastern Oregon/Data"

library(tidyverse)

#read in soil moisture data
soil_mois <- read_csv(paste(datpath, "/Soil_moisture/z6-05212 25Feb20-1208.csv", sep=""))
