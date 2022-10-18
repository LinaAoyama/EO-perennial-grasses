library(tidyverse)

#read in climate data
climate <- read_csv(paste(datpath, "/Weather station/PRISM_data.csv", sep=""))
rain <- read_csv(paste(datpath, "/Weather station/Riley_weather_station_monthly_ppt.csv", sep = ""))
