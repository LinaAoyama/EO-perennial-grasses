library(tidyverse)

#read in climate data
climate <- read_csv(paste(datpath, "/Weather station/PRISM_data.csv", sep=""))
rain <- read_csv(paste(datpath, "/Weather station/Riley_weather_station_monthly_ppt.csv", sep = ""))
weather <- read_csv(paste(datpath, "/Weather station/Weather x treatment.csv", sep = ""))
climate_all <- read_csv(paste(datpath, "/Weather station/Combined_weather_treatment.csv", sep = ""))
USFS_climate <- read_csv(paste(datpath, "/Weather station/Seedlot_climate_data.csv", sep = ""))
Grw_climate <- read_csv(paste(datpath, "/Weather station/Early_late_GS_NOAA_weather_station_all_sites.csv", sep = ""))
mix_climate <- read_csv(paste(datpath, "/Weather station/NOAA_PRISM_mixed.csv", sep = ""))
