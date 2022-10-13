library(tidyverse)

#read in common garden trait data
biomass <- read_csv(paste(datpath, "/Traits/Common garden traits/cleaned_data/Biomass_July_2022.csv", sep=""))
ht <- read_csv(paste(datpath, "/Traits/Common garden traits/cleaned_data/Ht_2021_2022.csv", sep = ""))
