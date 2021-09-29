library(tidyverse)

#read in climate data
climate <- read_csv(paste(datpath, "/Weather station/PRISM_data.csv", sep=""))
