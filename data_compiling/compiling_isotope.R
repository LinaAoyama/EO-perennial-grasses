#set data pathway
datpath <- "C:/Users/Lina/Dropbox/Academics/Projects/Perennial Grasses Eastern Oregon/Data"

library(tidyverse)

#read in stable isotope data
isotope_data <- read_csv(paste(datpath, "/Stable Isotopes/cleaned_data/stable_isotope_2021_2022.csv", sep=""))
