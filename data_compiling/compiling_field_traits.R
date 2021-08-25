#set data pathway
datpath <- "C:/Users/Lina/Dropbox/Academics/Projects/Perennial Grasses Eastern Oregon/Data"

library(tidyverse)

#read in field trait data
field_traits <- read_csv(paste(datpath, "/Traits/Field_traits_May_2019.csv", sep=""))
