library(tidyverse)

#read in soil moisture data
STMar2021_1 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/1temp.csv", sep=""))%>%
  mutate(ID = 1, Depth = "5 cm", Treatment = "80% cover")
STMar2021_2 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/2temp.csv", sep=""))%>%
  mutate(ID = 2, Depth = "15 cm", Treatment = "80% cover")
STMar2021_3 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/3temp.csv", sep=""))%>%
  mutate(ID = 3, Depth = "5 cm", Treatment = "50% cover")
STMar2021_4 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/4temp.csv", sep=""))%>%
  mutate(ID = 4, Depth = "15 cm", Treatment = "50% cover")
STMar2021_5 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/5temp.csv", sep=""))%>%
  mutate(ID = 5, Depth = "5 cm", Treatment = "ambient")
STMar2021_6 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/6temp.csv", sep=""))%>%
  mutate(ID = 6, Depth = "15 cm", Treatment = "ambient")
STMar2021_7 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/7temp.csv", sep=""))%>%
  mutate(ID = 7, Depth = "5 cm", Treatment = "ambient")
STMar2021_8 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/8temp.csv", sep=""))%>%
  mutate(ID = 8, Depth = "15 cm", Treatment = "ambient")
STMar2021_9 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/9temp.csv", sep=""))%>%
  mutate(ID = 9, Depth = "5 cm", Treatment = "80% cover")
STMar2021_10 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/10temp.csv", sep=""))%>%
  mutate(ID = 10, Depth = "15 cm", Treatment = "80% cover")
STMar2021_11 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/11temp.csv", sep=""))%>%
  mutate(ID = 11, Depth = "5 cm", Treatment = "50% cover")
STMar2021_12 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/12temp.csv", sep=""))%>%
  mutate(ID = 12, Depth = "15 cm", Treatment = "50% cover")
STMar2021_13 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/13temp.csv", sep=""))%>%
  mutate(ID = 13, Depth = "5 cm", Treatment = "80% cover")
STMar2021_14 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/14temp.csv", sep=""))%>%
  mutate(ID = 14, Depth = "15 cm", Treatment = "80% cover")
STMar2021_15 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/15temp.csv", sep=""))%>%
  mutate(ID = 15, Depth = "5 cm", Treatment = "50% cover")
STMar2021_16 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/16temp.csv", sep=""))%>%
  mutate(ID = 16, Depth = "15 cm", Treatment = "50% cover")
STMar2021_17 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/17temp.csv", sep=""))%>%
  mutate(ID = 17, Depth = "5 cm", Treatment = "ambient")
STMar2021_18 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/18temp.csv", sep=""))%>%
  mutate(ID = 18, Depth = "15 cm", Treatment = "ambient")
STMar2021_19 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/19temp.csv", sep=""))%>%
  mutate(ID = 19, Depth = "5 cm", Treatment = "50% cover")
STMar2021_20 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/20temp.csv", sep=""))%>%
  mutate(ID = 20, Depth = "15 cm", Treatment = "50% cover")
STMar2021_21 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/21temp.csv", sep=""))%>%
  mutate(ID = 21, Depth = "5 cm", Treatment = "ambient")
STMar2021_22 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/22temp.csv", sep=""))%>%
  mutate(ID = 22, Depth = "15 cm", Treatment = "ambient")
STMar2021_23 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/23temp.csv", sep=""))%>%
  mutate(ID = 23, Depth = "5 cm", Treatment = "80% cover")
STMar2021_24 <- read_csv(paste(datpath, "/Soil_temp/cleaned/March_2021/24temp.csv", sep=""))%>%
  mutate(ID = 24, Depth = "15 cm", Treatment = "80% cover")

STMay2021_1 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/1temp.csv", sep=""))%>%
  mutate(ID = 1, Depth = "5 cm", Treatment = "80% cover")
STMay2021_2 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/2temp.csv", sep=""))%>%
  mutate(ID = 2, Depth = "15 cm", Treatment = "80% cover")
STMay2021_3 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/3temp.csv", sep=""))%>%
  mutate(ID = 3, Depth = "5 cm", Treatment = "50% cover")
STMay2021_4 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/4temp.csv", sep=""))%>%
  mutate(ID = 4, Depth = "15 cm", Treatment = "50% cover")
STMay2021_5 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/5temp.csv", sep=""))%>%
  mutate(ID = 5, Depth = "5 cm", Treatment = "ambient")
STMay2021_6 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/6temp.csv", sep=""))%>%
  mutate(ID = 6, Depth = "15 cm", Treatment = "ambient")
STMay2021_7 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/7temp.csv", sep=""))%>%
  mutate(ID = 7, Depth = "5 cm", Treatment = "ambient")
STMay2021_8 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/8temp.csv", sep=""))%>%
  mutate(ID = 8, Depth = "15 cm", Treatment = "ambient")
STMay2021_9 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/9temp.csv", sep=""))%>%
  mutate(ID = 9, Depth = "5 cm", Treatment = "80% cover")
STMay2021_10 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/10temp.csv", sep=""))%>%
  mutate(ID = 10, Depth = "15 cm", Treatment = "80% cover")
STMay2021_11 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/11temp.csv", sep=""))%>%
  mutate(ID = 11, Depth = "5 cm", Treatment = "50% cover")
STMay2021_12 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/12temp.csv", sep=""))%>%
  mutate(ID = 12, Depth = "15 cm", Treatment = "50% cover")
STMay2021_13 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/13temp.csv", sep=""))%>%
  mutate(ID = 13, Depth = "5 cm", Treatment = "80% cover")
STMay2021_14 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/14temp.csv", sep=""))%>%
  mutate(ID = 14, Depth = "15 cm", Treatment = "80% cover")
STMay2021_15 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/15temp.csv", sep=""))%>%
  mutate(ID = 15, Depth = "5 cm", Treatment = "50% cover")
STMay2021_16 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/16temp.csv", sep=""))%>%
  mutate(ID = 16, Depth = "15 cm", Treatment = "50% cover")
STMay2021_17 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/17temp.csv", sep=""))%>%
  mutate(ID = 17, Depth = "5 cm", Treatment = "ambient")
STMay2021_18 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/18temp.csv", sep=""))%>%
  mutate(ID = 18, Depth = "15 cm", Treatment = "ambient")
STMay2021_19 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/19temp.csv", sep=""))%>%
  mutate(ID = 19, Depth = "5 cm", Treatment = "50% cover")
STMay2021_20 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/20temp.csv", sep=""))%>%
  mutate(ID = 20, Depth = "15 cm", Treatment = "50% cover")
STMay2021_21 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/21temp.csv", sep=""))%>%
  mutate(ID = 21, Depth = "5 cm", Treatment = "ambient")
STMay2021_22 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/22temp.csv", sep=""))%>%
  mutate(ID = 22, Depth = "15 cm", Treatment = "ambient")
STMay2021_23 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/23temp.csv", sep=""))%>%
  mutate(ID = 23, Depth = "5 cm", Treatment = "80% cover")
STMay2021_24 <- read_csv(paste(datpath, "/Soil_temp/cleaned/May_2021/24temp.csv", sep=""))%>%
  mutate(ID = 24, Depth = "15 cm", Treatment = "80% cover")

soil_temp_full <- full_join(STMar2021_1, STMar2021_2) %>%
  full_join(., STMar2021_3) %>%
  full_join(., STMar2021_4) %>%
  full_join(., STMar2021_5) %>%
  full_join(., STMar2021_6) %>%
  full_join(., STMar2021_7) %>%
  full_join(., STMar2021_8) %>%
  full_join(., STMar2021_9) %>%
  full_join(., STMar2021_10) %>%
  full_join(., STMar2021_11) %>%
  full_join(., STMar2021_12) %>%
  full_join(., STMar2021_13) %>%
  full_join(., STMar2021_14) %>%
  full_join(., STMar2021_15) %>%
  full_join(., STMar2021_16) %>%
  full_join(., STMar2021_17) %>%
  full_join(., STMar2021_18) %>%
  full_join(., STMar2021_19) %>%
  full_join(., STMar2021_20) %>%
  full_join(., STMar2021_21) %>%
  full_join(., STMar2021_22) %>%
  full_join(., STMar2021_23) %>%
  full_join(., STMar2021_24) %>%
  full_join(., STMay2021_1) %>%
  full_join(., STMay2021_2) %>%
  full_join(., STMay2021_3) %>%
  full_join(., STMay2021_4) %>%
  full_join(., STMay2021_5) %>%
  full_join(., STMay2021_6) %>%
  full_join(., STMay2021_7) %>%
  full_join(., STMay2021_8) %>%
  full_join(., STMay2021_9) %>%
  full_join(., STMay2021_10) %>%
  full_join(., STMay2021_11) %>%
  full_join(., STMay2021_12) %>%
  full_join(., STMay2021_13) %>%
  full_join(., STMay2021_14) %>%
  full_join(., STMay2021_15) %>%
  full_join(., STMay2021_16) %>%
  full_join(., STMay2021_17) %>%
  full_join(., STMay2021_18) %>%
  full_join(., STMay2021_19) %>%
  full_join(., STMay2021_20) %>%
  full_join(., STMay2021_21) %>%
  full_join(., STMay2021_22) %>%
  full_join(., STMay2021_23) %>%
  full_join(., STMay2021_24) 
