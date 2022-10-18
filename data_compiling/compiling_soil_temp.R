library(tidyverse)

#read in soil moisture data
STMar2021_1 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/1temp.csv", sep=""))%>%
  mutate(ID = 1, Depth = "5 cm", Treatment = "80% cover")
STMar2021_2 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/2temp.csv", sep=""))%>%
  mutate(ID = 2, Depth = "15 cm", Treatment = "80% cover")
STMar2021_3 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/3temp.csv", sep=""))%>%
  mutate(ID = 3, Depth = "5 cm", Treatment = "50% cover")
STMar2021_4 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/4temp.csv", sep=""))%>%
  mutate(ID = 4, Depth = "15 cm", Treatment = "50% cover")
STMar2021_5 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/5temp.csv", sep=""))%>%
  mutate(ID = 5, Depth = "5 cm", Treatment = "ambient")
STMar2021_6 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/6temp.csv", sep=""))%>%
  mutate(ID = 6, Depth = "15 cm", Treatment = "ambient")
STMar2021_7 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/7temp.csv", sep=""))%>%
  mutate(ID = 7, Depth = "5 cm", Treatment = "ambient")
STMar2021_8 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/8temp.csv", sep=""))%>%
  mutate(ID = 8, Depth = "15 cm", Treatment = "ambient")
STMar2021_9 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/9temp.csv", sep=""))%>%
  mutate(ID = 9, Depth = "5 cm", Treatment = "80% cover")
STMar2021_10 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/10temp.csv", sep=""))%>%
  mutate(ID = 10, Depth = "15 cm", Treatment = "80% cover")
STMar2021_11 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/11temp.csv", sep=""))%>%
  mutate(ID = 11, Depth = "5 cm", Treatment = "50% cover")
STMar2021_12 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/12temp.csv", sep=""))%>%
  mutate(ID = 12, Depth = "15 cm", Treatment = "50% cover")
STMar2021_13 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/13temp.csv", sep=""))%>%
  mutate(ID = 13, Depth = "5 cm", Treatment = "80% cover")
STMar2021_14 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/14temp.csv", sep=""))%>%
  mutate(ID = 14, Depth = "15 cm", Treatment = "80% cover")
STMar2021_15 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/15temp.csv", sep=""))%>%
  mutate(ID = 15, Depth = "5 cm", Treatment = "50% cover")
STMar2021_16 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/16temp.csv", sep=""))%>%
  mutate(ID = 16, Depth = "15 cm", Treatment = "50% cover")
STMar2021_17 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/17temp.csv", sep=""))%>%
  mutate(ID = 17, Depth = "5 cm", Treatment = "ambient")
STMar2021_18 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/18temp.csv", sep=""))%>%
  mutate(ID = 18, Depth = "15 cm", Treatment = "ambient")
STMar2021_19 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/19temp.csv", sep=""))%>%
  mutate(ID = 19, Depth = "5 cm", Treatment = "50% cover")
STMar2021_20 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/20temp.csv", sep=""))%>%
  mutate(ID = 20, Depth = "15 cm", Treatment = "50% cover")
STMar2021_21 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/21temp.csv", sep=""))%>%
  mutate(ID = 21, Depth = "5 cm", Treatment = "ambient")
STMar2021_22 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/22temp.csv", sep=""))%>%
  mutate(ID = 22, Depth = "15 cm", Treatment = "ambient")
STMar2021_23 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/23temp.csv", sep=""))%>%
  mutate(ID = 23, Depth = "5 cm", Treatment = "80% cover")
STMar2021_24 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/24temp.csv", sep=""))%>%
  mutate(ID = 24, Depth = "15 cm", Treatment = "80% cover")

STMay2021_1 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/1temp.csv", sep=""))%>%
  mutate(ID = 1, Depth = "5 cm", Treatment = "80% cover")
STMay2021_2 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/2temp.csv", sep=""))%>%
  mutate(ID = 2, Depth = "15 cm", Treatment = "80% cover")
STMay2021_3 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/3temp.csv", sep=""))%>%
  mutate(ID = 3, Depth = "5 cm", Treatment = "50% cover")
STMay2021_4 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/4temp.csv", sep=""))%>%
  mutate(ID = 4, Depth = "15 cm", Treatment = "50% cover")
STMay2021_5 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/5temp.csv", sep=""))%>%
  mutate(ID = 5, Depth = "5 cm", Treatment = "ambient")
STMay2021_6 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/6temp.csv", sep=""))%>%
  mutate(ID = 6, Depth = "15 cm", Treatment = "ambient")
STMay2021_7 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/7temp.csv", sep=""))%>%
  mutate(ID = 7, Depth = "5 cm", Treatment = "ambient")
STMay2021_8 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/8temp.csv", sep=""))%>%
  mutate(ID = 8, Depth = "15 cm", Treatment = "ambient")
STMay2021_9 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/9temp.csv", sep=""))%>%
  mutate(ID = 9, Depth = "5 cm", Treatment = "80% cover")
STMay2021_10 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/10temp.csv", sep=""))%>%
  mutate(ID = 10, Depth = "15 cm", Treatment = "80% cover")
STMay2021_11 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/11temp.csv", sep=""))%>%
  mutate(ID = 11, Depth = "5 cm", Treatment = "50% cover")
STMay2021_12 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/12temp.csv", sep=""))%>%
  mutate(ID = 12, Depth = "15 cm", Treatment = "50% cover")
STMay2021_13 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/13temp.csv", sep=""))%>%
  mutate(ID = 13, Depth = "5 cm", Treatment = "80% cover")
STMay2021_14 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/14temp.csv", sep=""))%>%
  mutate(ID = 14, Depth = "15 cm", Treatment = "80% cover")
STMay2021_15 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/15temp.csv", sep=""))%>%
  mutate(ID = 15, Depth = "5 cm", Treatment = "50% cover")
STMay2021_16 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/16temp.csv", sep=""))%>%
  mutate(ID = 16, Depth = "15 cm", Treatment = "50% cover")
STMay2021_17 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/17temp.csv", sep=""))%>%
  mutate(ID = 17, Depth = "5 cm", Treatment = "ambient")
STMay2021_18 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/18temp.csv", sep=""))%>%
  mutate(ID = 18, Depth = "15 cm", Treatment = "ambient")
STMay2021_19 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/19temp.csv", sep=""))%>%
  mutate(ID = 19, Depth = "5 cm", Treatment = "50% cover")
STMay2021_20 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/20temp.csv", sep=""))%>%
  mutate(ID = 20, Depth = "15 cm", Treatment = "50% cover")
STMay2021_21 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/21temp.csv", sep=""))%>%
  mutate(ID = 21, Depth = "5 cm", Treatment = "ambient")
STMay2021_22 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/22temp.csv", sep=""))%>%
  mutate(ID = 22, Depth = "15 cm", Treatment = "ambient")
STMay2021_23 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/23temp.csv", sep=""))%>%
  mutate(ID = 23, Depth = "5 cm", Treatment = "80% cover")
STMay2021_24 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/24temp.csv", sep=""))%>%
  mutate(ID = 24, Depth = "15 cm", Treatment = "80% cover")

STSep2021_1 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/1temp.csv", sep=""))%>%
  mutate(ID = 1, Depth = "5 cm", Treatment = "80% cover")
STSep2021_2 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/2temp.csv", sep=""))%>%
  mutate(ID = 2, Depth = "15 cm", Treatment = "80% cover")
STSep2021_3 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/3temp.csv", sep=""))%>%
  mutate(ID = 3, Depth = "5 cm", Treatment = "50% cover")
STSep2021_4 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/4temp.csv", sep=""))%>%
  mutate(ID = 4, Depth = "15 cm", Treatment = "50% cover")
STSep2021_5 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/5temp.csv", sep=""))%>%
  mutate(ID = 5, Depth = "5 cm", Treatment = "ambient")
STSep2021_6 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/6temp.csv", sep=""))%>%
  mutate(ID = 6, Depth = "15 cm", Treatment = "ambient")
STSep2021_7 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/7temp.csv", sep=""))%>%
  mutate(ID = 7, Depth = "5 cm", Treatment = "ambient")
STSep2021_8 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/8temp.csv", sep=""))%>%
  mutate(ID = 8, Depth = "15 cm", Treatment = "ambient")
STSep2021_9 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/9temp.csv", sep=""))%>%
  mutate(ID = 9, Depth = "5 cm", Treatment = "80% cover")
STSep2021_10 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/10temp.csv", sep=""))%>%
  mutate(ID = 10, Depth = "15 cm", Treatment = "80% cover")
STSep2021_11 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/11temp.csv", sep=""))%>%
  mutate(ID = 11, Depth = "5 cm", Treatment = "50% cover")
STSep2021_12 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/12temp.csv", sep=""))%>%
  mutate(ID = 12, Depth = "15 cm", Treatment = "50% cover")
STSep2021_13 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/13temp.csv", sep=""))%>%
  mutate(ID = 13, Depth = "5 cm", Treatment = "80% cover")
STSep2021_14 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/14temp.csv", sep=""))%>%
  mutate(ID = 14, Depth = "15 cm", Treatment = "80% cover")
STSep2021_15 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/15temp.csv", sep=""))%>%
  mutate(ID = 15, Depth = "5 cm", Treatment = "50% cover")
STSep2021_16 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/16temp.csv", sep=""))%>%
  mutate(ID = 16, Depth = "15 cm", Treatment = "50% cover")
STSep2021_17 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/17temp.csv", sep=""))%>%
  mutate(ID = 17, Depth = "5 cm", Treatment = "ambient")
STSep2021_18 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/18temp.csv", sep=""))%>%
  mutate(ID = 18, Depth = "15 cm", Treatment = "ambient")
STSep2021_19 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/19temp.csv", sep=""))%>%
  mutate(ID = 19, Depth = "5 cm", Treatment = "50% cover")
STSep2021_20 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/20temp.csv", sep=""))%>%
  mutate(ID = 20, Depth = "15 cm", Treatment = "50% cover")
STSep2021_21 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/21temp.csv", sep=""))%>%
  mutate(ID = 21, Depth = "5 cm", Treatment = "ambient")
STSep2021_22 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/22temp.csv", sep=""))%>%
  mutate(ID = 22, Depth = "15 cm", Treatment = "ambient")
STSep2021_23 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/23temp.csv", sep=""))%>%
  mutate(ID = 23, Depth = "5 cm", Treatment = "80% cover")
STSep2021_24 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/24temp.csv", sep=""))%>%
  mutate(ID = 24, Depth = "15 cm", Treatment = "80% cover")

STFeb2022_1 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/1temp.csv", sep=""))%>%
  mutate(ID = 1, Depth = "5 cm", Treatment = "80% cover")
STFeb2022_2 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/2temp.csv", sep=""))%>%
  mutate(ID = 2, Depth = "15 cm", Treatment = "80% cover")
STFeb2022_3 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/3temp.csv", sep=""))%>%
  mutate(ID = 3, Depth = "5 cm", Treatment = "50% cover")
STFeb2022_4 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/4temp.csv", sep=""))%>%
  mutate(ID = 4, Depth = "15 cm", Treatment = "50% cover")
STFeb2022_5 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/5temp.csv", sep=""))%>%
  mutate(ID = 5, Depth = "5 cm", Treatment = "ambient")
STFeb2022_6 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/6temp.csv", sep=""))%>%
  mutate(ID = 6, Depth = "15 cm", Treatment = "ambient")
STFeb2022_7 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/7temp.csv", sep=""))%>%
  mutate(ID = 7, Depth = "5 cm", Treatment = "ambient")
STFeb2022_8 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/8temp.csv", sep=""))%>%
  mutate(ID = 8, Depth = "15 cm", Treatment = "ambient")
STFeb2022_9 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/9temp.csv", sep=""))%>%
  mutate(ID = 9, Depth = "5 cm", Treatment = "80% cover")
STFeb2022_10 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/10temp.csv", sep=""))%>%
  mutate(ID = 10, Depth = "15 cm", Treatment = "80% cover")
STFeb2022_11 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/11temp.csv", sep=""))%>%
  mutate(ID = 11, Depth = "5 cm", Treatment = "50% cover")
STFeb2022_12 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/12temp.csv", sep=""))%>%
  mutate(ID = 12, Depth = "15 cm", Treatment = "50% cover")
STFeb2022_13 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/13temp.csv", sep=""))%>%
  mutate(ID = 13, Depth = "5 cm", Treatment = "80% cover")
STFeb2022_14 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/14temp.csv", sep=""))%>%
  mutate(ID = 14, Depth = "15 cm", Treatment = "80% cover")
STFeb2022_15 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/15temp.csv", sep=""))%>%
  mutate(ID = 15, Depth = "5 cm", Treatment = "50% cover")
STFeb2022_16 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/16temp.csv", sep=""))%>%
  mutate(ID = 16, Depth = "15 cm", Treatment = "50% cover")
STFeb2022_17 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/17temp.csv", sep=""))%>%
  mutate(ID = 17, Depth = "5 cm", Treatment = "ambient")
STFeb2022_18 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/18temp.csv", sep=""))%>%
  mutate(ID = 18, Depth = "15 cm", Treatment = "ambient")
STFeb2022_19 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/19temp.csv", sep=""))%>%
  mutate(ID = 19, Depth = "5 cm", Treatment = "50% cover")
STFeb2022_20 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/20temp.csv", sep=""))%>%
  mutate(ID = 20, Depth = "15 cm", Treatment = "50% cover")
STFeb2022_21 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/21temp.csv", sep=""))%>%
  mutate(ID = 21, Depth = "5 cm", Treatment = "ambient")
STFeb2022_22 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/22temp.csv", sep=""))%>%
  mutate(ID = 22, Depth = "15 cm", Treatment = "ambient")
STFeb2022_23 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/23temp.csv", sep=""))%>%
  mutate(ID = 23, Depth = "5 cm", Treatment = "80% cover")
STFeb2022_24 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/24temp.csv", sep=""))%>%
  mutate(ID = 24, Depth = "15 cm", Treatment = "80% cover")

STJul2022_1 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/1temp.csv", sep=""))%>%
  mutate(ID = 1, Depth = "5 cm", Treatment = "80% cover")
STJul2022_2 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/2temp.csv", sep=""))%>%
  mutate(ID = 2, Depth = "15 cm", Treatment = "80% cover")
STJul2022_3 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/3temp.csv", sep=""))%>%
  mutate(ID = 3, Depth = "5 cm", Treatment = "50% cover")
STJul2022_4 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/4temp.csv", sep=""))%>%
  mutate(ID = 4, Depth = "15 cm", Treatment = "50% cover")
STJul2022_5 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/5temp.csv", sep=""))%>%
  mutate(ID = 5, Depth = "5 cm", Treatment = "ambient")
STJul2022_6 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/6temp.csv", sep=""))%>%
  mutate(ID = 6, Depth = "15 cm", Treatment = "ambient")
STJul2022_7 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/7temp.csv", sep=""))%>%
  mutate(ID = 7, Depth = "5 cm", Treatment = "ambient")
STJul2022_8 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/8temp.csv", sep=""))%>%
  mutate(ID = 8, Depth = "15 cm", Treatment = "ambient")
STJul2022_9 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/9temp.csv", sep=""))%>%
  mutate(ID = 9, Depth = "5 cm", Treatment = "80% cover")
STJul2022_10 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/10temp.csv", sep=""))%>%
  mutate(ID = 10, Depth = "15 cm", Treatment = "80% cover")
STJul2022_11 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/11temp.csv", sep=""))%>%
  mutate(ID = 11, Depth = "5 cm", Treatment = "50% cover")
STJul2022_12 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/12temp.csv", sep=""))%>%
  mutate(ID = 12, Depth = "15 cm", Treatment = "50% cover")
STJul2022_13 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/13temp.csv", sep=""))%>%
  mutate(ID = 13, Depth = "5 cm", Treatment = "80% cover")
STJul2022_14 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/14temp.csv", sep=""))%>%
  mutate(ID = 14, Depth = "15 cm", Treatment = "80% cover")
STJul2022_15 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/15temp.csv", sep=""))%>%
  mutate(ID = 15, Depth = "5 cm", Treatment = "50% cover")
STJul2022_16 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/16temp.csv", sep=""))%>%
  mutate(ID = 16, Depth = "15 cm", Treatment = "50% cover")
STJul2022_17 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/17temp.csv", sep=""))%>%
  mutate(ID = 17, Depth = "5 cm", Treatment = "ambient")
STJul2022_18 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/18temp.csv", sep=""))%>%
  mutate(ID = 18, Depth = "15 cm", Treatment = "ambient")
STJul2022_19 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/19temp.csv", sep=""))%>%
  mutate(ID = 19, Depth = "5 cm", Treatment = "50% cover")
STJul2022_20 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/20temp.csv", sep=""))%>%
  mutate(ID = 20, Depth = "15 cm", Treatment = "50% cover")
STJul2022_21 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/21temp.csv", sep=""))%>%
  mutate(ID = 21, Depth = "5 cm", Treatment = "ambient")
STJul2022_22 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/22temp.csv", sep=""))%>%
  mutate(ID = 22, Depth = "15 cm", Treatment = "ambient")
STJul2022_23 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/23temp.csv", sep=""))%>%
  mutate(ID = 23, Depth = "5 cm", Treatment = "80% cover")
STJul2022_24 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/24temp.csv", sep=""))%>%
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
  full_join(., STMay2021_24) %>%
  full_join(., STSep2021_1) %>%
  full_join(., STSep2021_2) %>%
  full_join(., STSep2021_3) %>%
  full_join(., STSep2021_4) %>%
  full_join(., STSep2021_5) %>%
  full_join(., STSep2021_6) %>%
  full_join(., STSep2021_7) %>%
  full_join(., STSep2021_8) %>%
  full_join(., STSep2021_9) %>%
  full_join(., STSep2021_10) %>%
  full_join(., STSep2021_11) %>%
  full_join(., STSep2021_12) %>%
  full_join(., STSep2021_13) %>%
  full_join(., STSep2021_14) %>%
  full_join(., STSep2021_15) %>%
  full_join(., STSep2021_16) %>%
  full_join(., STSep2021_17) %>%
  full_join(., STSep2021_18) %>%
  full_join(., STSep2021_19) %>%
  full_join(., STSep2021_20) %>%
  full_join(., STSep2021_21) %>%
  full_join(., STSep2021_22) %>%
  full_join(., STSep2021_23) %>%
  full_join(., STSep2021_24) %>%
  full_join(., STFeb2022_1) %>%
  full_join(., STFeb2022_2) %>%
  full_join(., STFeb2022_3) %>%
  full_join(., STFeb2022_4) %>%
  full_join(., STFeb2022_5) %>%
  full_join(., STFeb2022_6) %>%
  full_join(., STFeb2022_7) %>%
  full_join(., STFeb2022_8) %>%
  full_join(., STFeb2022_9) %>%
  full_join(., STFeb2022_10) %>%
  full_join(., STFeb2022_11) %>%
  full_join(., STFeb2022_12) %>%
  full_join(., STFeb2022_13) %>%
  full_join(., STFeb2022_14) %>%
  full_join(., STFeb2022_15) %>%
  full_join(., STFeb2022_16) %>%
  full_join(., STFeb2022_17) %>%
  full_join(., STFeb2022_18) %>%
  full_join(., STFeb2022_19) %>%
  full_join(., STFeb2022_20) %>%
  full_join(., STFeb2022_21) %>%
  full_join(., STFeb2022_22) %>%
  full_join(., STFeb2022_23) %>%
  full_join(., STFeb2022_24) %>%
  full_join(., STJul2022_1) %>%
  full_join(., STJul2022_2) %>%
  full_join(., STJul2022_3) %>%
  full_join(., STJul2022_4) %>%
  full_join(., STJul2022_5) %>%
  full_join(., STJul2022_6) %>%
  full_join(., STJul2022_7) %>%
  full_join(., STJul2022_8) %>%
  full_join(., STJul2022_9) %>%
  full_join(., STJul2022_10) %>%
  full_join(., STJul2022_11) %>%
  full_join(., STJul2022_12) %>%
  full_join(., STJul2022_13) %>%
  full_join(., STJul2022_14) %>%
  full_join(., STJul2022_15) %>%
  full_join(., STJul2022_16) %>%
  full_join(., STJul2022_17) %>%
  full_join(., STJul2022_18) %>%
  full_join(., STJul2022_19) %>%
  full_join(., STJul2022_20) %>%
  full_join(., STJul2022_21) %>%
  full_join(., STJul2022_22) %>%
  full_join(., STJul2022_23) %>%
  full_join(., STJul2022_24) 

rm(STMar2021_1, STMar2021_2, STMar2021_3, STMar2021_4, STMar2021_5, 
   STMar2021_6, STMar2021_7, STMar2021_8, STMar2021_9, STMar2021_10,
   STMar2021_11, STMar2021_12, STMar2021_13, STMar2021_14, STMar2021_15, 
   STMar2021_16, STMar2021_17, STMar2021_18, STMar2021_19, STMar2021_20,
   STMar2021_21, STMar2021_22, STMar2021_23, STMar2021_24)

rm(STMay2021_1, STMay2021_2, STMay2021_3, STMay2021_4, STMay2021_5, 
   STMay2021_6, STMay2021_7, STMay2021_8, STMay2021_9, STMay2021_10,
   STMay2021_11, STMay2021_12, STMay2021_13, STMay2021_14, STMay2021_15, 
   STMay2021_16, STMay2021_17, STMay2021_18, STMay2021_19, STMay2021_20,
   STMay2021_21, STMay2021_22, STMay2021_23, STMay2021_24)

rm(STSep2021_1, STSep2021_2, STSep2021_3, STSep2021_4, STSep2021_5, 
   STSep2021_6, STSep2021_7, STSep2021_8, STSep2021_9, STSep2021_10,
   STSep2021_11, STSep2021_12, STSep2021_13, STSep2021_14, STSep2021_15, 
   STSep2021_16, STSep2021_17, STSep2021_18, STSep2021_19, STSep2021_20,
   STSep2021_21, STSep2021_22, STSep2021_23, STSep2021_24)

rm(STFeb2022_1, STFeb2022_2, STFeb2022_3, STFeb2022_4, STFeb2022_5, 
   STFeb2022_6, STFeb2022_7, STFeb2022_8, STFeb2022_9, STFeb2022_10,
   STFeb2022_11, STFeb2022_12, STFeb2022_13, STFeb2022_14, STFeb2022_15, 
   STFeb2022_16, STFeb2022_17, STFeb2022_18, STFeb2022_19, STFeb2022_20,
   STFeb2022_21, STFeb2022_22, STFeb2022_23, STFeb2022_24)

rm(STJul2022_1, STJul2022_2, STJul2022_3, STJul2022_4, STJul2022_5, 
   STJul2022_6, STJul2022_7, STJul2022_8, STJul2022_9, STJul2022_10,
   STJul2022_11, STJul2022_12, STJul2022_13, STJul2022_14, STJul2022_15, 
   STJul2022_16, STJul2022_17, STJul2022_18, STJul2022_19, STJul2022_20,
   STJul2022_21, STJul2022_22, STJul2022_23, STJul2022_24)
