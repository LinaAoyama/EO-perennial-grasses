#set data pathway
datpath <- "C:/Users/Lina/Dropbox/Academics/Projects/Perennial Grasses Eastern Oregon/Data"

library(tidyverse)

#read in air temp data
# ATMar2021_25 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/25temp.csv", sep=""))%>%
#   mutate(ID = 25, Treatment = "severe")
# ATMar2021_26 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/26temp.csv", sep=""))%>%
#   mutate(ID = 26, Treatment = "moderate")
# ATMar2021_27 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/27temp.csv", sep=""))%>%
#   mutate(ID = 27, Treatment = "ambient")
# ATMar2021_28 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/28temp.csv", sep=""))%>%
#   mutate(ID = 27, Treatment = "ambient")
# ATMar2021_29 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/29temp.csv", sep=""))%>%
#   mutate(ID = 29, Treatment = "severe")
# ATMar2021_30 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/30temp.csv", sep=""))%>%
#   mutate(ID = 30, Treatment = "moderate")
# ATMar2021_31 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/31temp.csv", sep=""))%>%
#   mutate(ID = 31, Treatment = "severe")
# ATMar2021_32 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/32temp.csv", sep=""))%>%
#   mutate(ID = 32, Treatment = "moderate")
# ATMar2021_33 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/33temp.csv", sep=""))%>%
#   mutate(ID = 33, Treatment = "ambient")
# ATMar2021_34 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/34temp.csv", sep=""))%>%
#   mutate(ID = 34, Treatment = "moderate")
# ATMar2021_35 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/35temp.csv", sep=""))%>%
#   mutate(ID = 35, Treatment = "ambient")
# ATMar2021_36 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/March_2021/36temp.csv", sep=""))%>%
#   mutate(ID = 36, Treatment = "severe")

ATMay2021_25 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/25temp.csv", sep=""))%>%
  mutate(ID = 25, Treatment = "severe")
ATMay2021_26 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/26temp.csv", sep=""))%>%
  mutate(ID = 26, Treatment = "moderate")
ATMay2021_27 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/27temp.csv", sep=""))%>%
  mutate(ID = 27, Treatment = "ambient")
ATMay2021_28 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/28temp.csv", sep=""))%>%
  mutate(ID = 27, Treatment = "ambient")
ATMay2021_29 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/29temp.csv", sep=""))%>%
  mutate(ID = 29, Treatment = "severe")
ATMay2021_30 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/30temp.csv", sep=""))%>%
  mutate(ID = 30, Treatment = "moderate")
ATMay2021_31 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/31temp.csv", sep=""))%>%
  mutate(ID = 31, Treatment = "severe")
ATMay2021_32 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/32temp.csv", sep=""))%>%
  mutate(ID = 32, Treatment = "moderate")
ATMay2021_33 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/33temp.csv", sep=""))%>%
  mutate(ID = 33, Treatment = "ambient")
ATMay2021_34 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/34temp.csv", sep=""))%>%
  mutate(ID = 34, Treatment = "moderate")
ATMay2021_35 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/35temp.csv", sep=""))%>%
  mutate(ID = 35, Treatment = "ambient")
ATMay2021_36 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/May_2021/36temp.csv", sep=""))%>%
  mutate(ID = 36, Treatment = "severe")

ATSep2021_25 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/25temp.csv", sep=""))%>%
  mutate(ID = 25, Treatment = "severe")
ATSep2021_26 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/26temp.csv", sep=""))%>%
  mutate(ID = 26, Treatment = "moderate")
ATSep2021_27 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/27temp.csv", sep=""))%>%
  mutate(ID = 27, Treatment = "ambient")
ATSep2021_28 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/28temp.csv", sep=""))%>%
  mutate(ID = 27, Treatment = "ambient")
ATSep2021_29 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/29temp.csv", sep=""))%>%
  mutate(ID = 29, Treatment = "severe")
ATSep2021_30 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/30temp.csv", sep=""))%>%
  mutate(ID = 30, Treatment = "moderate")
ATSep2021_31 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/31temp.csv", sep=""))%>%
  mutate(ID = 31, Treatment = "severe")
ATSep2021_32 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/32temp.csv", sep=""))%>%
  mutate(ID = 32, Treatment = "moderate")
ATSep2021_33 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/33temp.csv", sep=""))%>%
  mutate(ID = 33, Treatment = "ambient")
ATSep2021_34 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/34temp.csv", sep=""))%>%
  mutate(ID = 34, Treatment = "moderate")
ATSep2021_35 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/35temp.csv", sep=""))%>%
  mutate(ID = 35, Treatment = "ambient")
ATSep2021_36 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Sept_2021/36temp.csv", sep=""))%>%
  mutate(ID = 36, Treatment = "severe")

ATFeb2022_25 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/25temp.csv", sep=""))%>%
  mutate(ID = 25, Treatment = "severe")
ATFeb2022_26 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/26temp.csv", sep=""))%>%
  mutate(ID = 26, Treatment = "moderate")
ATFeb2022_27 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/27temp.csv", sep=""))%>%
  mutate(ID = 27, Treatment = "ambient")
ATFeb2022_28 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/28temp.csv", sep=""))%>%
  mutate(ID = 27, Treatment = "ambient")
ATFeb2022_29 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/29temp.csv", sep=""))%>%
  mutate(ID = 29, Treatment = "severe")
ATFeb2022_30 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/30temp.csv", sep=""))%>%
  mutate(ID = 30, Treatment = "moderate")
ATFeb2022_31 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/31temp.csv", sep=""))%>%
  mutate(ID = 31, Treatment = "severe")
ATFeb2022_32 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/32temp.csv", sep=""))%>%
  mutate(ID = 32, Treatment = "moderate")
ATFeb2022_33 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/33temp.csv", sep=""))%>%
  mutate(ID = 33, Treatment = "ambient")
ATFeb2022_34 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/34temp.csv", sep=""))%>%
  mutate(ID = 34, Treatment = "moderate")
ATFeb2022_35 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/35temp.csv", sep=""))%>%
  mutate(ID = 35, Treatment = "ambient")
ATFeb2022_36 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/Feb_2022/36temp.csv", sep=""))%>%
  mutate(ID = 36, Treatment = "severe")

ATJul2022_25 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/25temp.csv", sep=""))%>%
  mutate(ID = 25, Treatment = "severe")
ATJul2022_26 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/26temp.csv", sep=""))%>%
  mutate(ID = 26, Treatment = "moderate")
ATJul2022_27 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/27temp.csv", sep=""))%>%
  mutate(ID = 27, Treatment = "ambient")
ATJul2022_28 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/28temp.csv", sep=""))%>%
  mutate(ID = 27, Treatment = "ambient")
ATJul2022_29 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/29temp.csv", sep=""))%>%
  mutate(ID = 29, Treatment = "severe")
ATJul2022_30 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/30temp.csv", sep=""))%>%
  mutate(ID = 30, Treatment = "moderate")
ATJul2022_31 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/31temp.csv", sep=""))%>%
  mutate(ID = 31, Treatment = "severe")
ATJul2022_32 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/32temp.csv", sep=""))%>%
  mutate(ID = 32, Treatment = "moderate")
ATJul2022_33 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/33temp.csv", sep=""))%>%
  mutate(ID = 33, Treatment = "ambient")
ATJul2022_34 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/34temp.csv", sep=""))%>%
  mutate(ID = 34, Treatment = "moderate")
ATJul2022_35 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/35temp.csv", sep=""))%>%
  mutate(ID = 35, Treatment = "ambient")
ATJul2022_36 <- read_csv(paste(datpath, "/Soil_temp/cleaned_data/July_2022/36temp.csv", sep=""))%>%
  mutate(ID = 36, Treatment = "severe")

air_temp_full <- full_join(ATMay2021_25, ATMay2021_26) %>%
  full_join(., ATMay2021_27) %>%
  full_join(., ATMay2021_28) %>%
  full_join(., ATMay2021_29) %>%
  full_join(., ATMay2021_30) %>%
  full_join(., ATMay2021_31) %>%
  full_join(., ATMay2021_32) %>%
  full_join(., ATMay2021_33) %>%
  full_join(., ATMay2021_34) %>%
  full_join(., ATMay2021_35) %>%
  full_join(., ATMay2021_36) %>%
  full_join(., ATSep2021_25) %>%
  full_join(., ATSep2021_26) %>%
  full_join(., ATSep2021_27) %>%
  full_join(., ATSep2021_28) %>%
  full_join(., ATSep2021_29) %>%
  full_join(., ATSep2021_30) %>%
  full_join(., ATSep2021_31) %>%
  full_join(., ATSep2021_32) %>%
  full_join(., ATSep2021_33) %>%
  full_join(., ATSep2021_34) %>%
  full_join(., ATSep2021_35) %>%
  full_join(., ATSep2021_36) %>%
  full_join(., ATFeb2022_25) %>%
  full_join(., ATFeb2022_26) %>%
  full_join(., ATFeb2022_27) %>%
  full_join(., ATFeb2022_28) %>%
  full_join(., ATFeb2022_29) %>%
  full_join(., ATFeb2022_30) %>%
  full_join(., ATFeb2022_31) %>%
  full_join(., ATFeb2022_32) %>%
  full_join(., ATFeb2022_33) %>%
  full_join(., ATFeb2022_34) %>%
  full_join(., ATFeb2022_35) %>%
  full_join(., ATFeb2022_36) %>%  
  full_join(., ATJul2022_25) %>%
  full_join(., ATJul2022_26) %>%
  full_join(., ATJul2022_27) %>%
  full_join(., ATJul2022_28) %>%
  full_join(., ATJul2022_29) %>%
  full_join(., ATJul2022_30) %>%
  full_join(., ATJul2022_31) %>%
  full_join(., ATJul2022_32) %>%
  full_join(., ATJul2022_33) %>%
  full_join(., ATJul2022_34) %>%
  full_join(., ATJul2022_35) %>%
  full_join(., ATJul2022_36)   

rm(ATMar2021_25, ATMar2021_26, ATMar2021_27, ATMar2021_28, ATMar2021_29, 
   ATMar2021_30, ATMar2021_31, ATMar2021_32, ATMar2021_33, ATMar2021_34, ATMar2021_35, ATMar2021_36)
rm(ATMay2021_25, ATMay2021_26, ATMay2021_27, ATMay2021_28, ATMay2021_29, 
   ATMay2021_30, ATMay2021_31, ATMay2021_32, ATMay2021_33, ATMay2021_34, ATMay2021_35, ATMay2021_36)
rm(ATSep2021_25, ATSep2021_26, ATSep2021_27, ATSep2021_28, ATSep2021_29, 
   ATSep2021_30, ATSep2021_31, ATSep2021_32, ATSep2021_33, ATSep2021_34, ATSep2021_35, ATSep2021_36)
rm(ATFeb2022_25, ATFeb2022_26, ATFeb2022_27, ATFeb2022_28, ATFeb2022_29, 
   ATFeb2022_30, ATFeb2022_31, ATFeb2022_32, ATFeb2022_33, ATFeb2022_34, ATFeb2022_35, ATFeb2022_36)
rm(ATJul2022_25, ATJul2022_26, ATJul2022_27, ATJul2022_28, ATJul2022_29, 
   ATJul2022_30, ATJul2022_31, ATJul2022_32, ATJul2022_33, ATJul2022_34, ATJul2022_35, ATJul2022_36)
