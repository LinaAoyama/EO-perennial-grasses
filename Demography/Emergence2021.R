#Setwd 
datpath <- "C:/Users/Lina/Dropbox/Academics/Projects/Perennial Grasses Eastern Oregon/Data"
#Load data
library(tidyverse)
emergence <- read_csv(paste(datpath, "/Demography/Emergence_04132021.csv", sep=""))
library(ggplot2)

se<-function(x){
  sd(x)/sqrt(length(x))
} # this is a function for calculating standard error


#Seedling emergence rate in March
ggplot(emergence, aes(x = Population, y = March_23_2021_emergence_count/300, col = PPT_Treatment)) + #300 seeds sown per subplot
  geom_jitter() +
  theme_bw() +
  labs(y = "Seedling emergence rate", col = "Treatment")

#Seedling emergence rate in April
ggplot(emergence, aes(x = Population, y = April_13_2021_emergence_count/300, col = PPT_Treatment))+
  geom_jitter() +
  theme_bw()

#Summarized seedling emergence 
emergence_summary <- emergence %>%
  rename(March.2021 = March_23_2021_emergence_count,
         April.2021 = April_13_2021_emergence_count) %>%
  gather(key = "Time", value = "Emergence", March.2021, April.2021)%>%
  group_by(Population, PPT_Treatment, Time) %>%
  summarise(mean_emergence = mean(Emergence/300), 
            se_emergence = se(Emergence/300)) 
emergence_summary$Population <- ordered(emergence_summary$Population, levels = c("Roar",  "Susa", "Norc",  "Elko", "Vale", "Litt"))
emergence_summary$Time <- ordered(emergence_summary$Time, levels = c("March.2021", "April.2021"))

ggplot(emergence_summary, aes(y = mean_emergence, x = Time, col = PPT_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_emergence-se_emergence, ymax = mean_emergence+se_emergence), width = 0.4, alpha = 0.9, size = 1) +
  theme_bw() +
  facet_grid(~Population)+
  labs(y = "Seedling emergence rate")


