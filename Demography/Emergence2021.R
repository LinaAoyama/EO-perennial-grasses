#Setwd 
datpath <- "C:/Users/Lina/Dropbox/Academics/Projects/Perennial Grasses Eastern Oregon/Data"
#Load data
library(tidyverse)
emergence <- read_csv(paste(datpath, "/Demography/Emergence_03232021.csv", sep=""))
library(ggplot2)

se<-function(x){
  sd(x)/sqrt(length(x))
} # this is a function for calculating standard error


#Seedling emergence rate
ggplot(emergence, aes(x = Population, y = March_23_2021_emergence_count/300, col = PPT_Treatment)) + #300 seeds sown per subplot
  geom_jitter() +
  theme_bw() +
  labs(y = "Seedling emergence rate", col = "Treatment")

#Summarized seedling emergence 
emergence_summary <- emergence %>%
  group_by(Population, PPT_Treatment) %>%
  summarise(mean_emergence = mean(March_23_2021_emergence_count/300), 
            se_emergence = se(March_23_2021_emergence_count/300)) 
emergence_summary$Population <- ordered(emergence_summary$Population, levels = c("Roar",  "Susa", "Norc",  "Elko", "Vale", "Litt"))

ggplot(emergence_summary, aes(y = mean_emergence, x = Population, col = PPT_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_emergence-se_emergence, ymax = mean_emergence+se_emergence), width = 0.4, alpha = 0.9, size = 1) +
  theme_bw() +
  labs(y = "Seedling emergence rate")
