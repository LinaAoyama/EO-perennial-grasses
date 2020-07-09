#Setwd 
datpath <- "C:/Users/Lina/Dropbox/Academics/Projects/Perennial Grasses Eastern Oregon/Data"
#Load data
library(tidyverse)
germination <- read_csv(paste(datpath, "/Demography/Germination_06152020.csv", sep=""))

library(ggplot2)

germination_sorted <- germination %>%
  mutate(Genotype = if_else(Genotype %in% c("Diamond"), "Steens", as.character(Genotype))) %>%
  mutate(Genotype = if_else(Genotype %in% c("Water Canyon"), "Winnemuca", as.character(Genotype))) %>%
  mutate(Genotype = if_else(Genotype %in% c("Fernley"), "Reno", as.character(Genotype))) %>%
  mutate(Germination = pmax(Germination_counts_May , Seedling_counts_June))
  

germination_sorted$Genotype <- as.factor(germination_sorted$Genotype)
germination_sorted$Genotype <- ordered(germination_sorted$Genotype, levels = c("EOARC", "Diamond", "Steens","Butte Valley",
                                                              "Water Canyon", "Winnemuca", "Gund", "Fernley", "Reno"))

#Histogram of germinants per species and genotypes
ggplot(germination_sorted, aes(x = Genotype)) +
  geom_bar() + 
  facet_wrap(vars(Species), nrow = 6)

#Seedling emergence rate of Elymus elymoides only
ggplot(germination_sorted %>% filter(Species == "Elymus elymoides"), aes(x = Genotype, y = Germination/10)) +
  geom_jitter(aes(col = Plot))+
  theme_bw() +
  labs(y = "Seedling emergence rate", col = "Treatment")
