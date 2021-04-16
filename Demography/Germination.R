#Setwd 
datpath <- "C:/Users/Lina/Dropbox/Academics/Projects/Perennial Grasses Eastern Oregon/Data"
#Load data
library(tidyverse)
germination <- read_csv(paste(datpath, "/Demography/Germination_06152020_v2.csv", sep=""))
library(ggplot2)

se<-function(x){
  sd(x)/sqrt(length(x))
} # this is a function for calculating standard error

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
  geom_jitter() +
  theme_bw() +
  labs(y = "Seedling emergence rate", col = "Treatment")

#Summarized seedling emergence plot of Elymus elymoides
germination_summary <- germination_sorted %>%
  select(Species, Genotype, Germination) %>%
  group_by(Species, Genotype) %>%
  summarise(mean_emergence = mean(Germination)/10, 
            se_emergence = se(Germination)/10) %>%
  mutate(Genotype = if_else(Genotype %in% c("Butte Valley"), "BV", as.character(Genotype))) %>%
  mutate(Genotype = if_else(Genotype %in% c("Winnemuca"), "Winm", as.character(Genotype)))
germination_summary$Genotype <- ordered(germination_summary$Genotype, levels = c("BV",  "Steens", "EOARC",  "Gund", "Winm", "Reno"))

ggplot(germination_summary %>% filter(Species == "Elymus elymoides") %>% filter(Genotype != "Steens")
       , aes(y = mean_emergence, x = Genotype)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_emergence-se_emergence, ymax = mean_emergence+se_emergence), width = 0.4, alpha = 0.9, size = 1) +
  theme_bw() +
  labs(y = "Seedling emergence rate")
