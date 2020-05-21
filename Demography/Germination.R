#Setwd 
datpath <- "C:/Users/Lina/Dropbox/Academics/Projects/Perennial Grasses Eastern Oregon/Data"
#Load data
germination <- read_csv(paste(datpath, "/Demography/Germination_05132020.csv", sep=""))

library(ggplot2)

germination_sorted <- germination %>%
  mutate(Genotype = if_else(Genotype %in% c("Diamond"), "Steens", as.character(Genotype))) %>%
  mutate(Genotype = if_else(Genotype %in% c("Water Canyon"), "Winnemuca", as.character(Genotype))) %>%
  mutate(Genotype = if_else(Genotype %in% c("Fernley"), "Reno", as.character(Genotype)))
  

germination_sorted$Genotype <- as.factor(germination_sorted$Genotype)
germination_sorted$Genotype <- ordered(germination_sorted$Genotype, levels = c("EOARC", "Diamond", "Steens","Butte Valley",
                                                              "Water Canyon", "Winnemuca", "Gund", "Fernley", "Reno"))

#Histogram of germinants per species and genotypes
ggplot(germination_sorted, aes(x = Genotype)) +
  geom_bar() + 
  facet_wrap(vars(Species), nrow = 6)
