#First, load data from compiling_field_traits. We'll start with data visualization. 
library(ggplot2)

#Add a column for site groupings
   #Cooler sites - EOARC, Diamond, BLM, Steens
   #Warmer sites - Gund, Fernley, Butte Valley NG, Water Canyon
field_traits <- field_traits %>%
  mutate(site_groups = ifelse(Site %in% c("EOARC", "Diamond", "BLM", "Steens"), "Cool",
                              ifelse(Site %in% c("Gund", "Fernley", "ButteValleyNG", "WaterCanyon"), "Warm", ""))) %>%
  mutate(Site = factor(Site, levels = c("EOARC", "BLM", "Diamond", "Steens",
                            "WaterCanyon","ButteValleyNG", "Gund", "Fernley")))

#Height by species and location
ggplot(field_traits, aes(Site, Height_cm)) +
  facet_grid(~Species) +
  geom_point(aes(color = site_groups)) +
  scale_color_manual(values= c("deepskyblue","orange"))

#Stomata conductance by species and location
ggplot(field_traits, aes(Site, Stomata_Conductance_mmol_m2s)) +
  facet_grid(~Species) +
  geom_point(aes(color = site_groups)) +
  scale_color_manual(values= c("deepskyblue","orange"))

#SLA by species and location
ggplot(field_traits, aes(Site, SLA_cm2)) +
  facet_grid(~Species) +
  geom_point(aes(color = site_groups)) +
  scale_color_manual(values= c("deepskyblue","orange"))

#LDMC by species and location
ggplot(field_traits, aes(Site, LDMC)) +
  facet_grid(~Species) +
  geom_point(aes(color = site_groups)) +
  scale_color_manual(values= c("deepskyblue","orange"))
