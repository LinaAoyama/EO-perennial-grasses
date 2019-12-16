#First, load data from compiling_field_traits. We'll start with data visualization. 
library(ggplot2)

#Add a column for site groupings
   #Cooler sites - EOARC, Diamond, BLM, Steens
   #Warmer sites - Gund, Fernley, Butte Valley NG, Water Canyon

field_traits1 <- field_traits %>%
  mutate(Site = if_else(Site %in% c("BLM", "Diamond"), "Steens", as.character(Site))) %>%
  mutate(site_groups = ifelse(Site %in% c("EOARC", "Steens", "ButteValleyNG"), "Cool",
                              ifelse(Site %in% c("Gund", "Fernley", "WaterCanyon"), "Warm", ""))) %>%
  mutate(Site = factor(Site, levels = c("EOARC", "Steens", "ButteValleyNG", "WaterCanyon", "Gund", "Fernley"))) %>%
  filter(Species %in% c( "Poa secunda"))

library(ggpubr)
#Height by location
f1 <- ggplot(field_traits1, aes(Site, log(Height))) +
  geom_jitter(aes(color = site_groups)) +
  geom_boxplot(alpha = 0)+
  scale_color_manual(values= c("deepskyblue","orange")) +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank())

#Stomata conductance by location
f2 <- ggplot(field_traits1, aes(Site, Stomata_Conductance)) +
  geom_jitter(aes(color = site_groups)) +
  geom_boxplot(alpha = 0)+
  scale_color_manual(values= c("deepskyblue","orange")) +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank())+
  ylab("Stomata cond")

#SLA by location
f3<- ggplot(field_traits1, aes(Site, log(SLA))) +
  geom_jitter(aes(color = site_groups)) +
  geom_boxplot(alpha = 0)+
  scale_color_manual(values= c("deepskyblue","orange")) +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank())

#LDMC by location
f4 <- ggplot(field_traits1, aes(Site, LDMC)) +
  geom_jitter(aes(color = site_groups))+
  geom_boxplot(alpha = 0)+
  scale_color_manual(values= c("deepskyblue","orange")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggarrange(f1, f2, f3, f4,  ncol = 1, nrow = 4, 
          labels = c("a)" ,"b)",
                     "c)", "d)"),
          common.legend = TRUE, legend = "bottom", 
          font.label = list(size = 10), heights = c(2, 2, 2, 3.5))
