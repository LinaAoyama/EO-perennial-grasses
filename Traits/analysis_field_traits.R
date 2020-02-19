#First, load data from compiling_field_traits. We'll start with data visualization. 
library(ggplot2)
library(ggpubr)

#Add a column for site groupings
   #Cooler sites - EOARC, Diamond, BLM, Steens
   #Warmer sites - Gund, Fernley, Butte Valley NG, Water Canyon

field_traits1 <- field_traits %>%
  mutate(Site = if_else(Site %in% c("BLM", "Diamond"), "Steens", as.character(Site))) %>%
  mutate(site_groups = ifelse(Site %in% c("EOARC", "Steens", "ButteValleyNG"), "Cool",
                              ifelse(Site %in% c("Gund", "Fernley", "WaterCanyon"), "Warm", ""))) %>%
  mutate(Site = factor(Site, levels = c("EOARC", "Steens", "ButteValleyNG", "WaterCanyon", "Gund", "Fernley"))) %>%
  filter(Species %in% c("Poa secunda"))

#Height by location
f1 <- ggplot(field_traits1, aes(Site, log(Height))) +
  geom_jitter(aes(color = site_groups)) +
  geom_boxplot(alpha = 0)+
  scale_color_manual(values= c("deepskyblue","orange")) +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank())+
  annotate("text", x = 1:4, y = 4.2, label = "a")+
  annotate("text", x = 5, y = 4.2, label = "b")

#Stomata conductance by location
f2 <- ggplot(field_traits1, aes(Site, Stomata_Conductance)) +
  geom_jitter(aes(color = site_groups)) +
  geom_boxplot(alpha = 0)+
  scale_color_manual(values= c("deepskyblue","orange")) +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank())+
  ylab("Stomata cond") +
  annotate("text", x = 1:3, y = 560, label = "a")+
  annotate("text", x = 4:5, y = 560, label = "b")

#SLA by location
f3<- ggplot(field_traits1, aes(Site, log(SLA))) +
  geom_jitter(aes(color = site_groups)) +
  geom_boxplot(alpha = 0)+
  scale_color_manual(values= c("deepskyblue","orange")) +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  annotate("text", x = 1:3, y = 8.5, label = "a")+
  annotate("text", x = 4, y = 8.5, label = "b") +
  annotate("text", x = 5, y = 8.5, label = "a")

#LDMC by location
f4 <- ggplot(field_traits1, aes(Site, LDMC)) +
  geom_jitter(aes(color = site_groups))+
  geom_boxplot(alpha = 0)+
  scale_color_manual(values= c("deepskyblue","orange")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  annotate("text", x = 1:3, y = 0.5, label = "a")+
  annotate("text", x = 4, y = 0.5, label = "b") +
  annotate("text", x = 5, y = 0.5, label = "a")

ggarrange(f1, f2, f3, f4,  ncol = 1, nrow = 4, 
          labels = c("a)" ,"b)",
                     "c)", "d)"),
          common.legend = TRUE, legend = "bottom", 
          font.label = list(size = 10), heights = c(2, 2, 2, 3.5))

#ANOVA
Poa_Ht <- aov(lm(Height~Site, field_traits1))
TukeyHSD(Poa_Ht, conf.level = 0.95)
Poa_St <- aov(lm(Stomata_Conductance~Site, field_traits1))
TukeyHSD(Poa_St, conf.level = 0.95)
Poa_SLA <- aov(lm(SLA~Site, field_traits1))
TukeyHSD(Poa_SLA, conf.level = 0.95)
Poa_LDMC <- aov(lm(LDMC~Site, field_traits1))
TukeyHSD(Poa_LDMC, conf.level = 0.95)


