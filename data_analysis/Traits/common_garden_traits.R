### Packages
library(tidyverse)
library(ggplot2)

### Data Import
source("data_compiling/compiling_common_garden_traits.R")

# this is a function for calculating standard error
se<-function(x){
  sd(x)/sqrt(length(x))
} 

#ANOVA - SECOND YEAR PER CAPITA BIOMASS 
summary(aov(ELELY2_Biomass ~ Population, biomass)) #no significant difference by population
summary(aov(ELELY2_Biomass ~ Population*PPT_Treatment, biomass)) #no significant difference by population or ppt treatment

#Summarize SECOND YEAR PER CAPITA BIOMASS by population
biomass_summary <- biomass %>%
  drop_na() %>%
  group_by(Population) %>%
  summarise(mean_biomass = mean(ELELY2_Biomass, na.rm = TRUE), 
            se_biomass = se(ELELY2_Biomass)) 
biomass_summary$Population <- ordered(biomass_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))

#Visualize SECOND YEAR PER CAPITA BIOMASS by population and PPT
ggplot(biomass_summary, aes(y = mean_biomass, x = Population)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_biomass-se_biomass, ymax = mean_biomass+se_biomass), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  #facet_grid(~PPT_Treatment, scales="free")+
  ylab(bquote(italic(E.~elymoides)~Year~2~per~capita~Biomass~(g)))#+
  #labs(col = "Treatment")+
  #scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))

#Summarize SECOND YEAR PER CAPITA BIOMASS by population and PPT
biomass_summary <- biomass %>%
  drop_na() %>%
  group_by(Population, PPT_Treatment) %>%
  summarise(mean_biomass = mean(ELELY2_Biomass, na.rm = TRUE), 
            se_biomass = se(ELELY2_Biomass)) 
biomass_summary$Population <- ordered(biomass_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))

#Visualize SECOND YEAR PER CAPITA BIOMASS by population and PPT
ggplot(biomass_summary, aes(y = mean_biomass, x = Population, col = PPT_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_biomass-se_biomass, ymax = mean_biomass+se_biomass), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  facet_grid(~PPT_Treatment, scales="free")+
  ylab(bquote(italic(E.~elymoides)~Year~2~per~capita~Biomass))+
  labs(col = "Treatment")+
  scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))

#ANOVA - FIRST YEAR HEIGHT
summary(aov(Height ~ Population*Year*PPT_Treatment*BRTE_Treatment, ht%>%filter(ELEL_YR=="1"))) #no significant difference by population but sig diff by year and ppt treatment

#Summarize FIRST YEAR HEIGHT by year, population and PPT 
ht_summary <- ht %>%
  drop_na() %>%
  filter(ELEL_YR=="1")%>%
  group_by(Population, Year, PPT_Treatment) %>%
  summarise(mean_Y1_ht = mean(Height, na.rm = TRUE), 
            se_Y1_ht = se(Height)) 
ht_summary$Population <- ordered(ht_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))

#Visualize FIRST YEAR HEIGHT by year, population and PPT
ggplot(ht_summary, aes(y = mean_Y1_ht, x = Population, col = PPT_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_Y1_ht-se_Y1_ht, ymax = mean_Y1_ht+se_Y1_ht), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  facet_grid(Year~PPT_Treatment, scales="free")+
  ylab(bquote(italic(E.~elymoides)~Year~1~Height~(cm)))+
  labs(col = "Treatment")+
  scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))

#Summarize FIRST YEAR HEIGHT of 2022 only, population and PPT and BRTE
ht_summary <- ht %>%
  drop_na() %>%
  filter(ELEL_YR=="1")%>%
  filter(Year == "2022")%>%
  group_by(Population, BRTE_Treatment, PPT_Treatment) %>%
  summarise(mean_Y1_ht = mean(Height, na.rm = TRUE), 
            se_Y1_ht = se(Height)) 
ht_summary$Population <- ordered(ht_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))

#Visualize FIRST YEAR HEIGHT of 2022 only, population and PPT and BRTE
ggplot(ht_summary, aes(y = mean_Y1_ht, x = Population, col = BRTE_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_Y1_ht-se_Y1_ht, ymax = mean_Y1_ht+se_Y1_ht), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  facet_grid(~PPT_Treatment, scales="free")+
  ylab(bquote(italic(E.~elymoides)~Year~1~Height~(cm)))+
  labs(col = "Treatment")+
  scale_color_manual(values=c("#FC46AA", "#808080"))
