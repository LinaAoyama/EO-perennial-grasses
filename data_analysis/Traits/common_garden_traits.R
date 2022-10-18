### Packages
library(tidyverse)
library(ggplot2)
library(nlme) #linear mixed effects
library(multcomp) #tukey

### Data Import
source("data_compiling/compiling_common_garden_traits.R")

# this is a function for calculating standard error
se<-function(x){
  sd(x)/sqrt(length(x))
} 

#ANOVA - SECOND YEAR PER CAPITA BIOMASS 
summary(aov(ELELY2_Biomass ~ Population, biomass)) #no significant difference by population
TukeyHSD(aov(ELELY2_Biomass ~ Population, biomass))
summary(aov(ELELY2_Biomass ~ Population*PPT_Treatment, biomass)) #no significant difference by population or ppt treatment
TukeyHSD(aov(ELELY2_Biomass ~ Population*PPT_Treatment, biomass))
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
  ylab(bquote(italic(E.~elymoides)~Year~2~per~capita~Biomass~(g)))+
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

#ANOVA - FIRST YEAR SLA
summary(aov(SLA ~ Population*Year*PPT_Treatment*BRTE_Treatment, leaf%>%filter(ELEL_YR=="1"))) #no significant difference by population and ppt treatment 

#Summarize FIRST YEAR SLA by year, population and PPT 
SLA_summary <- leaf %>%
  drop_na() %>%
  filter(ELEL_YR=="1")%>%
  group_by(Population, Year, PPT_Treatment) %>%
  summarise(mean_Y1_sla = mean(SLA, na.rm = TRUE), 
            se_Y1_sla = se(SLA)) 
SLA_summary$Population <- ordered(SLA_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))

#Visualize FIRST YEAR SLA by year, population and PPT
ggplot(SLA_summary, aes(y = mean_Y1_sla, x = Population, col = PPT_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_Y1_sla-se_Y1_sla, ymax = mean_Y1_sla+se_Y1_sla), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  facet_grid(Year~PPT_Treatment, scales="free")+
  ylab(bquote(italic(E.~elymoides)~Year~1~SLA~(cm2/g)))+
  labs(col = "Treatment")+
  scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))

#Summarize FIRST YEAR SLA of 2022 only, population and PPT and BRTE
SLA_summary_BRTE <- leaf %>%
  drop_na() %>%
  filter(ELEL_YR=="1")%>%
  filter(Year == "2022")%>%
  group_by(Population, BRTE_Treatment, PPT_Treatment) %>%
  summarise(mean_Y1_sla = mean(SLA, na.rm = TRUE), 
            se_Y1_sla = se(SLA)) 
SLA_summary_BRTE$Population <- ordered(SLA_summary_BRTE$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))

#Visualize FIRST YEAR SLA of 2022 only, population and PPT and BRTE
ggplot(SLA_summary_BRTE, aes(y = mean_Y1_sla, x = Population, col = BRTE_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_Y1_sla-se_Y1_sla, ymax = mean_Y1_sla+se_Y1_sla), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  facet_grid(~PPT_Treatment, scales="free")+
  ylab(bquote(italic(E.~elymoides)~Year~1~SLA~(cm2/g)))+
  labs(col = "Treatment")+
  scale_color_manual(values=c("#FC46AA", "#808080"))

#ANOVA - SECOND YEAR SLA
summary(aov(SLA ~ Population*PPT_Treatment*BRTE_Treatment, leaf%>%filter(ELEL_YR=="2"))) #no significant difference by population but sig diff by pop and brte treatment interactions

#Summarize SECOND YEAR SLA by population and PPT and BRTE
SLA_summary <- leaf %>%
  drop_na() %>%
  filter(ELEL_YR=="2")%>%
  group_by(Population, PPT_Treatment, BRTE_Treatment) %>%
  summarise(mean_Y2_sla = mean(SLA, na.rm = TRUE), 
            se_Y2_sla = se(SLA)) 
SLA_summary$Population <- ordered(SLA_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))

#Visualize SECOND YEAR SLA by population and PPT and BRTE
ggplot(SLA_summary, aes(y = mean_Y2_sla, x = Population, col = BRTE_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_Y2_sla-se_Y2_sla, ymax = mean_Y2_sla+se_Y2_sla), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  facet_grid(~PPT_Treatment, scales="free")+
  ylab(bquote(italic(E.~elymoides)~Year~2~SLA~(cm2/g)))+
  labs(col = "Treatment")+
  scale_color_manual(values=c("#FC46AA", "#808080"))

#ANOVA - SECOND YEAR LDMC
summary(aov(LDMC ~ Population*Year*PPT_Treatment*BRTE_Treatment, leaf%>%filter(ELEL_YR=="2"))) #significant difference by population 

#Summarize SECOND YEAR LDMC by year, population and PPT 
LDMC_summary <- leaf %>%
  drop_na() %>%
  filter(ELEL_YR=="2")%>%
  group_by(Population, Year, PPT_Treatment) %>%
  summarise(mean_Y2_ldmc = mean(LDMC, na.rm = TRUE), 
            se_Y2_ldmc = se(LDMC)) 
LDMC_summary$Population <- ordered(LDMC_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))

#Visualize FIRST YEAR LDMC by year, population and PPT
ggplot(LDMC_summary, aes(y = mean_Y2_ldmc, x = Population, col = PPT_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_Y2_ldmc-se_Y2_ldmc, ymax = mean_Y2_ldmc+se_Y2_ldmc), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  facet_grid(Year~PPT_Treatment, scales="free")+
  ylab(bquote(italic(E.~elymoides)~Year~2~LDMC~(g/g)))+
  labs(col = "Treatment")+
  scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))

