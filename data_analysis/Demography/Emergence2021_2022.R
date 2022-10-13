### Packages
library(tidyverse)
library(ggplot2)

### Data Import
source("data_compiling/compiling_demography.R")

# this is a function for calculating standard error
se<-function(x){
  sd(x)/sqrt(length(x))
} 

#300 ELEL seeds sown per subplot
#100 BRTE seeds sown per competition subplot

#Seedling emergence rate = cumulative number of YR1 emerged seedlings/number of sown seeds
#Seedling mortality rate = number of dead YR1 seedlings/number of germinated seeds
#Seedling establishment rate = number of survived YR1 seedlings in July/number of sown seeds
#Seedling survival rate = number of survived YR2 seedlings in July/number of sown seeds

summary(aov(ELELY1EMG ~ Population*Year, emergence))
summary(aov(ELELY1EMG ~ Population*Year*PPT_Treatment, emergence))
summary(aov(ELELY1EMG ~ Population*Year*PPT_Treatment*BRTE_Treatment, emergence))

#Summarize FIRST YEAR EMERGENCE by population and month and PPT
emergence_summary <- emergence %>%
  group_by(Year, Month, Population, PPT_Treatment) %>%
  summarise(mean_emergence_Y1 = mean(ELELY1EMG/300, na.rm = TRUE), 
            se_emergence_Y1 = se(ELELY1EMG/300)) 
emergence_summary$Population <- ordered(emergence_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))
emergence_summary$Month <- ordered(emergence_summary$Month, levels = c("Feb", "Mar", "Apr", "May", "Jun", "Jul"))

#Visualize FIRST YEAR EMERGENCE by population and month and PPT
ggplot(emergence_summary, aes(y = mean_emergence_Y1, x = Month, col = PPT_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_emergence_Y1-se_emergence_Y1, ymax = mean_emergence_Y1+se_emergence_Y1), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  facet_grid(Year~Population, scales="free")+
  ylab(bquote(italic(E.~elymoides)~Year~1~Emergence~Rate))+
  labs(col = "Treatment")+
  scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))

#Summarize FIRST YEAR MORTALITY by population and month and PPT
mortality_summary <- emergence %>%
  mutate(mortality = ELELY1MOR/(ELELY1EMG+ELELY1MOR))%>%
  mutate(mortality = ifelse(is.na(mortality), 0, mortality)) %>%
  group_by(Year, Month, Population, PPT_Treatment) %>%
  summarise(mean_mortality_Y1 = mean(mortality, na.rm = TRUE),
            se_mortality_Y1 = se(mortality)) 
mortality_summary$Population <- ordered(mortality_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))
mortality_summary$Month <- ordered(mortality_summary$Month, levels = c("Feb", "Mar", "Apr", "May", "Jun", "Jul"))

#Visualize FIRST YEAR MORTALITY by population and month and PPT
ggplot(mortality_summary, aes(y = mean_mortality_Y1, x = Month, col = PPT_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_mortality_Y1-se_mortality_Y1, ymax = mean_mortality_Y1+se_mortality_Y1), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  facet_grid(Year~Population, scales="free")+
  ylab(bquote(italic(E.~elymoides)~Year~1~Mortality~Rate))+
  labs(col = "Treatment")+
  scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))

#Summarize FIRST YEAR ESTABLISHMENT by population and PPT 
establishment_summary <- emergence %>%
  filter(Month == "Jul") %>%
  mutate(establishment = ELELY1EMG/300)%>%
  group_by(Year, Population, PPT_Treatment) %>%
  summarise(mean_est_Y1 = mean(establishment, na.rm = TRUE),
            se_est_Y1 = se(establishment)) 
establishment_summary$Population <- ordered(establishment_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))

#Visualize FIRST YEAR ESTABLISHMENT by population and PPT
ggplot(establishment_summary, aes(y = mean_est_Y1, x = Population, col = PPT_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_est_Y1-se_est_Y1, ymax = mean_est_Y1+se_est_Y1), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  facet_grid(Year~PPT_Treatment, scales="free")+
  ylab(bquote(italic(E.~elymoides)~Year~1~Establishment~Rate))+
  labs(col = "Treatment")+
  scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))

#Summarize FIRST YEAR ESTABLISHMENT by population, PPT x BRTE
establishment_summary <- emergence %>%
  filter(Month == "Jul") %>%
  filter(Year == "2022") %>%
  mutate(establishment = ELELY1EMG/300)%>%
  group_by(Year, Population, PPT_Treatment, BRTE_Treatment) %>%
  summarise(mean_est_Y1 = mean(establishment, na.rm = TRUE),
            se_est_Y1 = se(establishment)) 
establishment_summary$Population <- ordered(establishment_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))

#Visualize FIRST YEAR ESTABLISHMENT by population, PPT x BRTE
ggplot(establishment_summary, aes(y = mean_est_Y1, x = Population, col = BRTE_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_est_Y1-se_est_Y1, ymax = mean_est_Y1+se_est_Y1), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  facet_grid(Year~PPT_Treatment, scales="free")+
  ylab(bquote(italic(E.~elymoides)~Year~1~Establishment~Rate))+
  labs(col = "Treatment")+
  scale_color_manual(values=c("#FC46AA", "#808080"))

#Summarize SECOND YEAR SURVIVAL by population and PPT 
survival_summary <- emergence %>%
  filter(Month == "Jul") %>%
  filter(Year == "2022") %>%
  mutate(survival = ELELY2EMG/300)%>%
  group_by(Year, Population, PPT_Treatment) %>%
  summarise(mean_survival_Y2 = mean(survival, na.rm = TRUE),
            se_survival_Y2 = se(survival)) 
survival_summary$Population <- ordered(survival_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))

#Visualize SECOND YEAR SURVIVAL by population and PPT
ggplot(survival_summary, aes(y = mean_survival_Y2, x = Population, col = PPT_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_survival_Y2-se_survival_Y2, ymax = mean_survival_Y2+se_survival_Y2), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  facet_grid(~PPT_Treatment, scales="free")+
  ylab(bquote(italic(E.~elymoides)~Year~2~Survival~Rate))+
  labs(col = "Treatment")+
  scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))

#Summarize SECOND YEAR SURVIVAL by population, PPT x BRTE
survival_summary <- emergence %>%
  filter(Month == "Jul") %>%
  filter(Year == "2022") %>%
  mutate(survival = ELELY2EMG/300)%>%
  group_by(Year, Population, PPT_Treatment, BRTE_Treatment) %>%
  summarise(mean_survival_Y2 = mean(survival, na.rm = TRUE),
            se_survival_Y2 = se(survival)) 
survival_summary$Population <- ordered(survival_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))

#Visualize SECOND YEAR SURVIVAL by population, PPT x BRTE
ggplot(survival_summary, aes(y = mean_survival_Y2, x = Population, col = BRTE_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_survival_Y2-se_survival_Y2, ymax = mean_survival_Y2+se_survival_Y2), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  facet_grid(~PPT_Treatment, scales="free")+
  ylab(bquote(italic(E.~elymoides)~Year~2~Survival~Rate))+
  labs(col = "Treatment")+
  scale_color_manual(values=c( "#FC46AA", "#808080"))
