### Packages
library(tidyverse)
library(ggplot2)
library(ggpubr)

### Data Import
source("data_compiling/compiling_demography.R")

# this is a function for calculating standard error
se<-function(x){
  sd(x)/sqrt(length(x))
} 

#300 ELEL seeds sown per subplot
#100 BRTE seeds sown per competition subplot

#Seedling emergence rate = number of live YR1 seedlings/number of sown seeds
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
f_emergence <- ggplot(emergence_summary, aes(y = mean_emergence_Y1, x = Month, col = PPT_Treatment)) +
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
                    ylab(bquote(Year~1~Emergence~Rate))+
                    labs(col = "Treatment")+
                    scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))

#Visualize FIRST YEAR EMERGENCE in 2021
ggplot(emergence_summary %>% filter(Year == "2021"), aes(y = mean_emergence_Y1, x = Month, col = PPT_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_emergence_Y1-se_emergence_Y1, ymax = mean_emergence_Y1+se_emergence_Y1), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "right")+
  facet_grid(~Population, scales="free")+
  ylab(bquote(Year~1~Emergence~Rate))+
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
f_mortality <- ggplot(mortality_summary %>% filter(Year == "2021"), aes(y = mean_mortality_Y1, x = Month, col = PPT_Treatment)) +
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
                    facet_grid(~Population, scales="free")+
                    ylab(bquote(Year~1~Mortality~Rate))+
                    labs(col = "Treatment")+
                    scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))

#ANOVA FIRST YEAR MORTALITY
summary(aov(mortality ~ Population*PPT_Treatment*Year, emergence %>%
              mutate(mortality = ELELY1MOR/(ELELY1EMG+ELELY1MOR))%>%
              mutate(mortality = ifelse(is.na(mortality), 0, mortality))))


#Summarize CUMULATIVE FIRST YEAR MORTALITY by population and PPT
mortality_summary <- emergence %>%
  filter(Year == "2021") %>%
  group_by(Block, Site, Subplot, Population, PPT_Treatment) %>%
  summarise(cum_mortality = sum(ELELY1MOR), 
            total_emerg = max(ELELY1EMG))%>%
  #mutate(mortality_rate = cum_mortality/total_emerg) %>%
  #mutate(mortality_rate = ifelse(is.na(mortality_rate), 0, mortality_rate)) %>%
  group_by(Population, PPT_Treatment) %>%
  summarise(mean_mortality_Y1 = mean(cum_mortality, na.rm = TRUE),
            se_mortality_Y1 = se(cum_mortality), 
            mean_max_emerg = mean(total_emerg, na.rm = TRUE),
            se_max_emerg = se(total_emerg)) 
mortality_summary$Population <- ordered(mortality_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))


#Visualize CUMULATIVE FIRST YEAR MORTALITY by population and PPT
ggplot(mortality_summary, aes(y = mean_mortality_Y1, x = Population, col = PPT_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_mortality_Y1-se_mortality_Y1, ymax = mean_mortality_Y1+se_mortality_Y1), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "right")+
  #facet_grid(~Year, scales="free")+
  ylab(bquote(Cumulative~Year~1~Dead~Seedlings))+
  labs(col = "Treatment")+
  scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))

#Visualize CUMULATIVE FIRST YEAR EMERGENCE by population and PPT
ggplot(mortality_summary, aes(y = mean_max_emerg, x = Population, col = PPT_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_max_emerg-se_max_emerg, ymax = mean_max_emerg+se_max_emerg), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "right")+
  #facet_grid(~PPT_Treatment, scales="free")+
  ylab(bquote(Cumulative~Year~1~Live~Seedlings))+
  ylim(0, 50)+
  labs(col = "Treatment")+
  scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))


#FIG Emergence and Mortality YR1
ggarrange(f_emergence, f_mortality, ncol = 1, labels = c("(a)", "(b)"), align = "v", common.legend = TRUE)

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

#Summarize SECOND YEAR SURVIVAL by population 
survival_summary <- emergence %>%
  filter(Month == "Jul") %>%
  filter(Year == "2022") %>%
  mutate(survival = ELELY2EMG/300)%>%
  group_by(Year, Population) %>%
  summarise(mean_survival_Y2 = mean(survival, na.rm = TRUE),
            se_survival_Y2 = se(survival)) 
survival_summary$Population <- ordered(survival_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))

ggplot(survival_summary, aes(y = mean_survival_Y2, x = Population)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_survival_Y2-se_survival_Y2, ymax = mean_survival_Y2+se_survival_Y2), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        axis.title.x = element_blank(),
        legend.position = "bottom")+
  #facet_grid(~PPT_Treatment, scales="free")+
  ylab(bquote(Year~2~Survival~Rate))
  #labs(col = "Treatment")+
  #scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))

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
f_survival <- ggplot(survival_summary, aes(y = mean_survival_Y2, x = Population)) +
                  geom_point(aes(col = PPT_Treatment)) +
                  geom_errorbar(aes(ymin = mean_survival_Y2-se_survival_Y2, ymax = mean_survival_Y2+se_survival_Y2, col = PPT_Treatment), width = 0.4, alpha = 0.9, size = 1) +
                  theme(text = element_text(size=15),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        axis.line = element_line(colour = "black"),
                        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
                        axis.title = element_text(size = 15),
                        axis.title.x = element_blank(),
                        legend.position = "bottom")+
                  facet_grid(~PPT_Treatment, scales="free")+
                  ylab(bquote(Year~2~Survival~Rate))+
                  labs(col = "Treatment")+
                  scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))

#ANOVA SECOND YEAR SURVIVAL
summary(aov(survival ~ Population*PPT_Treatment, emergence %>%
              filter(Month == "Jul") %>%
              filter(Year == "2022") %>%
              mutate(survival = ELELY2EMG/300)))
TukeyHSD(aov(survival ~ Population*PPT_Treatment, emergence %>%
               filter(Month == "Jul") %>%
               filter(Year == "2022") %>%
               mutate(survival = ELELY2EMG/300)))

#annotate f_survival
dat_text1 <- data.frame(
  label = c("a", "a", "a", "a", "a", "a"),
  PPT_Treatment   = factor(c("ambient"), levels = c( "ambient", "moderate", "severe")),
  x     = c(1, 2, 3, 4, 5, 6),
  y     = c(0.0085, 0.0085, 0.0085, 0.0085, 0.0085, 0.0085)
)
dat_text2 <- data.frame(
  label = c("a", "b", "a", "a", "a", "b"),
  PPT_Treatment   = factor(c("moderate"), levels = c( "ambient", "moderate", "severe")),
  x     = c(1, 2, 3, 4, 5, 6),
  y     = c(0.0085, 0.0085, 0.0085, 0.0085, 0.0085, 0.0085)
)
dat_text3 <- data.frame(
  label = c("a", "a", "a", "a", "a", "a"),
  PPT_Treatment   = factor(c("severe"), levels = c( "ambient", "moderate", "severe")),
  x     = c(1, 2, 3, 4, 5, 6),
  y     = c(0.0085, 0.0085, 0.0085, 0.0085, 0.0085, 0.0085)
)
f_survival_stats <- f_survival+geom_text(data = dat_text1, mapping = aes(x = x, y = y, label = label))+
  geom_text(data = dat_text2, mapping = aes(x = x, y = y, label = label))+
  geom_text(data = dat_text3, mapping = aes(x = x, y = y, label = label))

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
