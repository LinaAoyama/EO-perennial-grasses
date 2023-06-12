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

# #Summarize FIRST YEAR EMERGENCE by population and month and PPT
# emergence_summary <- emergence %>%
#   group_by(Year, Month, Population, PPT_Treatment) %>%
#   summarise(mean_emergence_Y1 = mean(ELELY1EMG/300, na.rm = TRUE), 
#             se_emergence_Y1 = se(ELELY1EMG/300)) 
# emergence_summary$Population <- ordered(emergence_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))
# emergence_summary$Month <- ordered(emergence_summary$Month, levels = c("Feb", "Mar", "Apr", "May", "Jun", "Jul"))
# 
# #Visualize FIRST YEAR EMERGENCE by population and month and PPT
# f_emergence <- ggplot(emergence_summary, aes(y = mean_emergence_Y1, x = Month, col = PPT_Treatment)) +
#                     geom_point() +
#                     geom_errorbar(aes(ymin = mean_emergence_Y1-se_emergence_Y1, ymax = mean_emergence_Y1+se_emergence_Y1), width = 0.4, alpha = 0.9, size = 1) +
#                     theme(text = element_text(size=15),
#                           panel.grid.major = element_blank(),
#                           panel.grid.minor = element_blank(),
#                           panel.background = element_blank(),
#                           axis.line = element_line(colour = "black"),
#                           panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
#                           axis.title = element_text(size = 15),
#                           legend.position = "bottom")+
#                     facet_grid(Year~Population, scales="free")+
#                     ylab(bquote(Year~1~Emergence~Rate))+
#                     labs(col = "Treatment")+
#                     scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))
# 
# #Visualize FIRST YEAR EMERGENCE in 2021
# ggplot(emergence_summary %>% filter(Year == "2021"), aes(y = mean_emergence_Y1, x = Month, col = PPT_Treatment)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = mean_emergence_Y1-se_emergence_Y1, ymax = mean_emergence_Y1+se_emergence_Y1), width = 0.4, alpha = 0.9, size = 1) +
#   theme(text = element_text(size=15),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
#         axis.title = element_text(size = 15),
#         legend.position = "right")+
#   facet_grid(~Population, scales="free")+
#   ylab(bquote(Year~1~Emergence~Rate))+
#   labs(col = "Treatment")+
#   scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))
# 
# #Summarize FIRST YEAR MORTALITY by population and month and PPT
# mortality_summary <- emergence %>%
#   mutate(mortality = ELELY1MOR/(ELELY1EMG+ELELY1MOR))%>%
#   mutate(mortality = ifelse(is.na(mortality), 0, mortality)) %>%
#   group_by(Year, Month, Population, PPT_Treatment) %>%
#   summarise(mean_mortality_Y1 = mean(mortality, na.rm = TRUE),
#             se_mortality_Y1 = se(mortality)) 
# mortality_summary$Population <- ordered(mortality_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))
# mortality_summary$Month <- ordered(mortality_summary$Month, levels = c("Feb", "Mar", "Apr", "May", "Jun", "Jul"))
# 
# #Visualize FIRST YEAR MORTALITY by population and month and PPT
# f_mortality <- ggplot(mortality_summary %>% filter(Year == "2021"), aes(y = mean_mortality_Y1, x = Month, col = PPT_Treatment)) +
#                     geom_point() +
#                     geom_errorbar(aes(ymin = mean_mortality_Y1-se_mortality_Y1, ymax = mean_mortality_Y1+se_mortality_Y1), width = 0.4, alpha = 0.9, size = 1) +
#                     theme(text = element_text(size=15),
#                           panel.grid.major = element_blank(),
#                           panel.grid.minor = element_blank(),
#                           panel.background = element_blank(),
#                           axis.line = element_line(colour = "black"),
#                           panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
#                           axis.title = element_text(size = 15),
#                           legend.position = "bottom")+
#                     facet_grid(~Population, scales="free")+
#                     ylab(bquote(Year~1~Mortality~Rate))+
#                     labs(col = "Treatment")+
#                     scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))
# 
# #ANOVA FIRST YEAR MORTALITY
# summary(aov(mortality ~ Population*PPT_Treatment*Year, emergence %>%
#               mutate(mortality = ELELY1MOR/(ELELY1EMG+ELELY1MOR))%>%
#               mutate(mortality = ifelse(is.na(mortality), 0, mortality))))


#Summarize CUMULATIVE FIRST YEAR EMERGENCE & MORTALITY by population and PPT
cumulative <- emergence %>%
  group_by(Block, Site, Year, Subplot, Population, PPT_Treatment) %>%
  summarise(cum_mortality = sum(ELELY1MOR), 
            total_emerg = max(ELELY1EMG)*4)
cumulative_summary <- cumulative %>%
  group_by(Population, PPT_Treatment, Year) %>%
  summarise(mean_mortality_Y1 = mean(cum_mortality, na.rm = TRUE),
            se_mortality_Y1 = se(cum_mortality), 
            mean_max_emerg = mean(total_emerg, na.rm = TRUE),
            se_max_emerg = se(total_emerg)) 
cumulative_summary$Population <- ordered(cumulative_summary$Population, levels = c("Norc", "Roar","Susa","Litt", "Elko", "Vale"))


#ANOVA CUMULATIVE FIRST YEAR EMERGENCE
summary(aov(total_emerg ~ Population*PPT_Treatment*Year, cumulative))
TukeyHSD(aov(total_emerg ~ Population*PPT_Treatment, cumulative%>%filter(Year == 2021)))
TukeyHSD(aov(total_emerg ~ Population*PPT_Treatment, cumulative%>%filter(Year == 2022)))

#Visualize CUMULATIVE FIRST YEAR EMERGENCE by population and PPT
f_cum_emergence <- ggplot(cumulative_summary, aes(y = mean_max_emerg, x = Population)) +
          geom_point(aes(col = PPT_Treatment)) +
          geom_errorbar(aes(ymin = mean_max_emerg-se_max_emerg, ymax = mean_max_emerg+se_max_emerg, col = PPT_Treatment), width = 0.4, alpha = 0.9, size = 1) +
          theme(text = element_text(size=15),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
                axis.title = element_text(size = 15),
                axis.title.x = element_blank(),
                legend.position = "right")+
          facet_grid(Year~PPT_Treatment, scales="free")+
          ylab(bquote(Year~1~Total~Emergence))+
          #ylim(0, 50)+
          labs(col = "Treatment")+
          scale_color_manual(values=c("#34cfeb", "#ebcf34", "#eb6734"))

#annotate f_establish
dat_text1 <- data.frame(
  label = c("a", "a", "a", "a", "a", "a"),
  PPT_Treatment   = factor(c("ambient"), levels = c( "ambient", "moderate", "severe")),
  Year = 2021,
  x     = c(1, 2, 3, 4, 5, 6),
  y     = 900
)
dat_text2 <- data.frame(
  label = c("a", "b", "b", "a", "a", "ab"),
  PPT_Treatment   = factor(c("moderate"), levels = c( "ambient", "moderate", "severe")),
  Year = 2021,
  x     = c(1, 2, 3, 4, 5, 6),
  y     = 900
)
dat_text3 <- data.frame(
  label = c("a", "b", "b", "a", "a", "a"),
  PPT_Treatment   = factor(c("severe"), levels = c( "ambient", "moderate", "severe")),
  Year = 2021,
  x     = c(1, 2, 3, 4, 5, 6),
  y     = 900
)
dat_text1_2 <- data.frame(
  label = c("a", "a", "a", "a", "a", "a"),
  PPT_Treatment   = factor(c("ambient"), levels = c( "ambient", "moderate", "severe")),
  Year = 2022,
  x     = c(1, 2, 3, 4, 5, 6),
  y     = 130
)
dat_text2_2 <- data.frame(
  label = c("a", "a", "a", "a", "a", "a"),
  PPT_Treatment   = factor(c("moderate"), levels = c( "ambient", "moderate", "severe")),
  Year = 2022,
  x     = c(1, 2, 3, 4, 5, 6),
  y     = 130
)
dat_text3_2 <- data.frame(
  label = c("a", "a", "a", "a", "a", "a"),
  PPT_Treatment   = factor(c("severe"), levels = c( "ambient", "moderate", "severe")),
  Year = 2022,
  x     = c(1, 2, 3, 4, 5, 6),
  y     = 130
)

f_cum_emergence_stats <- f_cum_emergence+geom_text(data = dat_text1, mapping = aes(x = x, y = y, label = label))+
  geom_text(data = dat_text2, mapping = aes(x = x, y = y, label = label))+
  geom_text(data = dat_text3, mapping = aes(x = x, y = y, label = label))+
  geom_text(data= dat_text1_2, mapping = aes(x =x, y = y, label = label))+
  geom_text(data= dat_text2_2, mapping = aes(x =x, y = y, label = label))+
  geom_text(data= dat_text3_2, mapping = aes(x =x, y = y, label = label))

#FIG Emergence and Mortality YR1
#ggarrange(f_emergence, f_mortality, ncol = 1, labels = c("(a)", "(b)"), align = "v", common.legend = TRUE)

#Summarize FIRST YEAR ESTABLISHMENT DENSITY by population and PPT 
establishment_summary <- emergence %>%
  filter(Month == "Jul") %>%
  mutate(establishment = ELELY1EMG*4)%>%
  group_by(Year, Population, PPT_Treatment) %>%
  summarise(mean_est_Y1 = mean(establishment, na.rm = TRUE),
            se_est_Y1 = se(establishment)) 
establishment_summary$Population <- ordered(establishment_summary$Population, levels = c("Norc", "Roar","Susa","Litt", "Elko", "Vale"))

#ANOVA FIRST YEAR ESTABLISHMENT DENSITY
summary(aov(ELELY1EMG ~ Population*PPT_Treatment*Year, emergence %>%
              filter(Month == "Jul")))
TukeyHSD(aov(ELELY1EMG ~ Population*PPT_Treatment, emergence %>%
               filter(Month == "Jul")%>%
               filter(Year == "2021")))
TukeyHSD(aov(ELELY1EMG ~ Population*PPT_Treatment, emergence %>%
               filter(Month == "Jul")%>%
               filter(Year == "2022")))

#Visualize FIRST YEAR ESTABLISHMENT DENSITY by population and PPT
f_establish <- ggplot(establishment_summary, aes(y = mean_est_Y1, x = Population)) +
  geom_point(aes(col = PPT_Treatment)) +
  geom_errorbar(aes(ymin = mean_est_Y1-se_est_Y1, ymax = mean_est_Y1+se_est_Y1, col = PPT_Treatment), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        axis.title.x = element_blank(),
        legend.position = "bottom")+
  facet_grid(Year~PPT_Treatment, scales="free")+
  ylab(bquote(Year~1~Density~(individual/m^2)))+
  labs(col = "Treatment")+
  scale_color_manual(values=c("#34cfeb", "#ebcf34", "#eb6734"))

#annotate f_establish
dat_text1 <- data.frame(
  label = c("a", "a", "a", "a", "a", "a"),
  PPT_Treatment   = factor(c("ambient"), levels = c( "ambient", "moderate", "severe")),
  Year = 2021,
  x     = c(1, 2, 3, 4, 5, 6),
  y     = 210
)
dat_text2 <- data.frame(
  label = c("a", "b", "a", "a", "a", "a"),
  PPT_Treatment   = factor(c("moderate"), levels = c( "ambient", "moderate", "severe")),
  Year = 2021,
  x     = c(1, 2, 3, 4, 5, 6),
  y     = 210
)
dat_text3 <- data.frame(
  label = c("a", "a", "a", "a", "a", "a"),
  PPT_Treatment   = factor(c("severe"), levels = c( "ambient", "moderate", "severe")),
  Year = 2021,
  x     = c(1, 2, 3, 4, 5, 6),
  y     = 210
)
dat_text1_2 <- data.frame(
  label = c("a", "a", "a", "a", "a", "a"),
  PPT_Treatment   = factor(c("ambient"), levels = c( "ambient", "moderate", "severe")),
  Year = 2022,
  x     = c(1, 2, 3, 4, 5, 6),
  y     = 45
)
dat_text2_2 <- data.frame(
  label = c("a", "a", "a", "a", "a", "a"),
  PPT_Treatment   = factor(c("moderate"), levels = c( "ambient", "moderate", "severe")),
  Year = 2022,
  x     = c(1, 2, 3, 4, 5, 6),
  y     = 45
)
dat_text3_2 <- data.frame(
  label = c("a", "a", "a", "a", "a", "a"),
  PPT_Treatment   = factor(c("severe"), levels = c( "ambient", "moderate", "severe")),
  Year = 2022,
  x     = c(1, 2, 3, 4, 5, 6),
  y     = 45
)

f_establish_stats <- f_establish+geom_text(data = dat_text1, mapping = aes(x = x, y = y, label = label))+
  geom_text(data = dat_text2, mapping = aes(x = x, y = y, label = label))+
  geom_text(data = dat_text3, mapping = aes(x = x, y = y, label = label))+
  geom_text(data= dat_text1_2, mapping = aes(x =x, y = y, label = label))+
  geom_text(data= dat_text2_2, mapping = aes(x =x, y = y, label = label))+
  geom_text(data= dat_text3_2, mapping = aes(x =x, y = y, label = label))

# #Summarize FIRST YEAR ESTABLISHMENT by population, PPT x BRTE
# establishment_summary <- emergence %>%
#   filter(Month == "Jul") %>%
#   filter(Year == "2022") %>%
#   mutate(establishment = ELELY1EMG/300)%>%
#   group_by(Year, Population, PPT_Treatment, BRTE_Treatment) %>%
#   summarise(mean_est_Y1 = mean(establishment, na.rm = TRUE),
#             se_est_Y1 = se(establishment)) 
# establishment_summary$Population <- ordered(establishment_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))
# 
# #Visualize FIRST YEAR ESTABLISHMENT by population, PPT x BRTE
# ggplot(establishment_summary, aes(y = mean_est_Y1, x = Population, col = BRTE_Treatment)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = mean_est_Y1-se_est_Y1, ymax = mean_est_Y1+se_est_Y1), width = 0.4, alpha = 0.9, size = 1) +
#   theme(text = element_text(size=15),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
#         axis.title = element_text(size = 15),
#         legend.position = "bottom")+
#   facet_grid(Year~PPT_Treatment, scales="free")+
#   ylab(bquote(italic(E.~elymoides)~Year~1~Establishment~Rate))+
#   labs(col = "Treatment")+
#   scale_color_manual(values=c("#FC46AA", "#808080"))

# #Summarize SECOND YEAR SURVIVAL by population 
# survival_summary <- emergence %>%
#   filter(Month == "Jul") %>%
#   filter(Year == "2022") %>%
#   mutate(survival = ELELY2EMG/300)%>%
#   group_by(Year, Population) %>%
#   summarise(mean_survival_Y2 = mean(survival, na.rm = TRUE),
#             se_survival_Y2 = se(survival)) 
# survival_summary$Population <- ordered(survival_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))
# 
# ggplot(survival_summary, aes(y = mean_survival_Y2, x = Population)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = mean_survival_Y2-se_survival_Y2, ymax = mean_survival_Y2+se_survival_Y2), width = 0.4, alpha = 0.9, size = 1) +
#   theme(text = element_text(size=15),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
#         axis.title = element_text(size = 15),
#         axis.title.x = element_blank(),
#         legend.position = "bottom")+
#   #facet_grid(~PPT_Treatment, scales="free")+
#   ylab(bquote(Year~2~Survival~Rate))
#   #labs(col = "Treatment")+
#   #scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))
# 
# #Summarize SECOND YEAR SURVIVAL by population and PPT 
# survival_summary <- emergence %>%
#   filter(Month == "Jul") %>%
#   filter(Year == "2022") %>%
#   mutate(survival = ELELY2EMG/300)%>%
#   group_by(Year, Population, PPT_Treatment) %>%
#   summarise(mean_survival_Y2 = mean(survival, na.rm = TRUE),
#             se_survival_Y2 = se(survival)) 
# survival_summary$Population <- ordered(survival_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))
# 
# #Visualize SECOND YEAR SURVIVAL by population and PPT
# f_survival <- ggplot(survival_summary, aes(y = mean_survival_Y2, x = Population)) +
#                   geom_point(aes(col = PPT_Treatment)) +
#                   geom_errorbar(aes(ymin = mean_survival_Y2-se_survival_Y2, ymax = mean_survival_Y2+se_survival_Y2, col = PPT_Treatment), width = 0.4, alpha = 0.9, size = 1) +
#                   theme(text = element_text(size=15),
#                         panel.grid.major = element_blank(),
#                         panel.grid.minor = element_blank(),
#                         panel.background = element_blank(),
#                         axis.line = element_line(colour = "black"),
#                         panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
#                         axis.title = element_text(size = 15),
#                         axis.title.x = element_blank(),
#                         legend.position = "bottom")+
#                   facet_grid(~PPT_Treatment, scales="free")+
#                   ylab(bquote(Year~2~Survival~Rate))+
#                   labs(col = "Treatment")+
#                   scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))
#  
# #annotate f_survival
# dat_text1 <- data.frame(
#   label = c("a", "a", "a", "a", "a", "a"),
#   PPT_Treatment   = factor(c("ambient"), levels = c( "ambient", "moderate", "severe")),
#   x     = c(1, 2, 3, 4, 5, 6),
#   y     = c(0.0085, 0.0085, 0.0085, 0.0085, 0.0085, 0.0085)
# )
# dat_text2 <- data.frame(
#   label = c("a", "b", "a", "a", "a", "b"),
#   PPT_Treatment   = factor(c("moderate"), levels = c( "ambient", "moderate", "severe")),
#   x     = c(1, 2, 3, 4, 5, 6),
#   y     = c(0.0085, 0.0085, 0.0085, 0.0085, 0.0085, 0.0085)
# )
# dat_text3 <- data.frame(
#   label = c("a", "a", "a", "a", "a", "a"),
#   PPT_Treatment   = factor(c("severe"), levels = c( "ambient", "moderate", "severe")),
#   x     = c(1, 2, 3, 4, 5, 6),
#   y     = c(0.0085, 0.0085, 0.0085, 0.0085, 0.0085, 0.0085)
# )
# f_survival_stats <- f_survival+geom_text(data = dat_text1, mapping = aes(x = x, y = y, label = label))+
#   geom_text(data = dat_text2, mapping = aes(x = x, y = y, label = label))+
#   geom_text(data = dat_text3, mapping = aes(x = x, y = y, label = label))

# #Summarize SECOND YEAR SURVIVAL by population, PPT x BRTE
# survival_summary <- emergence %>%
#   filter(Month == "Jul") %>%
#   filter(Year == "2022") %>%
#   mutate(survival = ELELY2EMG/300)%>%
#   group_by(Year, Population, PPT_Treatment, BRTE_Treatment) %>%
#   summarise(mean_survival_Y2 = mean(survival, na.rm = TRUE),
#             se_survival_Y2 = se(survival)) 
# survival_summary$Population <- ordered(survival_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))
# 
# #Visualize SECOND YEAR SURVIVAL by population, PPT x BRTE
# ggplot(survival_summary, aes(y = mean_survival_Y2, x = Population, col = BRTE_Treatment)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = mean_survival_Y2-se_survival_Y2, ymax = mean_survival_Y2+se_survival_Y2), width = 0.4, alpha = 0.9, size = 1) +
#   theme(text = element_text(size=15),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
#         axis.title = element_text(size = 15),
#         legend.position = "bottom")+
#   facet_grid(~PPT_Treatment, scales="free")+
#   ylab(bquote(italic(E.~elymoides)~Year~2~Survival~Rate))+
#   labs(col = "Treatment")+
#   scale_color_manual(values=c( "#FC46AA", "#808080"))

#Summarize SECOND YEAR DENSITY by population and PPT 
second_dens_summary <- emergence %>%
  filter(Month == "Jul") %>%
  filter(Year == "2022") %>%
  mutate(survival = ELELY2EMG*4)%>%
  group_by(Year, Population, PPT_Treatment) %>%
  summarise(mean_survival_Y2 = mean(survival, na.rm = TRUE),
            se_survival_Y2 = se(survival)) 
second_dens_summary$Population <- ordered(second_dens_summary$Population, levels = c("Norc", "Roar","Susa","Litt", "Elko", "Vale"))

#ANOVA SECOND YEAR SURVIVAL
summary(aov(survival ~ Population*PPT_Treatment, emergence %>%
              filter(Month == "Jul") %>%
              filter(Year == "2022") %>%
              mutate(survival = ELELY2EMG)))
TukeyHSD(aov(survival ~ Population*PPT_Treatment, emergence %>%
               filter(Month == "Jul") %>%
               filter(Year == "2022") %>%
               mutate(survival = ELELY2EMG)))

second_dens <- emergence %>%
  filter(Month == "Jul") %>%
  filter(Year == "2022") %>%
  mutate(survival = ELELY2EMG*4)%>%
  group_by( PPT_Treatment) %>%
  summarise(mean_survival_Y2 = mean(survival, na.rm = TRUE),
            se_survival_Y2 = se(survival)) 

#Visualize SECOND YEAR DENSITY by population and PPT
f_second_dens <- ggplot(second_dens_summary, aes(y = mean_survival_Y2, x = Population)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_survival_Y2-se_survival_Y2, ymax = mean_survival_Y2+se_survival_Y2), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "right")+
  facet_grid(~PPT_Treatment, scales="free")+
  ylab(expression(Year~2~Density~(individual/m^2)))+
  xlab("Seed Source")+
  labs(col = "Treatment")
  #scale_color_manual(values=c("#34cfeb", "#ebcf34", "#eb6734"))

#annotate f_second_dens
dat_text4 <- data.frame(
  label = c("N.S."),
  PPT_Treatment   = factor(c("ambient"), levels = c( "ambient", "moderate", "severe")),
  x     = c(3.5),
  y     = 10
)
dat_text5 <- data.frame(
  label = c( "*", "*"),
  PPT_Treatment   = factor(c("moderate"), levels = c( "ambient", "moderate", "severe")),
  x     = c( 4, 6),
  y     = 10
)
dat_text6 <- data.frame(
  label = c("N.S."),
  PPT_Treatment   = factor(c("severe"), levels = c( "ambient", "moderate", "severe")),
  x     = c(3.5),
  y     = 10
)

f_second_dens_stats <- f_second_dens+geom_text(data = dat_text4, mapping = aes(x = x, y = y, label = label), size = 4)+
  geom_text(data = dat_text5, mapping = aes(x = x, y = y, label = label),  size = 5)+
  geom_text(data = dat_text6, mapping = aes(x = x, y = y, label = label), size = 4)

#Combine 1st YR total emergence, 1st YR Density, 2nd YR Density plots
ggarrange(f_cum_emergence_stats, f_establish_stats, f_second_dens_stats, ncol = 1, nrow = 3, 
          common.legend = TRUE)


#ONLY 2021 for presentation
first_yr_summary <- left_join(cumulative_summary, establishment_summary)
f_first_yr <- ggplot(first_yr_summary %>% filter(Year == 2021), aes(x = Population)) +
  geom_point(aes(y = mean_max_emerg), color = "grey") +
  geom_errorbar(aes(ymin = mean_max_emerg-se_max_emerg, ymax = mean_max_emerg+se_max_emerg), 
                width = 0.4, alpha = 0.9, size = 1, color = "grey") +
  geom_point(aes(y = mean_est_Y1), color = "black") +
  geom_errorbar(aes(ymin = mean_est_Y1-se_est_Y1, ymax = mean_est_Y1+se_est_Y1), 
                width = 0.4, alpha = 0.9, size = 1, color = "black") +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        axis.title.x = element_blank(),
        legend.position = "right")+
  facet_grid(~PPT_Treatment, scales="free")+
  ylab(expression(Year~1~Density~(individual/m^2)))
  #ylim(0, 50)+
  #labs(col = "Treatment")+
  #scale_color_manual(values=c("#34cfeb", "#ebcf34", "#eb6734"))

#First year total emergence and July survival by cohorts
f_first_yr_cohorts <- ggplot(first_yr_summary, aes(x = Population)) +
  geom_point(aes(y = mean_max_emerg), color = "grey") +
  geom_errorbar(aes(ymin = mean_max_emerg-se_max_emerg, ymax = mean_max_emerg+se_max_emerg), 
                width = 0.4, alpha = 0.9, size = 1, color = "grey") +
  geom_point(aes(y = mean_est_Y1), color = "black") +
  geom_errorbar(aes(ymin = mean_est_Y1-se_est_Y1, ymax = mean_est_Y1+se_est_Y1), 
                width = 0.4, alpha = 0.9, size = 1, color = "black") +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        axis.title.x = element_blank(),
        legend.position = "right")+
  facet_grid(Year~PPT_Treatment, scales="free")+
  ylab(expression(Year~1~Density~(individual/m^2)))
#ylim(0, 50)+
#labs(col = "Treatment")+
#scale_color_manual(values=c("#34cfeb", "#ebcf34", "#eb6734"))

#annotate f_first_yr_cohorts
dat_text7 <- data.frame(
  label = c("N.S."),
  PPT_Treatment   = factor(c("ambient"), levels = c( "ambient", "moderate", "severe")),
  Year = 2021,
  x     = c(3.5),
  y     = 210
)
dat_text8 <- data.frame(
  label = c("*", "**"),
  PPT_Treatment   = factor(c("moderate"), levels = c( "ambient", "moderate", "severe")),
  Year = 2021,
  x     = c(3, 6),
  y     = 210
)
dat_text9 <- data.frame(
  label = c("*", "*"),
  PPT_Treatment   = factor(c("severe"), levels = c( "ambient", "moderate", "severe")),
  Year = 2021,
  x     = c(3, 6),
  y     = 210
)
dat_text7_2 <- data.frame(
  label = c("N.S."),
  PPT_Treatment   = factor(c("ambient"), levels = c( "ambient", "moderate", "severe")),
  Year = 2022,
  x     = c(3.5),
  y     = 33
)
dat_text8_2 <- data.frame(
  label = c("N.S."),
  PPT_Treatment   = factor(c("moderate"), levels = c( "ambient", "moderate", "severe")),
  Year = 2022,
  x     = c(3.5),
  y     = 33
)
dat_text9_2 <- data.frame(
  label = c("N.S."),
  PPT_Treatment   = factor(c("severe"), levels = c( "ambient", "moderate", "severe")),
  Year = 2022,
  x     = c(3.5),
  y     = 33
)
f_first_dens_stats <- f_first_yr_cohorts+geom_text(data = dat_text7, mapping = aes(x = x, y = y, label = label), size = 4)+
  geom_text(data = dat_text8, mapping = aes(x = x, y = y, label = label), size = 5)+
  geom_text(data = dat_text9, mapping = aes(x = x, y = y, label = label), size = 5)+
  geom_text(data= dat_text7_2, mapping = aes(x =x, y = y, label = label))+
  geom_text(data= dat_text8_2, mapping = aes(x =x, y = y, label = label))+
  geom_text(data= dat_text9_2, mapping = aes(x =x, y = y, label = label))
