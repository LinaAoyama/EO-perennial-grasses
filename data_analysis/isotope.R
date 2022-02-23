#First, load data from compiling_isotope. We'll start with data visualization. 
library(ggplot2)
library(ggpubr)
library(multcomp)

isotope <- isotope_data %>%
  mutate(Population = factor(Population, levels = c("Norcross", "Vale", "Susanville", "Roaring Spring", "Elko", "Little Sahara"))) %>%
  mutate(Month = factor(Month, levels = c("June", "July")))

ggplot(isotope, aes(x = Population, y = delta_13C)) +
  geom_jitter(aes(color = Treatment)) +
  geom_boxplot(alpha = 0)+
  facet_grid(~Month)+
  scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))+
  theme(text = element_text(size=15),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
      axis.title = element_text(size = 15),
      legend.position = "right")

#No sig difference by population in either June or July
summary(lm(delta_13C ~ Population, isotope%>%filter(Month == "June"))) 
summary(lm(delta_13C ~ Population, isotope%>%filter(Month == "July")))

ggplot(isotope, aes(x = Treatment, y = delta_13C)) +
  geom_jitter(aes(color = Population)) +
  geom_boxplot(alpha = 0)+
  facet_grid(~Month)+
  #scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))+
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "right")+
  annotate("text", x = 1, y = -25, label = "a")+
  annotate("text", x = 2:3, y = -25, label = "b")

#Sig higher delta-13C in ambient than in moderate or severe
summary(lm(delta_13C ~ Treatment, isotope%>%filter(Month == "June")))
TukeyHSD(aov(delta_13C ~ Treatment, isotope%>%filter(Month == "June")))
summary(lm(delta_13C ~ Treatment, isotope%>%filter(Month == "July")))
TukeyHSD(aov(delta_13C ~ Treatment, isotope%>%filter(Month == "July")))
