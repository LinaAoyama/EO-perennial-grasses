### Packages
library(tidyverse)
library(ggplot2)

### Data Import
source("data_compiling/compiling_demography.R")

# this is a function for calculating standard error
se<-function(x){
  sd(x)/sqrt(length(x))
} 

#300 seeds sown per subplot

#Summarized by month
emergence_overall <- emergence %>%
  rename(March = March_23_2021_emergence_count,
                                         April = April_13_2021_emergence_count, 
                                         May = May_9_2021_emergence_count,
                                         June = June_18_2021_emergence_count,
                                         July = July_15_2021_emergence_count) %>%
  gather(key = "Time", value = "Emergence", March, April, May, June, July)%>%
  group_by(Time, PPT_Treatment) %>%
  summarise(mean_emergence = mean(Emergence), 
            se_emergence = se(Emergence)) 
emergence_overall$Time <- ordered(emergence_overall$Time, levels = c("March", "April", "May", "June", "July"))

#Visualize by month
ggplot(emergence_overall%>%filter(PPT_Treatment == "ambient"), aes(y = mean_emergence, x = Time, col = PPT_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_emergence-se_emergence, ymax = mean_emergence+se_emergence), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15))+
  ylab(bquote(italic(E.~elymoides)~Seedling~Stem~Count))+
  scale_color_manual(values=c("#F1C40F"))+
  labs(col = "Treatment")+
  ylim(0, 30)

#Summarized by population and month
emergence_summary <- emergence %>%
  rename(March = March_23_2021_emergence_count,
         April = April_13_2021_emergence_count, 
         May = May_9_2021_emergence_count,
         June = June_18_2021_emergence_count,
         July = July_15_2021_emergence_count) %>%
  gather(key = "Time", value = "Emergence", March, April, May, June, July)%>%
  group_by(Population, PPT_Treatment, Time) %>%
  summarise(mean_emergence = mean(Emergence), 
            se_emergence = se(Emergence)) 
emergence_summary$Population <- ordered(emergence_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))
emergence_summary$Time <- ordered(emergence_summary$Time, levels = c("March", "April", "May", "June", "July"))

#Visualize seedling stem counts by pop
ggplot(emergence_summary, aes(y = mean_emergence, x = Time, col = PPT_Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_emergence-se_emergence, ymax = mean_emergence+se_emergence), width = 0.4, alpha = 0.9, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  facet_grid(~Population)+
  ylab(bquote(italic(E.~elymoides)~Seedling~Stem~Count))+
  labs(col = "Treatment")+
  ylim(0, 50)+
  scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))

