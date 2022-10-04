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

#Summarized by population and month
emergence_summary <- emergence %>%
  group_by(Year, Month, Population, PPT_Treatment) %>%
  summarise(mean_emergence_Y1 = mean(ELELY1EMG, na.rm = TRUE)/300, 
            se_emergence_Y1 = se(ELELY1EMG)/300,
            mean_emergence_Y2 = mean(ELELY2EMG, na.rm = TRUE)/300,
            se_emergence_Y2 = se(ELELY2EMG)/300,
            mean_mortality_Y1 = mean(ELELY1MOR, na.rm = TRUE)/300,
            se_mortality_Y1 = se(ELELY1MOR)/300,
            mean_mortality_Y2 = mean(ELELY2MOR, na.rm = TRUE)/300,
            se_mortality_Y2 = se(ELELY2MOR)/300) 
emergence_summary$Population <- ordered(emergence_summary$Population, levels = c("Norc", "Vale","Susa", "Roar","Elko" ,"Litt"))
emergence_summary$Month <- ordered(emergence_summary$Month, levels = c("Feb", "Mar", "Apr", "May", "Jun", "Jul"))

#Visualize by month
#Visualize seedling stem counts by pop
ggplot(emergence_summary%>%filter(Year == "2021"), aes(y = mean_emergence_Y1, x = Month, col = PPT_Treatment)) +
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
  facet_grid(~Population)+
  ylab(bquote(italic(E.~elymoides)~Seedling~Stem~Count))+
  labs(col = "Treatment")+
  #ylim(0, 50)+
  scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))

ggplot(emergence_summary%>%filter(Year == "2022"), aes(y = mean_emergence_Y1, x = Month, col = PPT_Treatment)) +
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
  facet_grid(~Population)+
  ylab(bquote(italic(E.~elymoides)~Seedling~Stem~Count))+
  labs(col = "Treatment")+
  #ylim(0, 50)+
  scale_color_manual(values=c("#F1C40F", "#F39C12", "#D35400"))
