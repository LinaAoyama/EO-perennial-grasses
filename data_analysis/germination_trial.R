### Packages
library(ggplot2)
library(tidyverse)
library(RColorBrewer)

### Data Import ###
source("data_compiling/compiling_germ_trial.R")

### Tidy Data
# this is a function for calculating standard error
se<-function(x){
  sd(x)/sqrt(length(x))
} 

# Select columns to graph
germ_trial_simple <- germ_trial %>%
  select(Temperature, Week, Germination, Replicate, Population) %>%
  filter(Population %in% c("Norcross", "Vale","Susanville", "Roaring Springs","Elko" ,"Little Sahara"))%>%
  filter(Week != "0")

# Reorder populations by aridity
germ_trial_simple$Population <- ordered(germ_trial_simple$Population, levels = c("Norcross", "Vale","Susanville", "Roaring Springs","Elko" ,"Little Sahara"))

### Visualize raw data
ggplot(germ_trial_simple, aes(x = Temperature, y = Germination/25, col = Week))+
  geom_jitter()+
  facet_grid(~Population)

### Summarize data
germ_trial_summary <- germ_trial_simple %>%
  group_by(Population, Week, Temperature) %>%
  summarise(mean_germination = mean(Germination/25),
            se_germination = se(Germination/25))

### Visualize summarized data
ggplot(germ_trial_summary%>%filter(Week == "2"), aes(x = Temperature, y = mean_germination, col = Population))+
  geom_point()+
  geom_line(size = 1)+
  facet_grid(~Population)+
  geom_errorbar(aes(ymin = mean_germination-se_germination, ymax = mean_germination+se_germination), width = 0.6, alpha = 1, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "none")+
  ylab(bquote(Germination~Rate))+
  xlab(bquote(Temperature*degree*C))+
  scale_color_manual( values = c("#01665E", "#5AB4AC" , "#C7EAE5","#F6E8C3","#D8B365", "#8C510A"  ))

ggplot(germ_trial_summary%>%filter(Week == "2"), aes(x = Temperature, y = mean_germination, col = Population))+
  geom_smooth(method="loess",se=TRUE)+
  geom_point()+
  #geom_line(size = 1)+
  #facet_grid(~Population)+
  #geom_errorbar(aes(ymin = mean_germination-se_germination, ymax = mean_germination+se_germination), width = 0.6, alpha = 1, size = 1) +
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "bottom")+
  ylab(bquote(Mean~Germination~Rate))+
  xlab(bquote(Temperature*degree*C))+
  scale_color_manual( values = c("#01665E", "#5AB4AC" , "#C7EAE5","#F6E8C3","#D8B365", "#8C510A"  ))

brewer.pal(n = 6, name = "BrBG")
