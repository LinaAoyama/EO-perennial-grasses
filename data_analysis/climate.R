
### Data Import
source("data_compiling/compiling_climate.R")

# Packages
library(tidyverse) #data wrangling
library(ggplot2) #plot
library(ggpubr) #combine plots
library(vegan) #nmds
library(corrplot) #correlation matrix
library(dplyr)

#PCA of climate variables

climate_matrix <- as.matrix(climate[,2:ncol(climate)]) 
pca_climate = rda(climate_matrix, scale = TRUE) #run PCA on all traits
biplot(pca_climate, display = c("sites", "species"), type = c("text", "points")) #plot biplot
pca_climate_scores <- as.data.frame(scores(pca_climate, choices=c(1,2), display=c("sites"))) #extract pca1 and pca2 scores
pca_climate_scores_lab = as.data.frame(cbind(climate[,1],pca_climate_scores))  #add plot info back
pca_climate_scores_lab$Population <- ordered(as.factor(pca_climate_scores_lab$Population), levels = c("Norc","NGBER", "Vale", "Susa",  "Roar",
                                                                                                  "Elko", "Litt"))
envout<-as.data.frame(scores(pca_climate, choices=c(1,2), display=c("species")))
summary(pca_climate)
ggplot(pca_climate_scores_lab, aes(x = PC1, y = PC2))+
  geom_point(size = 4, aes(colour = Population), alpha = 0.5)+
  theme(text = element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15))+
  geom_segment(data = envout, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               alpha = 0.5, size = 1, colour = "grey30") +
  geom_text(data = envout, aes(x = PC1, y = PC2), colour = "grey30",
            fontface = "bold", label = row.names(envout), size = 5)+
  xlim(-2, 2.3)+
  xlab("PC1 (50.7%)")+
  ylab("PC2 (29.5%)")