#First, load data from compiling_isotope. We'll start with data visualization. 
library(ggplot2)
library(ggpubr)
library(multcomp)
library(lme4)

isotope <- isotope_data %>%
  mutate(Population = factor(Population, levels = c("Norc", "Roar","Susa","Litt", "Elko", "Vale"))) %>%
  mutate(Time = factor(Time, levels = c("seed", "June_2021", "July_2021", "July_2022"))) %>%
  filter(Treatment != "seed")

# this is a function for calculating standard error
se<-function(x){
  sd(x)/sqrt(length(x))
} 

isotope_summary <- isotope %>%
  group_by(Time, Population) %>%
  summarize(mean_13C = mean(delta_13C),
            se_13C = se(delta_13C))


#Delta 13C by population and time
p1 <- ggplot(isotope_summary %>% filter(Time != "June_2021"), aes(x = Population, y = mean_13C)) +
  geom_point()+
  geom_errorbar(aes(ymin = mean_13C-se_13C, ymax = mean_13C+se_13C), width = 0.4, alpha = 0.9, size = 1) +
  facet_grid(~Time)+
  theme(text = element_text(size=15),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
      axis.title = element_text(size = 15),
      legend.position = "top")+
  ylab(bquote(Delta^13*C))+
  xlab("Seed Source")+
  scale_y_reverse()

#2021: Overall lower delta 13C in July than June. No sig difference by population in either June or July
summary(aov(delta_13C ~ Population*Month, isotope%>%filter(Year == "2021"))) #lower delta C in July than June
summary(aov(delta_13C ~ Population, isotope%>%filter(Year == "2021")%>%filter(Month == "July"))) #no pop diff in 2021 July
summary(aov(delta_13C ~ Population, isotope%>%filter(Year == "2021")%>%filter(Month == "June"))) #no pop diff in 2021 June
#2022: Lower delta 13C in Little Sahara compared to other populations.
summary(aov(delta_13C ~ Population, isotope%>%filter(Year == "2022"))) #sig pop diff in 2022
summary(aov(delta_13C ~ Population*Year, isotope%>%filter(Month == "July")))
summary(lm(delta_13C ~ Population, isotope%>%filter(Year == "2022")))
TukeyHSD(aov(delta_13C ~ Population, isotope%>%filter(Year == "2022")))

#annotate p1
dat_text1 <- data.frame(
  label = c("N.S."),
  Time   = factor(c("July_2021"), levels = c( "June_2021", "July_2021", "July_2022")),
  x     = c(3.5),
  y     = c(-25)
)
dat_text2 <- data.frame(
  label = c("*"),
  Time   = factor(c("July_2022"), levels = c( "June_2021", "July_2021", "July_2022")),
  x     = c(4),
  y     = -25
)

p1+geom_text(data = dat_text1, mapping = aes(x = x, y = y, label = label))+
  geom_text(data = dat_text2, mapping = aes(x = x, y = y, label = label), size = 5)


#Delta 13C by precipitation treatment
p2 <- ggplot(isotope, aes(x = Treatment, y = delta_13C, fill = Treatment)) +
  geom_boxplot(alpha = 0.8)+
  geom_jitter() +
  facet_grid(~Time)+
  scale_fill_manual(values=c("#F1C40F", "#F39C12", "#D35400"))+
  theme(text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        legend.position = "right")+
  ylab(bquote(Delta^13*C))

#Sig higher delta-13C in ambient than in moderate or severe in 2021 but no sig diff in 2022
summary(lm(delta_13C ~ Treatment, isotope%>%filter(Month == "June")%>%filter(Year == "2021")))
TukeyHSD(aov(delta_13C ~ Treatment, isotope%>%filter(Month == "June")%>%filter(Year == "2021")))
summary(lm(delta_13C ~ Treatment, isotope%>%filter(Month == "July")%>%filter(Year == "2021")))
TukeyHSD(aov(delta_13C ~ Treatment, isotope%>%filter(Month == "July")%>%filter(Year == "2021")))
summary(lm(delta_13C ~ Treatment, isotope%>%filter(Year == "2022")))
TukeyHSD(aov(delta_13C ~ Treatment, isotope%>%filter(Year == "2022")))

#annotate p2
dat_text4 <- data.frame(
  label = c("a", "b", "b"),
  Time   = factor(c("June_2021"), levels = c( "June_2021", "July_2021", "July_2022")),
  x     = c(1, 2, 3),
  y     = c(-24, -24,-24),
  Treatment = factor(c("ambient", "moderate", "severe"), levels = c( "ambient", "moderate", "severe"))
)
dat_text5 <- data.frame(
  label = c("a", "b", "b"),
  Time   = factor(c("July_2021"), levels = c( "June_2021", "July_2021", "July_2022")),
  x     = c(1, 2, 3),
  y     = c(-24, -24,-24),
  Treatment = factor(c("ambient", "moderate", "severe"), levels = c( "ambient", "moderate", "severe"))
)
dat_text6 <- data.frame(
  label = c("a", "a", "a"),
  Time   = factor(c("July_2022"), levels = c( "June_2021", "July_2021", "July_2022")),
  x     = c(1, 2, 3),
  y     = c(-24, -24,-24),
  Treatment = factor(c("ambient", "moderate", "severe"), levels = c( "ambient", "moderate", "severe"))
)
p2+geom_text(data = dat_text4, mapping = aes(x = x, y = y, label = label))+
  geom_text(data = dat_text5, mapping = aes(x = x, y = y, label = label))+
  geom_text(data = dat_text6, mapping = aes(x = x, y = y, label = label))

