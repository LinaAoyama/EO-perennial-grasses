#First, load data from compiling_isotope. We'll start with data visualization. 
library(ggplot2)
library(ggpubr)
library(multcomp)

isotope <- isotope_data %>%
  mutate(Population = factor(Population, levels = c("Norc", "Vale", "Susa", "Roar", "Elko", "Litt"))) %>%
  mutate(Time = factor(Time, levels = c("seed", "June_2021", "July_2021", "July_2022"))) %>%
  filter(Treatment != "seed")

#Delta 13C by population and time
p1 <- ggplot(isotope %>% filter(Time != "June_2021"), aes(x = Population, y = delta_13C)) +
  geom_jitter(aes(color = Treatment)) +
  geom_boxplot(alpha = 0)+
  facet_grid(~Time)+
  scale_color_manual(values=c("#34cfeb", "#ebcf34", "#eb6734"))+
  theme(text = element_text(size=15),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
      axis.title = element_text(size = 15),
      legend.position = "top")+
  ylab(bquote(Delta^13*C))+
  xlab("Seed Source")

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
  label = c("a", "a", "a", "a", "a", "a"),
  Time   = factor(c("June_2021"), levels = c( "June_2021", "July_2021", "July_2022")),
  x     = c(1, 2, 3, 4, 5, 6),
  y     = c(-24, -24,-24, -24, -24, -24)
)
dat_text2 <- data.frame(
  label = c("a", "a", "a", "a", "a", "a"),
  Time   = factor(c("July_2021"), levels = c( "June_2021", "July_2021", "July_2022")),
  x     = c(1, 2, 3, 4, 5, 6),
  y     = -24.5
)
dat_text3 <- data.frame(
  label = c("ab", "a", "a", "ab", "ab", "b"),
  Time   = factor(c("July_2022"),levels = c( "June_2021", "July_2021", "July_2022")),
  x     = c(1, 2, 3, 4, 5, 6),
  y     = -24.5
)
p1+geom_text(data = dat_text2, mapping = aes(x = x, y = y, label = label))+
  geom_text(data = dat_text3, mapping = aes(x = x, y = y, label = label))


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

