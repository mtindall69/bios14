#BIOS14 13-11 Exercise 3: Butterfly ANOVA

dat = read.csv("~/LU CLASS OF 2026/BIOS13-14/BIOS14- Processing and Analysis of Biological Data/data/butterflies.csv")
  
#dat$MaternalHost = paste0(dat$MaternalHost, "M")
#dat$LarvalHost = paste0(dat$LarvalHost, "L")
#means = tapply(dat$DevelopmentTime, list(dat$MaternalHost, dat$LarvalHost), mean)
#means

#base R
#boxplot(DevelopmentTime ~ LarvalHost + MaternalHost, data=dat)

library(tidyverse)
butt_sum <- dat %>%
  group_by(LarvalHost, MaternalHost) %>%
  summarise(dt_mean = mean(DevelopmentTime),
            dt_sd = sd(DevelopmentTime),
            dt_e_u = dt_mean+dt_sd/sqrt(n()),
            dt_e_l = dt_mean-dt_sd/sqrt(n()))

pair_freq <- as.data.frame(table(dat$LarvalHost, dat$MaternalHost))
# barplot(dat$LarvalHost)
pair_freq$Pair <- paste(pair_freq$Var1, pair_freq$Var2, sep = "-")
ggplot(pair_freq, aes(x = Pair, y = Freq)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  xlab("Larval-Maternal Host Pair") +
  ylab("Frequency") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

library(ggplot2)
ggplot(dat, aes(x=LarvalHost, y=DevelopmentTime, col=MaternalHost)) +
  geom_jitter(alpha=0.2)+
  geom_line(data=butt_sum,aes(x=LarvalHost,colour=MaternalHost,y=dt_mean, 
                              group=MaternalHost))+
  geom_pointrange(data=butt_sum, aes(x=LarvalHost, y=dt_mean, 
                  ymin=dt_e_l, ymax=dt_e_u, color=MaternalHost), size=0.7) +
  theme_bw()

# ggplot(dat, aes(x=LarvalHost, y=DevelopmentTime, col=MaternalHost)) + 
#   geom_boxplot() +
#   # geom_point(shape= 21, position = position_jitterdodge(jitter.width=0.2)) +
#   # scale_x_discrete("variable", drop = FALSE)
#   #geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(0.25))
#   geom_jitter(size=0.8, alpha=0.9) +
#   ggtitle("Host effects on Development Time") +
#   xlab("Larval Host") +
#   theme_bw()

library(gghalves)
ggplot(dat, aes(x=LarvalHost, y=DevelopmentTime, col=MaternalHost)) +
  geom_half_boxplot() +
  geom_half_point() +
  theme_bw()

#one way ANOVA
#summary(lm(DevelopmentTime~LarvalHost-1, data=dat))

#two way ANOVA without interaction
# mod <- aov(DevelopmentTime ~ LarvalHost * MaternalHost, data=dat)
# summary(mod)

#two way ANOVA unbalanced model
summary(lm(DevelopmentTime ~ LarvalHost * MaternalHost, data=dat))
library(car)
mod2 <- lm(DevelopmentTime ~ LarvalHost * MaternalHost, data=dat)
Anova(mod2)

#Growth Rate
butt_sum <- dat %>%
  group_by(LarvalHost, MaternalHost) %>%
  summarise(dt_mean = mean(GrowthRate),
            dt_sd = sd(GrowthRate),
            dt_e_u = dt_mean+dt_sd/sqrt(n()),
            dt_e_l = dt_mean-dt_sd/sqrt(n()))

ggplot(dat, aes(x=LarvalHost, y=GrowthRate, col=MaternalHost)) +
  geom_jitter(alpha=0.2)+
  geom_line(data=butt_sum,aes(x=LarvalHost,colour=MaternalHost,y=dt_mean, 
                              group=MaternalHost))+
  geom_pointrange(data=butt_sum, aes(x=LarvalHost, y=dt_mean, 
                  ymin=dt_e_l, ymax=dt_e_u, color=MaternalHost), size=0.7) +
  theme_bw()

ggplot(dat, aes(x=LarvalHost, y=GrowthRate, col=MaternalHost)) +
  geom_half_boxplot() +
  geom_half_point() +
  theme_bw()

#one way ANOVA
#summary(lm(GrowthRate~LarvalHost-1, data=dat))

#two way ANOVA without interaction
# mod <- aov(GrowthRate ~ LarvalHost * MaternalHost, data=dat)
# summary(mod)

#two way ANOVA unbalanced model
summary(lm(GrowthRate ~ LarvalHost * MaternalHost, data=dat))
mod2 <- lm(GrowthRate ~ LarvalHost * MaternalHost, data=dat)
Anova(mod2)

#Adult Weight
butt_sum <- dat %>%
  group_by(LarvalHost, MaternalHost) %>%
  summarise(dt_mean = mean(AdultWeight),
            dt_sd = sd(AdultWeight),
            dt_e_u = dt_mean+dt_sd/sqrt(n()),
            dt_e_l = dt_mean-dt_sd/sqrt(n()))

ggplot(dat, aes(x=LarvalHost, y=AdultWeight, col=MaternalHost)) +
  geom_jitter(alpha=0.2)+
  geom_line(data=butt_sum,aes(x=LarvalHost,colour=MaternalHost,y=dt_mean, 
                              group=MaternalHost))+
  geom_pointrange(data=butt_sum, aes(x=LarvalHost, y=dt_mean, 
                  ymin=dt_e_l, ymax=dt_e_u, color=MaternalHost), size=0.7) +
  theme_bw()

ggplot(dat, aes(x=LarvalHost, y=AdultWeight, col=MaternalHost)) +
  geom_half_boxplot() +
  geom_half_point() +
  theme_bw()

#one way ANOVA
#summary(lm(AdultWeight~LarvalHost-1, data=dat))

#two way ANOVA without interaction
# mod <- aov(AdultWeight ~ LarvalHost * MaternalHost, data=dat)
# summary(mod)

#two way ANOVA unbalanced model
summary(lm(AdultWeight ~ LarvalHost * MaternalHost, data=dat))
mod2 <- lm(AdultWeight ~ LarvalHost * MaternalHost, data=dat)
Anova(mod2)
