#BIOS14 Midterm: Chamois Model Selection

setwd("~/LU CLASS OF 2026/BIOS13-14/BIOS14- Processing and Analysis of Biological Data/data")
dat = read.table("chamois.txt", header=T)

library(psych)
#pairs.panels(dat)

#mean total horn length and for each sex
dat$hornT <- dat$hornL+dat$hornR
print(mean(dat$hornT))
male <- subset(dat, sex=='M')
female <- subset(dat, sex=='F')
print(mean(male$hornT))
print(mean(female$hornT))

#sex hunted count
sex_count <- table(dat$sex)
print(sex_count)
  # barplot(sex_count, las = 1, xlab = "Sex", ylab = "Total Chamois Hunted", ylim = c(0, 2500))

#barplot chamois hunted each season
all_years <- 1977:2016
season_factor <- factor(dat$season, levels = all_years)
season_count <- table(season_factor)
print(season_count)
barplot(season_count, las = 1, ylim = c(0,250),
        xlab = "Season (Year)", ylab = "Total Chamois Hunted", 
        main = "Chamois Hunted by Season")

#barplot chamois hunted each month
all_months <- c(9,10,11,12,1,2)
month_factor <- factor(dat$month, levels = all_months)
month_count <- table(month_factor)
print(month_count)
barplot(month_count, las = 1, ylim = c(0,1600),
        xlab = "Month", ylab = "Total Chamois Hunted", 
        main = "Chamois Hunted by Month")
#FIX   by sex
mmonth_factor <- factor(male$month, levels = all_months)
mmonth_count <- table(mmonth_factor)
print(mmonth_count)
fmonth_factor <- factor(female$month, levels = all_months)
fmonth_count <- table(fmonth_factor)
print(fmonth_count)

library(ggplot2)
ggplot(dat, aes(x=month, y=month_count, fill=sex)) +
  geom_bar(stat="identity", position="dodge")

#horn length each season by sex
colors = c("blue", "red")
plot(dat$season, dat$hornT, las=1, col=colors[factor(dat$sex)],
     xlab="Season (Year)", ylab="Total Horn Length")
legend("bottomleft", legend=c("males", "females"), lty="solid", pch=1, 
       col=c("red", "blue"))
g <- lm(dat$hornT ~ dat$season)
summary(g)
abline(g, lwd=2)
m <- lm(male$hornT ~ male$season)
summary(m)
abline(m, col="red", lwd=2)
f <- lm(female$hornT ~ female$season)
summary(f)
abline(f, col="blue", lwd=2)

#average age of chamois hunted increased slightly for both sexes
plot(dat$season, dat$age, las=1, pch=20, col=colors[factor(dat$sex)],
     xlab="Season (Year)", ylab="Age of Chamois Hunted")
legend("topleft", legend=c("males", "females"), lty="solid", pch=20, 
       col=c("red", "blue"))
g <- lm(dat$age ~ dat$season)
abline(g, lwd=2)
summary(g)
m <- lm(male$age ~ male$season)
summary(m)
abline(m, col="red", lwd=2)
f <- lm(female$age ~ female$season)
summary(f)
abline(f, col="blue", lwd=2)
print(mean(dat$age))

#FIX ^^ in ggplot
# library(ggplot2)
# ggplot(dat, aes(x=season, y=age, colour=sex) + 
#          geom_point(dat, aes(x=season, y=age, colour=sex))) +
#   geom_line(m, aes(colour="red")) +
#   geom_line(f, aes(colour="blue")) +
#   theme_bw()

#average mass of chamois hunted slightly decreased over time for both sexes
plot(dat$season, dat$mass, las=1, col=colors[factor(dat$sex)], 
     xlab="Season (Year)", ylab="Mass of Chamois Hunted")
legend("topleft", legend=c("males", "females"), lty="solid", pch=1, 
       col=c("red", "blue"))
g <- lm(dat$mass ~ dat$season)
abline(g, lwd=2)
m <- lm(male$mass ~ male$season)
summary(m)
abline(m, col="red", lwd=2)
f <- lm(female$mass ~ female$season)
summary(f)
abline(f, col="blue", lwd=2)
print(mean(dat$mass))

#trivial: mass increases with age
plot(dat$age, dat$mass, las=1)
m <- lm(dat$mass ~ dat$age)
abline(m)

#trivial: horn length increases with age
plot(dat$age, dat$hornT, las=1)
m <- lm(dat$hornT ~ dat$age)
abline(m)

#density does not significantly effect horn length
library(gghalves)
ggplot(dat, aes(x=density, y=hornT, col=sex)) +
  geom_half_boxplot() +
  geom_half_point() +
  theme_bw()

#count of chamois hunted by density at birth
density_count <- table(dat$density)
print(density_count)

#FIX binomial plot for density and sex
# plot(dat$season, dat$density, las=1)
# m <- glm(dat$hornT ~ dat$season, "binomial")
# abline(m)

#horn length of chamois hunted over time by density
#hunting began exclusively low density but became exclusively high density
hd <- subset(dat, density=='high')
ld <- subset(dat, density=='low')
print(mean(hd$hornT))
print(mean(ld$hornT))
colors = c("purple", "orangered")
plot(dat$season, dat$hornT, las=1, col=colors[factor(dat$density)],
     xlab="Season (Year)", ylab="Total Horn Length")
legend("bottomleft", legend=c("high density", "low density"), lty="solid", 
       pch=1, col=c("purple", "orangered"))
g <- lm(dat$hornT ~ dat$season)
summary(g)
abline(g, lwd=2)
h <- lm(hd$hornT ~ hd$season)
summary(h)
abline(h, col="purple", xlim=1998, lwd=2)
l <- lm(ld$hornT ~ ld$season)
summary(l)
abline(l, col="orangered", xlim=2012, lwd=2)
#FIX line ranges

#glm horn length predicted by sex (larger in males), age (larger with age), 
# density (larger at low density) - age and density as poisson
library(MuMIn)
library(MASS)
mod = glm(dat$hornT ~ dat$sex + dat$age + dat$mass + dat$density + dat$season,
          family="poisson")
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)

mod = glm.nb(dat$hornT ~ dat$sex + dat$age + dat$mass + dat$density + dat$season)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)

mod = glm.nb(dat$hornT ~ dat$age * dat$mass + dat$sex + dat$density + dat$season)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)

mod = glm.nb(dat$hornT ~ dat$age * dat$mass + dat$sex + dat$season)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)

mod = glm.nb(dat$hornT ~ dat$age * dat$mass + dat$sex)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)

library(glmmTMB)
mem = glmmTMB(data=dat, hornT ~ age + mass + sex + (1|season))
summary(mem)
r.squaredGLMM(mem)

mem = glmmTMB(data=dat, hornT ~ age + mass + sex + (1|season + density))
summary(mem)
r.squaredGLMM(mem)
