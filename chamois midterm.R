#BIOS14 Midterm: Chamois Model Selection

setwd("~/LU CLASS OF 2026/BIOS13-14/BIOS14- Processing and Analysis of Biological Data/data")
dat = read.table("chamois.txt", header=T)

#library(psych)
#pairs.panels(dat)

#mean, sd total horn length and for each sex
dat$hornT <- dat$hornL+dat$hornR
cat(mean(dat$hornT), sd(dat$hornT))
male <- subset(dat, sex=='M')
female <- subset(dat, sex=='F')
cat(mean(male$hornT), sd(male$hornT))
cat(mean(female$hornT), sd(female$hornT))

#sex hunted count
sex_count <- table(dat$sex)
print(sex_count)
  # barplot(sex_count, las = 1, xlab = "Sex", ylab = "Total Chamois Hunted", ylim = c(0, 2500))

#age hunted count
age_count <- table(dat$age)
print(age_count)
barplot(age_count, las = 1, ylim=c(0,1200),
        xlab = "Age", ylab = "Total Chamois Hunted", 
        main = "Chamois Hunted by Age")

#barplot chamois hunted each season
all_years <- 1977:2016
season_factor <- factor(dat$season, levels = all_years)
season_count <- table(season_factor)
print(season_count)
barplot(season_count, las = 1, ylim = c(0,250),
        xlab = "Season (Year)", ylab = "Total Chamois Hunted", 
        main = "Chamois Hunted by Season")
#   + by sex - some seasons far more males but some seasons more females
mseason_factor <- factor(male$season, levels = all_years)
mseason_count <- table(mseason_factor)
print(mseason_count)
fseason_factor <- factor(female$season, levels = all_years)
fseason_count <- table(fseason_factor)
print(fseason_count)
dat$season <- factor(dat$season, levels = all_years)
ct <- table(dat$sex, dat$season)
barplot(ct, beside = TRUE, ylim = c(0,130),
        xlab = "Season (Year)", ylab = "Total Chamois Hunted", 
        main = "Chamois Hunted by Season and Sex",
        col = c("steelblue", "darkorange")) 
legend("topleft", legend=c("Females", "Males"), pch=15, 
       col=c("steelblue", "darkorange"))

#barplot chamois hunted each month - sharp drop from 10-11
all_months <- c(9,10,11,12,1,2)
month_factor <- factor(dat$month, levels = all_months)
month_count <- table(month_factor)
print(month_count)
barplot(month_count, las = 1, ylim = c(0,1600),
        xlab = "Month", ylab = "Total Chamois Hunted", 
        main = "Chamois Hunted by Month")
#   + by sex - months 12-2 have greater proportion females
mmonth_factor <- factor(male$month, levels = all_months)
mmonth_count <- table(mmonth_factor)
print(mmonth_count)
fmonth_factor <- factor(female$month, levels = all_months)
fmonth_count <- table(fmonth_factor)
print(fmonth_count)
dat$month <- factor(dat$month, levels = all_months)
ct <- table(dat$sex, dat$month)
barplot(ct, beside = TRUE, ylim = c(0,1000),
        xlab = "Month", ylab = "Total Chamois Hunted", 
        main = "Chamois Hunted by Month and Sex",
        col = c("steelblue", "darkorange"), legend = c("Females", "Males"))

#reset data frame
dat = read.table("chamois.txt", header=T)
dat$hornT <- dat$hornL+dat$hornR
male <- subset(dat, sex=='M')
female <- subset(dat, sex=='F')

#horn length each season by sex
colors = c("blue", "red")
plot(dat$season, dat$hornT, las=1, col=colors[factor(dat$sex)],
     xlab="Season (Year)", ylab="Total Horn Length (mm)",
     main="Horn Length of Chamois by Season and Sex")
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
     xlab="Season (Year)", ylab="Age (Years)",
     main="Age of Chamois by Season and Sex")
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

#average mass of chamois hunted slightly decreased over time for both sexes
plot(dat$season, dat$mass, las=1, col=colors[factor(dat$sex)], 
     xlab="Season (Year)", ylab="Body Mass (kg)",
     main="Mass of Chamois by Season and Sex")
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

#mass increases with age
plot(dat$age, dat$mass, las=1, main="Age to Body Mass of Chamois",
     xlab="Age (Years)", ylab="Body Mass (kg)")
m <- lm(dat$mass ~ dat$age)
summary(m)
abline(m)

#horn length increases with age
plot(dat$age, dat$hornT, las=1, main="Age to Horn Length of Chamois",
     xlab="Age (Years)", ylab="Total Horn Length (mm)")
m <- lm(dat$hornT ~ dat$age)
summary(m)
abline(m)

#horn length increases with mass
plot(dat$mass, dat$hornT, las=1, main="Body Mass to Horn Length of Chamois",
     xlab="Body Mass (kg)", ylab="Total Horn Length (mm)")
m <- lm(dat$hornT ~ dat$mass)
summary(m)
abline(m)

#density does not significantly effect horn length
library(ggplot2)
library(gghalves)
ggplot(dat, aes(x=density, y=hornT, col=sex)) +
  geom_half_boxplot() +
  geom_half_point() +
  labs(x="Density", y="Total Horn Length (mm)") +
  ggtitle("Chamois Population Density vs Horn Length") +
  theme_bw()

#count of chamois hunted by density at birth and mean horn length by density
density_count <- table(dat$density)
print(density_count)
hd <- subset(dat, density=='high')
ld <- subset(dat, density=='low')
cat(mean(hd$hornT), sd(hd$hornT))
cat(mean(ld$hornT), sd(ld$hornT))

#logistic regression for density
#hunting began exclusively low density but became exclusively high density
colors = c("orangered", "purple")
dat$density <- ifelse(dat$density == "high", 1, 0)
plot(dat$season, dat$density, las=1, pch=19, col=colors[factor(dat$density)],
     xlab="Season (Year)", ylab="Population Density at Birth",
     main="Chamois Population Density vs Season", ylim=c(0,1))
legend("topleft", legend=c("Low","High"), col=c("orangered", "purple"), pch=19)
m <- glm(dat$density ~ dat$season, "binomial")
summary(m)
coefs = summary(m)$coef
logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))
x = dat$season
x_pred = seq(from=min(x), to=max(x), by=0.01)
y_hat = coefs[1,1] + coefs[2,1]*x_pred
p_hat = invlogit(y_hat)
lines(x_pred, p_hat, type="l")
abline(h=0.5, v=(-coefs[1,1])/coefs[2,1], lty=2)
cat(-coefs[1,1]/coefs[2,1], "\n")

#reset data frame
dat = read.table("chamois.txt", header=T)
dat$hornT <- dat$hornL+dat$hornR
male <- subset(dat, sex=='M')
female <- subset(dat, sex=='F')

#glm horn length predicted by sex (larger in males), age (larger with age), 
# mass (related to age and larger with mass), density (related to season, starts
# all low in 77 but all high by 2016) - sex and density as poisson
library(MuMIn)
library(MASS)
mod = glm(dat$hornT ~ dat$sex + dat$age + dat$mass + dat$density + dat$season,
          family="poisson")
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
#overdispersed - AIC: 87673  r2: 0.51

mod = glm.nb(dat$hornT ~ dat$sex + dat$age + dat$mass + dat$density + dat$season)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
# AIC: 50775  r2: 0.45

mod = glm.nb(dat$hornT ~ dat$age * dat$mass + dat$sex + dat$density + dat$season)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
# AIC: 49742  r2: 0.5588

mod = glm.nb(dat$hornT ~ dat$age * dat$mass + dat$sex + dat$density + dat$month + dat$season)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
# AIC: 49740  r2: 0.5592

mod = glm.nb(dat$hornT ~ dat$age * dat$mass + dat$sex * dat$month + dat$density + dat$season)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
# AIC: 49741  r2: 0.5593

mod = glm.nb(dat$hornT ~ dat$age * dat$mass + dat$sex + dat$season)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
# AIC: 49759  r2: 0.557

mod = glm.nb(dat$hornT ~ dat$age * dat$mass + dat$sex + dat$density)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
# AIC: 49753  r2: 0.558

mod = glm.nb(dat$hornT ~ dat$age * dat$mass + dat$sex)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
# AIC: 49757 r2: 0.557

library(glmmTMB)
mem = glmmTMB(data=dat, hornT ~ age + mass + sex + (1|season))
summary(mem)
r.squaredGLMM(mem)
# AIC: 47650  r2: 0.61/0.62

mem = glmmTMB(data=dat, hornT ~ age + mass + sex + (1|density))
summary(mem)
r.squaredGLMM(mem)
# AIC: 47738  r2: 0.61/0.61

mem = glmmTMB(data=dat, hornT ~ age + mass + sex + (1|season + density))
summary(mem)
r.squaredGLMM(mem)
# AIC: 47632  r2: 0.61/0.63

mem = glmmTMB(data=dat, hornT ~ age * mass + sex + month + (1|season + density))
summary(mem)
r.squaredGLMM(mem)
# AIC: 46611  r2: 0.687/0.712
# month does not substantially increase r2

mem = glmmTMB(data=dat, hornT ~ age * mass + sex + (1|season + density))
summary(mem)
r.squaredGLMM(mem)
# AIC: 46613  r2: 0.687/0.712

# percent variance explained by each parameter
library(performance)
# age marginal r2
r2_full <- r2_nakagawa(mem)
rmem = glmmTMB(data=dat, hornT ~ mass + sex + (1|season + density))
r2_r <- r2_nakagawa(rmem)
pe = (r2_full$R2_marginal - r2_r$R2_marginal)/r2_full$R2_marginal
print(pe)
pe = (r2_full$R2_conditional - r2_r$R2_conditional)/r2_full$R2_conditional
print(pe)

# mass marginal r2
r2_full <- r2_nakagawa(mem)
rmem = glmmTMB(data=dat, hornT ~ age + sex + (1|season + density))
r2_r <- r2_nakagawa(rmem)
pe = (r2_full$R2_marginal - r2_r$R2_marginal)/r2_full$R2_marginal
print(pe)
pe = (r2_full$R2_conditional - r2_r$R2_conditional)/r2_full$R2_conditional
print(pe)

# sex marginal r2
r2_full <- r2_nakagawa(mem)
rmem = glmmTMB(data=dat, hornT ~ age + mass + (1|season + density))
r2_r <- r2_nakagawa(rmem)
pe = (r2_full$R2_marginal - r2_r$R2_marginal)/r2_full$R2_marginal
print(pe)
pe = (r2_full$R2_conditional - r2_r$R2_conditional)/r2_full$R2_conditional
print(pe)

# age:mass marginal r2
r2_full <- r2_nakagawa(mem)
rmem = glmmTMB(data=dat, hornT ~ age + mass + sex + (1|season + density))
r2_r <- r2_nakagawa(rmem)
pe = (r2_full$R2_marginal - r2_r$R2_marginal)/r2_full$R2_marginal
print(pe)
pe = (r2_full$R2_conditional - r2_r$R2_conditional)/r2_full$R2_conditional
print(pe)

#seasonal conditional r2
r2_full <- r2_nakagawa(mem)
rmem = glmmTMB(data=dat, hornT ~ age + mass + sex + (1|density))
r2_r <- r2_nakagawa(rmem)
pe = (r2_full$R2_marginal - r2_r$R2_marginal)/r2_full$R2_marginal
print(pe)
pe = (r2_full$R2_conditional - r2_r$R2_conditional)/r2_full$R2_conditional
print(pe)

#density conditional r2
r2_full <- r2_nakagawa(mem)
rmem = glmmTMB(data=dat, hornT ~ age + mass + sex + (1|season))
r2_r <- r2_nakagawa(rmem)
pe = (r2_full$R2_marginal - r2_r$R2_marginal)/r2_full$R2_marginal
print(pe)
pe = (r2_full$R2_conditional - r2_r$R2_conditional)/r2_full$R2_conditional
print(pe)
