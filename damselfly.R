#BIOS14 15-1-25 FINAL: Damselflies

setwd("~/LU CLASS OF 2026/BIOS13-14/BIOS14- Processing and Analysis of Biological Data/data")

male_CS = read.csv("male_CS.csv")
male_CV = read.csv("male_CV.csv")
female_CS = read.csv("female_CS.csv")
female_CV = read.csv("female_CV.csv")
library(dplyr)
all_data = bind_rows(male_CS, male_CV, female_CS, female_CV)
cs_data = bind_rows(male_CS, female_CS)
cv_data = bind_rows(male_CV, female_CV)
male_data = bind_rows(male_CS, male_CV)
female_data = bind_rows(female_CS, female_CV)

#year weight
yr_count <- table(all_data$year)
print(yr_count)
# 2011 2012 2013 2014 2015 
# 229  561  795  771 1262 

#copulation success by year
cop_count <- table(all_data$cop, all_data$year)
percent_cop <- sweep(cop_count,2,yr_count,FUN="/")
print(percent_cop)
# 2012 had significantly higher success

#Species weights
sp_count <- table(all_data$sp)
print(sp_count)
#CS = 3025 ; CV = 593 - 84% CS

#species by year
sp_yr <- table(all_data$sp, all_data$year)
print(sp_yr)
print(prop.table(sp_yr, margin=2))
# CV decreasing over time but significantly fewer in 2014

#Sex weights
sex_count <- table(all_data$sex)
print(sex_count)
#F = 1234, M = 2384 - 66% M
cssex_count <- table(cs_data$sex)
cvsex_count <- table(cv_data$sex)
male_count <- table(male_data$sp)
female_count <- table(female_data$sp)

#Species/Sex plot
ct <- table(all_data$sex, all_data$sp)
print(ct)
#CS: M = 2035; F = 990 - 67% M
#CS: M = 349; F = 244 - 59% M
barplot(ct, beside = TRUE, ylim = c(0,2500),
        xlab = "Species", ylab = "Total Damselflies", 
        main = "Damselflies by Species and Sex",
        col = c("steelblue", "darkorange")) 
legend("topright", legend=c("Females", "Males"), pch=15, 
       col=c("steelblue", "darkorange"))

#Copulation success by species
cop_count <- table(all_data$cop, all_data$sp)
percent_cop <- sweep(cop_count,2,sp_count,FUN="/")
print(percent_cop)
#CV success higher than than CS (31.7% to 21.6%)
#female success higher than male success for both species
success = all_data[all_data$cop=="1",]
st <- table(success$sex, success$sp)
pst <- (st/ct)
print(pst)
barplot(pst, beside = TRUE, ylim = c(0,0.5),
        xlab = "Species", ylab = "Percent Mating Success", 
        main = "Damselfly Mating Success",
        col = c("steelblue", "darkorange")) 
legend("topleft", legend=c("Females", "Males"), pch=15, 
       col=c("steelblue", "darkorange"))

#body length
cat("TBL mean: ", mean(all_data$tbl))
cat("CS TBL mean: ", mean(cs_data$tbl), "F: ", mean(female_CS$tbl),
    "M: ", mean(male_CS$tbl), "\n")
cat("CV TBL mean: ", mean(cv_data$tbl), "F: ", mean(female_CV$tbl), 
    "M: ", mean(male_CV$tbl))

#thorax width
cat("THORW mean: ", mean(all_data$thorw))
cat("CS THORW mean: ", mean(cs_data$thorw), "F: ", mean(female_CS$thorw),
    "M: ", mean(male_CS$thorw), "\n")
cat("CV THORW mean: ", mean(cv_data$thorw), "F: ", mean(female_CV$thorw), 
    "M: ", mean(male_CV$thorw))

library(gghalves)
# ggplot(all_data, aes(x=sp, y=tbl, col=sex)) +
#   geom_half_boxplot() +
#   #geom_half_point() +
#   xlab("Species") +
#   ylab("Total Body Length (mm)") +
#   theme_bw()
## TBL NOT SIGNIFICANT

ggplot(all_data, aes(x=sp, y=thorw, col=sex)) +
  geom_half_boxplot() +
  #geom_half_point() +
  xlab("Species") +
  ylab("Thorax Width (mm)") +
  theme_bw()

#patch size to wing length
all_data$pa <- all_data$fpl * all_data$fpw # patch area
all_data$par <- all_data$fpw * all_data$fwl # patch aspect ratio
all_data$pp <- all_data$fpl/all_data$fwl # proportion patch
all_data$ptw <- all_data$pa/all_data$fwl # patch area to wing length
male_CS$ptw <- (male_CS$fpl * male_CS$fpw)/male_CS$fwl

# library(psych)
# pairs.panels(all_data |> dplyr::select(year, sp, sex, cop, thorl, thorw, 
#                                        fwl, hwl, fpl, fpw, pp, pa, par, ptw))
# # sex, thorax width, wing length, year
# 
# pairs.panels(cs_data |> dplyr::select(year, sex, cop, tbl, abl, 
#                                        thorl, thorw, fwl, hwl, fpl, fpw))
# # sex, year, thorax width, wing length
# 
# pairs.panels(cv_data |> dplyr::select(year, sex, cop, tbl, abl, 
#                                        thorl, thorw, fwl, hwl))
# # sex, wing length, thorax width
# 
# pairs.panels(male_data |> dplyr::select(year, sp, cop, tbl, abl, 
#                                        thorl, thorw, fwl, hwl, fpl, fpw))
# # thorax width, year, thorax width, forewing length
# 
# pairs.panels(male_CS |> dplyr::select(year, cop, tbl, abl, thorl, thorw, fwl, 
#                                       hwl, fpl, fpw))
# # thorax width, year, thorax length, forewing length
# 
# pairs.panels(female_data |> dplyr::select(year, sp, cop, tbl, abl, thorl, 
#                                           thorw, fwl, hwl))
# # thorax width, wing length, thorax width, year, species


# thorax width vs mating success
plot(all_data$thorw, all_data$cop, las=1, pch=1, xlab="Thorax Width (mm)",
     ylab="Mating Success")
m <- glm(all_data$cop ~ all_data$thorw, "binomial")
summary(m)
coefs = summary(m)$coef
logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))
x = all_data$thorw
x_pred = seq(from=min(x), to=max(x), by=0.01)
y_hat = coefs[1,1] + coefs[2,1]*x_pred
p_hat = invlogit(y_hat)
lines(x_pred, p_hat, type="l", lwd=2)
abline(h=0.5, v=(-coefs[1,1])/coefs[2,1], lty=2)
cat(-coefs[1,1]/coefs[2,1], "\n")

# wing length vs mating success
plot(all_data$fwl, all_data$cop, las=1, pch=1, xlab="Forewing Length (mm)",
     ylab="Mating Success")
m <- glm(all_data$cop ~ all_data$fwl, "binomial")
summary(m)
coefs = summary(m)$coef
logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))
x = all_data$fwl
x_pred = seq(from=min(x), to=max(x), by=0.01)
y_hat = coefs[1,1] + coefs[2,1]*x_pred
p_hat = invlogit(y_hat)
lines(x_pred, p_hat, type="l", lwd=2)
abline(h=0.5, v=(-coefs[1,1])/coefs[2,1], lty=2)
cat(-coefs[1,1]/coefs[2,1], "\n")

plot(all_data$hwl, all_data$cop, las=1, pch=1, xlab="Hindwing Length (mm)",
     ylab="Mating Success")
m <- glm(all_data$cop ~ all_data$hwl, "binomial")
summary(m)
coefs = summary(m)$coef
logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))
x = all_data$hwl
x_pred = seq(from=min(x), to=max(x), by=0.01)
y_hat = coefs[1,1] + coefs[2,1]*x_pred
p_hat = invlogit(y_hat)
lines(x_pred, p_hat, type="l", lwd=2)
abline(h=0.5, v=(-coefs[1,1])/coefs[2,1], lty=2)
cat(-coefs[1,1]/coefs[2,1], "\n")

# patch ratio to mating success
plot(male_CS$ptw, male_CS$cop, las=1, pch=1, 
     xlab= "Patch Area to Wing Length Ratio (mm^2/mm)", 
     ylab="Mating Success")
m <- glm(male_CS$cop ~ male_CS$ptw, "binomial")
summary(m)
coefs = summary(m)$coef
logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))
x = male_CS$ptw
x_pred = seq(from=min(x), to=max(x), by=0.01)
y_hat = coefs[1,1] + coefs[2,1]*x_pred
p_hat = invlogit(y_hat)
lines(x_pred, p_hat, type="l", lwd=2)

# patch width to mating success
plot(male_CS$fpw, male_CS$cop, las=1, pch=1, 
     xlab= "Forewing Patch Width (mm)", 
     ylab="Mating Success")
m <- glm(male_CS$cop ~ male_CS$fpw, "binomial")
summary(m)
coefs = summary(m)$coef
logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))
x = male_CS$fpw
x_pred = seq(from=min(x), to=max(x), by=0.01)
y_hat = coefs[1,1] + coefs[2,1]*x_pred
p_hat = invlogit(y_hat)
lines(x_pred, p_hat, type="l", lwd=2)
abline(h=0.5, v=(-coefs[1,1])/coefs[2,1], lty=2)
cat(-coefs[1,1]/coefs[2,1], "\n")

# MODELS
## thorax width best predictor for all, strongest with females
## wing length explains some for all, strongest with females, forewing with cs males
## year predicts cs mating success, species predicts female mating success
## patch width explains a little for male cs
## body length, abdomen length, and lifespan explain nothing

library(MuMIn)
library(MASS)
mod = glm(all_data$cop ~ all_data$sex + all_data$year + all_data$thorw 
          + all_data$fwl + all_data$hwl + all_data$ptw,
          family="poisson")
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
#overdispersed - AIC: 87673  r2: 0.51

mod = glm.nb(all_data$cop ~ all_data$sex + all_data$year + all_data$thorw 
             + all_data$fwl + all_data$hwl + all_data$ptw)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)

