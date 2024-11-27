#BIOS14 Exercise 6: Bee Distribution
setwd("~/LU CLASS OF 2026/BIOS13-14/BIOS14- Processing and Analysis of Biological Data/data/")

bdat = read.csv("Eulaema.csv", fileEncoding = "Latin1")

library(psych)
library(tidyverse)

pairs.panels(bdat)
pairs.panels(bdat |> select(Eulaema_nigrita, altitude, MAT, MAP, Tseason,
                            Pseason, forest., lu_het))

#log count data
bdat$logcount = log(bdat$Eulaema_nigrita+0.001)
pairs.panels(bdat |> select(logcount, altitude, MAT, MAP, Tseason,
                            Pseason, forest., lu_het))

#glm model with poisson distribution
mod = glm(bdat$Eulaema_nigrita ~ bdat$SA + bdat$SU + bdat$method + bdat$effort
           + bdat$altitude + bdat$MAT + bdat$MAP + bdat$Tseason + bdat$Pseason
           + bdat$forest. + bdat$lu_het, family="poisson")
summary(mod)

mod = glm(bdat$Eulaema_nigrita ~ bdat$MAP, family="poisson")
summary(mod)

mod = glm(bdat$Eulaema_nigrita ~ bdat$MAP + bdat$forest., family="poisson")
summary(mod)

#negative binomial errors model
library(MASS)
mod = glm.nb(bdat$Eulaema_nigrita ~ bdat$SA + bdat$SU + bdat$method + bdat$effort
             + bdat$altitude + bdat$MAT + bdat$MAP + bdat$Tseason + bdat$Pseason
             + bdat$forest. + bdat$lu_het)
summary(mod)

mod = glm.nb(bdat$Eulaema_nigrita ~ bdat$MAP)
summary(mod)

mod = glm.nb(bdat$Eulaema_nigrita ~ bdat$MAP + bdat$forest.)
summary(mod)

#null model for random factors
library(glmmTMB)
rmod = glmmTMB(bdat$Eulaema_nigrita ~ 1 + (1|bdat$SA))
summary(rmod)

#mixed effect model
mem = glmmTMB(bdat$Eulaema_nigrita ~ bdat$MAP + bdat$forest. + (1|bdat$SA) 
              + (1|bdat$method))
summary(mem)


# plot????
coefs = summary(mod)$coef

logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))

x = bdat$Eulaema_nigrita
x_pred = seq(from=min(x), to=max(x), by=0.01)
y_hat = coefs[1,1] + coefs[2,1]*x_pred
p_hat = invlogit(y_hat)

plot(x, bdat$MAP, xlab = "E. nigrita count", ylab = "MAP")
lines(x_pred, p_hat, type="l") 

#glm model of log data 
subbdat = bdat[bdat$Eulaema_nigrita != 0,]
modl = glm(subbdat$logcount ~ subbdat$MAP, family="poisson")
summary(modl)
coefs1 = summary(modl)$coef

x = subbdat$logcount
x_pred = seq(from=min(x), to=max(x), by=0.01)
y_hat = coefs1[1,1] + coefs1[2,1]*x_pred
p_hat = invlogit(y_hat)

plot(x, bdat$MAP, xlab = "log(E. nigrita count)", ylab = "MAP")
lines(x_pred, p_hat, type="l") 
