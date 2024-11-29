#BIOS14 Exercise 6: Bee Distribution
setwd("~/LU CLASS OF 2026/BIOS13-14/BIOS14- Processing and Analysis of Biological Data/data/")

bdat = read.csv("Eulaema.csv", fileEncoding = "Latin1")

#library(psych)
library(tidyverse)

pairs.panels(bdat)
pairs.panels(bdat |> dplyr::select(Eulaema_nigrita, SA, method, effort, MAP, 
                                   Pseason, forest.))

# plots
par(mfrow = c(2,2))
plot(bdat$MAP, bdat$Eulaema_nigrita, xlab = "Mean Annual Precipitation (mm)", 
     ylab = "E. nigrita count")
plot(bdat$Pseason, bdat$Eulaema_nigrita, xlab= "Seasonal Precipitation", 
     ylab = "E. nigrita count")
plot(bdat$forest., bdat$Eulaema_nigrita, xlab = "Forest cover %", 
     ylab = "E. nigrita count")
plot(bdat$altitude, bdat$Eulaema_nigrita, xlab = "Altitude (m)",
     ylab = "E. nigrita count")
par(mfrow = c(1,1))


#glm global model with poisson distribution
mod = glm(bdat$Eulaema_nigrita ~ bdat$SA + bdat$method + bdat$effort
           + bdat$altitude + bdat$MAT + bdat$MAP + bdat$forest. + bdat$lu_het, 
          family="poisson")
summary(mod)

library(MuMIn)

options(na.action = "na.fail")

aic_table <- dredge(mod, rank="AICc") 

mod = glm(bdat$Eulaema_nigrita ~ bdat$SA + bdat$MAP + bdat$forest. 
          + bdat$method, family="poisson")
summary(mod)

#negative binomial errors model
library(MASS)
mod = glm.nb(bdat$Eulaema_nigrita ~ bdat$SA + bdat$method + bdat$effort
             + bdat$altitude + bdat$MAT + bdat$MAP + bdat$Tseason + bdat$Pseason
             + bdat$forest. + bdat$lu_het)
summary(mod)

mod = glm.nb(bdat$Eulaema_nigrita ~ bdat$MAP + bdat$forest. + bdat$method)
summary(mod)

mod = glm.nb(bdat$Eulaema_nigrita ~ bdat$MAP + bdat$forest.)
summary(mod)

mod = glm.nb(bdat$Eulaema_nigrita ~ bdat$MAP)
summary(mod)


#null model for random factors
library(glmmTMB)
rmod = glmmTMB(data = bdat, Eulaema_nigrita ~ 1 + (1|SA))
summary(rmod)

#mixed effect model
mem = glmmTMB(data = bdat, Eulaema_nigrita ~ MAP + forest. + (1|SA) + (1|method))
summary(mem)

