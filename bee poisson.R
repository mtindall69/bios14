#BIOS14 Exercise 6: Bee Distribution

bdat = read.csv("Eulaema.csv", fileEncoding = "Latin1")
# count is y variable
# test effects of altitude, temp(MAT, Tseason), precip(MAP, Pseason), 
# forest cover, and land use 
# location directly related to each of these

mets = c("altitude", "MAT", "MAP", "Tseason", "Pseason", "forest", "lu_het")

y = bdat$Eulaema_nigrita

library(psych)
#library(tidyverse)

pairs.panels(bdat)

#log count data
bdat$logcount = log(bdat$Eulaema_nigrita+0.001)
pairs.panels(bdat %>% select(logcount, altitude, MAT, MAP, Tseason,
                             Pseason, forest., lu_het))

#glm model 
mod = glm(bdat$Eulaema_nigrita ~ bdat$MAP, family="poisson")
summary(mod)
mod = glm(bdat$Eulaema_nigrita ~ bdat$SA * bdat$SU * bdat$method * bdat$effort 
          * bdat$altitude * bdat$MAT * bdat$MAP * bdat$Tseason * bdat$Pseason 
          * bdat$forest. * bdat$lu_het, family="poisson")


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
