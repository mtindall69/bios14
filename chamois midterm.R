#BIOS14 Midterm: Chamois Model Selection

setwd("~/LU CLASS OF 2026/BIOS13-14/BIOS14- Processing and Analysis of Biological Data/data")
dat = read.table("chamois.txt", header=T)

library(psych)
pairs.panels(dat)

#response variable: hornL+hornR = hornT, count, age, mass - over time
dat$hornT <- dat$hornL+dat$hornR
print(mean(dat$hornT))

sex_count <- table(dat$sex)
print(sex_count)
barplot(sex_count, las = 1,
        xlab = "Sex", ylab = "Total Chamois Hunted", ylim = c(0, 2500))

season <- dat$season
hist(season, las = 1, breaks = seq(min(year), max(year), by = 1),
     xlim = c(1977, 2016), ylim = c(0,250),
     xlab = "Season (year)", ylab = "Total Chamois Hunted", 
     main = "Chamois Hunted by Season")

season_count <- table(dat$season)
print(season_count)
barplot(season_count, las = 1, ylim = c(0,250),
        xlab = "Season (year)", ylab = "Total Chamois Hunted", 
        main = "Chamois Hunted by Season")
