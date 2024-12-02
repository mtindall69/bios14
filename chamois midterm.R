#BIOS14 Midterm: Chamois Model Selection

setwd("~/LU CLASS OF 2026/BIOS13-14/BIOS14- Processing and Analysis of Biological Data/data")
dat = read.table("chamois.txt", header=T)

library(psych)
pairs.panels(dat)

#response variable: hornL+hornR = hornT
dat$hornT <- dat$hornL+dat$hornR
print(mean(dat$hornT))

sex_count <- table(dat$sex)
print(sex_count)
barplot(sex_count, las = 1,
        xlab = "Sex", ylab = "Total Hunted", ylim = c(0, 2500))

