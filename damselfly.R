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

#Species weights
sp_count <- table(all_data$sp)
print(sp_count)
#CS = 3025 ; CV = 593

#Species/Sex plot
ct <- table(all_data$sex, all_data$sp)
print(ct)
#CS: M = 2035; F = 990
#CS: M = 349; F = 244
barplot(ct, beside = TRUE, ylim = c(0,2500),
        xlab = "Species", ylab = "Total Damselflies", 
        main = "Damselflies by Species and Sex",
        col = c("steelblue", "darkorange")) 
legend("topright", legend=c("Females", "Males"), pch=15, 
       col=c("steelblue", "darkorange"))

mean(all_data$tbl)
mean(female_CS$tbl)
mean(female_CV$tbl)
mean(male_CS$tbl)
mean(male_CV$tbl)
hist(all_data$tbl)
mean(cs_data$tbl)
hist(cs_data$tbl)
mean(cv_data$tbl)
hist(cv_data$tbl)

