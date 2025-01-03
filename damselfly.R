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

#Species weights
sp_count <- table(all_data$sp)
print(sp_count)
#CS = 3025 ; CV = 593 - 84% CS

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
