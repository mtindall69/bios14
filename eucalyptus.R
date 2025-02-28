#BIOS14 28-2-25 REEXAM: Eucalyptus

setwd("~/LU CLASS OF 2026/BIOS13-14/BIOS14- Processing and Analysis of Biological Data/data")

dat = read.csv("exam2023_data-1.csv")

#Total eucalyptus seedlings
dat$total_euc_sdlgs=dat$euc_sdlgs0_50cm+dat$euc_sdlgs50cm.2m+dat$euc_sdlgs.2m

#Seedling count- by height
cat("Seedling count\n 0-50cm:", sum(dat$euc_sdlgs0_50cm), "  50cm-2m:", 
    sum(dat$euc_sdlgs50cm.2m), "  >2m:", sum(dat$euc_sdlgs.2m),
    "   total:", sum(dat$total_euc_sdlgs))

#Histogram of seedlings
hist(dat$total_euc_sdlgs, breaks=150, las=1, xlim=c(0,90), xaxt="n",
     main="Histogram of seedling counts", xlab="Seedling counts", ylab="Frequency")
axis(side=1, at=seq(0, 90, by=10))

#Property count- number samples at each property
# 18 total properties, each quadrat sampled 3 times
prop_count <- table(dat$Property)
quads <- (prop_count/3)
print(quads)
print(mean(quads)) #6.5
barplot(quads, las=2, ylim=c(0,12), main="Quadrats per Property")

#Map of Properties using coordinates
library(ggplot2)
ggplot(dat, aes(x=Easting, y=Northing, col=Property)) +
  geom_point() +
  labs(title = "Property Map") +
  scale_color_manual(
    values = c("Barlow" = "#875F00", "Blaber" = "hotpink", "DClark" = "orange",
               "Gough" = "#FFF000", "Green" = "#33FF99", "Hawkey" = "#D78787",
               "JClark" = "#118DFF", "Kellock" = "blue", "Martin" = "steelblue",
               "McCracken" = "purple", "Olive" = "forestgreen", "Rokahr" = "darkgrey",
               "Sharrock" = "#00FF00", "Staff" = "turquoise", "Stoney" = "magenta",
               "Taylor" = "red", "Wakefield" = "navy", "Yorston" = "black")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Seedlings per Property
library(dplyr)
summary_table <- dat %>%
  group_by(Property) %>%
  summarise(small = sum(euc_sdlgs0_50cm), med = sum(euc_sdlgs50cm.2m),
            tall = sum(euc_sdlgs.2m), total_sdlgs = sum(total_euc_sdlgs))
print(summary_table)
#Olive, Rokahr, and Taylor=0, Yorston=1, Stoney=2

summary_table <- dat %>%
  group_by(Property) %>%
  summarise(total_sdlgs = median(total_euc_sdlgs))
print(summary_table)

ggplot(dat, aes(x = Property, y = total_euc_sdlgs)) +
  geom_boxplot() +
  labs(title = "Seedlings Counts per Quadrat by Property", 
       y = "Total Eucalyptus Seedlings") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

#Percent plots by Property with at least 1 seedling
dat_presence <- dat %>%
  mutate(presence = total_euc_sdlgs > 0) %>%
  group_by(Property) %>%
  summarize(prop_present = mean(presence))

ggplot(dat_presence, aes(x = Property, y = prop_present)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() +
  labs(x = "Property",
       y = "Percent of Plots with ≥1 Seedling")

#Landscape count
lcount <- table(dat$Landscape.position)
print(lcount)
barplot(lcount, las=1, ylim=c(0,250), main="Landscape Samples")

#Seedlings per Landscape
library(dplyr)
summary_table <- dat %>%
  group_by(Landscape.position) %>%
  summarise(total_sdlgs = sum(total_euc_sdlgs))
print(summary_table)

# Percent plots by landscape with at least 1 seedling
dat_presence <- dat %>%
  mutate(presence = total_euc_sdlgs > 0) %>%
  group_by(Landscape.position) %>%
  summarize(prop_present = mean(presence))

ggplot(dat_presence, aes(x = Landscape.position, y = prop_present)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() +
  labs(x = "Landscape Position",
       y = "Percent of Plots with ≥1 Seedling")

#Distance to canopy vs Seedlings
plot(dat$Distance_to_Eucalypt_canopy.m., dat$total_euc_sdlgs,
     main="Distance to Canopy vs Eucalyptus Seedling Density",
     xlab="Distance to Eucalyptus Canopy (m)", ylab="Total Eucalyptus seedlings")
d <- lm(dat$total_euc_sdlgs ~ dat$Distance_to_Eucalypt_canopy.m.)
summary(d)
abline(d, lwd=2, col="blue")
#estimate beta= -0.0448 ste 0.0229 pvalue=0.05 adjusted r2 0.008

#Converting properties to numbers
dat$Property_numeric <- as.numeric(factor(dat$Property))

#Converting landscapes to numbers
dat$Landscape_numeric <- as.numeric(factor(dat$Landscape.position))

#Converting aspect to numbers
dat$Aspect_numeric <- as.numeric(factor(dat$Aspect))


#seedling response
#numeric: canopy, distance to canopy, PET, annual precip, MrVBF, sradjan, sradjul
#nonnumeric: property, aspect, landscape
library(psych)
pairs.panels(dat |> dplyr::select(euc_sdlgs0_50cm, Distance_to_Eucalypt_canopy.m.,
                                  PET, Landscape.position, Th_ppm, U_ppm, BareGround_cover,
                                  ExoticAnnualHerb_cover, NativePerennialGrass_cover))
#bareground, Uppm, landscape, distance, PET, exoticannualherb

pairs.panels(dat |> dplyr::select(euc_sdlgs50cm.2m, Distance_to_Eucalypt_canopy.m.,
                                  PET, Property, NativePerennialFern_cover, Th_ppm,
                                  U_ppm, MossLichen_cover, ExoticAnnualHerb_cover,
                                  Rock_cover, ExoticPerennialHerb_cover, 
                                  ExoticAnnualGrass_cover, NativePerennialGrass_cover,
                                  NativePerennialHerb_cover))
#Thppm, fern, mosslichen, Uppm, exoticannualherb, rock, PET, exoticperennialherb, 
# distance

pairs.panels(dat |> dplyr::select(euc_sdlgs.2m, Distance_to_Eucalypt_canopy.m.,
                                  Property, Season, Th_ppm, U_ppm, 
                                  ExoticAnnualGrass_cover, NativePerennialGrass_cover,
                                  Litter_cover, ExoticAnnualHerb_cover))
#season, Thppm, Uppm, property, distance, exoticannualgrass, nativeperennialgrass,
# litter

#TOTAL SEEDLINGS
pairs.panels(dat |> dplyr::select(total_euc_sdlgs, Distance_to_Eucalypt_canopy.m.,
                                  PET, Landscape.position,
                                  NativePerennialFern_cover, Th_ppm, U_ppm,
                                  BareGround_cover, ExoticPerennialHerb_cover,
                                  ExoticPerennialGrass_cover))
#Uppm, PET, Thppm, distance to canopy, bareground, fern, landscape,  
# exoticperennialherb, exoticperennialgrass

plot(dat$U_ppm, dat$total_euc_sdlgs, las=1, 
     main="Soil uranium concentration vs seedling count",
     xlab="Estimated uranium concentration (ppm)",
     ylab="Total Eucalyptus Seedlings")
u <- lm(dat$total_euc_sdlgs ~ dat$U_ppm)
summary(u)
abline(u, lwd=2, col="red")


ggplot(dat, aes(x = Landscape.position, y = total_euc_sdlgs)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log1p") +
  labs(title="Landscape position vs total seedlings (log scaled axis)",
       y="Total Eucalyptus Seedlings", x="Landscape Position") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
  


# additional parameter tests
pairs.panels(dat |> dplyr::select(total_euc_sdlgs, euc_sdlgs0_50cm, euc_sdlgs50cm.2m,
                                  euc_sdlgs.2m, SurveyID, Season, Property,
                                  precipitation_warmest_quarter, MrVBF,
                                  K_perc, Landscape.position))

#MODELS
library(MuMIn)
library(MASS)
#global model
gmod = glm(data = dat, total_euc_sdlgs ~ U_ppm + PET + Th_ppm + 
             Distance_to_Eucalypt_canopy.m. + BareGround_cover + 
             NativePerennialFern_cover + Landscape.position + Property +
             ExoticPerennialHerb_cover + ExoticPerennialGrass_cover + 
             ExoticAnnualGrass_cover + NativePerennialGrass_cover +
             ExoticAnnualHerb_cover + Litter_cover + Season + K_perc + 
             SRad_Jan,
           family="poisson")
summary(gmod)
r.squaredGLMM(gmod)
1- (gmod$deviance/gmod$null.deviance)
#AIC: 1716  pseudor2: 0.462
# low significance- Thppm, epg


#start with Poisson
mod = glm(data = dat, total_euc_sdlgs ~ U_ppm + PET + Th_ppm + 
            Distance_to_Eucalypt_canopy.m. + BareGround_cover + 
            NativePerennialFern_cover + Landscape.position + Property +
            ExoticPerennialHerb_cover + ExoticPerennialGrass_cover,
          family="poisson")
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
#OVERDISPERSED
# AIC: 1880 pseudor2: 0.39

#negative binomial
gmod = glm.nb(data = dat, total_euc_sdlgs ~ U_ppm + PET + Th_ppm + 
                Distance_to_Eucalypt_canopy.m. + BareGround_cover + 
                NativePerennialFern_cover + Landscape.position + Property +
                ExoticPerennialHerb_cover + ExoticPerennialGrass_cover + 
                ExoticAnnualGrass_cover + NativePerennialGrass_cover +
                ExoticAnnualHerb_cover + Litter_cover + Season + K_perc + 
                SRad_Jan)
summary(gmod)
r.squaredGLMM(gmod)
1- (gmod$deviance/gmod$null.deviance)
# AIC: 870.44  pseudor2: 0.4594
# low significance- bareground, fern, EPG, EAG, EAH, SRadJan

mod = glm.nb(data = dat, total_euc_sdlgs ~ U_ppm + PET + Th_ppm + 
               Distance_to_Eucalypt_canopy.m. + BareGround_cover + 
               NativePerennialFern_cover + Landscape.position + Property +
               ExoticPerennialHerb_cover + ExoticPerennialGrass_cover)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
#242.78 deviance/345 df - dispersion better but significance crashed
# low significance- PET, Thppm, fern, EPG
# AIC: 860.73  pseudor2: 0.4305

mod = glm.nb(data = dat, total_euc_sdlgs ~ U_ppm + Th_ppm + 
               Distance_to_Eucalypt_canopy.m. + BareGround_cover + 
               NativePerennialFern_cover + Landscape.position + Property +
               ExoticPerennialHerb_cover + ExoticPerennialGrass_cover)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
# AIC: 857.94  pseudor2: 0.4330

mod = glm.nb(data = dat, total_euc_sdlgs ~ U_ppm + Th_ppm + 
               Distance_to_Eucalypt_canopy.m. + BareGround_cover + 
               Landscape.position + Property +
               ExoticPerennialHerb_cover + ExoticPerennialGrass_cover)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
# AIC: 855.78  pseudor2: 0.4335

mod = glm.nb(data = dat, total_euc_sdlgs ~ U_ppm + Th_ppm + 
               Distance_to_Eucalypt_canopy.m. + BareGround_cover + 
               Landscape.position + Property + ExoticPerennialHerb_cover + 
               ExoticPerennialGrass_cover + NativePerennialGrass_cover)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
# AIC: 855.6  pseudor2: 0.4403

mod = glm.nb(data = dat, total_euc_sdlgs ~ U_ppm + Th_ppm + 
               Distance_to_Eucalypt_canopy.m. + BareGround_cover + 
               Landscape.position + Property + ExoticPerennialHerb_cover + 
               NativePerennialGrass_cover)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
# AIC: 853.83  pseudor2: 0.4396

mod = glm.nb(data = dat, total_euc_sdlgs ~ U_ppm + Th_ppm + K_perc +
               Distance_to_Eucalypt_canopy.m. + BareGround_cover + 
               Landscape.position + Property + ExoticPerennialHerb_cover + 
               NativePerennialGrass_cover)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
# AIC: 852.02  pseudor2: 0.4513

mod = glm.nb(data = dat, total_euc_sdlgs ~ U_ppm + Th_ppm + K_perc +
               Distance_to_Eucalypt_canopy.m. + BareGround_cover + 
               Landscape.position + Property + ExoticPerennialHerb_cover)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
# AIC: 852.64  pseudor2: 0.4432

mod = glm.nb(data = dat, total_euc_sdlgs ~ U_ppm + Th_ppm +
               Distance_to_Eucalypt_canopy.m. + BareGround_cover + 
               Landscape.position + Property + ExoticPerennialHerb_cover)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
# AIC: 854.42  pseudor2: 0.4315

mod = glm.nb(data = dat, total_euc_sdlgs ~ U_ppm +
               Distance_to_Eucalypt_canopy.m. + BareGround_cover + 
               Landscape.position + Property + ExoticPerennialHerb_cover)
summary(mod)
r.squaredGLMM(mod)
1- (mod$deviance/mod$null.deviance)
# AIC: 854.39  pseudor2: 0.4254

mod = glm.nb(data = dat, total_euc_sdlgs ~ U_ppm + Distance_to_Eucalypt_canopy.m. + 
               BareGround_cover + Landscape.position + Property)
summary(mod)
r.squaredGLMM(mod)
full_r2 <- 1- (mod$deviance/mod$null.deviance)
print(full_r2)
# AIC: 853.17  pseudor2: 0.4232 
#--- WINNING MODEL---

predictors <- c("U_ppm", "Distance_to_Eucalypt_canopy.m.", "BareGround_cover", 
                "Landscape.position", "Property")

# percent variance explained by each predictor
percent_explained <- numeric(length(predictors))
names(percent_explained) <- predictors

for (pred in predictors) {
  reduced_formula <- update(formula(mod), paste(". ~ . -", pred))
  mod_reduced <- glm.nb(formula = reduced_formula, data = dat)
  r2_reduced <- 1 - (mod_reduced$deviance / mod_reduced$null.deviance)
  percent_explained[pred] <- 100 * (full_r2 - r2_reduced) / full_r2
}
variance_table <- data.frame(
  Parameter = names(percent_explained),
  PercentVarianceExplained = percent_explained
)

print(variance_table)

library(car)
anova(mod)
Anova(mod)

#Mixed effects
library(lme4)
mod = glmer(data = dat, total_euc_sdlgs ~ U_ppm + Th_ppm + K_perc +
              Distance_to_Eucalypt_canopy.m. + BareGround_cover + 
              Landscape.position + ExoticPerennialHerb_cover + 
              NativePerennialGrass_cover + (1|Property), family = "poisson")
summary(mod)
r.squaredGLMM(mod)
# AIC: 1919  r2: 0.197/0.944

mod = glmer.nb(data = dat, total_euc_sdlgs ~ U_ppm + Th_ppm + K_perc +
                 Distance_to_Eucalypt_canopy.m. + BareGround_cover + 
                 Landscape.position + ExoticPerennialHerb_cover + 
                 NativePerennialGrass_cover + (1|Property))
summary(mod)
r.squaredGLMM(mod)
# AIC: 876.2  r2: 0.201/0.503

mod = glmer.nb(data = dat, total_euc_sdlgs ~ U_ppm + Th_ppm +
                 Distance_to_Eucalypt_canopy.m. + BareGround_cover + 
                 Landscape.position + ExoticPerennialHerb_cover + 
                 NativePerennialGrass_cover + (1|Property))
summary(mod)
r.squaredGLMM(mod)
# AIC: 876  r2: 0.223/0.477

mod = glmer.nb(data = dat, total_euc_sdlgs ~ U_ppm + Th_ppm +
                 Distance_to_Eucalypt_canopy.m. + BareGround_cover + 
                 Landscape.position + ExoticPerennialHerb_cover + (1|Property))
summary(mod)
r.squaredGLMM(mod)
# AIC: 874.5  r2: 0.221/0.464
