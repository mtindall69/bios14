#BIOS14 28-2-25 REEXAM: Eucalyptus

setwd("~/LU CLASS OF 2026/BIOS13-14/BIOS14- Processing and Analysis of Biological Data/data")

dat = read.csv("exam2023_data-1.csv")

#Property count- number samples at each property
# 18 total properties, each quadrat sampled 3 times
prop_count <- table(dat$Property)
quads <- (prop_count/3)
print(quads)
print(mean(quads)) #6.5
barplot(quads, las=2, ylim=c(0,12), main="Quadrats per Property")

#Map of Properties using coordinates
x=dat$Easting
y=dat$Northing
colors=c("red", "hotpink", "orange", "#FFF000", "#00FF00", "forestgreen",
         "#118DFF", "blue", "steelblue", "purple", "magenta", "darkgrey",
         "darkred", "turquoise", "#D78787", "#875F00", "navy", "black")
plot(x, y, pch=19, col=colors[factor(dat$Property)], main="Property Map",
     xlab= "Easting coordinate", ylab= "Northing coordinate")
legend("bottomright", legend=c("Barlow", "Blaber", "DClark", "Gough", "Green",
                                "Hawkey", "JClark", "Kellock", "Martin",
                                "McCracken", "Olive", "Rokahr", "Sharrock",
                                "Staff", "Stoney", "Taylor", "Wakefield", "Yorston"),
       pch=19, col=colors)

#Converting properties to numbers
dat$Property_numeric <- as.numeric(factor(dat$Property))
print(dat$Property_numeric)

#Converting landscapes to numbers
dat$Landscape_numeric <- as.numeric(factor(dat$Landscape.position))
print(dat$Landscape_numeric)

#Converting aspect to numbers
dat$Aspect_numeric <- as.numeric(factor(dat$Aspect))
print(dat$Aspect_numeric)

#Total eucalyptus seedlings
dat$total_euc_sdlgs=dat$euc_sdlgs0_50cm+dat$euc_sdlgs50cm.2m+dat$euc_sdlgs.2m

#seedling response
#numeric: canopy, PET, annual precip, MrVBF, sradjan, sradjul
#nonnumeric: property, aspect, landscape

library(psych)
pairs.panels(dat |> dplyr::select(total_euc_sdlgs, Euc_canopy_cover, PET,
                                  SRad_Jan, SRad_Jul, annual_precipitation,
                                  MrVBF, Property, Aspect, Landscape.position,
                                  NativePerennialFern_cover, Th_ppm, U_ppm))
#Uppm, PET, Thppm
#fern, landscape, property, MrVBF

plot(dat$PET, dat$total_euc_sdlgs)
plot(dat$MrVBF, dat$total_euc_sdlgs)
plot(dat$U_ppm, dat$total_euc_sdlgs)
plot(dat$Th_ppm, dat$total_euc_sdlgs)
plot(dat$NativePerennialFern_cover, dat$total_euc_sdlgs)
plot(dat$Property_numeric, dat$total_euc_sdlgs)
plot(dat$Landscape_numeric, dat$total_euc_sdlgs)

library(ggplot2)
ggplot(dat, aes(x = Landscape.position, y = total_euc_sdlgs)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log1p") +
  theme_bw()

library(dplyr)
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

#canopy response
#numeric: exoticannualgrass, exoticannualherb, exoticperennialherb, 
# exoticperennialgrass, nativeperennialgrass, nativeperennialherb,
# nativeperennialgraminoid, bareground, rock, totalseedlings, PET, MrVBF, 
# sradjan, sradjul
#nonnumeric: property, aspect, landscape
pairs.panels(dat |> dplyr::select(Euc_canopy_cover, ExoticAnnualGrass_cover,
                                  ExoticAnnualHerb_cover, ExoticPerennialHerb_cover,
                                  ExoticPerennialGrass_cover, NativePerennialGrass_cover,
                                  NativePerennialHerb_cover, NativePerennialGraminoid_cover,
                                  BareGround_cover, Rock_cover, Litter_cover))
#litter, nativeperennialgrass, bareground, property and exoticannualgrass
#landscape, aspect, fern

plot(dat$NativePerennialGrass_cover, dat$Euc_canopy_cover)
plot(dat$BareGround_cover, dat$Euc_canopy_cover)
plot(dat$ExoticAnnualGrass_cover, dat$Euc_canopy_cover)
plot(dat$Litter_cover, dat$Euc_canopy_cover)
plot(dat$Property_numeric, dat$Euc_canopy_cover)
plot(dat$Landscape_numeric, dat$Euc_canopy_cover) #boxplot

ggplot(dat, aes(x = Landscape.position, y = Euc_canopy_cover)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log1p") +
  theme_bw()

plot(dat$Aspect_numeric, dat$Euc_canopy_cover)


# additional parameter tests
# pairs.panels(dat |> dplyr::select(Euc_canopy_cover, total_euc_sdlgs, SurveyID,
#                                   Season, Easting, Northing, K_perc,
#                                   precipitation_warmest_quarter,
#                                   precipitation_coldest_quarter, ExoticShrub_cover,
#                                   NativeShrub_cover,MossLichen_cover,
#                                   Distance_to_Eucalypt_canopy.m.))



