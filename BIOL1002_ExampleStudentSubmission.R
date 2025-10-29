# title: BIOL 1002 Moose Assignment Answer Key
# date: June 1st
# author: EMACK


# notes -------------------------------------------------------------------

#
#
#

# Part 1: Moose Populations in Newfoundland -------------------------------------------------------------

#Question 1
install.packages("dplyr")
library(dplyr)

#Question 2
Moosedata <- read.csv("./MoosePopulation.csv")

#Question 3
Moosedata <- na.omit(Moosedata)

#Question 4
Moosedata <- select(Moosedata, Ecoregion, Year, Area, Estimated_Moose_Pop)

#Question 5
#a)
min(Moosedata$Year) #Answer 1904
#b) 
max(Moosedata$Estimated_Moose_Pop) #Answer 41250

#Question 6
Moosedata <- mutate(Moosedata, MooseDensity = Estimated_Moose_Pop / Area)

#Question 7
plot(Moosedata$Year, Moosedata$MooseDensity,
     xlab = "Year",
     ylab = "Moose Density",
     main = "Moose Density over time")

#General trends: First observation in 1904, inital peak in 1960, 
# slight decrease in 1980, populations are highest now 

#Question 8
MooseData_5 <- Moosedata %>% 
  filter(Ecoregion == "Western_Forests")

plot(MooseData_5$Year, MooseData_5$MooseDensity,
     type = "l",
     xlab = "Year",                
     ylab = "Moose Density",       
     main = "Moose Density Over Time in Western Forests")

#Question 9
#a)
MooseData_2020 <- filter(Moosedata, Year == 2020)
#b)
MooseData_2020_b <- filter(MooseData_2020, MooseDensity > 2.0)
#c)
MooseData_c <- arrange(MooseData_3, desc(MooseDensity))
#d)
print(MooseData_c)

#Question 10
Moosedata4 <- Moosedata %>% 
  filter(Year == 2020) %>% 
  filter(MooseDensity > 2.0) %>% 
  arrange(desc(MooseDensity)) %>% 
  print()

# Part 2:Tree Sapling Study -----------------------------------------------------------------

#Question 11
Saplings <- read.csv("SaplingStudy.csv")

Saplings <- na.omit(Saplings)


#Question 12
#a)
AvgBrowsing <- Saplings %>%
  group_by(Ecoregion) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>%
  print()
#b)
# StraitOfBelleIsleBarrens has the lowest browsing score
# Northern_Peninsula_Forests has the highest browsing score 

#Question 13
#a)
AvgHeight <- Saplings %>% 
  group_by(Ecoregion) %>% 
  summarise(AvgHeight = mean(Height)) %>% 
  print()
#b)
#	Western_Forests and Northern_Peninsula_Forests have average heights less than 20cm

#Question 14
#a)
AverageBrowsing <- Saplings %>% 
  group_by(Species) %>%
  summarise(AverageBrowsing = mean(BrowsingScore)) %>% 
  print()
#b)
# Black spruce has the lowest browsing score
# Black ash has the highest browsing score

#Question 15
BalsamFir<- Saplings %>% 
  filter(Species == "Balsam_Fir") %>% 
  group_by(Ecoregion) %>%
  summarise(AvgBrowsingScore = mean(BrowsingScore)) %>%
  print()

#Question 16
barplot(BalsamFir$AvgBrowsingScore,
        names.arg = BalsamFir$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Balsam Fir Browsing Intensity by Ecoregion",
        col = "forestgreen")

#Question 17
#a)
Black_Spruce <- BrowsingBySpeciesRegion %>% 
  filter(Species == "Black_Spruce")

barplot(Black_Spruce$AvgBrowsing,
        names.arg = Black_Spruce$Ecoregion,
        xlab = "Ecoregion",
        ylab = "Average Browsing Score",
        main = "Black_Spruce Browsing Intensity by Ecoregion",
        col = "forestgreen")

#b) Balsam Fir browsing pressure is high across all ecoregions
# Black_Spruce browsing pressure is only high in a few ecoregions


#Question 18
EcoregionTally<- Saplings %>%
  group_by(Ecoregion) %>%
  tally() %>% 
  print()

#Question 19
EcoregionTally<- Saplings %>%
  group_by(Species) %>%
  tally() %>% 
  print()

#Question 20
#a)
# In terms of ecoregions, data is not evenly distributed 
# Northern_Peninsula_Forests is over represented. 
# StraitOfBelleIsleBarrens is under represented.

# In terms of tree species, data is not evenly distributed 
# Balsam_Fir is over represented. 
# Black ash is under represented.

#b)
# It’s important to recognize bias in ecological datasets because 
# uneven sampling can lead to misleading conclusions.

# Part 3: Creating and joining datasets -----------------------------------

#Question 1
SaplingMoose <- Saplings %>%
  left_join(MooseData_2020, by = "Ecoregion")

#Question 2
BrowsingBySpeciesDensity <- SaplingMoose %>%
group_by(Species, Ecoregion) %>%
  summarise(AvgBrowsing = mean(BrowsingScore),
            AvgDensity = mean(MooseDensity))

#Question 3

#a)

#b)

#c)

#Question 4
Collisions_2020 <- c(56, 60, 14, 36, 48, 10, 40, 110, 6)
HumanPopulation <- c(18000, 12000, 4000, 75100, 24000,3500, 32000, 270000,    2300)
StudySites <- c("North_Shore_Forests","Northern_Peninsula_Forests", "Long_Range_Barrens","Central_Forests","Western_Forests","EasternHyperOceanicBarrens","Maritime_Barrens","Avalon_Forests","StraitOfBelleIsleBarrens")

MooseCollisions <- data.frame(StudySites, Collisions_2020, HumanPopulation)

#Question 5
#a)
# Have no common variables, 
# columns called Ecoregion vs. StudySites

#b)
MooseCollisions <- MooseCollisions %>% 
  rename(Ecoregion = StudySites)

#c)
MooseMerged <- MooseData_2020 %>%
  left_join(MooseCollisions, by = "Ecoregion")

#Question 6
#a)
plot(MooseMerged$MooseDensity, MooseMerged$Collisions_2020,
     xlab = "Moose Density",
     ylab = "Collisions in 2020",
     main = "Moose Density vs Moose-Vehicle Collisions")

#b)
# As Moose density increases, so does collisions 
# Avalon Peninsula is an outlier, more people 


#Question 7
MooseMerged <- MooseMerged %>%
  mutate(CollisionsPerCapita = Collisions_2020 / HumanPopulation)

#Question 8
plot(MooseMerged$HumanPopulation, MooseMerged$CollisionsPerCapita,
     xlab = "Human Population",
     ylab = "Collisions per Person",
     main = "Collisions per Capita vs Population Size",
     pch = 19,
     col = "purple")

#Question 9
#Rural populations have a righer risk of collisions per person
#This makes sense, more moose in rural areas with less people


