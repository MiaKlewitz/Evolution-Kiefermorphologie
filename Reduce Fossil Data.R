library(dplyr)

setwd("C:/Users/Mr-Wo/Desktop/Bachelorarbeit/Data")


fossils.full <- read.csv(file = 'Fossil Multi Primate Rodent Mammals for mia.csv',header = T)#, sep = ',')

View(fossils.full)

# choose only columns that we want
fossils.cols <- fossils.full %>%
  select(species, chosen.groups.only.a, period, jaw.depth, jaw.length, dental.row.length, molar.row.length,
         diastema.length, distance.from.condyle.to.coronoid, distance.from.condyle.to.middle.cusp.last.premolar,
         coronoid.length, coronoid.width, distance.from.jaw.joint.to.tooth.row)

View(fossils.cols)

# rename columns to match Mias dataset
fossils.cols <- fossils.cols %>% 
  rename(age = period, clade = chosen.groups.only.a)

View(fossils.cols)

write.csv(file = 'Fossil Data for Mia (reduced).csv', fossils.cols)
