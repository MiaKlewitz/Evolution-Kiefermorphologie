library(dplyr)


setwd("C:/Users/Mr-Wo/Desktop/Bachelorarbeit/Data/Original_Data")

# read in full extant dataset
gemma.full <- read.csv("Extant Mammal Data Mia Full.csv", sep=";",header = T)

# select only rodents and primates
gemma.clade <- gemma.full[which(gemma.full$chosen.groups.only.a == "Rodentia" | 
                                  gemma.full$chosen.groups.only.a == "Primates" ),]


View(gemma.clade)
# choose only columns that we want
gemma.cols <- gemma.clade %>%
  select(species, chosen.groups.only.a, period, jaw.depth, jaw.length, dental.row.length, molar.row.length,
         diastema.length, distance.from.condyle.to.coronoid, distance.from.condyle.to.middle.cusp.last.premolar,
         coronoid.length, coronoid.width, distance.from.jaw.joint.to.tooth.row)

# rename columns to match Mias dataset
gemma.cols <- gemma.cols %>% 
  rename(age = period, clade = chosen.groups.only.a)

# replace names of species with new species name
gemma.cols[gemma.cols == 'Acomys cahirinus hunteri'] <- 'Acomys cahirinus'
gemma.cols[gemma.cols == 'Chaetodipus intermedius intermedius'] <- 'Chaetodipus intermedius'
gemma.cols[gemma.cols == 'Dipodomys ordii longipes'] <- 'Dipodomys ordii'
gemma.cols[gemma.cols == 'Heteromys desmarestianus zonalis'] <- 'Heteromys desmarestianus'
gemma.cols[gemma.cols == 'Taterillus harringtoni nubilus'] <- 'Taterillus harringtoni'
gemma.cols[gemma.cols == 'Thomomys umbrinus intermedius'] <- 'Thomomys umbrinus'
gemma.cols[gemma.cols == 'Zapus hudsonius campestris'] <- 'Zapus hudsonius'

# remove any rows with species we don't want
gemma.cols[gemma.cols$species != "Cebus", ]   ###### MIA TO CHECK FOR MORE --> done that

# write-csv
#write.csv(file = 'Gemma extant data for Mia.csv', gemma.cols)


setwd("C:/Users/Mr-Wo/Desktop/Bachelorarbeit/Data/Original_Data")
# read in Mias data
mia <- read.csv(file = 'Mias Mammal Data.csv', sep = ';')
gemma.extant <- read.csv(file = 'Gemma extant data for Mia.csv', sep = ';')

full <- rbind(mia,gemma.extant)
View(full)
setwd("C:/Users/Mr-Wo/Desktop/Bachelorarbeit/Data/Neue Daten")
#write.csv(file = 'Mia and Gemma Data combined.csv', full)

data.full <- read.csv(file = 'Mia and Gemma Data combined.csv', sep = ';')

#------

setwd("C:/Users/Mr-Wo/Desktop/Bachelorarbeit/Data/Original_Data")
##### read elton trait dataset
elton <- read.csv(file = 'EltonTraits Mammal Dataset.csv', sep = ';')

# choose only columns that we want
elton.new <- elton %>%
  select(Scientific, Diet.Inv, Diet.Vend, Diet.Vect, Diet.Vfish, Diet.Vunk, Diet.Scav,
         Diet.Fruit, Diet.Nect, Diet.Seed, Diet.PlantO)

# rename columns to match Mias dataset
elton.1 <- elton.new %>% 
  rename(species = Scientific)
View(elton.1)
View(data.full)
# merge trait dataset with mias dataset
mia.trait <- merge(data.full,elton.1,by="species", all.x = TRUE)

View(mia.trait)

# remove any species that have NA
mia.trait.pop<- mia.trait %>%
  filter_at(vars(jaw.depth, jaw.length, dental.row.length, molar.row.length,
                 diastema.length, distance.from.condyle.to.coronoid, distance.from.condyle.to.middle.cusp.last.premolar,
                 coronoid.length, coronoid.width, distance.from.jaw.joint.to.tooth.row), all_vars(!is.na(.)))


####################### MIA TO DO 
# choose only columns that we want again 
gemma.cols <- gemma.clade %>%
select(species, chosen.groups.only.a, period, jaw.depth, jaw.length, dental.row.length, molar.row.length,
         diastema.length, distance.from.condyle.to.coronoid, distance.from.condyle.to.middle.cusp.last.premolar,
         coronoid.length, coronoid.width, distance.from.jaw.joint.to.tooth.row)

setwd("C:/Users/Mr-Wo/Desktop/Bachelorarbeit/Data/Neue Daten")
write.csv(file = 'Mammal Data with Elton (reduced).csv', mia.trait.pop)
View(mia.trait.pop)
