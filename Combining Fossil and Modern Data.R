library(dplyr)

setwd("C:/Users/Mr-Wo/Desktop/Bachelorarbeit/Data/Neue Daten")

fossil.data <- read.csv("Fossil Data for Mia (reduced).csv", sep = ';', header = T)
modern.data <- read.csv("Mammal Data with different Feeding Groups.csv", sep = ';', header = T)


full.data <- rbind(modern.data,fossil.data)

View(full.data)


write.csv(file = 'Modern and fossil data combined (with diet).csv', full.data)
