
#get data
setwd("C:/Users/Mr-Wo/Desktop/Bachelorarbeit/Data/Neue Daten")
data <- read.csv(file = 'Mammal Data with Elton (reduced).csv', sep = ';')
#View(data)

#add columns together
data$Carnivore <- data$Diet.Inv + data$Diet.Vend + data$Diet.Vect + data$Diet.Vfish + data$Diet.Vunk + data$Diet.Scav
data$Herbivore <- data$Diet.Fruit + data$Diet.Nect + data$Diet.Seed + data$Diet.PlantO
data$Invertebrates <- data$Diet.Inv
data$Vertebrates <- data$Diet.Vend + data$Diet.Vect + data$Diet.Vfish + data$Diet.Vunk
data$Fruit.Nectar <- data$Diet.Fruit + data$Diet.Nect
data$Seeds <- data$Diet.Seed
data$Plants <- data$Diet.PlantO

data.new <- data

#loop through all rows of your dataset
for (i in 1:nrow(data.new)) {
  # Trophic levels:
  if(data.new[i,"Carnivore"] > data.new[i,"Herbivore"]
     & !is.na(data.new[i,"Carnivore"])
     & !is.na(data.new[i,"Herbivore"]))
    {data.new[i,"Trophic level"] <- "Carnivore"}
  
  if(data.new[i,"Carnivore"] < data.new[i,"Herbivore"]
     & !is.na(data.new[i,"Carnivore"])
     & !is.na(data.new[i,"Herbivore"]))
    {data.new[i,"Trophic level"] <- "Herbivore"}
  
  if(data.new[i,"Carnivore"] >= 40 & data.new[i,"Herbivore"] >= 40
     & !is.na(data.new[i,"Carnivore"])
     & !is.na(data.new[i,"Herbivore"]))
    {data.new[i,"Trophic level"] <- "Omnivore"}
  
  # Diet trend
  if(data.new[i,"Herbivore"] >= 80 & data.new[i,"Carnivore"] <= 20
     & !is.na(data.new[i,"Carnivore"])
     & !is.na(data.new[i,"Herbivore"]))
     {data.new[i,"Diet trend"] <- "Herbivore"}
  
  if(data.new[i,"Herbivore"] >= 60 & data.new[i,"Carnivore"] >= 20
     & !is.na(data.new[i,"Carnivore"])
     & !is.na(data.new[i,"Herbivore"]))
     {data.new[i,"Diet trend"] <- "mostly Herbivore"}
     
  if(data.new[i,"Herbivore"] >= 40 & data.new[i,"Carnivore"] >= 40
     & !is.na(data.new[i,"Carnivore"])
     & !is.na(data.new[i,"Herbivore"]))
     {data.new[i,"Diet trend"] <- "Omnivore"}
  
  if(data.new[i,"Carnivore"] >= 60 & data.new[i,"Herbivore"] >= 20
     & !is.na(data.new[i,"Carnivore"])
     & !is.na(data.new[i,"Herbivore"]))
     {data.new[i,"Diet trend"] <- "mostly Carnivore"}
     
  if(data.new[i,"Carnivore"] >= 80
     & !is.na(data.new[i,"Carnivore"])
     & !is.na(data.new[i,"Herbivore"]))
     {data.new[i,"Diet trend"] <- "Carnivore"}
  
  # Dietary guilds
  # CARNIVORE
  if(data.new[i,"Carnivore"] > data.new[i,"Herbivore"] 
     & !is.na(data.new[i,"Herbivore"])
     & data.new[i,"Invertebrates"] > data.new[i,"Vertebrates"] 
     & !is.na(data.new[i,"Vertebrates"]))
    {data.new[i,"Dietary guilds"] <- "Invertebrates"}
  
  if(data.new[i,"Carnivore"] > data.new[i,"Herbivore"] 
     & !is.na(data.new[i,"Herbivore"])
     & data.new[i,"Vertebrates"] > data.new[i,"Invertebrates"] 
     & !is.na(data.new[i,"Invertebrates"]))
    {data.new[i,"Dietary guilds"] <- "Vertebrates"}
  
  if (data.new[i,"Carnivore"] > data.new[i,"Herbivore"]
     & !is.na(data.new[i,"Herbivore"])
     & data.new[i,"Vertebrates"] > data.new[i,"Invertebrates"] 
     & !is.na(data.new[i,"Invertebrates"]))
    {data.new[i,"Dietary guilds"] <- "Carnivore"}
  
  # HERBIVORE 
  if(data.new[i,"Herbivore"] > data.new[i,"Carnivore"] 
     & !is.na(data.new[i,"Carnivore"])
     & data.new[i,"Fruit.Nectar"] > data.new[i,"Seeds"] 
     & data.new[i,"Fruit.Nectar"] > data.new[i,"Plants"]
     & !is.na(data.new[i,"Seeds"]) 
     & !is.na(data.new[i,"Plants"]))
    {data.new[i,"Dietary guilds"] <- "Frugivore & Nectarivore"}
  
  if(data.new[i,"Herbivore"] > data.new[i,"Carnivore"]  
     & !is.na(data.new[i,"Carnivore"])
     & data.new[i,"Seeds"] > data.new[i,"Fruit.Nectar"] 
     & data.new[i,"Seeds"] > data.new[i,"Plants"] 
     & !is.na(data.new[i,"Fruit.Nectar"]) 
     & !is.na(data.new[i,"Plants"]))
    {data.new[i,"Dietary guilds"] <- "Granivore"}

    if(data.new[i,"Herbivore"] > data.new[i,"Carnivore"]       
     & !is.na(data.new[i,"Carnivore"])
     & data.new[i,"Plants"] > data.new[i,"Fruit.Nectar"]
     & data.new[i,"Plants"] > data.new[i,"Seeds"]
     & !is.na(data.new[i,"Fruit.Nectar"]) 
     & !is.na(data.new[i,"Seeds"]))
    {data.new[i,"Dietary guilds"] <- "Folivore"}
  
  # OMNIVORE
  if(data.new[i,"Carnivore"] >= 40 & data.new[i,"Herbivore"] >= 40
     & !is.na(data.new[i,"Carnivore"]) 
     & !is.na(data.new[i,"Herbivore"]))
    {data.new[i,"Dietary guilds"] <- "Omnivore"}
  
  # SPECIAL CASES
  if(data.new[i,"Carnivore"] > data.new[i,"Herbivore"] 
     & !is.na(data.new[i,"Herbivore"])
     & data.new[i,"Invertebrates"] == data.new[i,"Vertebrates"] 
     & !is.na(data.new[i,"Vertebrates"]))
    {data.new[i,"Dietary guilds"] <- "Carnivore"}
  
  if(data.new[i,"Herbivore"] > data.new[i,"Carnivore"]       
     & !is.na(data.new[i,"Carnivore"])
     & data.new[i,"Plants"] > data.new[i,"Fruit.Nectar"]
     & data.new[i,"Plants"] == data.new[i,"Seeds"]
     & !is.na(data.new[i,"Fruit.Nectar"])
     & !is.na(data.new[i,"Seeds"]))
    {data.new[i,"Dietary guilds"] <- "Herbivore"}
  
  if(data.new[i,"Herbivore"] > data.new[i,"Carnivore"]       
     & !is.na(data.new[i,"Carnivore"])
     & data.new[i,"Plants"] == data.new[i,"Fruit.Nectar"]
     & data.new[i,"Plants"] > data.new[i,"Seeds"]
     & !is.na(data.new[i,"Fruit.Nectar"])
     & !is.na(data.new[i,"Seeds"]))
    {data.new[i,"Dietary guilds"] <- "Herbivore"}
  
  if(data.new[i,"Herbivore"] > data.new[i,"Carnivore"]       
     & !is.na(data.new[i,"Carnivore"])
     & data.new[i,"Seeds"] > data.new[i,"Fruit.Nectar"]
     & data.new[i,"Seeds"] == data.new[i,"Plants"]
     & !is.na(data.new[i,"Fruit.Nectar"])
     & !is.na(data.new[i,"Plants"]))
    {data.new[i,"Dietary guilds"] <- "Herbivore"}
  
  if(data.new[i,"Herbivore"] > data.new[i,"Carnivore"]       
     & !is.na(data.new[i,"Carnivore"])
     & data.new[i,"Seeds"] == data.new[i,"Fruit.Nectar"]
     & data.new[i,"Seeds"] > data.new[i,"Plants"]
     & !is.na(data.new[i,"Fruit.Nectar"])
     & !is.na(data.new[i,"Plants"]))
    {data.new[i,"Dietary guilds"] <- "Herbivore"}
  
  if(data.new[i,"Herbivore"] > data.new[i,"Carnivore"]       
     & !is.na(data.new[i,"Carnivore"])
     & data.new[i,"Fruit.Nectar"] > data.new[i,"Seeds"]
     & data.new[i,"Fruit.Nectar"] == data.new[i,"Plants"]
     & !is.na(data.new[i,"Seeds"])
     & !is.na(data.new[i,"Plants"]))
    {data.new[i,"Dietary guilds"] <- "Herbivore"}
  
  if(data.new[i,"Herbivore"] > data.new[i,"Carnivore"]       
     & !is.na(data.new[i,"Carnivore"])
     & data.new[i,"Fruit.Nectar"] == data.new[i,"Seeds"]
     & data.new[i,"Fruit.Nectar"] > data.new[i,"Plants"]
     & !is.na(data.new[i,"Seeds"])
     & !is.na(data.new[i,"Plants"]))
    {data.new[i,"Dietary guilds"] <- "Herbivore"}
   
   print(i)
}

#View(data.new)
setwd("C:/Users/Mr-Wo/Desktop/Bachelorarbeit/Data/Neue Daten")
write.csv(file = 'Mammal Data with different Feeding Groups.csv', data.new)


