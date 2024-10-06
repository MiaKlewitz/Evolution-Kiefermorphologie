# Needed librarys
library(scales)
library(car)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(ggnewscale)
library(patchwork)
# ------------------------------------------
# GET DATA
# ------------------------------------------
# setwd("C:/Users/Mr-Wo/Desktop/BA/Neue Daten/Deutsch")
# all.data <- read.csv("Data Deutsch.csv", sep = ';', header = T)
setwd("C:/Users/Mr-Wo/Desktop")
all.data <- read.csv("Analyse Daten - Kurze Version.csv", sep = ';', header = T)

# Calculate Ratios
all.data$diastema.ratio<-(all.data$diastema.length/all.data$dental.row.length)
all.data$molar.ratio<-(all.data$molar.row.length/all.data$dental.row.length)
all.data$closing.ma.premolar<-(all.data$distance.from.condyle.to.coronoid/all.data$distance.from.condyle.to.middle.cusp.last.premolar)
all.data$jaw.slenderness<-((all.data$jaw.depth/all.data$jaw.length)*100)
all.data$coronoid.slenderness<-((all.data$coronoid.length/all.data$coronoid.width)*100)
all.data$trough.offset.ratio<-(all.data$distance.from.jaw.joint.to.tooth.row/all.data$jaw.length)

# ------------------------------------------
#  Principle Component Analysis (PCA)
#  FOR: Rodents, Multituberculata
# ------------------------------------------
rodent.data<-all.data %>% filter (Ordnung != 'Primaten')

# 1. ARCsine the ratios
# ---------------------
# (arcsine) logit() scews the data and spreads it out more at the tails of the distribution
rodent.data$diastema.ratio.logit<-logit(rodent.data$diastema.ratio,percents=max(rodent.data$diastema.ratio, na.rm = TRUE) > 1)
# Replacement of invalid values with NA:
rodent.data$molar.ratio[rodent.data$molar.ratio < 0 | rodent.data$molar.ratio > 1] <- NA
rodent.data$molar.ratio.logit<-logit(rodent.data$molar.ratio,percents=max(rodent.data$molar.ratio, na.rm = TRUE) > 1)

# 2. z-transform
# --------------
# (z-transform) scale() takes all of you ratios and rescales them between 0 and 1,
# so that small and large things can be equally compared
rodent.data$diastema.ratio.z<-scale(rodent.data[,'diastema.ratio.logit'])
rodent.data$molar.ratio.z<-scale(rodent.data[,'molar.ratio.logit'])
rodent.data$closing.ma.premolar.z<-scale(rodent.data[,'closing.ma.premolar'])   
rodent.data$jaw.slenderness.z<-scale(rodent.data[,'jaw.slenderness'])
rodent.data$coronoid.slenderness.z<-scale(rodent.data[,'coronoid.slenderness'])
rodent.data$trough.offset.ratio.z<-scale(rodent.data[,'trough.offset.ratio'])

# 3. Apply PCA
# ------------
rodent.data <- rodent.data[complete.cases(rodent.data[ ,c("diastema.ratio.z","molar.ratio.z","closing.ma.premolar.z",
                                                          "jaw.slenderness.z","coronoid.slenderness.z","trough.offset.ratio.z")]), ]


rodent.pca.func <- prcomp(subset(rodent.data, select = c(diastema.ratio.z:trough.offset.ratio.z)), scale. = T)
rodent.summ <- summary(rodent.pca.func)
rodent.summ
rodent.summ$importance[2,]  #second row [2,] of importance data

# just the pca for dist matrix production
rodent.pca.func.x<-rodent.pca.func$x

# add pca columns
rodent.func<-cbind(rodent.data,rodent.pca.func.x)



# ---------------
# get the percentage variance that is explained by each principle component / see which ratios correlate with which PCA axes
rodent.pca.func.loadings<-rodent.pca.func$rotation


# (x) DELETE RECENT MAMMALS WITHOUT DIET or known age (x)
to.delete.rodent<-rodent.func %>% filter (Alter =='Rezent' & is.na(Nahrungsstufe))

to.delete.rodent<- to.delete.rodent$Spezies

NA.func.rodent <- rodent.func[!rodent.func$Spezies %in% to.delete.rodent, ]

# order your time bins
NA.func.rodent$Alter<-factor(NA.func.rodent$Alter, levels = c("Jura", "Kreide", "Palaeozaen", "Eozaen", "Rezent"))

# (!) Replacing diet NA for the fossil data with "Unknown (Fossil)" (!)
# with a loop through all rows of the dataset
for (i in 1:nrow(NA.func.rodent)) {
  # Trophic levels:
  if(NA.func.rodent[i,"Alter"] != 'Rezent'
     & is.na(NA.func.rodent[i,"Nahrungsstufe"]))
  {NA.func.rodent[i,"Nahrungsstufe"] <- "Unbekannt"}
  
  # Diet trend:
  if(NA.func.rodent[i,"Alter"] != 'Rezent'
     & is.na(NA.func.rodent[i,"Ernaehrungstrend"]))
  {NA.func.rodent[i,"Ernaehrungstrend"] <- "Unbekannt"}
  
  # Dietary guilds:
  if(NA.func.rodent[i,"Alter"] != 'Rezent'
     & is.na(NA.func.rodent[i,"Ernaehrungsgruppen"]))
  {NA.func.rodent[i,"Ernaehrungsgruppen"] <- "Unbekannt"}
}

# (!) Adding column if species is "Extinct" or "Extant" (!)
# with a loop through all rows of the dataset
for (i in 1:nrow(NA.func.rodent)) {
  if(NA.func.rodent[i,"Alter"] == 'Rezent' & !is.na(NA.func.rodent[i,"Alter"]))
  {NA.func.rodent[i,"Status"] <- "Bestehend"}
  else 
  {NA.func.rodent[i,"Status"] <- "Ausgestorben"}
}

NA.func.rodent$Status <- factor(NA.func.rodent$Status, levels = rev(levels(factor(NA.func.rodent$Status))))


# x-----------x
#   PC1 & PC2
# x-----------x

# yhulls #
yfind_hull.rod <- function(NA.func.rodent) NA.func.rodent[chull(NA.func.rodent$PC1, NA.func.rodent$PC2), ]

yhulls.rod <- ddply(NA.func.rodent, .(Ordnung), yfind_hull.rod)
yhulls.rm.rod <- yhulls.rod
yhulls.rm.rod$Ernaehrungstrend <- NA  ## if you don't want the yhulls to be defined by a variable, replace it with NA in the yhulls dataset
yhulls.rm.rod$Status <- NA

# Subset the whole data:
## Subset: Alter ##
NA.func.recent.rod<-subset(NA.func.rodent, Alter == "Rezent")
NA.func.jur.rod<-subset(NA.func.rodent, Alter == "Jura")
NA.func.cre.rod<-subset(NA.func.rodent, Alter == "Kreide")
NA.func.pal.rod<-subset(NA.func.rodent, Alter == "Palaeozaen")
NA.func.eo.rod<-subset(NA.func.rodent, Alter == "Eozaen")

# Recent
yhulls.rec.rod <- ddply(NA.func.recent.rod, .(Ordnung), yfind_hull.rod)
yhulls.rec.rm.rod <- yhulls.rec.rod
yhulls.rec.rm.rod$Ernaehrungstrend <- NA  ## if you don't want the yhulls to be defined by a variable, replace it with NA in the yhulls dataset
yhulls.rec.rm.rod$Status <- NA
# Jurassic
yhulls.jur.rod <- ddply(NA.func.jur.rod, .(Ordnung), yfind_hull.rod)
yhulls.jur.rm.rod <- yhulls.jur.rod
yhulls.jur.rm.rod$Ernaehrungstrend <- NA  ## if you don't want the yhulls to be defined by a variable, replace it with NA in the yhulls dataset
yhulls.jur.rm.rod$Status <- NA
# Cretaceaous
yhulls.cre.rod <- ddply(NA.func.cre.rod, .(Ordnung), yfind_hull.rod)
yhulls.cre.rm.rod <- yhulls.cre.rod
yhulls.cre.rm.rod$Ernaehrungstrend <- NA  ## if you don't want the yhulls to be defined by a variable, replace it with NA in the yhulls dataset
yhulls.cre.rm.rod$Status <- NA
# Eocene
yhulls.eo.rod <- ddply(NA.func.eo.rod, .(Ordnung), yfind_hull.rod)
yhulls.eo.rm.rod <- yhulls.eo.rod
yhulls.eo.rm.rod$Ernaehrungstrend <- NA  ## if you don't want the yhulls to be defined by a variable, replace it with NA in the yhulls dataset
yhulls.eo.rm.rod$Status <- NA

## Subset: FEEDING GROUPS ##
# Trophic Level
yhulls.rec.trophic.rod <- ddply(NA.func.recent.rod, .(Nahrungsstufe), yfind_hull.rod)
yhulls.rec.trophic.rm.rod <- yhulls.rec.trophic.rod
yhulls.rec.trophic.rm.rod$Ordnung <- NA  ## if you don't want the yhulls to be defined by a variable, replace it with NA in the yhulls dataset
yhulls.rec.trophic.rm.rod$Status <- NA
# Diet Trend
yhulls.rec.diet.rod <- ddply(NA.func.recent.rod, .(Ernaehrungstrend), yfind_hull.rod)
yhulls.rec.diet.rm.rod <- yhulls.rec.diet.rod
yhulls.rec.diet.rm.rod$Ordnung <- NA  ## if you don't want the yhulls to be defined by a variable, replace it with NA in the yhulls dataset
yhulls.rec.diet.rm.rod$Status <- NA
# Dietary Guilds
yhulls.rec.dietary.rod <- ddply(NA.func.recent.rod, .(Ernaehrungsgruppen), yfind_hull.rod)
yhulls.rec.dietary.rm.rod <- yhulls.rec.dietary.rod
yhulls.rec.dietary.rm.rod$Ordnung <- NA  ## if you don't want the yhulls to be defined by a variable, replace it with NA in the yhulls dataset
yhulls.rec.dietary.rm.rod$Status <- NA

# NEW STUFF #

##### make dataset that will plot modern yhulls on other time slices
# Trophic level
yhulls.rec.trophic.rm.rod.JUR <- yhulls.rec.trophic.rm.rod
for (i in 1:nrow(yhulls.rec.trophic.rm.rod.JUR)) {           ### Doing it this way instead of 'yhulls.rec.trophic.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(yhulls.rec.trophic.rm.rod.JUR[i,"Alter"] == 'Rezent')
  {yhulls.rec.trophic.rm.rod.JUR[i,"Alter"] <- "Jura"}
}

yhulls.rec.trophic.rm.rod.CRE <- yhulls.rec.trophic.rm.rod
for (i in 1:nrow(yhulls.rec.trophic.rm.rod.CRE)) {           ### Doing it this way instead of 'yhulls.rec.trophic.rm.rod.JUR$Alter <- 'Cretaceous'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(yhulls.rec.trophic.rm.rod.CRE[i,"Alter"] == 'Rezent')
  {yhulls.rec.trophic.rm.rod.CRE[i,"Alter"] <- "Kreide"}
}

yhulls.rec.trophic.rm.rod.PAL <- yhulls.rec.trophic.rm.rod
for (i in 1:nrow(yhulls.rec.trophic.rm.rod.PAL)) {           ### Doing it this way instead of 'yhulls.rec.trophic.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(yhulls.rec.trophic.rm.rod.PAL[i,"Alter"] == 'Rezent')
  {yhulls.rec.trophic.rm.rod.PAL[i,"Alter"] <- "Palaeozaen"}
}

yhulls.rec.trophic.rm.rod.EO <- yhulls.rec.trophic.rm.rod
for (i in 1:nrow(yhulls.rec.trophic.rm.rod.EO)) {           ### Doing it this way instead of 'yhulls.rec.trophic.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(yhulls.rec.trophic.rm.rod.EO[i,"Alter"] == 'Rezent')
  {yhulls.rec.trophic.rm.rod.EO[i,"Alter"] <- "Eozaen"}
}

# Diet trend
yhulls.rec.diet.rm.rod.JUR <- yhulls.rec.diet.rm.rod
for (i in 1:nrow(yhulls.rec.diet.rm.rod.JUR)) {           ### Doing it this way instead of 'yhulls.rec.trophic.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(yhulls.rec.diet.rm.rod.JUR[i,"Alter"] == 'Rezent')
  {yhulls.rec.diet.rm.rod.JUR[i,"Alter"] <- "Jura"}
}
yhulls.rec.diet.rm.rod.CRE <- yhulls.rec.diet.rm.rod
for (i in 1:nrow(yhulls.rec.diet.rm.rod.CRE)) {           ### Doing it this way instead of 'yhulls.rec.diet.rm.rod.JUR$Alter <- 'Cretaceous'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(yhulls.rec.diet.rm.rod.CRE[i,"Alter"] == 'Rezent')
  {yhulls.rec.diet.rm.rod.CRE[i,"Alter"] <- "Kreide"}
}

yhulls.rec.diet.rm.rod.PAL <- yhulls.rec.diet.rm.rod
for (i in 1:nrow(yhulls.rec.diet.rm.rod.PAL)) {           ### Doing it this way instead of 'yhulls.rec.diet.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(yhulls.rec.diet.rm.rod.PAL[i,"Alter"] == 'Rezent')
  {yhulls.rec.diet.rm.rod.PAL[i,"Alter"] <- "Palaeozaen"}
}

yhulls.rec.diet.rm.rod.EO <- yhulls.rec.diet.rm.rod
for (i in 1:nrow(yhulls.rec.diet.rm.rod.EO)) {           ### Doing it this way instead of 'yhulls.rec.diet.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(yhulls.rec.diet.rm.rod.EO[i,"Alter"] == 'Rezent')
  {yhulls.rec.diet.rm.rod.EO[i,"Alter"] <- "Eozaen"}
}
# Dietary guilds
yhulls.rec.dietary.rm.rod.JUR <- yhulls.rec.dietary.rm.rod
for (i in 1:nrow(yhulls.rec.dietary.rm.rod.JUR)) {           ### Doing it this way instead of 'yhulls.rec.dietary.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(yhulls.rec.dietary.rm.rod.JUR[i,"Alter"] == 'Rezent')
  {yhulls.rec.dietary.rm.rod.JUR[i,"Alter"] <- "Jura"}
}

yhulls.rec.dietary.rm.rod.CRE <- yhulls.rec.dietary.rm.rod
for (i in 1:nrow(yhulls.rec.dietary.rm.rod.CRE)) {           ### Doing it this way instead of 'yhulls.rec.dietary.rm.rod.JUR$Alter <- 'Cretaceous'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(yhulls.rec.dietary.rm.rod.CRE[i,"Alter"] == 'Rezent')
  {yhulls.rec.dietary.rm.rod.CRE[i,"Alter"] <- "Kreide"}
}

yhulls.rec.dietary.rm.rod.PAL <- yhulls.rec.dietary.rm.rod
for (i in 1:nrow(yhulls.rec.dietary.rm.rod.PAL)) {           ### Doing it this way instead of 'yhulls.rec.dietary.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(yhulls.rec.dietary.rm.rod.PAL[i,"Alter"] == 'Rezent')
  {yhulls.rec.dietary.rm.rod.PAL[i,"Alter"] <- "Palaeozaen"}
}

yhulls.rec.dietary.rm.rod.EO <- yhulls.rec.dietary.rm.rod
for (i in 1:nrow(yhulls.rec.dietary.rm.rod.EO)) {           ### Doing it this way instead of 'yhulls.rec.dietary.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(yhulls.rec.dietary.rm.rod.EO[i,"Alter"] == 'Rezent')
  {yhulls.rec.dietary.rm.rod.EO[i,"Alter"] <- "Eozaen"}
}

# x-----------x
#   PC3 & PC4
# x-----------x

# zhulls #
zfind_hull.rod <- function(NA.func.rodent) NA.func.rodent[chull(NA.func.rodent$PC3, NA.func.rodent$PC4), ]

zhulls.rod <- ddply(NA.func.rodent, .(Ordnung), zfind_hull.rod)
zhulls.rm.rod <- zhulls.rod
zhulls.rm.rod$Ernaehrungstrend <- NA  ## if you don't want the zhulls to be defined by a variable, replace it with NA in the zhulls dataset
zhulls.rm.rod$Status <- NA

# Subset the whole data:
## Subset: Alter ##
NA.func.recent.rod<-subset(NA.func.rodent, Alter == "Rezent")
NA.func.jur.rod<-subset(NA.func.rodent, Alter == "Jura")
NA.func.cre.rod<-subset(NA.func.rodent, Alter == "Kreide")
NA.func.pal.rod<-subset(NA.func.rodent, Alter == "Palaeozaen")
NA.func.eo.rod<-subset(NA.func.rodent, Alter == "Eozaen")

# Recent
zhulls.rec.rod <- ddply(NA.func.recent.rod, .(Ordnung), zfind_hull.rod)
zhulls.rec.rm.rod <- zhulls.rec.rod
zhulls.rec.rm.rod$Ernaehrungstrend <- NA  ## if you don't want the zhulls to be defined by a variable, replace it with NA in the zhulls dataset
zhulls.rec.rm.rod$Status <- NA
# Jurassic
zhulls.jur.rod <- ddply(NA.func.jur.rod, .(Ordnung), zfind_hull.rod)
zhulls.jur.rm.rod <- zhulls.jur.rod
zhulls.jur.rm.rod$Ernaehrungstrend <- NA  ## if you don't want the zhulls to be defined by a variable, replace it with NA in the zhulls dataset
zhulls.jur.rm.rod$Status <- NA
# Cretaceaous
zhulls.cre.rod <- ddply(NA.func.cre.rod, .(Ordnung), zfind_hull.rod)
zhulls.cre.rm.rod <- zhulls.cre.rod
zhulls.cre.rm.rod$Ernaehrungstrend <- NA  ## if you don't want the zhulls to be defined by a variable, replace it with NA in the zhulls dataset
zhulls.cre.rm.rod$Status <- NA
# Eocene
zhulls.eo.rod <- ddply(NA.func.eo.rod, .(Ordnung), zfind_hull.rod)
zhulls.eo.rm.rod <- zhulls.eo.rod
zhulls.eo.rm.rod$Ernaehrungstrend <- NA  ## if you don't want the zhulls to be defined by a variable, replace it with NA in the zhulls dataset
zhulls.eo.rm.rod$Status <- NA

## Subset: FEEDING GROUPS ##
# Trophic Level
zhulls.rec.trophic.rod <- ddply(NA.func.recent.rod, .(Nahrungsstufe), zfind_hull.rod)
zhulls.rec.trophic.rm.rod <- zhulls.rec.trophic.rod
zhulls.rec.trophic.rm.rod$Ordnung <- NA  ## if you don't want the zhulls to be defined by a variable, replace it with NA in the zhulls dataset
zhulls.rec.trophic.rm.rod$Status <- NA
# Diet Trend
zhulls.rec.diet.rod <- ddply(NA.func.recent.rod, .(Ernaehrungstrend), zfind_hull.rod)
zhulls.rec.diet.rm.rod <- zhulls.rec.diet.rod
zhulls.rec.diet.rm.rod$Ordnung <- NA  ## if you don't want the zhulls to be defined by a variable, replace it with NA in the zhulls dataset
zhulls.rec.diet.rm.rod$Status <- NA
# Dietary Guilds
zhulls.rec.dietary.rod <- ddply(NA.func.recent.rod, .(Ernaehrungsgruppen), zfind_hull.rod)
zhulls.rec.dietary.rm.rod <- zhulls.rec.dietary.rod
zhulls.rec.dietary.rm.rod$Ordnung <- NA  ## if you don't want the zhulls to be defined by a variable, replace it with NA in the zhulls dataset
zhulls.rec.dietary.rm.rod$Status <- NA

# NEW STUFF #

##### make dataset that will plot modern zhulls on other time slices
# Trophic level
zhulls.rec.trophic.rm.rod.JUR <- zhulls.rec.trophic.rm.rod
for (i in 1:nrow(zhulls.rec.trophic.rm.rod.JUR)) {           ### Doing it this way instead of 'zhulls.rec.trophic.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(zhulls.rec.trophic.rm.rod.JUR[i,"Alter"] == 'Rezent')
  {zhulls.rec.trophic.rm.rod.JUR[i,"Alter"] <- "Jura"}
}

zhulls.rec.trophic.rm.rod.CRE <- zhulls.rec.trophic.rm.rod
for (i in 1:nrow(zhulls.rec.trophic.rm.rod.CRE)) {           ### Doing it this way instead of 'zhulls.rec.trophic.rm.rod.JUR$Alter <- 'Cretaceous'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(zhulls.rec.trophic.rm.rod.CRE[i,"Alter"] == 'Rezent')
  {zhulls.rec.trophic.rm.rod.CRE[i,"Alter"] <- "Kreide"}
}

zhulls.rec.trophic.rm.rod.PAL <- zhulls.rec.trophic.rm.rod
for (i in 1:nrow(zhulls.rec.trophic.rm.rod.PAL)) {           ### Doing it this way instead of 'zhulls.rec.trophic.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(zhulls.rec.trophic.rm.rod.PAL[i,"Alter"] == 'Rezent')
  {zhulls.rec.trophic.rm.rod.PAL[i,"Alter"] <- "Palaeozaen"}
}

zhulls.rec.trophic.rm.rod.EO <- zhulls.rec.trophic.rm.rod
for (i in 1:nrow(zhulls.rec.trophic.rm.rod.EO)) {           ### Doing it this way instead of 'zhulls.rec.trophic.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(zhulls.rec.trophic.rm.rod.EO[i,"Alter"] == 'Rezent')
  {zhulls.rec.trophic.rm.rod.EO[i,"Alter"] <- "Eozaen"}
}

# Diet trend
zhulls.rec.diet.rm.rod.JUR <- zhulls.rec.diet.rm.rod
for (i in 1:nrow(zhulls.rec.diet.rm.rod.JUR)) {           ### Doing it this way instead of 'zhulls.rec.trophic.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(zhulls.rec.diet.rm.rod.JUR[i,"Alter"] == 'Rezent')
  {zhulls.rec.diet.rm.rod.JUR[i,"Alter"] <- "Jura"}
}
zhulls.rec.diet.rm.rod.CRE <- zhulls.rec.diet.rm.rod
for (i in 1:nrow(zhulls.rec.diet.rm.rod.CRE)) {           ### Doing it this way instead of 'zhulls.rec.diet.rm.rod.JUR$Alter <- 'Cretaceous'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(zhulls.rec.diet.rm.rod.CRE[i,"Alter"] == 'Rezent')
  {zhulls.rec.diet.rm.rod.CRE[i,"Alter"] <- "Kreide"}
}

zhulls.rec.diet.rm.rod.PAL <- zhulls.rec.diet.rm.rod
for (i in 1:nrow(zhulls.rec.diet.rm.rod.PAL)) {           ### Doing it this way instead of 'zhulls.rec.diet.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(zhulls.rec.diet.rm.rod.PAL[i,"Alter"] == 'Rezent')
  {zhulls.rec.diet.rm.rod.PAL[i,"Alter"] <- "Palaeozaen"}
}

zhulls.rec.diet.rm.rod.EO <- zhulls.rec.diet.rm.rod
for (i in 1:nrow(zhulls.rec.diet.rm.rod.EO)) {           ### Doing it this way instead of 'zhulls.rec.diet.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(zhulls.rec.diet.rm.rod.EO[i,"Alter"] == 'Rezent')
  {zhulls.rec.diet.rm.rod.EO[i,"Alter"] <- "Eozaen"}
}
# Dietary guilds
zhulls.rec.dietary.rm.rod.JUR <- zhulls.rec.dietary.rm.rod
for (i in 1:nrow(zhulls.rec.dietary.rm.rod.JUR)) {           ### Doing it this way instead of 'zhulls.rec.dietary.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(zhulls.rec.dietary.rm.rod.JUR[i,"Alter"] == 'Rezent')
  {zhulls.rec.dietary.rm.rod.JUR[i,"Alter"] <- "Jura"}
}

zhulls.rec.dietary.rm.rod.CRE <- zhulls.rec.dietary.rm.rod
for (i in 1:nrow(zhulls.rec.dietary.rm.rod.CRE)) {           ### Doing it this way instead of 'zhulls.rec.dietary.rm.rod.JUR$Alter <- 'Cretaceous'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(zhulls.rec.dietary.rm.rod.CRE[i,"Alter"] == 'Rezent')
  {zhulls.rec.dietary.rm.rod.CRE[i,"Alter"] <- "Kreide"}
}

zhulls.rec.dietary.rm.rod.PAL <- zhulls.rec.dietary.rm.rod
for (i in 1:nrow(zhulls.rec.dietary.rm.rod.PAL)) {           ### Doing it this way instead of 'zhulls.rec.dietary.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(zhulls.rec.dietary.rm.rod.PAL[i,"Alter"] == 'Rezent')
  {zhulls.rec.dietary.rm.rod.PAL[i,"Alter"] <- "Palaeozaen"}
}

zhulls.rec.dietary.rm.rod.EO <- zhulls.rec.dietary.rm.rod
for (i in 1:nrow(zhulls.rec.dietary.rm.rod.EO)) {           ### Doing it this way instead of 'zhulls.rec.dietary.rm.rod.JUR$Alter <- 'Jurassic'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(zhulls.rec.dietary.rm.rod.EO[i,"Alter"] == 'Rezent')
  {zhulls.rec.dietary.rm.rod.EO[i,"Alter"] <- "Eozaen"}
}

# ---------------------------
# PLOT 1-2
plot1 <-
ggplot(data = NA.func.rodent, aes(PC1, PC2, fill = Nahrungsstufe, pch = Ordnung, size = Status))+  
#ggplot(data = NA.func.rodent, aes(PC1, PC2, fill = Ernaehrungstrend, pch = Ordnung, size = Status))+ 
#ggplot(data = NA.func.rodent, aes(PC1, PC2, fill = Ernaehrungsgruppen, pch = Ordnung, size = Status))+ 
  facet_wrap(~ Alter, ncol = 5) +
  # ----
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  scale_shape_manual(values = c(21,24)) +  # shapes with boarders (boarder = col, main colour = fill)
  scale_fill_manual(values = c('coral','dodgerblue'))+
  # ----
  #geom_point(col = 'black') +
  #geom_polygon(data = yhulls.rm.rod, alpha = 0, size = 0.6) + # NO facet wrap!
  #stat_ellipse(geom = "polygon", aes(fill = Ordnung), alpha = 0.25) +  #these OR yhulls
  # ---- STUFE
  geom_polygon(data = yhulls.rec.trophic.rm.rod.JUR, alpha = 0.4, size = 0.6) +
  geom_polygon(data = yhulls.rec.trophic.rm.rod.CRE, alpha = 0.4, size = 0.6) +
  geom_polygon(data = yhulls.rec.trophic.rm.rod.PAL, alpha = 0.4, size = 0.6) +
  geom_polygon(data = yhulls.rec.trophic.rm.rod.EO, alpha = 0.4, size = 0.6) +
  geom_polygon(data = yhulls.rec.trophic.rm.rod, alpha = 0.4, size = 0.6) +
  scale_fill_manual(values=c('darkgreen','darkred','blue', 'white'))+
  geom_point(col = 'black') + #option 1
  # ---- TREND
  # geom_polygon(data = yhulls.rec.diet.rm.rod.JUR, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = yhulls.rec.diet.rm.rod.CRE, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = yhulls.rec.diet.rm.rod.PAL, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = yhulls.rec.diet.rm.rod.EO, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = yhulls.rec.diet.rm.rod, alpha = 0.4, size = 0.6) +
  # scale_fill_manual(values=c('darkgreen','darkred','blue','white','green','red'))+
  # geom_point(col = 'black') + #option 1
  # ---- GRUPPE
  # geom_polygon(data = yhulls.rec.dietary.rm.rod.JUR, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = yhulls.rec.dietary.rm.rod.CRE, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = yhulls.rec.dietary.rm.rod.PAL, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = yhulls.rec.dietary.rm.rod.EO, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = yhulls.rec.dietary.rm.rod, alpha = 0.4, size = 0.6) +
  # scale_fill_manual(values=c('lightgreen','magenta','orange','darkgreen','purple','darkred','blue','white'))+
  # geom_point(col = 'black')+
  # ----
  #geom_text(size=3.6, col ="black", hjust=-0.07, vjust=0.3, fontface="italic") +
  # ----
  xlab("PC1 (29.56%)") +   
  ylab("PC2 (22.04%)") +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20),
        strip.text = element_text(size = 20))+
  theme(legend.text=element_text(size =20), 
        legend.position='none',
        legend.title.position = 'top',
        legend.title=element_text(size =20, face="bold"),
        legend.key.width = unit(1.5, 'cm'))+
  guides(pch = guide_legend(order=2,ncol = 1,override.aes = list(size =5)), size = guide_legend(order=1,ncol = 1),
         fill = guide_legend(order=3,ncol =3, override.aes = list(shape = NA)))
# Plot 3-4
plot2 <-
  ggplot(data = NA.func.rodent, aes(PC3, PC4, fill = Nahrungsstufe, pch = Ordnung, size = Status))+  
  #ggplot(data = NA.func.rodent, aes(PC3, PC4, fill = Ernaehrungstrend, pch = Ordnung, size = Status))+ 
  #ggplot(data = NA.func.rodent, aes(PC3, PC4, fill = Ernaehrungsgruppen, pch = Ordnung, size = Status))+ 
  facet_wrap(~ Alter, ncol = 5) +
  # ----
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  scale_shape_manual(values = c(21,24)) +  # shapes with boarders (boarder = col, main colour = fill)
  scale_fill_manual(values = c('coral','dodgerblue'))+
  # ----
  #geom_point(col = 'black') +
  #geom_polygon(data = zhulls.rm.rod, alpha = 0, size = 0.6) + # NO facet wrap!
  #stat_ellipse(geom = "polygon", aes(fill = Ordnung), alpha = 0.25) +  #these OR zhulls
  # ---- STUFE
  geom_polygon(data = zhulls.rec.trophic.rm.rod.JUR, alpha = 0.4, size = 0.6) +
  geom_polygon(data = zhulls.rec.trophic.rm.rod.CRE, alpha = 0.4, size = 0.6) +
  geom_polygon(data = zhulls.rec.trophic.rm.rod.PAL, alpha = 0.4, size = 0.6) +
  geom_polygon(data = zhulls.rec.trophic.rm.rod.EO, alpha = 0.4, size = 0.6) +
  geom_polygon(data = zhulls.rec.trophic.rm.rod, alpha = 0.4, size = 0.6) +
  scale_fill_manual(values=c('darkgreen','darkred','blue', 'white'))+
  geom_point(col = 'black') + #option 1
  # ---- TREND
  # geom_polygon(data = zhulls.rec.diet.rm.rod.JUR, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = zhulls.rec.diet.rm.rod.CRE, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = zhulls.rec.diet.rm.rod.PAL, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = zhulls.rec.diet.rm.rod.EO, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = zhulls.rec.diet.rm.rod, alpha = 0.4, size = 0.6) +
  # scale_fill_manual(values=c('darkgreen','darkred','blue','white','green','red'))+
  # geom_point(col = 'black') + #option 1
  # ---- GRUPPE
  # geom_polygon(data = zhulls.rec.dietary.rm.rod.JUR, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = zhulls.rec.dietary.rm.rod.CRE, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = zhulls.rec.dietary.rm.rod.PAL, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = zhulls.rec.dietary.rm.rod.EO, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = zhulls.rec.dietary.rm.rod, alpha = 0.4, size = 0.6) +
  # scale_fill_manual(values=c('lightgreen','magenta','orange','darkgreen','purple','darkred','blue','white'))+
  # geom_point(col = 'black')+
  # ----
  #geom_text(size=3.6, col ="black", hjust=-0.07, vjust=0.3, fontface="italic") +
  # ----
  xlab("PC3 (16.26%)") +   
  ylab("PC4 (15.83%)") +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20),
        strip.text = element_text(size = 20))+
  theme(legend.text=element_text(size =20), 
        legend.position='bottom',
        legend.title.position = 'top',
        legend.title=element_text(size =20, face="bold"),
        legend.key.width = unit(1.5, 'cm'))+
  guides(pch = guide_legend(order=2,ncol = 1,override.aes = list(size =5)), size = guide_legend(order=1,ncol = 1),
         fill = guide_legend(order=3,ncol =3, override.aes = list(shape = NA)))

# ---------------------------
combined_plot <- plot1 / plot2 + plot_layout(guides = "collect") & theme(legend.position = "bottom") 
combined_plot
