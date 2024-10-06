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
#  FOR: Primaten, Rodents, Multituberculata
# ------------------------------------------
# 1. ARCsine the ratios
# ---------------------
# (arcsine) logit() scews the data and spreads it out more at the tails of the distribution
all.data$diastema.ratio.logit<-logit(all.data$diastema.ratio,percents=max(all.data$diastema.ratio, na.rm = TRUE) > 1)
# Replacement of invalid values with NA:
all.data$molar.ratio[all.data$molar.ratio < 0 | all.data$molar.ratio > 1] <- NA
all.data$molar.ratio.logit<-logit(all.data$molar.ratio,percents=max(all.data$molar.ratio, na.rm = TRUE) > 1)

# 2. z-transform
# --------------
# (z-transform) scale() takes all of you ratios and rescales them between 0 and 1,
# so that small and large things can be equally compared
all.data$diastema.ratio.z<-scale(all.data[,'diastema.ratio.logit'])
all.data$molar.ratio.z<-scale(all.data[,'molar.ratio.logit'])
all.data$closing.ma.premolar.z<-scale(all.data[,'closing.ma.premolar'])   
all.data$jaw.slenderness.z<-scale(all.data[,'jaw.slenderness'])
all.data$coronoid.slenderness.z<-scale(all.data[,'coronoid.slenderness'])
all.data$trough.offset.ratio.z<-scale(all.data[,'trough.offset.ratio'])

# 3. Apply PCA
# ------------
all.data <- all.data[complete.cases(all.data[ ,c("diastema.ratio.z","molar.ratio.z","closing.ma.premolar.z",
                                                 "jaw.slenderness.z","coronoid.slenderness.z","trough.offset.ratio.z")]), ]

all.pca.func <- prcomp(subset(all.data, select = c(diastema.ratio.z:trough.offset.ratio.z)), scale. = T)
all.summ <- summary(all.pca.func)
all.summ
all.summ$importance[2,]  #second row [2,] of importance data

# just the pca for dist matrix production
all.pca.func.x<-all.pca.func$x

# add pca columns
all.func<-cbind(all.data,all.pca.func.x)

# 4. Get Loadings
# ---------------
# get the percentage variance that is explained by each principle component / see which ratios correlate with which PCA axes
all.pca.func.loadings<-all.pca.func$rotation

# (X) Delete Rezent mammals without diet (x)
to.delete.all<-all.func %>% filter (Alter =='Rezent' & is.na(Nahrungsstufe))

to.delete.all<- to.delete.all$Spezies

NA.func.all <- all.func[!all.func$Spezies %in% to.delete.all, ]

# order your time bins
all.func$Alter<-factor(all.func$Alter, levels = c("Jura", "Kreide", "Palaeozaen", "Eozaen", "Rezent"))
NA.func.all$Alter<-factor(NA.func.all$Alter, levels = c("Jura", "Kreide", "Palaeozaen", "Eozaen", "Rezent"))

# (!) Replacing diet NA for the Status data with "Unbekannt" (!)
# with a loop through all rows of the dataset
for (i in 1:nrow(NA.func.all)) {
  # Trophic levels:
  if(NA.func.all[i,"Alter"] != 'Rezent'
     & is.na(NA.func.all[i,"Nahrungsstufe"]))
  {NA.func.all[i,"Nahrungsstufe"] <- "Unbekannt"}
  
  # Diet trend:
  if(NA.func.all[i,"Alter"] != 'Rezent'
     & is.na(NA.func.all[i,"Ernaehrungstrend"]))
  {NA.func.all[i,"Ernaehrungstrend"] <- "Unbekannt"}
  
  # Dietary guilds:
  if(NA.func.all[i,"Alter"] != 'Rezent'
     & is.na(NA.func.all[i,"Ernaehrungsgruppen"]))
  {NA.func.all[i,"Ernaehrungsgruppen"] <- "Unbekannt"}
}

# (!) Adding column if Spezies is "Ausgestorben" or "Bestehend" (!)
# with a loop through all rows of the dataset
for (i in 1:nrow(all.func)) {
  # Trophic levels:
  if(all.func[i,"Alter"] == 'Rezent')
  {all.func[i,"Status"] <- "Bestehend"}
  else 
  {all.func[i,"Status"] <- "Ausgestorben"}
}
all.func$Status <- factor(all.func$Status, levels = rev(levels(factor(all.func$Status))))

for (i in 1:nrow(NA.func.all)) {
  # Trophic levels:
  if(NA.func.all[i,"Alter"] == 'Rezent')
  {NA.func.all[i,"Status"] <- "Bestehend"}
  else 
  {NA.func.all[i,"Status"] <- "Ausgestorben"}
}
NA.func.all$Status <- factor(NA.func.all$Status, levels = rev(levels(factor(NA.func.all$Status))))

#Plot PCA
# -----------
# x-----------x
#   PC1 & PC2
# x-----------x

# HULLS #
find_hull.all <- function(all.func) all.func[chull(all.func$PC1, all.func$PC2), ]
find_hull <- function(NA.func.all) NA.func.all[chull(NA.func.all$PC1, NA.func.all$PC2), ]

hulls.all <- ddply(all.func, .(Ordnung), find_hull.all)
hulls <- ddply(NA.func.all, .(Ordnung), find_hull)
hulls.rm <- hulls
hulls.rm$Ernaehrungstrend <- NA  ## if you don't want the hulls to be defined by a variable, replace it with NA in the hulls dataset
hulls.rm$Status <- NA

# Subset the whole data:
## Subset: Alter ##
all.func.Rezent<-subset(all.func, Alter == "Rezent")
all.func.jur<-subset(all.func, Alter == "Jura")
all.func.cre<-subset(all.func, Alter == "Kreide")
all.func.pal<-subset(all.func, Alter == "Palaeozaen")
all.func.eo<-subset(all.func, Alter == "Eozaen")

NA.func.Rezent<-subset(NA.func.all, Alter == "Rezent")
NA.func.jur<-subset(NA.func.all, Alter == "Jura")
NA.func.cre<-subset(NA.func.all, Alter == "Kreide")
NA.func.pal<-subset(NA.func.all, Alter == "Palaeozaen")
NA.func.eo<-subset(NA.func.all, Alter == "Eozaen")

# Rezent
hulls.rec <- ddply(NA.func.Rezent, .(Ordnung), find_hull)
hulls.rec.rm <- hulls.rec
hulls.rec.rm$Ernaehrungstrend <- NA  ## if you don't want the hulls to be defined by a variable, replace it with NA in the hulls dataset
hulls.rec.rm$Status <- NA
# Jura
hulls.jur <- ddply(NA.func.jur, .(Ordnung), find_hull)
hulls.jur.rm <- hulls.jur
hulls.jur.rm$Ernaehrungstrend <- NA  ## if you don't want the hulls to be defined by a variable, replace it with NA in the hulls dataset
hulls.jur.rm$Status <- NA
# Cretaceaous
hulls.cre <- ddply(NA.func.cre, .(Ordnung), find_hull)
hulls.cre.rm <- hulls.cre
hulls.cre.rm$Ernaehrungstrend <- NA  ## if you don't want the hulls to be defined by a variable, replace it with NA in the hulls dataset
hulls.cre.rm$Status <- NA
# Palaeozaen
hulls.pal <- ddply(NA.func.pal, .(Ordnung), find_hull)
hulls.pal.rm <- hulls.pal
hulls.pal.rm$Ernaehrungstrend <- NA  ## if you don't want the hulls to be defined by a variable, replace it with NA in the hulls dataset
hulls.pal.rm$Status <- NA
# Eozaen
hulls.eo <- ddply(NA.func.eo, .(Ordnung), find_hull)
hulls.eo.rm <- hulls.eo
hulls.eo.rm$Ernaehrungstrend <- NA  ## if you don't want the hulls to be defined by a variable, replace it with NA in the hulls dataset
hulls.eo.rm$Status <- NA

## Subset: FEEDING GROUPS ##
# Trophic Level
hulls.rec.trophic <- ddply(NA.func.Rezent, .(Nahrungsstufe), find_hull)
hulls.rec.trophic.rm <- hulls.rec.trophic
hulls.rec.trophic.rm$Ordnung <- NA  ## if you don't want the hulls to be defined by a variable, replace it with NA in the hulls dataset
hulls.rec.trophic.rm$Status <- NA
# Diet Trend
hulls.rec.diet <- ddply(NA.func.Rezent, .(Ernaehrungstrend), find_hull)
hulls.rec.diet.rm <- hulls.rec.diet
hulls.rec.diet.rm$Ordnung <- NA  ## if you don't want the hulls to be defined by a variable, replace it with NA in the hulls dataset
hulls.rec.diet.rm$Status <- NA
# Dietary Guilds
hulls.rec.dietary <- ddply(NA.func.Rezent, .(Ernaehrungsgruppen), find_hull)
hulls.rec.dietary.rm <- hulls.rec.dietary
hulls.rec.dietary.rm$Ordnung <- NA  ## if you don't want the hulls to be defined by a variable, replace it with NA in the hulls dataset
hulls.rec.dietary.rm$Status <- NA

# NEW STUFF #

##### make dataset that will plot modern hulls on other time slices
# Trophic level
hulls.rec.trophic.rm.JUR <- hulls.rec.trophic.rm
for (i in 1:nrow(hulls.rec.trophic.rm.JUR)) {           ### Doing it this way instead of 'hulls.rec.trophic.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(hulls.rec.trophic.rm.JUR[i,"Alter"] == 'Rezent')
  {hulls.rec.trophic.rm.JUR[i,"Alter"] <- "Jura"}
}

hulls.rec.trophic.rm.CRE <- hulls.rec.trophic.rm
for (i in 1:nrow(hulls.rec.trophic.rm.CRE)) {           ### Doing it this way instead of 'hulls.rec.trophic.rm.JUR$Alter <- 'Kreide'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(hulls.rec.trophic.rm.CRE[i,"Alter"] == 'Rezent')
  {hulls.rec.trophic.rm.CRE[i,"Alter"] <- "Kreide"}
}

hulls.rec.trophic.rm.PAL <- hulls.rec.trophic.rm
for (i in 1:nrow(hulls.rec.trophic.rm.PAL)) {           ### Doing it this way instead of 'hulls.rec.trophic.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(hulls.rec.trophic.rm.PAL[i,"Alter"] == 'Rezent')
  {hulls.rec.trophic.rm.PAL[i,"Alter"] <- "Palaeozaen"}
}

hulls.rec.trophic.rm.EO <- hulls.rec.trophic.rm
for (i in 1:nrow(hulls.rec.trophic.rm.EO)) {           ### Doing it this way instead of 'hulls.rec.trophic.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(hulls.rec.trophic.rm.EO[i,"Alter"] == 'Rezent')
  {hulls.rec.trophic.rm.EO[i,"Alter"] <- "Eozaen"}
}

# Diet trend
hulls.rec.diet.rm.JUR <- hulls.rec.diet.rm
for (i in 1:nrow(hulls.rec.diet.rm.JUR)) {           ### Doing it this way instead of 'hulls.rec.trophic.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(hulls.rec.diet.rm.JUR[i,"Alter"] == 'Rezent')
  {hulls.rec.diet.rm.JUR[i,"Alter"] <- "Jura"}
}
hulls.rec.diet.rm.CRE <- hulls.rec.diet.rm
for (i in 1:nrow(hulls.rec.diet.rm.CRE)) {           ### Doing it this way instead of 'hulls.rec.diet.rm.JUR$Alter <- 'Kreide'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(hulls.rec.diet.rm.CRE[i,"Alter"] == 'Rezent')
  {hulls.rec.diet.rm.CRE[i,"Alter"] <- "Kreide"}
}

hulls.rec.diet.rm.PAL <- hulls.rec.diet.rm
for (i in 1:nrow(hulls.rec.diet.rm.PAL)) {           ### Doing it this way instead of 'hulls.rec.diet.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(hulls.rec.diet.rm.PAL[i,"Alter"] == 'Rezent')
  {hulls.rec.diet.rm.PAL[i,"Alter"] <- "Palaeozaen"}
}

hulls.rec.diet.rm.EO <- hulls.rec.diet.rm
for (i in 1:nrow(hulls.rec.diet.rm.EO)) {           ### Doing it this way instead of 'hulls.rec.diet.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(hulls.rec.diet.rm.EO[i,"Alter"] == 'Rezent')
  {hulls.rec.diet.rm.EO[i,"Alter"] <- "Eozaen"}
}

# Dietary guilds
hulls.rec.dietary.rm.JUR <- hulls.rec.dietary.rm
for (i in 1:nrow(hulls.rec.dietary.rm.JUR)) {           ### Doing it this way instead of 'hulls.rec.dietary.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(hulls.rec.dietary.rm.JUR[i,"Alter"] == 'Rezent')
  {hulls.rec.dietary.rm.JUR[i,"Alter"] <- "Jura"}
}

hulls.rec.dietary.rm.CRE <- hulls.rec.dietary.rm
for (i in 1:nrow(hulls.rec.dietary.rm.CRE)) {           ### Doing it this way instead of 'hulls.rec.dietary.rm.JUR$Alter <- 'Kreide'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(hulls.rec.dietary.rm.CRE[i,"Alter"] == 'Rezent')
  {hulls.rec.dietary.rm.CRE[i,"Alter"] <- "Kreide"}
}

hulls.rec.dietary.rm.PAL <- hulls.rec.dietary.rm
for (i in 1:nrow(hulls.rec.dietary.rm.PAL)) {           ### Doing it this way instead of 'hulls.rec.dietary.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(hulls.rec.dietary.rm.PAL[i,"Alter"] == 'Rezent')
  {hulls.rec.dietary.rm.PAL[i,"Alter"] <- "Palaeozaen"}
}

hulls.rec.dietary.rm.EO <- hulls.rec.dietary.rm
for (i in 1:nrow(hulls.rec.dietary.rm.EO)) {           ### Doing it this way instead of 'hulls.rec.dietary.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(hulls.rec.dietary.rm.EO[i,"Alter"] == 'Rezent')
  {hulls.rec.dietary.rm.EO[i,"Alter"] <- "Eozaen"}
}

# x-----------x
#   PC3 & PC4
# x-----------x

# HULLS #
xfind_hull.all <- function(all.func) all.func[chull(all.func$PC3, all.func$PC4), ]
xfind_hull <- function(NA.func.all) NA.func.all[chull(NA.func.all$PC3, NA.func.all$PC4), ]

xhulls.all <- ddply(all.func, .(Ordnung), xfind_hull.all)
xhulls <- ddply(NA.func.all, .(Ordnung), xfind_hull)
xhulls.rm <- xhulls
xhulls.rm$Ernaehrungstrend <- NA  ## if you don't want the xhulls to be defined by a variable, replace it with NA in the xhulls dataset
xhulls.rm$Status <- NA

# Subset the whole data:
## Subset: Alter ##
all.func.Rezent<-subset(all.func, Alter == "Rezent")
all.func.jur<-subset(all.func, Alter == "Jura")
all.func.cre<-subset(all.func, Alter == "Kreide")
all.func.pal<-subset(all.func, Alter == "Palaeozaen")
all.func.eo<-subset(all.func, Alter == "Eozaen")

NA.func.Rezent<-subset(NA.func.all, Alter == "Rezent")
NA.func.jur<-subset(NA.func.all, Alter == "Jura")
NA.func.cre<-subset(NA.func.all, Alter == "Kreide")
NA.func.pal<-subset(NA.func.all, Alter == "Palaeozaen")
NA.func.eo<-subset(NA.func.all, Alter == "Eozaen")

# Rezent
xhulls.rec <- ddply(NA.func.Rezent, .(Ordnung), xfind_hull)
xhulls.rec.rm <- xhulls.rec
xhulls.rec.rm$Ernaehrungstrend <- NA  ## if you don't want the xhulls to be defined by a variable, replace it with NA in the xhulls dataset
xhulls.rec.rm$Status <- NA
# Jura
xhulls.jur <- ddply(NA.func.jur, .(Ordnung), xfind_hull)
xhulls.jur.rm <- xhulls.jur
xhulls.jur.rm$Ernaehrungstrend <- NA  ## if you don't want the xhulls to be defined by a variable, replace it with NA in the xhulls dataset
xhulls.jur.rm$Status <- NA
# Cretaceaous
xhulls.cre <- ddply(NA.func.cre, .(Ordnung), xfind_hull)
xhulls.cre.rm <- xhulls.cre
xhulls.cre.rm$Ernaehrungstrend <- NA  ## if you don't want the xhulls to be defined by a variable, replace it with NA in the xhulls dataset
xhulls.cre.rm$Status <- NA
# Palaeozaen
xhulls.pal <- ddply(NA.func.pal, .(Ordnung), xfind_hull)
xhulls.pal.rm <- xhulls.pal
xhulls.pal.rm$Ernaehrungstrend <- NA  ## if you don't want the xhulls to be defined by a variable, replace it with NA in the xhulls dataset
xhulls.pal.rm$Status <- NA
# Eozaen
xhulls.eo <- ddply(NA.func.eo, .(Ordnung), xfind_hull)
xhulls.eo.rm <- xhulls.eo
xhulls.eo.rm$Ernaehrungstrend <- NA  ## if you don't want the xhulls to be defined by a variable, replace it with NA in the xhulls dataset
xhulls.eo.rm$Status <- NA

## Subset: FEEDING GROUPS ##
# Trophic Level
xhulls.rec.trophic <- ddply(NA.func.Rezent, .(Nahrungsstufe), xfind_hull)
xhulls.rec.trophic.rm <- xhulls.rec.trophic
xhulls.rec.trophic.rm$Ordnung <- NA  ## if you don't want the xhulls to be defined by a variable, replace it with NA in the xhulls dataset
xhulls.rec.trophic.rm$Status <- NA
# Diet Trend
xhulls.rec.diet <- ddply(NA.func.Rezent, .(Ernaehrungstrend), xfind_hull)
xhulls.rec.diet.rm <- xhulls.rec.diet
xhulls.rec.diet.rm$Ordnung <- NA  ## if you don't want the xhulls to be defined by a variable, replace it with NA in the xhulls dataset
xhulls.rec.diet.rm$Status <- NA
# Dietary Guilds
xhulls.rec.dietary <- ddply(NA.func.Rezent, .(Ernaehrungsgruppen), xfind_hull)
xhulls.rec.dietary.rm <- xhulls.rec.dietary
xhulls.rec.dietary.rm$Ordnung <- NA  ## if you don't want the xhulls to be defined by a variable, replace it with NA in the xhulls dataset
xhulls.rec.dietary.rm$Status <- NA

# NEW STUFF #

##### make dataset that will plot modern xhulls on other time slices
# Trophic level
xhulls.rec.trophic.rm.JUR <- xhulls.rec.trophic.rm
for (i in 1:nrow(xhulls.rec.trophic.rm.JUR)) {           ### Doing it this way instead of 'xhulls.rec.trophic.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(xhulls.rec.trophic.rm.JUR[i,"Alter"] == 'Rezent')
  {xhulls.rec.trophic.rm.JUR[i,"Alter"] <- "Jura"}
}

xhulls.rec.trophic.rm.CRE <- xhulls.rec.trophic.rm
for (i in 1:nrow(xhulls.rec.trophic.rm.CRE)) {           ### Doing it this way instead of 'xhulls.rec.trophic.rm.JUR$Alter <- 'Kreide'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(xhulls.rec.trophic.rm.CRE[i,"Alter"] == 'Rezent')
  {xhulls.rec.trophic.rm.CRE[i,"Alter"] <- "Kreide"}
}

xhulls.rec.trophic.rm.PAL <- xhulls.rec.trophic.rm
for (i in 1:nrow(xhulls.rec.trophic.rm.PAL)) {           ### Doing it this way instead of 'xhulls.rec.trophic.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(xhulls.rec.trophic.rm.PAL[i,"Alter"] == 'Rezent')
  {xhulls.rec.trophic.rm.PAL[i,"Alter"] <- "Palaeozaen"}
}

xhulls.rec.trophic.rm.EO <- xhulls.rec.trophic.rm
for (i in 1:nrow(xhulls.rec.trophic.rm.EO)) {           ### Doing it this way instead of 'xhulls.rec.trophic.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(xhulls.rec.trophic.rm.EO[i,"Alter"] == 'Rezent')
  {xhulls.rec.trophic.rm.EO[i,"Alter"] <- "Eozaen"}
}

# Diet trend
xhulls.rec.diet.rm.JUR <- xhulls.rec.diet.rm
for (i in 1:nrow(xhulls.rec.diet.rm.JUR)) {           ### Doing it this way instead of 'xhulls.rec.trophic.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(xhulls.rec.diet.rm.JUR[i,"Alter"] == 'Rezent')
  {xhulls.rec.diet.rm.JUR[i,"Alter"] <- "Jura"}
}
xhulls.rec.diet.rm.CRE <- xhulls.rec.diet.rm
for (i in 1:nrow(xhulls.rec.diet.rm.CRE)) {           ### Doing it this way instead of 'xhulls.rec.diet.rm.JUR$Alter <- 'Kreide'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(xhulls.rec.diet.rm.CRE[i,"Alter"] == 'Rezent')
  {xhulls.rec.diet.rm.CRE[i,"Alter"] <- "Kreide"}
}

xhulls.rec.diet.rm.PAL <- xhulls.rec.diet.rm
for (i in 1:nrow(xhulls.rec.diet.rm.PAL)) {           ### Doing it this way instead of 'xhulls.rec.diet.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(xhulls.rec.diet.rm.PAL[i,"Alter"] == 'Rezent')
  {xhulls.rec.diet.rm.PAL[i,"Alter"] <- "Palaeozaen"}
}

xhulls.rec.diet.rm.EO <- xhulls.rec.diet.rm
for (i in 1:nrow(xhulls.rec.diet.rm.EO)) {           ### Doing it this way instead of 'xhulls.rec.diet.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(xhulls.rec.diet.rm.EO[i,"Alter"] == 'Rezent')
  {xhulls.rec.diet.rm.EO[i,"Alter"] <- "Eozaen"}
}

# Dietary guilds
xhulls.rec.dietary.rm.JUR <- xhulls.rec.dietary.rm
for (i in 1:nrow(xhulls.rec.dietary.rm.JUR)) {           ### Doing it this way instead of 'xhulls.rec.dietary.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(xhulls.rec.dietary.rm.JUR[i,"Alter"] == 'Rezent')
  {xhulls.rec.dietary.rm.JUR[i,"Alter"] <- "Jura"}
}

xhulls.rec.dietary.rm.CRE <- xhulls.rec.dietary.rm
for (i in 1:nrow(xhulls.rec.dietary.rm.CRE)) {           ### Doing it this way instead of 'xhulls.rec.dietary.rm.JUR$Alter <- 'Kreide'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(xhulls.rec.dietary.rm.CRE[i,"Alter"] == 'Rezent')
  {xhulls.rec.dietary.rm.CRE[i,"Alter"] <- "Kreide"}
}

xhulls.rec.dietary.rm.PAL <- xhulls.rec.dietary.rm
for (i in 1:nrow(xhulls.rec.dietary.rm.PAL)) {           ### Doing it this way instead of 'xhulls.rec.dietary.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(xhulls.rec.dietary.rm.PAL[i,"Alter"] == 'Rezent')
  {xhulls.rec.dietary.rm.PAL[i,"Alter"] <- "Palaeozaen"}
}

xhulls.rec.dietary.rm.EO <- xhulls.rec.dietary.rm
for (i in 1:nrow(xhulls.rec.dietary.rm.EO)) {           ### Doing it this way instead of 'xhulls.rec.dietary.rm.JUR$Alter <- 'Jura'' because then we keep the levels we set so that it knows what order to plot our time bins in
  if(xhulls.rec.dietary.rm.EO[i,"Alter"] == 'Rezent')
  {xhulls.rec.dietary.rm.EO[i,"Alter"] <- "Eozaen"}
}
#############################################################################

#############################################################################

#############################################################################
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


##########################################################################



# Plotting
# -----------

# PLOT 1-2
plot1 <- ggplot(data = NA.func.all, aes(PC1, PC2, fill = Ordnung, pch = Ordnung, size = Status, label=Spezies))+ #, label=Spezies))+
  #ggplot(data = NA.func.all, aes(PC1, PC2, col = Nahrungsstufe, fill = Nahrungsstufe, pch = Ordnung, size = Status, label=Spezies))+ 
  #ggplot(data = NA.func.all, aes(PC1, PC2, col = Ernaehrungstrend, fill = Ernaehrungstrend, pch = Ordnung, size = Status))+#, label=Spezies))+ 
  #ggplot(data = NA.func.all, aes(PC1, PC2, col = Ernaehrungsgruppen, fill = Ernaehrungsgruppen, pch = Ordnung, size = Status, label=Spezies))+ 
  #facet_wrap(~ Alter, ncol = 5) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  scale_shape_manual(values = c(21,22,24)) +  # shapes with boarders (boarder = col, main colour = fill)
  scale_fill_manual(values = c('coral','olivedrab3','dodgerblue'))+
  # ----
  # geom_polygon(data = hulls.rm, alpha = 0, size = 0.6) + # NO facet wrap!
  #stat_ellipse(geom = "polygon", aes(fill = Ordnung), alpha = 0.25) +  #these OR hulls
  geom_point(col = 'black') +
  # ---- STUFE
  # geom_polygon(data = hulls.rec.trophic.rm, alpha = 0.2, size = 0.6) +
  # scale_color_manual(values=c('darkgreen','red','blue','grey'))+
  # scale_fill_manual(values=c('darkgreen','red','blue','grey'))+
  # geom_point(col = 'black') +
  # ---- TREND
  # geom_polygon(data = hulls.rec.diet.rm, alpha = 0.2, size = 0.6) +
  # scale_color_manual(values=c('darkgreen','red','blue','green','darkorange','grey'))+
  # scale_fill_manual(values=c('darkgreen','red','blue','grey','green','darkorange'))+
  # geom_point(col = 'black') +
  # ---- GRUPPE
  # geom_polygon(data = hulls.rec.dietary.rm, alpha = 0.2, size = 0.6) +
  #   scale_color_manual(values=c('#66FF72','magenta','#FF8F4F','darkgreen','cyan','red','blue','yellow'))+
  #   scale_fill_manual(values=c('#66FF72','magenta','#FF8F4F','darkgreen','cyan','red','blue','grey','yellow'))+
  #   geom_point(col = 'black') +
  # ----
  geom_text(size=3.6, col ="black", hjust=-0.07, vjust=0.3, fontface="italic") +
  # ----
  xlab("PC1 (38.38%)") +   
  ylab("PC2 (20.43%)") +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=17),
        legend.text=element_text(size =20), 
        legend.position='none',
        legend.title.position = 'top',
        legend.title=element_text(size =20, face="bold"),
        legend.key.width = unit(1.5, 'cm'))+
  guides(pch = guide_legend(ncol = 2,override.aes = list(size =7)), size = guide_legend(ncol = 1))

# PLOT 3-4
plot2 <- ggplot(data = NA.func.all, aes(PC3, PC4, fill = Ordnung, pch = Ordnung, size = Status, label=Spezies))+ #, label=Spezies))+
  #ggplot(data = NA.func.all, aes(PC1, PC2, col = Nahrungsstufe, fill = Nahrungsstufe, pch = Ordnung, size = Status, label=Spezies))+ 
  #ggplot(data = NA.func.all, aes(PC1, PC2, col = Ernaehrungstrend, fill = Ernaehrungstrend, pch = Ordnung, size = Status))+#, label=Spezies))+ 
  #ggplot(data = NA.func.all, aes(PC1, PC2, col = Ernaehrungsgruppen, fill = Ernaehrungsgruppen, pch = Ordnung, size = Status, label=Spezies))+ 
  #facet_wrap(~ Alter, ncol = 5) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  scale_shape_manual(values = c(21,22,24)) +  # shapes with boarders (boarder = col, main colour = fill)
  scale_fill_manual(values = c('coral','olivedrab3','dodgerblue'))+
  # ----
  # geom_polygon(data = xhulls.rm, alpha = 0, size = 0.6) + # NO facet wrap!
  #stat_ellipse(geom = "polygon", aes(fill = Ordnung), alpha = 0.25) +  #these OR xhulls
  geom_point(col = 'black') +
  # ---- STUFE
  # geom_polygon(data = xhulls.rec.trophic.rm, alpha = 0.2, size = 0.6) +
  # scale_color_manual(values=c('darkgreen','red','blue','grey'))+
  # scale_fill_manual(values=c('darkgreen','red','blue','grey'))+
  # geom_point(col = 'black') +
  # ---- TREND
  # geom_polygon(data = xhulls.rec.diet.rm, alpha = 0.2, size = 0.6) +
  # scale_color_manual(values=c('darkgreen','red','blue','green','darkorange','grey'))+
  # scale_fill_manual(values=c('darkgreen','red','blue','grey','green','darkorange'))+
  # geom_point(col = 'black') +
  # ---- GRUPPE
  # geom_polygon(data = xhulls.rec.dietary.rm, alpha = 0.2, size = 0.6) +
  #   scale_color_manual(values=c('#66FF72','magenta','#FF8F4F','darkgreen','cyan','red','blue','yellow'))+
  #   scale_fill_manual(values=c('#66FF72','magenta','#FF8F4F','darkgreen','cyan','red','blue','grey','yellow'))+
  #   geom_point(col = 'black') +
  # ----
  geom_text(size=3.6, col ="black", hjust=-0.07, vjust=0.3, fontface="italic") +
  # ----
  xlab("PC3 (17.58%)") +   
  ylab("PC4 (10.97%)") +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=17),
        legend.text=element_text(size =20), 
        legend.position='bottom',
        legend.title.position = 'top',
        legend.title=element_text(size =20, face="bold"),
        legend.key.width = unit(1.5, 'cm'))+
  guides(pch = guide_legend(ncol = 2,override.aes = list(size =7)), size = guide_legend(ncol = 1))

# ---------------------------
plot3 <- ggplot(data = NA.func.rodent, aes(PC1, PC2, col = Ordnung, fill = Ordnung, pch = Ordnung, size = Status, label=Spezies))+
  #ggplot(data = NA.func.rodent, aes(PC1, PC2, col = Nahrungsstufe, fill = Nahrungsstufe, pch = Ordnung, size = Status))+ 
  #ggplot(data = NA.func.rodent, aes(PC1, PC2, col = Ernaehrungstrend, fill = Ernaehrungstrend, pch = Ordnung, size = Status))+ 
  #ggplot(data = NA.func.rodent, aes(PC1, PC2, col = Ernaehrungsgruppen, fill = Ernaehrungsgruppen, pch = Ordnung, size = Status))+ 
  #facet_wrap(~ Alter, ncol = 5) +
  # ----
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  scale_shape_manual(values = c(21,24)) +  # shapes with boarders (boarder = col, main colour = fill)
  scale_fill_manual(values = c('coral','dodgerblue'))+
  # ----
  #geom_polygon(data = yhulls.rm.rod, alpha = 0, size = 0.6) + # NO facet wrap!
  #stat_ellipse(geom = "polygon", aes(fill = Ordnung), alpha = 0.25) +  #these OR yhulls
  geom_point(col = 'black') +
  # ---- STUFE
  # geom_polygon(data = yhulls.rec.trophic.rm.rod, alpha = 0.2, size = 0.6) +
  # scale_color_manual(values=c('green','red','blue','grey'))+
  # scale_fill_manual(values=c('green','red','blue','grey'))+
  # geom_point(col = 'black') +
  #stat_ellipse(geom = "polygon", aes(fill = Ordnung), alpha = 0.25, size = 0.5) +  ##these OR yhulls
  # ---- TREND
  # geom_polygon(data = yhulls.rec.diet.rm.rod, alpha = 0.2, size = 0.6) +
  # scale_color_manual(values=c('darkgreen','red','blue','green','darkorange'))+
  # scale_fill_manual(values=c('darkgreen','red','blue','grey','green','darkorange'))+
  # geom_point(col = 'black') +
  # ---- GRUPPE
  # geom_polygon(data = yhulls.rec.dietary.rm.rod, alpha = 0.2, size = 0.6) +
  # scale_color_manual(values=c('#66FF72','magenta','#FF8F4F','darkgreen','cyan','red','blue','grey'))+
  # scale_fill_manual(values=c('#66FF72','magenta','#FF8F4F','darkgreen','cyan','red','blue','grey'))+
  # geom_point(col = 'black') +
  #stat_ellipse(geom = "polygon", aes(fill = Ordnung), alpha = 0.25, size = 0.5) +  ##these OR yhulls
  # ----
  geom_text(size=3.6, col ="black", hjust=-0.07, vjust=0.3,fontface="italic") +
  # ----
  xlab("PC1 (29.56%)") +   
  ylab("PC2 (22.04%)") +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=17),
        legend.text=element_text(size =20), 
        legend.position='none',
        legend.title.position = 'top',
        legend.title=element_text(size =20, face="bold"),
        legend.key.width = unit(1.5, 'cm'))+
  guides(pch = guide_legend(ncol = 2,override.aes = list(size =7)), size = guide_legend(ncol = 1))

plot4 <- ggplot(data = NA.func.rodent, aes(PC3, PC4, col = Ordnung, fill = Ordnung, pch = Ordnung, size = Status, label=Spezies))+
  #ggplot(data = NA.func.rodent, aes(PC3, PC4, col = Nahrungsstufe, fill = Nahrungsstufe, pch = Ordnung, size = Status))+ 
  #ggplot(data = NA.func.rodent, aes(PC3, PC4, col = Ernaehrungstrend, fill = Ernaehrungstrend, pch = Ordnung, size = Status))+ 
  #ggplot(data = NA.func.rodent, aes(PC3, PC4, col = Ernaehrungsgruppen, fill = Ernaehrungsgruppen, pch = Ordnung, size = Status))+ 
  #facet_wrap(~ Alter, ncol = 5) +
  # ----
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  scale_shape_manual(values = c(21,24)) +  # shapes with boarders (boarder = col, main colour = fill)
  scale_fill_manual(values = c('coral','dodgerblue'))+
  # ----
  #geom_polygon(data = hulls.rm.rod, alpha = 0, size = 0.6) + # NO facet wrap!
  #stat_ellipse(geom = "polygon", aes(fill = Ordnung), alpha = 0.25) +  #these OR hulls
  geom_point(col = 'black') +
  # ---- STUFE
  # geom_polygon(data = hulls.rec.trophic.rm.rod, alpha = 0.2, size = 0.6) +
  # scale_color_manual(values=c('green','red','blue','grey'))+
  # scale_fill_manual(values=c('green','red','blue','grey'))+
  # geom_point(col = 'black') +
  #stat_ellipse(geom = "polygon", aes(fill = Ordnung), alpha = 0.25, size = 0.5) +  ##these OR hulls
  # ---- TREND
  # geom_polygon(data = hulls.rec.diet.rm.rod, alpha = 0.2, size = 0.6) +
  # scale_color_manual(values=c('darkgreen','red','blue','green','darkorange'))+
  # scale_fill_manual(values=c('darkgreen','red','blue','grey','green','darkorange'))+
  # geom_point(col = 'black') +
  # ---- GRUPPE
  # geom_polygon(data = hulls.rec.dietary.rm.rod, alpha = 0.2, size = 0.6) +
  # scale_color_manual(values=c('#66FF72','magenta','#FF8F4F','darkgreen','cyan','red','blue','grey'))+
  # scale_fill_manual(values=c('#66FF72','magenta','#FF8F4F','darkgreen','cyan','red','blue','grey'))+
  # geom_point(col = 'black') +
  #stat_ellipse(geom = "polygon", aes(fill = Ordnung), alpha = 0.25, size = 0.5) +  ##these OR hulls
  # ----
  geom_text(size=3.6, col ="black", hjust=-0.07, vjust=0.3,fontface="italic") +
  # ----
  xlab("PC3 (16.26%)") +   
  ylab("PC4 (15.83%)") +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=17),
        legend.text=element_text(size =20), 
        legend.position='none',
        legend.title.position = 'top',
        legend.title=element_text(size =20, face="bold"),
        legend.key.width = unit(1.5, 'cm'))+
  guides(pch = guide_legend(ncol = 2,override.aes = list(size =7)), size = guide_legend(ncol = 1))

# ---------------------------
combined_plot1 <- plot1 + plot2 + plot_layout(guides = "collect") & theme(legend.position = "bottom") 
combined_plot2 <- plot3 + plot4

combined_plot_all <- combined_plot1 / combined_plot2
combined_plot_all

