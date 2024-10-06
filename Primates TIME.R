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
setwd("C:/Users/Mr-Wo/Desktop/BA/Neue Daten/Deutsch")
all.data <- read.csv("Data Deutsch.csv", sep = ';', header = T)

# Calculate Ratios
all.data$diastema.ratio<-(all.data$diastema.length/all.data$dental.row.length)
all.data$molar.ratio<-(all.data$molar.row.length/all.data$dental.row.length)
all.data$closing.ma.premolar<-(all.data$distance.from.condyle.to.coronoid/all.data$distance.from.condyle.to.middle.cusp.last.premolar)
all.data$jaw.slenderness<-((all.data$jaw.depth/all.data$jaw.length)*100)
all.data$coronoid.slenderness<-((all.data$coronoid.length/all.data$coronoid.width)*100)
all.data$trough.offset.ratio<-(all.data$distance.from.jaw.joint.to.tooth.row/all.data$jaw.length)

# ------------------------------------------
#  Principle Component Analysis (PCA)
#  FOR: Primates, Rodents, Multituberculata
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

# ---------------------------
# PLOT 1-2
plot1 <-
#ggplot(data = NA.func.all, aes(PC1, PC2, col = Ordnung, fill = Ordnung, pch = Ordnung, size = Status))+
ggplot(data = NA.func.all, aes(PC1, PC2, fill = Nahrungsstufe, pch = Ordnung, size = Status, label=Spezies))+  # col = Nahrungsstufe, # now we don't need to col by trophic, cos both are fill (the points with outlines use fill). you would only need this if you want the outline around your hulls. that is a artistic choice you can make :) 
#ggplot(data = NA.func.all, aes(PC1, PC2, fill = Ernaehrungstrend, pch = Ordnung, size = Status))+ 
#ggplot(data = NA.func.all, aes(PC1, PC2, fill = Ernaehrungsgruppen, pch = Ordnung, size = Status))+ 
  facet_wrap(~ Alter, ncol = 5) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  scale_shape_manual(values = c(21,24,22)) +  # shapes with boarders (boarder = col, main colour = fill)
  scale_fill_manual(values = c('coral','dodgerblue','olivedrab3'))+
  # ----
  # geom_polygon(data = hulls.rm, alpha = 0, size = 0.6) + # NO facet wrap!
  # geom_point(col = 'black') +
  #stat_ellipse(geom = "polygon", aes(fill = Ordnung), alpha = 0.25) +  #these OR hulls
  # ---- STUFE
  geom_polygon(data = hulls.rec.trophic.rm.JUR, alpha = 0.4, size = 0.6) +
  geom_polygon(data = hulls.rec.trophic.rm.CRE, alpha = 0.4, size = 0.6) +
  geom_polygon(data = hulls.rec.trophic.rm.PAL, alpha = 0.4, size = 0.6) +
  geom_polygon(data = hulls.rec.trophic.rm.EO, alpha = 0.4, size = 0.6) +
  geom_polygon(data = hulls.rec.trophic.rm, alpha = 0.4, size = 0.6) +
  scale_fill_manual(values=c('darkgreen','darkred','blue', 'white'))+
  geom_point(col = 'black') + #option 1
  # ---- TREND
  # geom_polygon(data = hulls.rec.diet.rm.JUR, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = hulls.rec.diet.rm.CRE, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = hulls.rec.diet.rm.PAL, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = hulls.rec.diet.rm.EO, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = hulls.rec.diet.rm, alpha = 0.4, size = 0.6) +
  # scale_fill_manual(values=c('darkgreen','darkred','blue','white','green','red'))+
  # geom_point(col = 'black') + #option 1
  # ---- GRUPPE
  # geom_polygon(data = hulls.rec.dietary.rm.JUR, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = hulls.rec.dietary.rm.CRE, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = hulls.rec.dietary.rm.PAL, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = hulls.rec.dietary.rm.EO, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = hulls.rec.dietary.rm, alpha = 0.4, size = 0.6) +
  # scale_fill_manual(values=c('lightgreen','magenta','orange','darkgreen','purple','darkred','blue','white','yellow'))+
  # geom_point(col = 'black')+
  # ----
  #geom_text(size=3.6, col ="black", hjust=-0.07, vjust=0.3, fontface="italic") +
  # ----
  xlab("PC1 (38.38%)") +   
  ylab("PC2 (20.43%)") +
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

# Plot 3-4
plot2 <-
#ggplot(data = NA.func.all, aes(PC3, PC4, col = Ordnung, fill = Ordnung, pch = Ordnung, size = Status))+
ggplot(data = NA.func.all, aes(PC3, PC4, fill = Nahrungsstufe, pch = Ordnung, size = Status))+  # col = Nahrungsstufe, # now we don't need to col by trophic, cos both are fill (the points with outlines use fill). you would only need this if you want the outline around your hulls. that is a artistic choice you can make :) 
#ggplot(data = NA.func.all, aes(PC3, PC4, fill = Ernaehrungstrend, pch = Ordnung, size = Status))+ 
#ggplot(data = NA.func.all, aes(PC3, PC4, fill = Ernaehrungsgruppen, pch = Ordnung, size = Status))+ 
  facet_wrap(~ Alter, ncol = 5) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  scale_shape_manual(values = c(21,24,22)) +  # shapes with boarders (boarder = col, main colour = fill)
  scale_fill_manual(values = c('coral','dodgerblue','olivedrab3'))+
  # ----
  # geom_polygon(data = hulls.rm, alpha = 0, size = 0.6) + # NO facet wrap!
  # geom_point(col = 'black') +
  #stat_ellipse(geom = "polygon", aes(fill = Ordnung), alpha = 0.25) +  #these OR hulls
  # ---- STUFE
  geom_polygon(data = xhulls.rec.trophic.rm.JUR, alpha = 0.4, size = 0.6) +
  geom_polygon(data = xhulls.rec.trophic.rm.CRE, alpha = 0.4, size = 0.6) +
  geom_polygon(data = xhulls.rec.trophic.rm.PAL, alpha = 0.4, size = 0.6) +
  geom_polygon(data = xhulls.rec.trophic.rm.EO, alpha = 0.4, size = 0.6) +
  geom_polygon(data = xhulls.rec.trophic.rm, alpha = 0.4, size = 0.6) +
  scale_fill_manual(values=c('darkgreen','darkred','blue', 'white'))+
  geom_point(col = 'black') + #option 1
  # ---- TREND
  # geom_polygon(data = xhulls.rec.diet.rm.JUR, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = xhulls.rec.diet.rm.CRE, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = xhulls.rec.diet.rm.PAL, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = xhulls.rec.diet.rm.EO, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = xhulls.rec.diet.rm, alpha = 0.4, size = 0.6) +
  # scale_fill_manual(values=c('darkgreen','darkred','blue','white','green','red'))+
  # geom_point(col = 'black') + #option 1
  # ---- GRUPPE
  # geom_polygon(data = xhulls.rec.dietary.rm.JUR, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = xhulls.rec.dietary.rm.CRE, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = xhulls.rec.dietary.rm.PAL, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = xhulls.rec.dietary.rm.EO, alpha = 0.4, size = 0.6) +
  # geom_polygon(data = xhulls.rec.dietary.rm, alpha = 0.4, size = 0.6) +
  # scale_fill_manual(values=c('lightgreen','magenta','orange','darkgreen','purple','darkred','blue','white','yellow'))+
  # geom_point(col = 'black')+
  # ----
  #stat_ellipse(geom = "polygon", aes(fill = Ordnung), alpha = 0.25, size = 0.5) +  ##these OR hulls
  # ----
  #geom_text(size=3.6, col ="black", hjust=-0.07, vjust=0.3, fontface="italic") +
  # ----
  xlab("PC3 (17.58%)") +   
  ylab("PC4 (10.97%)") +
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
