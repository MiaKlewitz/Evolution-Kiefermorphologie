##### JAW LENGTH DATASET #####


#install.packages("geomorph")
#install.packages("Morpho")
#install.packages("surface")
#install.packages("phangorn")
#install.packages("ggplot2")
#install.packages("ggfortify")
#install.packages("FactoMineR")
#install.packages("geoscale")
#install.packages('abind')
#install.packages("splitstackshape")
#install.packages("plyr")
library (plyr)
library (splitstackshape)
library (geomorph)
library (Morpho)
library (ape)
library (phytools)
library (surface)
library (phangorn)
library (ggplot2)
library (ggfortify)
library (FactoMineR)
library (geoscale)
library (abind)

# Call in personal edited TPS function. readland.gem.tps:
setwd("C:/Users/Mr-Wo/Desktop/Bachelorarbeit/R_Code")
source("readland.gem.tps function.R")


# Working directory Problem Mammal Images:
setwd("C:/Users/Mr-Wo/Desktop/Bachelorarbeit/R_Code/TPS_Files_with_landmarks")
# Import Problem Mammal Images Images.tps data (the landmarked photo data):
fossil.mammals<-readland.gem.tps(file=("Primates_data.TPS"), specID ="imageID", readcurves=FALSE, warnmsg=T)
# read in the actual length of the scale:
fossil.scale<-read.csv(file="Primates_Scale.csv", header = F)

# Check data:

str(fossil.mammals)

# bind data:
fossil.mammals[fossil.mammals == -1] <- NA 

# In a 3D array in this case... fossil.mammals[landmark,XandY,imagename]
# My measurements:
jaw.depth.a<-sqrt((fossil.mammals[5,1,]-fossil.mammals[6,1,])^2+(fossil.mammals[5,2,]-fossil.mammals[6,2,])^2)
jaw.length.a<-sqrt((fossil.mammals[3,1,]-fossil.mammals[4,1,])^2+(fossil.mammals[3,2,]-fossil.mammals[4,2,])^2)
dental.row.a<-sqrt((fossil.mammals[5,1,]-fossil.mammals[7,1,])^2+(fossil.mammals[5,2,]-fossil.mammals[7,2,])^2)
molar.row.a<-sqrt((fossil.mammals[5,1,]-fossil.mammals[8,1,])^2+(fossil.mammals[5,2,]-fossil.mammals[8,2,])^2)
diastema.a<-sqrt((fossil.mammals[7,1,]-fossil.mammals[9,1,])^2+(fossil.mammals[7,2,]-fossil.mammals[9,2,])^2)
###pre.diastema.a<-sqrt((fossil.mammals[11,1,]-fossil.mammals[12,1,])^2+(fossil.mammals[11,2,]-fossil.mammals[12,2,])^2)
condyle.coronoid.a<-sqrt((fossil.mammals[10,1,]-fossil.mammals[11,1,])^2+(fossil.mammals[10,2,]-fossil.mammals[11,2,])^2)
###condyle.tip.a<-sqrt((fossil.mammals[1,1,]-fossil.mammals[12,1,])^2+(fossil.mammals[1,2,]-fossil.mammals[12,2,])^2)
###condyle.angular.a<-sqrt((fossil.mammals[1,1,]-fossil.mammals[15,1,])^2+(fossil.mammals[1,2,]-fossil.mammals[15,2,])^2)
###condyle.base.a<-sqrt((fossil.mammals[1,1,]-fossil.mammals[14,1,])^2+(fossil.mammals[1,2,]-fossil.mammals[14,2,])^2)
condyle.premolar.a<-sqrt((fossil.mammals[10,1,]-fossil.mammals[12,1,])^2+(fossil.mammals[10,2,]-fossil.mammals[12,2,])^2)
coronoid.length.a<-sqrt((fossil.mammals[11,1,]-fossil.mammals[13,1,])^2+(fossil.mammals[11,2,]-fossil.mammals[13,2,])^2)
coronoid.width.a<-sqrt((fossil.mammals[14,1,]-fossil.mammals[15,1,])^2+(fossil.mammals[14,2,]-fossil.mammals[15,2,])^2)
###articulation.dip.height.a<-sqrt((fossil.mammals[2,1,]-fossil.mammals[23,1,])^2+(fossil.mammals[2,2,]-fossil.mammals[23,2,])^2)
articulation.height.a<-sqrt((fossil.mammals[16,1,]-fossil.mammals[17,1,])^2+(fossil.mammals[16,2,]-fossil.mammals[17,2,])^2)

# Scale bar in image:
landmark.scale<-sqrt((fossil.mammals[1,1,]-fossil.mammals[2,1,])^2+(fossil.mammals[1,2,]-fossil.mammals[2,2,])^2)

# Scale all measurements taken:
# DEPTH:
jaw.depth<-(fossil.scale*jaw.depth.a)/landmark.scale
# LENGTH:
jaw.length<-(fossil.scale*jaw.length.a)/landmark.scale
# DENTAL ROW:
dental.row<-(fossil.scale*dental.row.a)/landmark.scale
# MOLAR ROW:
molar.row<-(fossil.scale*molar.row.a)/landmark.scale
# DIASTEMA:
diastema<-(fossil.scale*diastema.a)/landmark.scale
#### PRE DIASTEMA:
###pre.diastema<-(fossil.scale*pre.diastema.a)/landmark.scale
# CONDYLE CORONOID:
condyle.coronoid<-(fossil.scale*condyle.coronoid.a)/landmark.scale
#### CONDYLE TIP:
###condyle.tip<-(fossil.scale*condyle.tip.a)/landmark.scale
#### CONDYLE ANGULAR:
###condyle.angular<-(fossil.scale*condyle.angular.a)/landmark.scale
#### CONDYLE BASE:
###condyle.base<-(fossil.scale*condyle.base.a)/landmark.scale
# CONDYLE PREMOLAR:
condyle.premolar<-(fossil.scale*condyle.premolar.a)/landmark.scale
# CORONOID LENGTH:
coronoid.length<-(fossil.scale*coronoid.length.a)/landmark.scale
# CORONOID WIDTH:
coronoid.width<-(fossil.scale*coronoid.width.a)/landmark.scale
#### ARTICULATION DIP HEIGHT:
###articulation.dip.height<-(fossil.scale*articulation.dip.height.a)/landmark.scale
# ARTICULATION HEIGHT:
articulation.height<-(fossil.scale*articulation.height.a)/landmark.scale


# extract species names:
sp.names<-cbind(dimnames(fossil.mammals)[[3]])


# To bind all the values together into a csv file.
fossil.mammals.measurements<-cbind(sp.names, jaw.depth, jaw.length, dental.row, molar.row, diastema, condyle.coronoid, 
                                   condyle.premolar, coronoid.length, coronoid.width, articulation.height)

#recent.lm.measurements<-cSplit(recent.lm.measurements, "sp.names", " - ")
# rename the columns:
colnames(fossil.mammals.measurements)<-c('species', 'jaw.depth', 'jaw.length', 'dental.row.length', 
                                    'molar.row.length', 'diastema.length', 'distance.from.condyle.to.coronoid',
                                    'distance.from.condyle.to.middle.cusp.last.premolar', 'coronoid.length',
                                    'coronoid.width', 'distance.from.jaw.joint.to.tooth.row')


setwd("C:/Users/Mr-Wo/Desktop/Bachelorarbeit/R_Code")
write.csv(fossil.mammals.measurements,"Primates_data_measured.csv")



fossil.mammals[[1]]



