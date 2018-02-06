
# chargement des packages/fonctions n?cessaires
library(xlsx)
library(vegan)
library(maptools)
library(gdistance)
library(rgdal)
library(raster)
library(ggplot2)
library(rgeos)
library(ggmap)
library(plyr)

#####AJOUT DE MES POINTS########
#appel de ma base de donnees
setwd("C://Users//USER//Desktop//georef")
# BD  <- read.csv("geo_ref.csv", header=FALSE,sep=";")
BD  <- read.csv("coordinates.csv", header=FALSE,sep=",")
View(BD)

#mettre colonne lat et long en numerique :
BD[,2] <- abbreviate((BD[,5]), 8)
BD[,3] <- abbreviate((BD[,6]), 8)
BD[,2] <- as.numeric(BD[,5])
BD[,3] <- as.numeric(BD[,6])


BD <- BD[,c(6,5,1)]

setwd("C://Users//USER//Desktop//georef//LME66")

# placer  les coordonnees dans le bon référentiel
BDgeo <- SpatialPointsDataFrame(BD[,1:2],BD[,1:2],
          proj4string= CRS("+proj=longlat +datum=WGS84"))

####FOND DE CARTE MONDE#####
FDC <- readOGR(dsn=path.expand("C://Users//USER//Desktop//georef//LME66"),
            layer = "LME66")
#pour savoir si le fond de carte est projet et dans quel
# systeme de projection
summary(FDC)
plot(FDC)
plot(BDgeo, add=TRUE, col="red", pch=20, label = as.character(BD[,3]))
# text(BDgeo, BD[,3], cex=0.5, pos=1, col="red")

####################################

# Les informations sur Hawaii nous obligent à définir une zone plutôt
# que des points d échantillonnage :
FDCHAWAII <- crop(FDC,extent(-153, -178, 29, 18))
plot(FDCHAWAII, col="red", add=TRUE)

# plot(BDgeo, add=TRUE, col="darkolivegreen1", pch=20)




