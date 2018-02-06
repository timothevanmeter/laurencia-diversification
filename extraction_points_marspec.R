# 
# SCRIPT EXTRACTION DE DONNEES PONCTUELLES DE MARSPEC

library(raster)
library(sp)
library(rgdal)
library(stringi)
library(stringr)
library(ape)
library(regexr)
library(seqinr)

# DEFINE USER FUNCTIONS
####################################################################
# FONCTION permettant de trouver & enregistrer la/les valeur(s) des cellules 
# non-vides les plus proches. On part des coordonnées (row,col) de la cellule
# vide désignée par (z,w) et on explore les 8 cellules adjacentes (s=1),
# puis les  16 prochaines cellules adjacentes (s=2) et ainsi de suite par
# implémentation de s. On ne réexplore pas les mêmes cellules pour 2 rangs
# de s consécutifs. On obtient ensuite la moyenne des valeurs associées
# au premier rang non-nul de s.

# Cette fonction fonctionne pour un raster (une seule couche) !

#  r un objet de type raster
#  z,w les coordonnées ligne, colonne de la cellule avec une valeur NA
adj.cells.search <- function(r,z,w){
  s <- 1
  adj.cells <- NULL
  for(s in 1:10000){
    # for(s in 1:(min(dim(r)[1:2])/10)){
    
    m <- z-s
    n <- z+s
    p <- w-s
    q <- w+s
    
    for(i in m:n){
      
      for(j in p:q){
        
        if(i!=n && i!=m  &&  j!=q && j!=p){ break }
        if(is.na(r[i,j])==F){
          value <- as.numeric(r[i,j])
          adj.cells <- rbind(adj.cells, value)
          # adj.cells <- rbind(adj.cells, as.numeric(r[i,j]))
        }
      }
    }
    if(is.null(adj.cells)==F){ break }
  }
  return(adj.cells)
}
##################
rowcol.cell <- function(r,cell.number){
  # Fonction renvoyant ligne et colonne d une cellule d un raster
  # r un objet de type raster
  
  row <- floor(cell.number/ncol(r))
  col <- cell.number%%ncol(r)
  rowcol.cell <- rbind(row,col)
  return(rowcol.cell)
}
############################
# On visualise la region dans laquelle se situe le point 
# contenant une valeur nulle
region.viz <-function(r, row, col, range){
  
  row <- row
  col <- col
  range <- range
  long <- xFromCol(r, col = col)
  lat <- yFromRow(r, row = row)
  region <- crop(r, extent(r, row-range, row+range, col-range, col+range))
  
  plot(region)
  points(long,lat, col="red", pch=20)
    
return(region)
}
#############################
####################################################################

setwd("C://Users//USER//Desktop")

coordinates <- read.csv("coordinates.csv", header=T, sep = ","
                        , dec = ".", skipNul = T)

coordinates.origin <- coordinates
names(coordinates.origin) <- c("id", "genre", "espece", "region", 
                               "long", "lat")
###################################################
# On ne garde que les taxons qui ont ete retenus pour l analyse
# Phylogénétique

phy <- read.tree("RAxML_bestTree.consensus2_1000.nwk")
taxa <- phy$tip.label
pos <- NULL
id <- NULL
idi <- NULL

for(i in 1:length(taxa)){
  
  pos <- str_locate(taxa[i], "[[:upper:]]+[[:digit:]]+")
  idi <- substr(taxa[i], 1, pos[1,2])
  id<- c(id, idi)
}
 
lol <- c(1:length(taxa))
id <- cbind(id , lol)
names(id) <- c("id", "lol")

coordinates <- merge(coordinates.origin, id, by="id")
# On enlève les doublons !
coordinates <- coordinates[-c(189,356:358,370:371),]
coordinates.origin.excluded <- coordinates

###################################################


# coordinates <- as.matrix(coordinates)
coordinates <- data.frame(coordinates)
# La projection se fait en latlong et non pas en longlat
coordinates <- coordinates[,-c(1:4,7)]
coordinates <- coordinates[,c(2:1)]


# SUpprime les lignes contenant des NA
pos <- NULL
for(i in 1:nrow(coordinates)){
  if(is.na(coordinates[i,2]) == T){
    posi <- FALSE
    pos <- c(pos, posi)
  }else{
    posi <- TRUE
    pos <- c(pos, posi)
  }
}
pos
coordinates <- coordinates[pos,]

# coordinates <- SpatialPoints(coordinates, proj4string=CRS(
#   "+proj=longlat +datum=WGS84"))


####################################################################

# Les données biogeo de MARSPEC
r <- raster("C://Users//USER//Desktop//data_MARSPEC//biogeo08_17_30s//biogeo13_30s")

# On fournit le facteur de correction des données MARSPEC pour la remise à l échelle
scaling.factor <- 100

# Extraction des valeurs des points cohérents avec la position d une cellule
points <- extract(r, coordinates, method='simple', cellnumbers=T)
colnames(points) <- c("cell.number", "annual.SST.Mean")
points <- cbind(points, data.frame(coordinates))

# Enregistre uniquement les cellules renvoyant des valeurs nulles
pos <- NULL
for(i in 1:nrow(points)){
  if(is.na(points[i,2]) == T){
    posi <- T
    pos <- c(pos, posi)
  }else{
    posi <- F
    pos <- c(pos, posi)
  }
}
pos
points.NA <- points[pos,]
points.NA <- points.NA[,c(3,4,1,2)]
names(points.NA) <- c("long", "lat", "cell.number", "annual.SST.Mean")
names(points) <- c("cell.number", "annual.SST.Mean", "long", "lat")

# Supprime les lignes contenant des NA
pos <- NULL
for(i in 1:nrow(points)){
  if(is.na(points[i,2]) == T){
    posi <- FALSE
    pos <- c(pos, posi)
  }else{
    posi <- TRUE
    pos <- c(pos, posi)
  }
}
pos
points.T <- points[pos,]
points.T <- points.T[,c(3,4,1,2)]


####################################################################
# On visualise la distribution des différents types de points

r <- r/scaling.factor
plot(r)
points(coordinates, col="red", pch=20)
points(points.T, col="blue")
points(points.NA, col="red")

####################################################################

# On récupère, pour les points.NA, des valeurs non-nulles dans les 
# cellules adjacentes

temp <- NULL
h <- NULL
j <- NULL
rowcol.NA <- NULL

# On obtient ligne et colonne pour chaque point NA
for(i in 1:nrow(points.NA)){
  temp <- rowcol.cell(r, points.NA[i,3])
  h <- rbind(h, temp[1])    # fournit la ligne correspondante 
  j <- rbind(j, temp[2])    # fournit la colonne correspondante
}
rowcol.NA <- cbind(h, j)
colnames(rowcol.NA) <- c("row", "col")

adj.cells.mean <- NULL

# On récupère les valeurs non-nulles pour tous nos points NA

############################################### !!!!!! ATTENTION !!!!!! #######################################################
# Cette partie du script peut-être très longue suivant votre nombre de points !!!!!!

for(i in 1:nrow(rowcol.NA)){ 
  
  adj.cells <- adj.cells.search(r,as.numeric(rowcol.NA[i,1]),
                                as.numeric(rowcol.NA[i,2]))
  
  
  adj.cells.mean <- rbind(adj.cells.mean, mean(as.numeric(adj.cells)))
}

graal.precious <- adj.cells.mean

###########################################

# On rassemble les données pour tous les points

points.NA <- points.NA[,-c(1,2,4)]
points.NA <- cbind(points.NA,adj.cells.mean)
colnames(points.NA) <- c("cell.number", "annual.SST.Mean")

# On retire les points répétés qui perturbent la fonction merge,
# la fonction attend un antécédent y, unique par image x, une injection.
pos <- NULL
rep <- c(duplicated(points.NA[,1]))

for(i in 1:nrow(points.NA)){
  if(rep[i]){
    posi <- FALSE
    pos <- c(pos,posi)
  }else{
    posi <- TRUE
    pos <- c(pos,posi)
  }
}
points.NA <- points.NA[pos,]

# Executer une seule des deux lignes suivantes :

# 1) Pour une analyse avec QUaSSE celle-ci correspondant à la 
# phylogénie utilisée
points <- cbind(points, coordinates.origin.excluded[,c(1:4)])
# 2) Pour récupérer des valeurs de traits environmentaux
points <- cbind(points, coordinates.origin[,c(1:4)])

points.total <- merge(points, points.NA, by="cell.number", all.x=T)

# On rassemble les deux colonnes contenant les valeurs de SST
for(i in 1:nrow(points.total)){
  if(is.na(points.total[i,2])==T){
    points.total[i,2] <- points.total[i,9]
  }
}

points.total <- points.total[,-c(1,9)]
names(points.total) <- c("annual.SST.Mean", "long", "lat", "id",
                         "genre", "espece", "region")
# Sans oublier de mettre les valuers à l'échelle
points.total[,1] <- points.total[,1]/scaling.factor

# On exporte le jeu de données une fois compléter
setwd("C://Users//USER//Desktop")
write.table(points.total, file="annual.SST.mean_MARSPEC_consensus2.csv", sep=",", dec=".", eol="\n"
            , row.names=F, col.names=T)


