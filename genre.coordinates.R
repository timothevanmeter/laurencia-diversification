# On ne garde que les taxons qui ont ete retenus pour l analyse
# Phylogénétique

setwd("C://Users//USER//Desktop")
c. <- read.csv("cconsensus2.species.list_laurencia.csv", header=T, sep = ","
                        , dec = ".", skipNul = T, header=F)








names(coordinates) <- c("id", "genre", "espece", "region", 
                               "long", "lat")



phy <- read.tree("RAxML_bestTree.laurencia_final_100.nwk")
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

coordinates <- merge(id, coordinates, by="id", all.x=T)
# On enlève les doublons !
pos <- NULL
for(i in 1:nrow(coordinates)){
  if(duplicated(coordinates[,1])[i] == T){
    posi <- i
    pos <- rbind(pos, posi)
  }
}
coordinates <- coordinates[-c(pos[,1]),]

coordinates <- coordinates[,c(1,5:6,8:9)]
names(coordinates) <- c('id','genre','espece','lat','long')

setwd("C://Users//USER//Desktop")
write.table(coordinates, file="genre_coordinates.csv", 
            sep=",", dec=".", eol="\n", row.names=F, col.names=T)






