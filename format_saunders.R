
#------------------------------------------------------------------------------------------------
#----------------------------------###SAUNDERS###-------------------------------------------------
#-------------------------------------------------------------------------------------------------

#On se place dans le dossier contenant les données :
setwd("C://Users//USER//Desktop//SAUNDERS")



#CREATION NOM DE GENRE, NOM D'ESPECE

#Notre jeu de données :
Laurencia2 <- read.table("Laurencia.csv", header = F, sep = ",")
bs <- Laurencia2

bs <- bs[, -c(4:6, 9, 12:15)]
names(bs) <- c("id1", "id2", "nom", "pays", "region", "lat", "long")

# Notre motif : [Genre][espace]|[espèce]|[...]
#On cherche à trouver les positions symbolisées par |

#On cherche pour chaque ligne la position du premier espace séparant le nom de genre de 
# celui d'espèce : posbegin et la position de fin du nom d'espèce : posend

#Puis on cherche le motif [caractère][espace][suite de caractères>1][non-caractère] qui permet de reconnaître
# la position de la fin du nom d'epsèce

posbegin <- NULL
posend <- NULL
for(i in 1:nrow(Laurencia2)){
  pos <- str_locate(bs$nom[i], "[[:blank:]]")
  posbegin <- rbind(posbegin, pos)
  posi <- str_locate(bs$nom[i], "[[:alpha:]][[:blank:]][[:alpha:]]+[^[:alpha:]]")
  posend <- rbind(posend, posi)
}
head(posbegin)
head(posend)

#On utilise ensuite ces positions pour extraire les noms de genre et d'espèce dans deux nouvelles 
# colonnes réinsérées dans notre jeu de données
genre <- NULL
espece <- NULL
for(i in 1:nrow(Laurencia2)){
  generic <-  substr(bs$nom[i], 1, posbegin[i,1]-1)
  genre <- rbind(genre, generic)
  specific <-  substr(bs$nom[i], posbegin[i,1]+1, posend[i,2]-1)
  espece <- rbind(espece, specific)
}
row.names(genre) <- NULL
row.names(espece) <- NULL
head(espece)
head(genre)
bs <- cbind(bs,genre)
bs <- cbind(bs,espece)


#-------------------------------------------------------------------------------------------------
#PAYS --> REGION MANQUANTE

#On ne conserve que la colonne région dans le jeu de données global. Pour les region[i] vides 
# on remplace par le pays[i] correspondant, pour garder une traçabilité.


bs$region <- as.character(bs$region)
bs$pays <- as.character(bs$pays)

for(i in 1:nrow(Laurencia2)){
  if(bs$region[i] == ""){
    bs$region[i] <- as.character(bs$pays[i])
  }
}
bs$region[1]

#-------------------------------------------------------------------------------------------------
#INSERTION DES SEQUENCES CORRESPONDANTES

#Author code : FLorence
#On crée d'abord un csv à partir du fichier fasta correspondant :
input <- readLines("FirstEdit.fasta")
output <- file("seq_Saunders.csv","w")

for(i in 1:length(input)) {
  if(strtrim(input[i], 1) == ">") {
    writeLines(paste(input[i],";"), output, sep="")
  }
  else {
    writeLines(paste(input[i]), output, sep="\n")
  }
}
output
close(output)

#On ouvre ensuite le fichier csv créé pour l'utiliser en data.frame :

csv <- read.csv("seq_Saunders.csv", header = F, sep = ";", encoding = "latin1")

pos <- NULL
id <- NULL
idi <- NULL

#On récupère l'identifiant pour comparer avec ceux dans la table contenant les informations supplémentaires

for(i in 1:nrow(csv)){
  pos <- str_locate(csv[i,1], "[^[:alpha:]][[:upper:]]{3}[[:digit:]]+[^[:alpha:]]")
  idi <-  substr(csv[i,1], pos[1,1]+1, pos[1,2]-1)
  id <- rbind(id, idi)
  pos <- NULL
  idi <- NULL
}
rownames(id) <- NULL
csv <- cbind(csv,id)


#Comparaison des identifiants entre les tables csv et bs :
#On change le nom de la colonne d'identifiant pour bs :

colnames(bs)[2] <- "id"
colnames(csv)[2] <- "seq"

#On supprime les colonnes inutiles :
csv <- csv[,-1]
#On garde un trace de la provenance des données :
origine <- rep("G. Saunders", nrow(Laurencia2))
bs <- cbind(bs, origine, deparse.level = 1)


#On ajoute aux lignes ayant les mêmes identifiants les séquences correspondantes issues de csv :
bs <- merge(bs, csv, by = 'id',  all.x =T) 


#On mets les colonnes dans le bon ordre (id | genre | espece | region | lat | long | origine | seq)
# Et on nettoie le jeu de données :
bs <- bs[,-c(2:4)]
bs <- bs[,c(1,5,6,2,3,4,7,8)]


#On exporte en csv le jeu de données :
write.table(bs, file = "data_saunders.csv", append = FALSE, quote = TRUE, sep = ";",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = c("escape", "double"),
            fileEncoding = "")














