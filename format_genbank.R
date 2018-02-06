library("stringi", lib.loc="~/R/win-library/3.2")
library("stringr", lib.loc="~/R/win-library/3.2")
library("ape", lib.loc="~/R/win-library/3.2")
library("regexr", lib.loc="~/R/win-library/3.2")
library("seqinr", lib.loc="~/R/win-library/3.2")


#-------------------------------------------------------------------------------------------------
#----------------------------------###GENBANK###--------------------------------------------------
#-------------------------------------------------------------------------------------------------
#EXTRACTION NOM DE GENRE, NOM D'ESPECE POUR LES DONNEES GenBank


#On se place dans le dossier contenant les donnees :
setwd("C://Users//USER//Desktop//GenBank")

#Notre jeu de donnees :
alig_ordered_unique <- read.table("alig_ordered_unique.csv", header = TRUE, sep = ";")
bs <- alig_ordered_unique

# Notre motif : [_]|[Genre]|[_][esp?ce]|[_][...]
#On cherche a trouver les positions symbolisees par |

#On cherche pour chaque ligne la position de la majuscule du nom de genre : posbegin[,1] , le d?but 
# du nom d'espece : posbegin[,2] et la position de fin du nom d'espece : posend[,2]
posbegin <- NULL
posmid <- NULL
posend <- NULL
for(i in 1:nrow(alig_ordered_unique)){
  pos <- str_locate(bs$Namefasta[i], "[[:punct:]][[:upper:]][[:lower:]]+[[:punct:]]")
  posbegin <- rbind(posbegin, pos)
  posii <- str_locate(bs$Namefasta[i], "[[:punct:]][[:upper:]][[:lower:]]+[[:punct:]][[:lower:]]+[[:punct:]]")
  posend <- rbind(posend, posii)
}

#On peut verifier le contenu de nos variables pour plus de securite
# head(posbegin)
# head(posmid)
# head(posend)

#On utilise ensuite ces positions pour extraire les noms de genre et d'espece dans deux nouvelles 
# colonnes reinserees dans notre jeu de donnees
genre <- NULL
espece <- NULL
for(i in 1:nrow(alig_ordered_unique)){
  generic <-  substr(bs$Namefasta[i], posbegin[i,1]+1, posbegin[i,2]-1)
  genre <- rbind(genre, generic)
  specific <-  substr(bs$Namefasta[i], posbegin[i,2]+1, posend[i,2]-1)
  espece <- rbind(espece, specific)
}

#On supprime les row.names pour qu'ils ne soient pas en double lors de la concatenation

row.names(genre) <- NULL
row.names(espece) <- NULL
bs <- cbind(bs,genre)
bs <- cbind(bs,espece)
head(espece)
head(genre)

id <- NULL


#On recupere l'identifiant dans la 3eme colonne
#motif : _voucher_ARS03697_

for(i in 1:nrow(alig_ordered_unique)){
  pos <- str_locate(bs[i,3], pattern = "voucher_[[:alnum:]]+_")
  idi <- substr(bs[i,3], pos[1,1]+8, pos[1,2]-1)
  id <- rbind(id, idi)
}
rownames(id) <- NULL
bs <- cbind(bs,id)


#On garde un trace de la provenance des donnees :
origine <- rep("GenBank", nrow(alig_ordered_unique))
bs <- cbind(bs, origine, deparse.level = 1)

#On prepare des colonnes supplementaires :
lat <- rep("NA", nrow(alig_ordered_unique))
long <- rep("NA", nrow(alig_ordered_unique))
region <- rep("NA", nrow(alig_ordered_unique))
bs <- cbind(bs, lat, long, region, deparse.level = 1)

#On supprime les colonnes inutiles pour les analyses :
bs <- bs[,-c(1:4,6,7)]
bs <- bs[,c(4,2,3,8,6,7,5,1)]



#-------------------------------------------------------------------------------------------------
#AJOUT DES INFORMATIONS GEOGRAPHIQUES :

#On ouvre ensuite le fichier csv cree pour l'utiliser en data.frame :
supp_data <- read.csv("ars_georef.csv", header = T, sep = ",", encoding = "latin1")

colnames(supp_data) <- c("id", "genre", "espece", "region", "lat", "long", "origine")

#On ajoute aux lignes ayant les m?mes identifiants les informations issues de  supplementary_data.csv :
bs <- merge(bs, supp_data, by = 'id', all.x = T)


# Pour les ARS on supprime les informations redondantes :
ars <- bs[1:61,]
ars <- ars[,-c(2:7)]
ars <- ars[,c(1,3:8,2)]

# Pour les autres on supprime les colonnes issues du merge avec supp_data :
bs <- bs[-c(1:61),]
bs <- bs[,-c(9:15)]

# On rassemble les jeux de donn?es nettoy?s :
colnames(ars) <- c("id", "genre", "espece", "region", "lat", "long", "origine", "seq")
colnames(bs) <- c("id", "genre", "espece", "region", "lat", "long", "origine", "seq")

bs[,5] <- as.numeric(is.na(bs[,5]))
bs[,6] <- as.numeric(is.na(bs[,6]))

bs <- rbind(bs, ars)
for(i in 1:nrow(bs)){
  if(bs[i,5] == 0.00){
    bs[i,c(5:6)] <- NA
  }
}


#On exporte le jeu de donn?es :
write.table(bs, file = "data_gb.csv", append = FALSE, quote = TRUE, sep = ";",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = c("escape", "double"),
            fileEncoding = "")




