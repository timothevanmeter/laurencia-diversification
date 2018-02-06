library("stringi", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("stringr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("ape", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("regexr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("seqinr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")

setwd("//home//user//Bureau")

bs <- read.csv("data_merge3.csv", header = F, sep = ";", encoding="UTF-8")
colnames(bs) <- c('id', 'genre', 'sp', 'util', 'region', 'lat', 'long', 'origin', 'seq')

# Supprime les lignes contenant des NA pour les coordonnees geographiques
pos <- NULL
for(i in 1:nrow(bs)){
  if(is.na(bs[i,5]) == T){
    posi <- FALSE
    pos <- c(pos, posi)
  }else{
    posi <- TRUE
    pos <- c(pos, posi)
  }
}
pos
bs <- bs[pos,]



exclude <- read.csv("names_excluded.csv", header = F, sep = ",")

# Retire les specimens a exclure du fichier de sortie
# Les specimens sont specifies dans le fichier "names_excluded.csv"
keep <- NULL
buzz <- NULL

for(i in 1:nrow(bs)){
  for(j in 1:nrow(exclude)){
    
    buzz <- 0
    if(as.character(exclude[j,1])==as.character(bs[i,1])){
      keepi<- FALSE
      keep <- c(keep, keepi)
      buzz <- 1
    }
    if(buzz == 1){ break }
  }
  if(buzz == 0){ 
    keepi<- TRUE
    keep <- c(keep, keepi)
    }
}
head(keep)
bs <- bs[keep,]



# On conserve une table de correspondance entre les sequences analysees et
# leurs coordonnees geographiques
coordinates <- bs[,-c(7:8)]
names(coordinates)[1] <- "id"

bs <- bs[,-c(4,5,6,7)]
bs<- data.frame(id = bs[,1], genre = bs[,2], sp = bs[,3], seq = bs[,4])

# Ce script effectue les corrections suivantes : Retire les doublons, ordonne les sequences
# par taille (gap exclus) et convertit le resultat en un fichier fasta

# IL FAUT AVOIR RETIRER LES DOUBLONS DANS LE JEU DE DONNEES AVANT D EXECUTER CE SCRIPT :
# Autrement la fonction merge() va artificiellement augmenter le nombre de sequences
# On retire les doublons, c'est-a-dire les sequences avec le meme identifiant

pos <- NULL

for(i in 1:nrow(bs)){
  if(duplicated(bs$id)[i] == T){
    posi <- i
    pos <- rbind(pos, posi)
  }
}
bs <- bs[-c(pos[,1]),]


# Ordonne les sequences par taille decroissante en utilisant la troisieme 
# colonne pour calculer la taille des sequences en enlevant prealablement les gaps "-"
# Il faut bien verifier que l'on mesure la longueur dans la colonne
# correspondant bien aux sequences

bs$seq <- gsub(pattern = "[[:punct:]]", "", x = bs$seq)

length <- NULL
id <- NULL

for(i in 1:nrow(bs)){
  lengthi <- nchar(as.character(bs[i,9]))
  idi <- as.character(bs[i,1])
  length <- rbind(length, lengthi)
  id <- rbind(id, idi)
}
tablel <- cbind(id, length)
tablel<- data.frame(id = tablel[,1], length = tablel[,2])

# On cree une copie
bs2 <- bs
bs2 <- bs2[,-c(2:4)]
# On ajoute aux lignes ayant les memes identifiants les valeurs de taille
bs <- merge(bs, tablel, by = 'id')
 
bs2 <- merge(bs2, coordinates, by = 'id')
coordinates <- bs2[,-2]
# sort by length
bs <- bs[rev(order(bs$length)),]



#Elimine premier caractere des seq, le N
bs$seq <- gsub('^.','',as.character(bs$seq))

finalfasta <- character(2 * nrow(bs))
finalfasta[c(TRUE, FALSE)]<- paste0(">", as.character(bs$id))# ,"_",as.character(bs$genre),"_",as.character(bs$sp) )

finalfasta[c(FALSE, TRUE)]<-as.character(bs$seq)
finalfasta
# writeLines(finalfasta, "newnames_excluded_seqs_doublon_length-sorted.fas", sep = "\n")
writeLines(finalfasta, "newdata_merge3_excluded-doublon_length-sorted.fas", sep = "\n")

#############################
#On enregistre coordinates
write.table(coordinates, file = "coordinates.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = c("escape", "double"),
            fileEncoding = "")


#############################
# View the cumulative distribution of sequence size to decide where to cut sequence for further analyses

plot(ecdf(as.numeric(levels(tablel$length))), xlim = c(532,674), xlab = "taille des sequences", 
     ylab = "Nombre cumule de sequences", main = "Distribution Cumulative des tailles de sequences")


