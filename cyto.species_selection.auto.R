library("stringi", lib.loc="~/R/win-library/3.2")
library("stringr", lib.loc="~/R/win-library/3.2")


# Ce script permet une fois que les fichiers "genre_sp.delimitation_consensus.csv"
# sont créés de générer de nouvelles sélections aléatoires pour l'ensemble de
# nos délimitations d'espèces

setwd("C://Users//USER//Desktop//réseaux")

genus.list <- c("osmundea","palisada","laurencia","laurenciella",
"chondrophycus","coronaphycus")

# Il faut d'abord enregistrer dans la session la fonction consensus.graph
# avant de lancer les lignes ci-dessous

for(i in 1:length(genus.list)){
  
  genus <- genus.list[i]
  consensus.graph(genus)
  
}


consensus.graph <- function(genus){
  
  name.csv <- paste(genus, "_sp.delimitation_consensus.csv", sep = "")
  # On récupère le fichier csv correspondant
  # C'est à l'utilisateur de convertir le fichier txt précédent 
  # en un fichier au format csv avec un éditeur de texte
  csv <- read.csv(name.csv, header = F, sep = ",", encoding="UTF-8")
  cr <- csv[1,2]
  # On prévoit la taille du tableau d'arrivée en fonction 
  # du nombre d'espèces
  x <- nrow(csv)
  size <- as.numeric(csv[x,2])
  tab <- data.frame(matrix(0, nrow = size, ncol = 3))
  n <- 1
  names(tab) <- c('species', 'start.position', 'end.position')
  tab[1,1] <- cr
  tab[1,2] <- 1
  
  for(i in 1:nrow(csv)){
    if(csv[i,2] == cr){}
    else{
      tab[n,3]<- i-1
      tab[n+1,2]<- i
      tab[n+1,1]<- csv[i,2]
      cr <- csv[i,2]
      n <- n+1
    }
    if(i == nrow(csv)){tab[n,3]<- i}
  }
  
  # On doit sélectionner de façon aléatoire un spécimen par
  # espèce
  sp <- NULL
  sp.num <- NULL
  sp.list <- NULL
  for(i in 1:nrow(tab)){
    if(tab[i,2] != tab[i,3]){
      # ch <- floor(runif(1, min=tab[i,2], max=tab[i,3]))
      ch <- sample(tab[i,2]:tab[i,3], 1)
      sp <- as.character(csv[ch,1])
      # sp.num <- c(sp.num, csv[ch,2])
    }else{
      ch <- tab[i,2]
      sp <- as.character(csv[ch,1])
      # sp.num <- c(sp.num, csv[ch,2])
    }
    sp.list <- c(sp.list, sp)
  }
  sp.num <- 1:nrow(tab)
  sp.list <- cbind(sp.num, sp.list)
  
  name <- paste("consensus2.species.list_", genus, ".txt", sep = "")
  b <- toupper(str_sub(genus, start=1, end=1))
  s <- paste(b, str_sub(genus, start=2), sep="")
  dir <- paste("C://Users//USER//Desktop//", s, "//", name, sep = "")
  
  write.table(sp.list, file=dir, sep = ",", quote = F, row.names = F)
}