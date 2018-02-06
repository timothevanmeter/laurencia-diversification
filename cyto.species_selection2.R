library("stringi", lib.loc="~/R/win-library/3.2")
library("stringr", lib.loc="~/R/win-library/3.2")

# Ce script permet de récupérer le consensus de délimitation d'espèces
# obtenu grâce à Cytoscape et de choisir aléatoirement un réprésentant
# unique par espèce


# Accessing cytoscape data, species delimitation
txt <- readLines("C://Users//USER//Desktop//réseaux//clusters_palisada.txt")
setwd("C://Users//USER//Desktop//réseaux")


for(i in 1:length(txt)){
  
  ch1 <- str_extract(txt[i], pattern = "[:alpha:]+[:space:][:digit:]+")
  if(is.na(ch1)==F){
    
    ch2 <- stri_replace(ch1, "Laurencia,sp_", regex = "[:alpha:]+[:space:]")
    txt[i] <- '\n'
  }else{
    
    ch2 <- stri_replace(ch2, "", regex = "Laurencia,sp_")
    txt[i] <- stri_join(txt[i], ch2, sep=',')
  }
  writeLines(txt,"palisada_sp.delimitation_consensus.txt")
}

# On récupère le fichier csv correspondant
# C'est à l'utilisateur de convertir le fichier txt précédent 
# en un fichier au format csv avec un éditeur de texte
csv <- read.csv("palisada_sp.delimitation_consensus.csv", 
               header = F, sep = ",", encoding="UTF-8")
head(csv)
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
head(tab)

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
head(sp.list)

file <- "C://Users//USER//Desktop//Laurenciella//consensus.species.list_laurenciella.txt"
write.table(sp.list, file=file, sep = ",", quote = F, row.names = F)




