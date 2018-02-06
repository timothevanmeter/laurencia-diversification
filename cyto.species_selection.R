library("stringi", lib.loc="~/R/win-library/3.2")
library("stringr", lib.loc="~/R/win-library/3.2")


# Ce script permet de récupérer le consensus de délimitation d'espèces
# obtenu grâce à Cytoscape et de choisir aléatoirement un réprésentant
# unique par espèce


# Accessing cytoscape data, species delimitation
csv <- read.csv("C://Users//USER//Desktop//réseaux//sortie_cyto.csv",
                header = T, sep = ",", encoding = "latin1")


input <- readLines("C://Users//USER//Desktop//réseaux//clusters_laurencia.modf.txt")
# Le tableau doit être trier par numéro d'espèce croissant

# sort(csv) ......

# On repère les positions de chaque espèce dans le tableau

cr <- csv[1,2]
# On prévoit la taille du tableau d'arrivée en fonction 
# du nombre d'espèces
x <- nrow(csv)
size <- csv[x,2]

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

file <- "C://Users//USER//Desktop//species.list_Chondrophycus.txt"
write.table(sp.list, file=file, sep = ",", quote = F, row.names = F)














