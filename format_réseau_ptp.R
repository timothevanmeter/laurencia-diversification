library("stringi", lib.loc="~/R/win-library/3.2")
library("stringr", lib.loc="~/R/win-library/3.2")
library("ape", lib.loc="~/R/win-library/3.2")
library("regexr", lib.loc="~/R/win-library/3.2")
library("seqinr", lib.loc="~/R/win-library/3.2")

setwd("C://Users//USER//Desktop//Chondrophycus//ABGD")

file <- "abgd_recursive_partition.txt"
csv <- stri_read_lines(file)

pos <- NULL
posi <- NULL
idi <- NULL
id <- NULL

# Se souvenir que 01A07 Ohelopapa ne correspond pas au pattern attendu
# pour les identifiants classiques, à changer manuellement !!!

# Pour chaque ligne du fichier texte on récupère toutes les occurences
# du pattern regex 

for(i in 1:length(csv)){
  posi <- gregexpr(pattern = "[[:upper:]]+[[:digit:]]+", csv[i])
  pos <- regmatches(csv[i], posi)
  pos <- as.data.frame(pos)
  
  if(length(row.names(pos)) != 0){
    id <- paste(pos[,1], collapse = ",")
    csv[i] <- id
    writeLines(csv,"abgd_recursive_partition2.txt")
  }
}


# SCRIPT TEST !!!
pos <- c("RIRI", "FIFI", "LOULOU")
pos <- as.data.frame(pos)
id <- character()

id <- paste(pos[,1], collapse = ",")
print(idi)
print(id)
