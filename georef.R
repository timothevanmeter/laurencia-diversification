library("stringi", lib.loc="~/R/win-library/3.2")
library("stringr", lib.loc="~/R/win-library/3.2")
library("ape", lib.loc="~/R/win-library/3.2")
library("regexr", lib.loc="~/R/win-library/3.2")
library("seqinr", lib.loc="~/R/win-library/3.2")

#------------------------------------------------------------------------------------------------
#----------------------------------###GEO-REFERENCEMENT###-------------------------------------------------
#-------------------------------------------------------------------------------------------------

#On se place dans le dossier contenant les données :
setwd("C://Users//USER//Desktop//georef")



#Les jeux de données :
saunders <- read.table("data_saunders.csv", header = F, sep = ";")
line <- read.table("data_line.csv", header = F, sep = ";")
gb <- read.table("data_gb.csv", header = F, sep = ";")

colnames(saunders) <- c("id", "genre", "espece", "region", "lat", "long", "origine", "seq")
colnames(line) <- c("id", "genre", "espece", "region", "lat", "long", "origine", "seq")
colnames(gb) <- c("id", "genre", "espece", "region", "lat", "long", "origine", "seq")


saunders <- saunders[, -c(2:4,7,8)]
line <- line[, -c(2:4,7,8)]
gb <- gb[, -c(2:4,7,8)]


#On retire les objets sans géo-référencement :

pos <- NULL
for(i in 1:nrow(line)){
  if(is.na(line[i,2]) == T){
    posi <- FALSE
    pos <- c(pos, posi)
  }else{
    posi <- TRUE
    pos <- c(pos, posi)
  }
}
pos
line <- line[pos,]

pos <- NULL
for(i in 1:nrow(saunders)){
  if(is.na(saunders[i,2]) == T){
    posi <- FALSE
    pos <- c(pos, posi)
  }else{
    posi <- TRUE
    pos <- c(pos, posi)
  }
}
pos
saunders <- saunders[pos,]

pos <- NULL
for(i in 1:nrow(gb)){
  if(is.na(gb[i,2]) == T){
    posi <- FALSE
    pos <- c(pos, posi)
  }else{
    posi <- TRUE
    pos <- c(pos, posi)
  }
}
pos
gb <- gb[pos,]


georef <- rbind(line, saunders, gb)

#-----------------------------------------------
setwd("C://Users//USER//Desktop//georef")

#On exporte en csv le jeu de données :

write.table(georef, file = "geo_ref.csv", append = FALSE, quote = TRUE, sep = ";",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = c("escape", "double"),
            fileEncoding = "")



