library("stringi", lib.loc="~/R/win-library/3.2")
library("stringr", lib.loc="~/R/win-library/3.2")
library("ape", lib.loc="~/R/win-library/3.2")
library("regexr", lib.loc="~/R/win-library/3.2")
library("seqinr", lib.loc="~/R/win-library/3.2")


setwd("C://Users//USER//Desktop//réseaux//guillaume")


csv <- read.csv("gmyc.delimitation_total_data.csv", header = T, 
                sep = ",", encoding = "latin1")



#ON extrait l'identifiant (voucher), 
# les noms de genre, d'espèce et de région

pos <- NULL
pos1 <- NULL
idi <- NULL
id <- NULL
for(i in 1:nrow(csv)){

  pos <- str_locate(csv[i,2], "[[:upper:]]+[[:digit:]]+")
  idi <- substr(csv[i,2], 1, pos[1,2])
  id<- rbind(id, idi)
}
csv <- cbind(csv, id)
csv <- csv[, c(1,3)]


#On exporte en csv le jeu de données :

write.table(csv, file = "gmyc.delimitation_total_data2.csv", append = FALSE,
            sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = c("escape", "double"),
            fileEncoding = "")



