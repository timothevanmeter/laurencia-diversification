library("stringi", lib.loc="~/R/win-library/3.2")
library("stringr", lib.loc="~/R/win-library/3.2")
library("ape", lib.loc="~/R/win-library/3.2")
library("regexr", lib.loc="~/R/win-library/3.2")
library("seqinr", lib.loc="~/R/win-library/3.2")

#------------------------------------------------------------------------------------------------
#----------------------------------###CONSTRUCTION JEU DE DONNEES GLOBAL###----------------------
#-------------------------------------------------------------------------------------------------

#On charge les différents jeux de données :

setwd("C://Users//USER//Desktop//GenBank")
gb <- read.table("data_gb.csv", header = FALSE, sep = ";")


setwd("C://Users//USER//Desktop//SAUNDERS")
saunders <- read.table("data_saunders.csv", header = FALSE, sep = ";")


setwd("C://Users//USER//Desktop//LINE")
line <- read.table("data_line.csv", header = FALSE, sep = ";")


setwd("C://Users//USER//Desktop//HEROEN")
heroen <- read.table("data_heroen.csv", header = FALSE, sep = ";")


#On regroupe tous les jeux de données :
data <- rbind(heroen, line, saunders, gb)


#On nomme les colonnes :
colnames(data)[1] <- "id"
colnames(data)[2] <- "genre"
colnames(data)[3] <- "espece"
colnames(data)[4] <- "region"
colnames(data)[5] <- "lat"
colnames(data)[6] <- "long"
colnames(data)[7] <- "origine"
colnames(data)[8] <- "seq"



setwd("C://Users//USER//Desktop")

#On exporte en csv le jeu de données :

write.table(data, file = "data_merge.csv", append = FALSE, quote = TRUE, sep = ";",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = c("escape", "double"),
            fileEncoding = "")




