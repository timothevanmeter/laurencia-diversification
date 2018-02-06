library("stringi", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("stringr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("ape", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("regexr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("seqinr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")

#-----------------------------------------------------------------------------------------------------
#--------------------------------------------LINE-----------------------------------------------------
#-----------------------------------------------------------------------------------------------------

#On se place dans le dossier contenant les donnees :
setwd("C://Users//USER//Desktop//LINE")

#Auteur du code initial : Florence Rousseau
#On cree d'abord un csv a partir du fichier fasta correspondant :
input <- readLines("laurencia_clean2.fas")
output <- file("seq_LINE.csv","w")

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

#On ouvre ensuite le fichier csv cree pour l'utiliser en data.frame :
#Notre jeu de donnees :
csv <- read.csv("seq_LINE.csv", header = F, sep = ";", encoding = "latin1")

setwd("//home//user//Bureau//data")
csv <- read.csv("seq_Laurencia_supp.csv", header = F, sep = ";", encoding = "latin1")

# setwd("//home//user//Bureau")
# csv.new <- read.csv("newdata_merge2.csv", header = F, sep = ";", encoding = "latin1")
# csv <- read.csv("data_merge3.csv", header = F, sep = ";", encoding = "latin1")
# 
# w <- rep(NA, 107)
# y <- rep('NEW.DATA', 107)
# 
# csv.new2 <- data.frame(csv.new, w, w, w, y)
# csv.new2 <- csv.new2[,c(1:3,6,4,7:9,5)]
# colnames(csv.new2) <- c('id', 'genre', 'sp', 'util', 'region', 'lat', 'long', 'origin', 'seq')
# colnames(csv.old) <- c('id', 'genre', 'sp', 'util', 'region', 'lat', 'long', 'origin', 'seq')
# 
# csv.g <- rbind(csv.old, csv.new2)


#On nomme les colonnes :
colnames(csv)[1] <- "id"
colnames(csv)[2] <- "seq"

#On extrait a partir de la 1ere colonne l'identifiant (voucher), les noms de genre et d'espece et la region

pos <- NULL
pos1 <- NULL
idi <- NULL
id <- NULL
genrei <- NULL
genre <- NULL
spi <- NULL
sp <- NULL
regioni <- NULL
region <- NULL
for(i in 1:nrow(csv)){
  
  pos <- str_locate(csv[i,1], "[[:upper:]]+[[:alnum:]]+")
  idi <- substr(csv[i,1], 1, pos[1,2])
  id<- rbind(id, idi)
  
  pos1 <- str_locate(csv[i,1], "[[:digit:]][[:punct:]][[:upper:]][[:lower:]]{4,}")
  genrei <- substr(csv[i,1], pos1[1,1]+2, pos1[1,2])
  genre <- rbind(genre, genrei)
  
  # pos <- str_locate(csv[i,1], "[[:lower:]][[:punct:]][^[:upper:]][[:upper:]][[:alnum:]]+")
  # regioni <- substr(csv[i,1], pos[1,1]+2, pos[1,2])
  # region <- rbind(region, regioni)
  
  pos <- str_locate(csv[i,1], "[[:lower:]][[:punct:]][[:upper:]][[:alpha:]]+")
  regioni <- substr(csv[i,1], pos[1,1]+2, pos[1,2])
  region <- rbind(region, regioni)
  
  pos <- str_locate(csv[i,1], "[[:punct:]][[:lower:]]+[[:alnum:]]+")
  spi <- substr(csv[i,1], pos[1,1]+1, pos[1,2])
  sp <- rbind(sp, spi)
}
rownames(id) <- NULL
rownames(genre) <- NULL
rownames(region) <- NULL

#On ajoute les informations dans de nouvelles colonnes :
csv <- cbind(csv, id)
csv <- cbind(csv, genre)
csv <- cbind(csv, sp)
csv <- cbind(csv, region)
#On garde un trace de la provenance des donnees :
origine <- rep("L. Le Gall", nrow(csv))
csv <- cbind(csv, origine, deparse.level = 1)

#nettoyage / changement de l'ordre des colonnes :
csv <- csv[,-1]
csv <- csv[,c(2,3,4,5,1)]



#----------------------------------------------------------------------
#On visualise le nombre d'identifiants manquants et leurs positions :
# pos.NA <- NULL
# csvid.NA <- 0
# for(i in 1:nrow(csv)){
#   
#   if(is.na(csv[i,3]) == TRUE){
#     csvid.NA <- csvid.NA+1
#     posi.NA <- i
#     pos.NA <- c(pos.NA, posi.NA)
#   }
# }
# length(csvid.NA)
# pos.NA

#On visualise les lignes pour lesquelles la colonne region ($region) est vide :
# csvregion.NA <- NULL
# for(i in 1:nrow(csv)){
#     if(is.na(csv[i,5]) == TRUE){
#     rowNA <- c(as.character(csv[i,1]))
#     rowNA <- cbind(rowNA, c(as.character(csv[i,2])))
#     rowNA <- cbind(rowNA, c(as.character(csv[i,3])))
#     rowNA <- cbind(rowNA, c(as.character(csv[i,4])))
#     rowNA <- cbind(rowNA, c(as.character(csv[i,5])))
#     csvregion.NA <- rbind(csvregion.NA, rowNA)
#   }
# }
# View(csvregion.NA)
#La verification visuelle permet de dire que les informations sur les regions Sont correctements extraites.
#L'ensemble des lignes presentant des NA pour la colonne region ne possedent effectivement aucune information initiale. 

#On visualise les lignes pour lesquelles la colonne genre ($genre) est vide :
# csvgenre.NA <- NULL
# for(i in 1:nrow(csv)){
#   #   if(is.na(csv[i,4]) == TRUE){
#     rowNA <- c(as.character(csv[i,1]))
#     rowNA <- cbind(rowNA, c(as.character(csv[i,2])))
#     rowNA <- cbind(rowNA, c(as.character(csv[i,3])))
#     rowNA <- cbind(rowNA, c(as.character(csv[i,4])))
#     rowNA <- cbind(rowNA, c(as.character(csv[i,5])))
#     csvgenre.NA <- rbind(csvgenre.NA, rowNA)
#   }
# }
# View(csvgenre.NA)

#On exporte en csv la liste "nettoyee" des identifiants :
# write.table(csv[,3], file = "id_seq_line.csv", append = FALSE, quote = TRUE, sep = "",
#             eol = "\n", na = "NA", dec = ".", row.names = F,
#             col.names = F, qmethod = c("escape", "double"),
#             fileEncoding = "")

# #On visualise le nombre d'incoherences et leurs positions pour les regions x et y :
# 
# pos.dif <- NULL
# posi.dif <- NULL
# csvdif <- NULL
# for(i in 1:nrow(csv)){
#   
#   if(identical(csv$region, csv$region.y) != T){
#     csvdif <- csvdif+1
#     posi.dif <- i
#     pos.dif <- c(pos.dif, posi.dif)
#   }
# }
# csvdif
# pos.dif
# 
# #Suivant le r?sulat :
# csv <- csv[,]

#----------------------------------------------------------------------------------------
#AJOUT DES SUPPLEMENTARY DATA :


#On ouvre ensuite le fichier csv cree pour l'utiliser en data.frame :
# supp_data <- read.csv("supplementary_data.csv", header = F, sep = ",", encoding = "latin1")
setwd("//home//user//Bureau//data//new_data_Line")
supp_data <- read.csv("newdata_line.csv", header = F, sep = ";", encoding = "latin1")
setwd("//home//user//Bureau//data")
supp_data2 <- read.csv("GB2017_04_03_Laurencia_newdata.csv", header = F, sep = ";", encoding = "latin1")
supp_data <- supp_data[,c(2:6)]
colnames(supp_data) <- c("id", "genre", "espece", "region", "sequence")
x <- rep('NA', 5)
supp_data2 <- cbind(supp_data2, x)
supp_data2 <- supp_data2[,c(1:3,5,4)]
colnames(supp_data2) <- c("id", "genre", "espece", "region", "sequence")

supp_data.g <- rbind(supp_data, supp_data2)

# On enleve les doublons dans les donnees :
csv <- data.frame(csv[!duplicated(csv),])
supp_data.g <- data.frame(supp_data.g[!duplicated(supp_data.g[id]),])

# colnames(supp_data) <- c("id", "lat", "long", "region", "genre", "espece")

#On ajoute aux lignes ayant les memes identifiants les informations issues de  supplementary_data.csv :
csv.g <- merge(csv, supp_data.g, by = 'id')

#On mets les colonnes dans le bon ordre (id | genre | espece | region | lat | long | origine | seq) :
csv <- csv[,-c(2,3)]
csv <- csv[,c(1,7,8,6,4,5,2,3)]



# On extrait les identifiants pour les objets sans georeferencement :

pos.NA <- NULL
posi.NA <- NULL
csvid.NA <- NULL
for(i in 1:nrow(csv)){
  
  if(is.na(csv[i,5]) == TRUE){
    csvid.NA <- csvid.NA+1
    posi.NA <- c(as.character(csv[i,1]))
    pos.NA <- rbind(pos.NA, posi.NA)
  }
}
length(csvid.NA)
pos.NA
#On exporte en csv ces identifiants :

write.table(pos.NA, file = "id_missing_georef.csv", append = FALSE, quote = TRUE, sep = ";",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = c("escape", "double"),
            fileEncoding = "")







#----------------------------------------------------------------------------------------
#On prepare le jeu de donnees :



#On exporte en csv le jeu de donnees :

write.table(csv.g, file = "data_merge3.csv", append = FALSE, quote = TRUE, sep = ";",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = c("escape", "double"),
            fileEncoding = "")







