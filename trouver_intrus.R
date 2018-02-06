library("stringr")
library("stringi")
setwd("//home//user//Bureau")


input <- read.csv("//home//user//Bureau//data//data_merge3//data_merge3.csv", sep=';')
input2 <- readLines("osmundea.txt")

genus <- "Osmundea"

id <- data.frame(input[,1:2])
id2 <- data.frame(input2)

# SCRIPT de recherche de differences entre deux jeux de donnees :
# On recherche les objets n'apparaissant pas dans un des jeux de donnees,
# ici on cherche les objets presents dans id et absents de id2.


# pos <- NULL
# pos1 <- NULL
# idi <- NULL
# id <- NULL
# for(i in 1:length(input)){
#   
#   pos <- str_locate(input[i,1], "[[:upper:]]+[[:digit:]]+")
#   idi <- substr(input[i,1], 2, pos[1,2])
#   id<- rbind(id, idi, input[i,2])
# }
# pos <- NULL
# for(i in 1:length(id)){
#   if(is.na(id[i]) == T){
#     posi <- FALSE
#     pos <- c(pos, posi)
#   }else{
#     posi <- TRUE
#     pos <- c(pos, posi)
#   }
# }
# pos
# id <- id[pos]

# pos <- NULL
# pos1 <- NULL
# idi <- NULL
# id2 <- NULL
# for(i in 1:length(input2)){
#   
#   pos <- str_locate(input2[i], "[[:upper:]]+[[:digit:]]+")
#   idi <- substr(input2[i], 2, pos[1,2])
#   id2<- rbind(id2, idi)
# }
# pos <- NULL
# for(i in 1:length(id2)){
#   if(is.na(id2[i]) == T){
#     posi <- FALSE
#     pos <- c(pos, posi)
#   }else{
#     posi <- TRUE
#     pos <- c(pos, posi)
#   }
# }
# pos
# id2 <- id2[pos]

length(id[,1])
length(id2)
# id <- data.frame(id)
# id2 <- data.frame(id2)
# id <- cbind(id, rep("N", length(id[,1])))
# id2 <- cbind(id2, rep("O", length(id2[,1])))

# On retire les doublons du jeu de donnees :
id <- id[!duplicated(id),]
id2 <- id2[!duplicated(id2),]

id2 <- data.frame(id2)
names(id) <- "id"
names(id2) <- "id"

# On compare les jeux de donnees :
# tab_comp <- merge(id2, id, by="id", all = T)
tab_comp <- merge(id2, id, by="id", all.x = T)
tab_comp <- tab_comp[!duplicated(tab_comp[,1]),]

etc <- round(table(tab_comp[,2])/length(tab_comp[,2]), digits=2)

write.table(etc, file = "etc.csv", append = FALSE, quote = TRUE, sep = ";",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = c("escape", "double"),
            fileEncoding = "")

# ---------------------------------------------------------------------------------------------

setwd("//home//user//Bureau")
csv.old <- read.csv("data_merge2.csv", header = F, sep = ",", encoding = "latin1")
colnames(csv.old)[1] <- "id"
csv.old <- cbind(csv.old, rep("OLD", length(csv.old$id)))
colnames(csv.old)[10] <- "comp"
csv.old <- csv.old[,c(1,10,2:9)]



setwd("//home//user//Bureau//data")
csv.line <- read.csv("newdata_line.csv", header = F, sep = ";", encoding = "latin1")
csv.gb <- read.csv("GB2017_04_03_Laurencia_newdata.csv", header = F, sep = ";", encoding = "latin1")

# On ne garde que la colonne correspondant aux identifiants
csv.line1 <- data.frame(csv.line[,c(2)])
csv.gb1 <- data.frame(csv.gb[,c(1)])
colnames(csv.gb1) <- "id"
colnames(csv.line1) <- "id"

csv.new <- rbind(csv.line1, csv.gb1)

# On compare les deux jeux de donnees pour voir quels sont les specimens vraiment
# nouveaux :
id2 <- merge(csv.old, csv.new, by="id", all = T)

# On peut obtenir le nombre de specimens deja presents dans l'ancien jeu de donnees : 
length(csv.old$id)+length(csv.new$id)-length(id2$id)

# et le nombre de specimens inedits :
length(id2$id)-length(csv.old$id)

# On extrait la liste des specimens inedits du jeu de donnees grace a la colonne
# d'identification comp ajoutee au jeu de donnees csv.old
pos <- NULL
posi <- NULL
id.new <- NULL
for(i in 1:length(id2$id)){
  if(is.na(id2$comp[i]) == T){
    posi <- TRUE
    pos <- c(pos, posi)
  }else{
    posi <- FALSE
    pos <- c(pos, posi)
  }
}
id.new <- id2[pos,]
id.new <- id.new[,1]

# On exporte les resultats :
write.table(id.new, file = "newdata_merge.csv", append = FALSE, quote = TRUE, sep = ";",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = c("escape", "double"),
            fileEncoding = "")








