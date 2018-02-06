library("stringi", lib.loc="~/R/win-library/3.2")
library("stringr", lib.loc="~/R/win-library/3.2")
library("ape", lib.loc="~/R/win-library/3.2")
library("regexr", lib.loc="~/R/win-library/3.2")
library("seqinr", lib.loc="~/R/win-library/3.2")

setwd("C://Users//USER//Desktop")
seq <- readLines("excluded_total_seqs_doublon_length-sorted_align_clean_unique_haplotypes.fas")
setwd("C://Users//USER//Desktop//Osmundea")
sp.os <- read.csv('consensus2.species.list_osmundea.csv')
setwd("C://Users//USER//Desktop//Palisada")
sp.pa <- read.csv('consensus2.species.list_palisada.csv')
setwd("C://Users//USER//Desktop//Laurencia")
sp.la <- read.csv('consensus2.species.list_laurencia.csv')
setwd("C://Users//USER//Desktop//Laurenciella")
sp.lae <- read.csv('consensus2.species.list_laurenciella.csv')
setwd("C://Users//USER//Desktop//Chondrophycus")
sp.ch <- read.csv('consensus2.species.list_chondrophycus.csv')
setwd("C://Users//USER//Desktop//Coronaphycus")
sp.co <- read.csv('consensus2.species.list_coronaphycus.csv')
setwd("C://Users//USER//Desktop//genres")
sp.oh <- read.csv('consensus.species.list_ohelopapa.csv')
sp.ro <- read.csv('consensus.species.list_rodriguezella.csv')
names(sp.os) <- c('sp.num', 'id')
names(sp.pa) <- c('sp.num', 'id')
names(sp.la) <- c('sp.num', 'id')
names(sp.lae) <- c('sp.num', 'id')
names(sp.ch) <- c('sp.num', 'id')
names(sp.co) <- c('sp.num', 'id')
names(sp.oh) <- c('sp.num', 'id')
names(sp.ro) <- c('sp.num', 'id')


output <- file("seq.consensus.csv","w")

for(i in 1:length(seq)) {
  if(strtrim(seq[i], 1) == ">") {
    writeLines(paste('\n', seq[i],','), output, sep='')
  }
  else {
    writeLines(paste(seq[i]), output, sep='')
  }
}
output
close(output)

#On ouvre ensuite le fichier csv créé pour l'utiliser en data.frame :
#Notre jeu de données :
csv <- read.csv("seq.consensus.csv", header = F, sep = ",", encoding = "latin1")
names(csv) <- c('name', 'seq')

pos <- NULL
pos1 <- NULL
idi <- NULL
id <- NULL
for(i in 1:nrow(csv)){
  
  pos <- str_locate(csv[i,1], "[[:upper:]]+[[:digit:]]+")
  idi <- substr(csv[i,1], 3, pos[1,2])
  id<- rbind(id, idi)
}
rownames(id) <- NULL
#On ajoute les informations dans de nouvelles colonnes :
csv <- cbind(csv, id)

os <- cbind(merge(sp.os, csv, by='id'), c(rep('os', nrow(sp.os))))
pa <- cbind(merge(sp.pa, csv, by='id'), c(rep('pa', nrow(sp.pa))))
la <- cbind(merge(sp.la, csv, by='id'), c(rep('la', nrow(sp.la))))
lae <- cbind(merge(sp.lae, csv, by='id'), c(rep('lae', nrow(sp.lae))))
ch <- cbind(merge(sp.ch, csv, by='id'), c(rep('ch', nrow(sp.ch))))
co <- cbind(merge(sp.co, csv, by='id'), c(rep('co', nrow(sp.co))))
oh <- cbind(merge(sp.oh, csv, by='id'), c(rep('oh', nrow(sp.oh))))
ro <- cbind(merge(sp.ro, csv, by='id'), c(rep('ro', nrow(sp.ro))))
names(os) <- c('id', 'sp.num', 'name', 'seq', 'genre')
names(pa) <- c('id', 'sp.num', 'name', 'seq', 'genre')
names(la) <- c('id', 'sp.num', 'name', 'seq', 'genre')
names(lae) <- c('id', 'sp.num', 'name', 'seq', 'genre')
names(ch) <- c('id', 'sp.num', 'name', 'seq', 'genre')
names(co) <- c('id', 'sp.num', 'name', 'seq', 'genre')
names(oh) <- c('id', 'sp.num', 'name', 'seq', 'genre')
names(ro) <- c('id', 'sp.num', 'name', 'seq', 'genre')

seq_consensus <- rbind(os, pa, la, lae, ch, co, oh, ro)
seq_consensus <- seq_consensus[,c(1:3,5,4)]


###################################################################


# Ordonne les séquences par taille décroissante en utilisant la troisième 
# colonne pour calculer la taille des séquences en enlevant préalablement les gaps "-"

seq_consensus$seq <- gsub(pattern = "[[:punct:]]", "", x = seq_consensus$seq)

length <- NULL
id <- NULL

for(i in 1:nrow(seq_consensus)){
  lengthi <- nchar(as.character(seq_consensus[i,5]))
  idi <- as.character(seq_consensus[i,1])
  length <- rbind(length, lengthi)
  id <- rbind(id, idi)
}
tablel <- cbind(id, length)
tablel<- data.frame(id = tablel[,1], length = tablel[,2])

# On ajoute aux lignes ayant les mêmes identifiants les valeurs de taille
seq_consensus <- merge(seq_consensus, tablel, by = 'id')

# sort by length
seq_consensus <- seq_consensus[rev(order(seq_consensus$length)),]



#élimine premier caractère des seq, le N
seq_consensus$seq <- gsub('^.','',as.character(seq_consensus$seq))

finalfasta <- character(2 * nrow(seq_consensus))
finalfasta[c(TRUE, FALSE)]<- paste0(as.character(seq_consensus$name))# ,"_",as.character(bs$genre),"_",as.character(bs$sp) )

finalfasta[c(FALSE, TRUE)]<-as.character(seq_consensus$seq)

setwd("C://Users//USER//Desktop")
writeLines(finalfasta, "consensus2_seqs_doublon_length-sorted.fas", sep = "\n")












