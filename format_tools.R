################################################################################################    


#unwanted_array = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
#  'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
# 'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
# 'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
# 'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )


#Preparer un fichier csv avec 5 colonnes: ID, Nomgenre, Nomesp, localité  et  pays
#Attention ne pas mettre de titre de colonne
#Possibilité de probleme d'encodage...
GoodN <- read.csv("CACAFRARED_clean3.csv", header = F, sep = ";", encoding="UTF-8") #Ici fichier sans titre de colonne
GoodNdf<- data.frame(ID = GoodN[,1], Genus = GoodN[,2], species = GoodN[,3], locality = GoodN[,4], Country = GoodN[,5])
GoodNdf$Genus <-as.character(GoodNdf$Genus)
GoodNdf$species <-as.character(GoodNdf$species)
GoodNdf$locality <-as.character(GoodNdf$locality)
GoodNdf$Country <-as.character(GoodNdf$Country)
GoodNdf<- GoodNdf[order(GoodNdf$ID, na.last = FALSE, decreasing = FALSE), ]

#Nettoyage accent BASIQ
GoodNdf$Genus <-chartr("éèëêÉÈËÊàÀçÇñZ", "eeeeEEEEaAcCnZ", GoodNdf$Genus)
GoodNdf$species <-chartr("éèëêÉÈËÊàÀçÇ", "eeeeEEEEaAcC", GoodNdf$species)
GoodNdf$locality <-chartr("éèëêÉÈËÊàÀçÇ", "eeeeEEEEaAcC", GoodNdf$locality)
GoodNdf$Country <-chartr("éèëêÉÈËÊàÀçÇ", "eeeeEEEEaAcC", GoodNdf$Country)

#Nettoyage accent plus complet
unwanted_array = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                      'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                      'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                      'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                      'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

for(i in seq_along(unwanted_array))
  GoodNdf$species <- gsub(names(unwanted_array)[i],unwanted_array[i],GoodNdf$species)

for(i in seq_along(unwanted_array))
  GoodNdf$Genus <- gsub(names(unwanted_array)[i],unwanted_array[i],GoodNdf$Genus)

for(i in seq_along(unwanted_array))
  GoodNdf$locality <- gsub(names(unwanted_array)[i],unwanted_array[i],GoodNdf$locality)

for(i in seq_along(unwanted_array))
  GoodNdf$Country <- gsub(names(unwanted_array)[i],unwanted_array[i],GoodNdf$Country)

#Script pr file alignement .csv obtenu avec Script fasto2csv2.R (4 colonnes dont ID et colonne seq)
IDalig <- read.csv("aligTMP3.csv", header = T, sep = ";", encoding = "UTF-8")#Ici fichier sans titre de colonne
IDalig[,3] <- as.character(IDalig[,3])
IDaligdf <- data.frame(Namefasta = IDalig$Namefasta, ID = IDalig$ID, seq = IDalig$seq, stringsAsFactors = F)
IDaligdf<- IDaligdf[order(IDaligdf$ID, na.last = TRUE, decreasing = FALSE), ]

#############################################################

#Etape de combinaison Merge
Finaldf <- merge(IDaligdf, GoodNdf, by = 'ID',  all.x =T) #all.x=T Merge sur tous les ID de "IDaligdf" et met si présent la correspondance de "GoodNdf"
Finaldf$Finalname <- paste0(as.character(Finaldf$ID),"_",as.character(Finaldf$Genus),"_",as.character(Finaldf$species),"_",as.character(Finaldf$locality),"_",as.character(Finaldf$Country))
write.csv2(Finaldf, "FinalTMP1.csv")

#élimine les double"_" et NA dans titre seq
Finaldf$Finalname<- gsub("__", "", Finaldf$Finalname)
Finaldf$Finalname<- gsub("_NA", "", Finaldf$Finalname)
write.csv2(Finaldf, "FinalTMP2.csv")#Bien verifier ce fichier avant passage en fasta

#élimine premier caractère des seq, le N
Finaldf$seq <- gsub('^.','',as.character(Finaldf$seq))

finalfasta <- character(2 * nrow(Finaldf))
finalfasta[c(TRUE, FALSE)]<- paste0(">", as.character(Finaldf$Finalname))
finalfasta[c(FALSE, TRUE)]<-as.character(Finaldf$seq)
finalfasta
writeLines(finalfasta, "CleanNames_alig.fas", sep = "\n")


#####################################



#Crée une nouvelle colonne dans XXX comprenant les lignes de caractères des colonnes 7 et 8#
#les caractères sont séparés par une virgule, sep=','#

XXX$loc = paste((XXX[,7]),XXX[,8],sep=',')
