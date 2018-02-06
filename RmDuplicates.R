
root<-getwd()
# Fichier sans doublon et si doublon prend la seq la plus longue
alig <- read.csv2(paste0(root, "/aligTMP3.csv"), header=TRUE)
View(alig)
alig$seq_cor <- gsub("-", "", as.character(alig$seq))
alig$seq_cor <- gsub("N", "", as.character(alig$seq_cor))
alig$length <- nchar(as.character(alig$seq_cor))

alig_ordered <- alig[order(alig$length, decreasing=TRUE),]
View(alig_ordered)
alig_ordered_unique <- subset(alig_ordered, !duplicated(ID))
View(alig_ordered_unique)
write.csv2(alig_ordered_unique, 'alig_ordered_unique.csv')



# SORTIR LES DOUBLONS
alig_ordered_ID<- alig_ordered[order(alig_ordered$ID,decreasing = FALSE),]

duplicated2 <- function(x){ 
  if (sum(dup <- duplicated(x))==0) 
    return(dup) 
  if (class(x) %in% c("data.frame","matrix")) 
    duplicated(rbind(x[dup,],x))[-(1:sum(dup))] 
  else duplicated(c(x[dup],x))[-(1:sum(dup))] 
}
alig_ordered_doublon <- subset(alig_ordered_ID, duplicated2(ID))
View(alig_ordered_doublon)
write.csv2(alig_ordered_doublon, 'doublon.csv')




# Essai script
# data <- read.csv2(paste0(root, "/headers.csv"), header=FALSE)
# View(data)
# expr7 <- substring(as.character(data$V2), 1, 7)
# data$expr7<- expr7
# data$expr7 <- factor(data$expr7)
# 
# unique(data$V2) #3496
# levels(data$V2)
# unique(data$expr7) # 3048
# levels(unique(data$expr7))
# 
# write.table(unique(data$V2), "unique_names.txt", quote=F, row.names=F, col.names=F)
# 
# %not.in%
#   
# substring(as.character(data$V2), 1, 7)


# alig_ordered <- alig[order(alig$ID, decreasing=TRUE),]
# 
# alig_ordered$dup <- duplicated(alig_ordered$ID)
# 
# duplicated(alig_ordered$ID[990:1000])
# unique(alig_ordered$ID[990:1000])
# 
# table(alig_ordered$)
# subset(alig_ordered, unique(ID))
# gg <- unique(alig_ordered, incomparables = !alig_ordered$ID)
# gg[gg$ID=="MMS0032",]
# View(gg)
