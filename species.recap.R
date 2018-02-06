
# Crée la table finale récapitulant les espèces délimitées 
# au sein de chaque genre et les haplotypes et les spécimens 
# correspondants et le représentant sélectionné.


list <- c('coronaphycus','chondrophycus','laurencia','laurenciella',
          'osmundea','palisada','yuzurua')
dir <- 'C://Users//USER//Desktop//'
nom <- 'consensus2.species.list_'
method <- 1

# À modifier selon le nom des fichiers auxquels accéder
open.access <- function(char,dir,nom, method){
  if(method==1){
    fol <- paste(dir,char,sep='')
    setwd(fol)
    name <- paste(nom,char,'.csv',sep='')}
  else{
    fol <- dir
    setwd(fol)
    name <- paste(char,nom,'.csv',sep='')}
  
  c <- read.csv(name, header=T, sep = ","
                   , dec = ".", skipNul = T)
  genre <- rep(char, length(c[,1]))
  glist<- cbind(c, genre)
  
  if(method==1){names(glist) <- c("sp.num","representant","genre")}
  else{names(glist) <- c("id","sp.num","genre")}
  glist <- glist[,c(3,1,2)]
  
  return(glist)
}
consensus <- NULL


# On récupère les consensus de délimitation d'espèce final
for(i in 1:length(list)){
  if(i==1){consensus.co <- open.access(list[i],dir,nom,method)}
  if(i==2){consensus.ch <- open.access(list[i],dir,nom,method)}
  if(i==3){consensus.l <- open.access(list[i],dir,nom,method)}
  if(i==4){consensus.le <- open.access(list[i],dir,nom,method)}
  if(i==5){consensus.o <- open.access(list[i],dir,nom,method)}
  if(i==6){consensus.p <- open.access(list[i],dir,nom,method)}
  if(i==7){consensus.y <- open.access(list[i],dir,nom,method)}
}

splist.y <- consensus.y
consensus.y <- consensus.y[c(2,4,5),]
names(splist.y) <- c('genre','sp.num','id')

dir <- 'C://Users//USER//Desktop//réseaux'
nom <- '_sp.delimitation_consensus'
method <- 2


# On récupère les haplotypes uniques inclus dans ces espèces finales
for(i in 1:length(list)){
  if(i==1){splist.co <- open.access(list[i],dir,nom,method)}
  if(i==2){splist.ch <- open.access(list[i],dir,nom,method)}
  if(i==3){splist.l <- open.access(list[i],dir,nom,method)}
  if(i==4){splist.le <- open.access(list[i],dir,nom,method)}
  if(i==5){splist.o <- open.access(list[i],dir,nom,method)}
  if(i==6){splist.p <- open.access(list[i],dir,nom,method)}
  if(i==7){NULL}
}


glist.y <- merge(consensus.y,splist.y,by='sp.num',all.y=T)
glist.y <- glist.y[,-4]
glist.o <- merge(consensus.o,splist.o,by='sp.num',all.y=T)
glist.o <- glist.o[,-4]
glist.p <- merge(consensus.p,splist.p,by='sp.num',all.y=T)
glist.p <- glist.p[,-4]
glist.l <- merge(consensus.l,splist.l,by='sp.num',all.y=T)
glist.l <- glist.l[,-4]
glist.le <- merge(consensus.le,splist.le,by='sp.num',all.y=T)
glist.le <- glist.le[,-4]
glist.co <- merge(consensus.co,splist.co,by='sp.num',all.y=T)
glist.co <- glist.co[,-4]
glist.ch <- merge(consensus.ch,splist.ch,by='sp.num',all.y=T)
glist.ch <- glist.ch[,-4]

glist <- rbind(glist.l,glist.ch,glist.co,
      glist.le,glist.p,glist.o,glist.y)

names(glist) <- c('sp.num','genre','representant','id')



# On exporte la nouvelle table récapitulative
setwd("C://Users//USER//Desktop//DATA")
write.table(glist, file="species.recap_consensus2.csv", sep=",", dec=".", eol="\n"
            , row.names=F, col.names=T)




# On récupère les spécimens correspondants




