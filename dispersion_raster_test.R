# SCRIPT BETA RASTER


library(raster)



r <- raster(ncol=10 , nrow=10)
ncell(r)


set.seed(0)



# On récupère les cellules ayant une valeur
row <- NULL
col <- NULL
rowi <- NULL
coli <- NULL
values(r) <- rpois(ncell(r), lambda = 0.05)

for(i in 1:ncell(r)){
  if(r[i] != 0){
    rowi <- ceiling(i/ncol(r))
    coli <- i-floor(i/ncol(r))*ncol(r)
    row <- rbind(row, rowi)
    col <- rbind(col, coli)
}}
pois <- cbind(row,col)
names(pois) <- NULL
# tirage de la proba 1/8
p <- floor(runif(1, min = 0, max = 8))

# FONCTION de diffusion
# On utilise comme entrée les points placés sur la zone par le processus de poisson comme départ 
# pour une diffusion.
i <- pois[1,1]
j <- pois[1,2]

diffusion <- function(i,j,p){
  # suivant sa valeur la particule va dans une des 8 cases adjacentes à la position actuelle
  if(p==0){j=j-1}
  if(p==1){j=j-1
  i=i+1}
  if(p==2){i=i+1}
  if(p==3){j=j+1
  i=i+1}
  if(p==4){j=j+1}
  if(p==5){j=j+1
  i=i-1}
  if(p==6){i=i-1}
  if(p==7){j=j-1
  i=i-1}
  
  return(i,j)
}


# values(r) <- runif(ncell(r))
hasValues(r)
plot(r)



# Tentative avec switch :

# switch(as.chararcter(p),
#        0 = {j=j-1},
#        1 = {j=j-1,i=i+1},
#        2 = {i=i+1},
#        3 = {j=j+1,i=i+1},
#        4 = {j=j+1},
#        5 = {j=j+1,i=i-1},
#        6 = {i=i-1},
#        7 = {j=j-1,i=i-1},)
# 
# return(i=i,j=j)