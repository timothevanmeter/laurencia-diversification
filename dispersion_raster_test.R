# SCRIPT BETA RASTER


library(raster)


r <- raster(ncol=50 , nrow=50)
ncell(r)


set.seed(0)

# On récupère les cellules ayant une valeur
row <- NULL
col <- NULL
rowi <- NULL
coli <- NULL
values(r) <- rpois(ncell(r), lambda = 0.01)

for(i in 1:ncell(r)){
  if(r[i] != 0){
    rowi <- ceiling(i/ncol(r))
    coli <- i-floor(i/ncol(r))*ncol(r)
    x <- rbind(x, xFromCell(r,i))
    y <- rbind(y, yFromCell(r,i))
    row <- rbind(row, rowi)
    col <- rbind(col, coli)
  }
}
pois <- cbind(row,col)
names(pois) <- NULL
lonlat <- cbind(x,y)


plot(r)

#########################
# Probabilité d accession aux différentes cases adjacentes
# 2 modes : Si d > dseuil un mode neutre où le tirage s effectue sur une distribution 
# uniforme. Si d < dseuil un mode attractif où le tirage s effectue sur une 
# distribution normale centrée sur la position adjacente la plus proche de 
# l attracteur considéré et avec une variance fonction de la distance à 
# l attracteur.

tirage.proba <- function(d, dseuil, ){
if(d>dseuil){
    p <- floor(runif(1, min = 0, max = 8))}

  p <- floor(rnorm(1, mean=, sd=1/dist^3))
}
#########################

i <- pois[1,1]
j <- pois[1,2]


x <- seq(0,10,1)
plot(x,x^3)


for(i in 1:nrow(lonlat)-1){
  d <- pointDistance(lonlat[i,], lonlat[i+1,], lonlat=F)
  dist <- rbind(i,as.numeric(d))
}

d <- as.numeric(d)


#######################
# FONCTION de diffusion
# On utilise comme entrée les points placés sur la zone par le 
# processus de poisson comme départ pour une diffusion.
diffusion <- function(i,j,p){
  # suivant sa valeur la particule va dans une des 8 cases 
  # adjacentes à la position actuelle
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
########################


q <- seq(0,8,0.1)
p <- dnorm(q,mean=8, sd=1)
p2 <- dnorm(q,mean=1, sd=1)
plot(q,p)
plot(q,p2,add=T)



