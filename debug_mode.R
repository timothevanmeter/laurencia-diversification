

r <- raster("C://Users//USER//Desktop//biogeo08_17_30s//biogeo16_30s")

adj.cells.search(12918,3675,r)

###################################
# FONCTIONNE !!!
###################################
adj.cells.search <- function(r,z,w){
  s <- 1
  adj.cells <- NULL
  for(s in 1:10000){
  # for(s in 1:(min(dim(r)[1:2])/10)){
    
    m <- z-s
    n <- z+s
    p <- w-s
    q <- w+s
     
    for(i in m:n){
      
      for(j in p:q){
        
        if(i!=n && i!=m  &&  j!=q && j!=p){ break }
        if(is.na(r[i,j])==F){
          value <- as.numeric(r[i,j])
          adj.cells <- rbind(adj.cells, value)
          # adj.cells <- rbind(adj.cells, as.numeric(r[i,j]))
        }
      }
    }
    if(is.null(adj.cells)==F){ break }
  }
  return(adj.cells)
}
####################################
testd <- matrix(data=NA, nrow = 10000, ncol = 10000)
testd[5000,3000] <- 1
####################################
adj.cells.search2 <- function(r,z,w){
  s <- 1
  adj.cells <- NULL
  for(s in 1:10000){
    
    m <- z-s
    n <- z+s
    p <- w-s
    q <- w+s
     
    for(i in m:n){
      
      for(j in p:q){
        
        if(i!=n && i!=m  &&  j!=q && j!=p){ break }
        if(is.na(r[i,j])==FALSE){
          value <- r[i,j]
          adj.cells <- rbind(adj.cells, value)
          # adj.cells <- rbind(adj.cells, as.numeric(r[i,j]))
        }
      }
    }
    if(is.null(adj.cells)==F){ break }
  }
  return(adj.cells)
}
####################################








# Fonction 2 recherche en carré
# 1ère boucle : z-s,(w-s,w+s)
# 2ème boucle : w+s,(z-s+1,z+s)
# 3ème boucle : z+s,(w+s-1,w-s)
# 4ème boucle : w-s,(z+s-1,z-s+1)

adj.cells.search3 <- function(r,z,w){
  adj.cells <- NULL
  for(s in 1:10000){
    m <- z-s
    n <- z+s
    p <- w-s
    q <- w+s
    for(j in p:q){
        i <- m
        if(is.na(r[i,j])==F){
          value <- as.numeric(r[i,j])
          adj.cells <- rbind(adj.cells, value)
        }
      }
    for(i in m+1:n){
        # On ne modifie pas j qui reste à w+s
        if(is.na(r[i,j])==F){
          value <- as.numeric(r[i,j])
          adj.cells <- rbind(adj.cells, value)
        }
      }
    for(j in q-1:p){
        # On ne modifie pas i qui reste à z+s
        if(is.na(r[i,j])==F){
          value <- as.numeric(r[i,j])
          adj.cells <- rbind(adj.cells, value)
        }
      }
    for(i in n-1:m+1){
        # On ne modifie pas j qui reste à w-s
        if(is.na(r[i,j])==F){
          value <- as.numeric(r[i,j])
          adj.cells <- rbind(adj.cells, value)
        }
      }
    if(is.null(adj.cells)==F){ break }
  }
  return(adj.cells)
}  
###########################

# RESULTS
#
# R called with > microbenchmark(adj.cells.search3(testd,5000,5000),
#                  adj.cells.search2(testd,5000,5000), times = 1)
# Unit: seconds
#                               expr       min        lq      mean    median
# adj.cells.search3(testd, 5000, 5000)  9.924792  9.924792  9.924792  9.924792
# adj.cells.search2(testd, 5000, 5000) 49.865480 49.865480 49.865480 49.865480
#    uq        max      neval
# 9.924792  9.924792     1
# 49.865480 49.865480     1

