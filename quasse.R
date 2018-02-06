# Script pour l analyse avec QUaSSE

library(diversitree)
library(stringr)
library(geiger)
library(ape)
library(phytools)

setwd("C://Users//USER//Desktop")
phy <- read.tree("consensus_ultrametric.tree.nwk")
taxa <- phy$tip.label

# d <- read.csv("annual.SST.Range_yuzurua.csv", sep = ",", 
#               dec = ".", header = T)
# data <- d
# names(d) <- c("annual.SST.Range", "id")

phy <- read.nexus("display_consensus2.tree.nex")
taxa <- phy$tip.label


setwd("C://Users//USER//Desktop")
d <- read.csv("annual.SST.mean_MARSPEC_consensus2.csv", sep = ",", 
              dec = ".", header = T)
data <- d

#############################################################
# On fait correspondre les données de taxon présentes sur l arbre et 
# celles des valeurs de traits
id <- NULL
idi <- NULL
lol <- rep("LOL",length(taxa))
for(i in 1:length(taxa)){
  pos <- str_locate(taxa[i], "[[:upper:]]+[[:digit:]]+")
  idi <- substr(taxa[i], 1, pos[1,2])
  id<- c(id, idi)
}
id <- cbind(id,lol)
names(id) <- c("id","lol")
id1 <- id
id <- id1

id <- merge(id, d, by="id", all.y = T)
id[,c(1:3,6)]
# Visualiser les données !!!
id <- id[c(1:145),]
#---------------------------------
pos <- NULL
for(i in 1:nrow(id)){
  if(duplicated(id[,1])[i] == T){
    posi <- i
    pos <- rbind(pos, posi)
  }
}
id <- id[-c(pos[,1]),]
#---------------------------------
id <- id[,-2] # Plus besoin de LOL !
tab.corres <- cbind(id, taxa) # On garde la correspondance 
# entre l'arbre et les données
id <- tab.corres
id <- id[,-c(1,3:7)]

# On conserve les valeurs de traits pour le groupe étudié
# setwd("C://Users//USER//Desktop//Osmundea")
# write.table(id, file = "annual.SST.Range_osmundea.csv", 
#             append = FALSE, quote = TRUE, sep = ",",
#             eol = "\n", na = "NA", dec = ".", row.names = F,
#             col.names = T, qmethod = c("escape", "double"))#,
            #fileEncoding = "")
#############################################################
# Analyse QUaSSE

sst.range <- log(id[,1])
names(sst.range) <- id$taxa

# Valeur d'incertitude sur le trait utilisé
sst.range.sd <- 1/50 # Aléatoire !


# starting.point.quasse fournit un point de départ pour les fonctions
# de ML
p <- starting.point.quasse(phy, sst.range)


xr <- range(sst.range) + c(-1,1) * 20 * p["diffusion"]
linear.x <- make.linear.x(xr[1], xr[2])

p.start <- c(p[1], p[1], mean(sst.range), 1, p[2:3])

lower <- c(0, 0, min(sst.range), -Inf, 0, 0)


lik <- make.quasse(phy, sst.range, sst.range.sd, sigmoid.x, constant.x)


lik.nodrift <- constrain(lik, drift ~ 0)


mle.n <- find.mle(lik.nodrift, p.start, control=list(parscale=.1),
                lower=lower, verbose=0)
lik.constant <- constrain(lik.nodrift, l.y1 ~ l.y0, l.xmid ~ 0, l.r ~ 1)

p.c <- mle.n$par.full

mle.c <- find.mle(lik.constant,
                         p.c,
                         control=list(parscale=.1), lower=0,
                         verbose=0)

anova(mle.n, constant=mle.c)

# Les valeurs de lambda et mu (mle.c$par) estimées constitueront 
# nos nouvelles coordonnées initiales

p.c <- mle.c$par
# On utilise ce point initial pour tous les modèles
p.l <- c(p.c[1], l.m=0, p.c[2:3])
p.s <- p.h <- c(p.c[1], p.c[1], mean(xr), 1, p.c[2:3])
# names(p.s) <- argnames(nodrift(f.s))
# names(p.h) <- argnames(nodrift(f.h))


# On peut utiliser plusieurs fonctions différentes 
# donnant la relation x ~ (lambda, mu) :
constant.x(xr, c(p.c[1], p.c[5]))
sigmoid.x(x, y0, y1, xmid, r)
stepf.x(x, y0, y1, xmid)
noroptimal.x(x, y0, y1, xmid, s2)
make.linear.x(x0, x1)
make.brownian.with.drift

# Fonction générale permettant la création des différents modèles
# utilisés par la suite. Par modèles on entend ici fonction de
# vraisemblance utilisables pour du maximum de vraisemblance ou
# de l'inférence bayésienne
make.model <- function(lambda, mu){
  make.quasse(phy, sst.range, sst.range.sd, lambda, mu)}

# Fonction permettant d'inclure de la diffusion dans un modèle
nodrift <- function(f){
  constrain(f, drift ~ 0)}

# On crée les modèles voulus
f.c <- make.model(constant.x, constant.x)
f.l <- make.model(linear.x, constant.x)
f.s <- make.model(sigmoid.x, constant.x)
f.h <- make.model(noroptimal.x, constant.x)


# Pour être comparés les différentes fonctions de vraisemblance
# doivent avoir un point de départ identique !!
# C'est pourquoi on effectue une analyse contrainte pour définir un
# point de départ pertinent qui sera utilisé pour tous les modèles

# control définit les paramètres d'intégration de la vraisemblance
control <- list(parscale=.1, reltol=0.001)
# On effectue une première recherche pour un modèle sans diffusion, 
# c'est notre analyse contrainte :
mle.c <- find.mle(nodrift(f.c), p, lower=0, control=control,
                  verbose=0)

# Recherche du point de vraisemblance maximum pour le modèle spécifié
mle.l <- find.mle(nodrift(f.l), p.l, control=control, verbose=0)
mle.s <- find.mle(nodrift(f.s), p.s, control=control, verbose=0)
mle.h <- find.mle(nodrift(f.h), p.h, control=control, verbose=0)

# On compare les différents modèles
anova(mle.c, linear=mle.l, sigmoidal=mle.s, hump=mle.h)

# + Diffusion
mle.d.c <- find.mle(f.c, coef(mle.c, TRUE), control=control, verbose=0)
mle.d.l <- find.mle(f.l, coef(mle.l, TRUE), control=control, verbose=0)
mle.d.s <- find.mle(f.s, coef(mle.s, TRUE), control=control, verbose=0)
mle.d.h <- find.mle(f.h, coef(mle.h, TRUE), control=control, verbose=0)


# Comparaison des différents modèles avec diffusion
anova(mle.d.c, linear=mle.d.l, sigmoidal=mle.d.s, hump=mle.d.h)

# Comparaison des différents modèles avec diffusion contre modèle constant
# sans diffusion
anova(mle.c, linear=mle.d.l, sigmoidal=mle.d.s, hump=mle.d.h)


#--------------------------------------------------------------------------

# DIPLAY RESULTS

y0 <- mle.d.h$par[1]
y1 <- mle.d.h$par[2]
xmid <- mle.d.h$par[3]
s2 <- mle.d.h$par[4]

a <- mle.d.h$par[1]
b <- mle.d.h$par[2]
c <- mle.d.h$par[3]
s <- mle.d.h$par[4]


x <- sst.range

plot(dnorm(x, mean=(xmid), sd=s2*100))





for(i in 1:length(sst.range)){
  
  yi <- y0+(y1-y0)*exp(-(sst.range[i]-xmid)/(2*s2**2))
  y <- rbind(y, yi)
  
}


plot(y ~ sst.range, data=sst.range)




