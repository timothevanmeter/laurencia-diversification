#################################
# CALIBRATION D'ARBRES -- CHRONOS
# DELIMITATION D'ESPSECES -- GMYC
#################################

library(MASS)
library(paran)
library(ape)
library(splits)

setwd("C://Users//USER//Desktop")

# tree <- read.nexus("RAxML_subtree.laurencia.nex")
tree <- read.tree("RAxML_bestTree.consensus2_1000.nwk")

plot(tree, show.tip.label = F)

# Teste differents modeles d evolution moleculaire
# chr.cor <- chronos(tree, model ="correlated")
# chr.dis <- chronos(tree, model ="discrete")
# chr.rel <- chronos(tree, model ="relaxed")

# Differents parametres pour la calibration de l'arbre
control <- chronos.control(tol=1e-18, nb.rate.cat=100)
# Calibre sur Chondrophycus 

# chr.dis1 <- chronos(tree, model ="relaxed", lambda = 1e-10, 
                     # control = control,quiet = T)
chr.dis <- chronos(tree, control = control ,model ="discrete", 
                    lambda = 10, quiet = T)
# chr.dis3 <- chronos(tree, control = control , model ="discrete", 
                    # lambda = 1e+10, quiet = T)

# On observe le resultat
# mltt.plot(chr.dis,chr.rel,chr.cor)
# mltt.plot(chr.cor, chr.rel, chr.dis, log="y")
# mltt.plot(chr.dis1,chr.dis2,chr.dis3)
# mltt.plot(chr.dis1,chr.dis2,chr.dis3, log="y")
ltt.plot(chr.dis)
ltt.plot(chr.dis, log="y")

# On garde l'arbre ultrametrique correspondant
write.tree(chr.dis, file = "consensus_ultrametric.tree.nwk")

# On lance l'analyse avec GMYC
gmyc.dis <- gmyc(chr.dis, method = "single", quiet = T)
# gmyc.rel <- gmyc(chr.rel, method = "single", quiet = T)
# gmyc.cor <- gmyc(chr.cor, method = "single", quiet = T)
# gmyc.dis1 <- gmyc(chr.dis1, method = "single", quiet = T)
# gmyc.dis2 <- gmyc(chr.dis2, method = "single", quiet = T)
# gmyc.dis3 <- gmyc(chr.dis3, method = "single", quiet = T)
# gmyc.dis1.m <- gmyc(chr.dis.1, method = "multiple")

# makeChronosCalib(phy, node = "root", age.min = 1,
                 # age.max = age.min, interactive = FALSE, soft.bounds = FALSE)

# Resultats
summary(gmyc.dis)
plot(gmyc.dis)

# Rendre des resultats plus presentable !!!!
# Voir : ape
nodelabels(node=gmyc.dis$tree$Nnode, pch = 18, col = species.list[,1])

plot(gmyc.dis$tree, col=col, no.margin=TRUE, font=1, show.tip.label = F,
     show.node.label = T)

# Export des resultats
write.tree(gmyc.dis$tree, file = "GMYC_results_palisada.nwk")

# Export de la delimitation d'especes
species.list <- spec.list(gmyc.dis)
write.table(species.list, file = "gmyc.dis_laurencia.delimitation.csv", sep = ",",
            row.names = F, col.names = T)








############################
# Fonction japonaise plot et affinites
plot.result <- function(res, cex=0.5, edge.width=1, no.margin=F, show.tip.label=T, label.offset=0) {
  plot.tree <- function(tr, mrca, cex=0.5, edge.width=1, no.margin=F, show.tip.label=T, label.offset=0) {
    traverse.subtree <- function(tr, n=1) {
      numtip <- length(tr$tip.label)
      sub.node <- tr$edge[which(tr$edge[,1] == n+numtip), 2]
      
      res.node <- c()
      for (sn in sub.node) {
        res.node <- c(res.node, sn)
        res.node <- c(res.node, traverse.subtree(tr, sn-numtip))
      }
      return (res.node)
    }
    
    numtip <- length(tr$tip.label)
    br.col <- rep(1, length(tr$edge.length))
    
    for (i in mrca) {
      for (j in traverse.subtree(tr, i-numtip)) {
        br.col[which(tr$edge[,2]==j)] <- 2
        
      }
    }
    
    plot(tr, edge.color=br.col, show.tip.label=show.tip.label, cex=cex, edge.width=edge.width, no.margin=no.margin, label.offset=label.offset)
  }
  
  plot.tree(res$tree, res$MRCA[[which.max(res$likelihood)]]+length(res$tree$tip.label), cex=cex, edge.width=edge.width, no.margin=no.margin, show.tip.label=show.tip.label, label.offset=label.offset)
}
############################