
# Essai de correction de l'occurence thining par le pourcentage d'accumulation

# Formule : original Proba of thining * (1 - accumulation percentage)^2

plot(x=(1:100)/100, y=0.5*(1-(1:100)/100)^2,  xlab = "accumulation percentage", 
     ylab = "corrected thining probability, P=0.5", 
     type='l', 
     main = "Correction of occurence thining
     by specific diversity accumulation percentage")
points(0.01,0.49, pch=20, cex=2., col='red')
text(0.01,0.49, pos=4, labels='Initial thining probability', col='red')
# On peut contraindre les valeurs faibles de pourcentages
# d'accumulation en augmentant la puissance
# de la difference (1 - accumulation percentage)^2

# Il faut encore définir la valeur de cette puissance par des simulations !
# Penser à obtenir des données pour calibration


