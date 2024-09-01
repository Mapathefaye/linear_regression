library(ggplot2)

## charger les données

ozone <- read.table(file="ozone_simple.txt", header = T, sep = ";", dec = ".")

summary(ozone$O3)
summary(ozone$T12)
var(ozone$O3)
var(ozone$T12)

## représentation graphique

hist(ozone$O3)
hist(ozone$T12)
plot(ozone$T12,ozone$O3)

ggplot(ozone, aes(x=T12, y=O3))+  ### meilleure solution
  geom_point()+
  xlab("T12")+
  ylab("maxO3")

## lançons une régression linéaire simple sur ce nuage de points

reg_simple <- lm(O3~T12, data = ozone)
summary(reg_simple)
#####################################################################################
## Avec summary: Nous obtenons des statistiques sur les résidus, avec le minimum, 
#le maximum et les 3 quartiles, ainsi que des statistiques sur les coefficients obtenus :  
#leur valeur, leur écart-type, la statistique de test de Student, et la p-valeur
#(le test effectué sur le paramètre est ici le test de significativité : 
#le paramètre vaut 0 versus le paramètre est différent de 0).
###### Les p-valeurs sont suppérieures à 5 %.
#À un niveau de test de 5 %, on conserve donc l'hypothèse selon laquelle le paramètre est égal à 0 : les paramètres ne 
#sont donc pas significativement différents de 0. Ici, on voit que la variable T12 est insignificative.
#####Quant au R2 , il est de l'ordre de 0.2791 donc faible : le modèle n'est peut-etre pas bon.

# visualisation de la droite de régression
ggplot(ozone,aes(x=T12,y=O3))+
  geom_point()+
  stat_smooth(method="lm",se=FALSE)+
  xlab("T12")+
  ylab("MaxO3")
# On peut également représenter les valeurs ajustées en fonction des valeurs observées

ozone$O3_ajust_s <- reg_simple$fit

ggplot(ozone, aes(x=O3,y=O3_ajust_s))+
  geom_point()+
  geom_abline(intercept=0,slope=1,color="red")+
  xlab("MaxO3")+
  ylab("MaxO3 ajusté")
###### La droite qui s'affiche est la première bissectrice. Si le modèle était parfait,
#les valeurs réelles et les valeurs ajustées seraient égales, donc sur un tel graphique, 
#les points seraient  alignés sur la droite d'équation y=x , soit la première bissectrice.

## récupérons les résidus
ozone_residus <- reg_simple$residuals
## représentaion des résidus

plot(residuals(reg_simple))

## prédiction de O3 pour T12 = 25

a_prevoir <- data.frame(T12=25)
maxO3_prev <- predict(reg_simple,a_prevoir)
round(maxO3_prev, digits=2)
