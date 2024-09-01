# Q1
ozone<- read.table(file="ozone3.txt", sep=";",header=TRUE,dec=".")

# Q2: Analyse descriptive

## Premier aperçu
head(ozone)
summary(ozone)

## Distribution de la variable d'intérêt

par(mfrow=c(1,3))
plot(ecdf(ozone$O3),main="Fonction de repartition",xlab="")
hist(ozone$O3,main="Histogramme",xlab="")
plot(density(ozone$O3),main="Estimation de densite",xlab="")

### Commentaires: 
### - domaine de valeurs entre 40 et 140
### - pas de "trou" dans la répartition: on a des observations dans l'ensemble
###   de ce domaine
### - mode de la distribution autour de 80
### - un peu moins d'observations dans les grandes valeurs (qui sont celles qui
###   nous intéresseraient pour la prévision de pics de pollution...)

## Graphes paire à paire

avisualiser<-c("O3","O3v","T12","T15","Vx")
pairs(O3~.,data=ozone[,avisualiser])


### On remarque notamment: 
### - O3 semble liée à toutes les covariables (pas forcément linéairement, mais
###   il y a un lien visible "à l'oeil")
### - T12 et T15 semblent fortement corrélées (ce qui n'est pas une surprise, les
###   températures à 12h et à 15h sont liées). 
###   Il faudra faire attention si on les met toutes les deux dans un même modèle.

## Matrice de corrélation

cor(ozone[,avisualiser])
heatmap(abs(cor(ozone[,avisualiser])))

### - Confirmation du fait que O3 est corrélée à toutes les covariables. 
###   La corrélation est plus forte avec O3v: c'est sans doute la covariable qui nous
###   fournira le meilleur modèle lin\éaire (et ce n'est pas une surprise). 
### - Confirmation de la très forte corrélation entre T12 et T15 (>0.95). 
### - Les autres corrélations entre covariables sont plus faibles. 
###   En particulier, Vx est très peu corrélée à T12, ce qui est une bonne nouvelle:
###   ces deux covariables apportent des informations différentes sur O3. 


## Effet du vent?
## Il semble qu'il soit peut-être une bonne idée de le prendre
## en compte dans le modèle... cf note sur variables qualitatives
library(ggplot2)
library(tidyverse)
ozone %>% ggplot() + aes(x = vent, y  =O3) + geom_boxplot()

# Q4

regOTV <- lm(O3~O3v+T12+Vx,data=ozone) # Modèle complet sans vent
summary(regOTV)

regCompletVent <-lm(O3~.-Date-T15,data=ozone) # MC avec vent
summary(regCompletVent)


## Facultatif, mais bienvenu.
# Mieux de prendr en compte le vent?
# Non! le test de Fisher n'est pas significatif, on conserve le modèle
# sans le vent
## !!!!! On n'a pas encore fait les diagnostiques!!!
anova(regCompletVent,regOTV)

### Remarque: Si l'on avait voulu ajuster un modèle sans la constante (ce qui ne
### se justifie pas ici), il aurait fallu le préciser dans l'appel à la fonction
### lm: 
  
regOTVsansconstante <- lm(O3~O3v+T12+Vx-1,data=ozone)
anova(regOTVsansconstante,regOTV) #BONUS

# Q5

## Ajustement du modèle à une seule covariable: T12
regT12 <- lm(O3~T12,data=ozone)
summary(regT12)
anova(regT12,regOTV)

### A 5%, on rejette H_0 (p-valeur \approx 1.9 \times 10^{-7}): 
### le modèle complet est donc préfèrable au modèle simple construit avec
### T12 seule.
### Cela signifie que, en plus de T12, au moins l'une des variables O3v
### et Vx a un effet sur O3. 

## Ajustement du modèle à une seule covariable: O3v
regO3v <- lm(O3~O3v,data=ozone)
summary(regO3v)
anova(regO3v,regOTV)

### conclusion similaire à celle obtenue avec T12.

## Ajustement du modèle à une seule covariable: Vx
regVx <- lm(O3~Vx,data=ozone)
summary(regVx)
anova(regVx,regOTV)

### conclusion similaire à celle obtenue avec T12. 


## Autre moyen de comparaison: R^2
summary(regO3v)$r.squared
summary(regT12)$r.squared
summary(regVx)$r.squared
summary(regOTV)$r.squared


### le modèle complet semble le meilleur (63% de variance expliquée contre 44%
### maximum. Attention! Pas fiable pour comparer des modèles n'incluant pas le
### même nombre de covariables! 

## Autre moyen de comparaison: Ra^2
summary(regO3v)$adj.r.squared
summary(regT12)$adj.r.squared
summary(regVx)$adj.r.squared
summary(regOTV)$adj.r.squared

### le modèle complet reste le meilleur  (61% contre 43% maximum). 
### Mais c'est moins convaincant que des tests (qu'il est possible de réaliser ici).


# Q6

summary(regOTV)[[4]]

### Les p-valeurs se lisent dans la colonne de droite.

# Q7
# Déjà répondu en Q5!


# Q8

## H0: Beta_T12 <= 0
## H1: Beta_T12 > 0

alpha<-0.05
betahT12 <- coef(regOTV)[["T12"]]
sigmahbetahT12 <- summary(regOTV)$coefficients["T12",][[2]]
seuil <- betahT12 - sigmahbetahT12 * qt(1-alpha,nrow(ozone)-4)

### On rejette H0 si 0 n'appartient pas 0 < seuil


# Q9

## On conserve toutes les covariables, puisque les résultats des tests rejettent
## tous les sous-modèles à 0, 1, 2 covariables (en plus de la constante). 
##
## Ceci signifie que chacune des covariables apporte de l'information sur
## O3 que les autres covariables n'apportent pas déjà. 


# Q10

regOTTV <- lm(O3~O3v+T12+T15+Vx,data=ozone)
summary(regOTTV)
