### TP6 - Selection de variables
### Code R pour commencer

# Question 1
diabetes1 <- read.table(file="diabetes1.txt")
# A compléter...

# Question 3
# Analyse du lien entre ser3 et sex, puis entre ser4 et sex
par(mfrow=c(1,2))
boxplot(ser3~sex,data=diabetes1,xlab="sex",ylab="ser3")
boxplot(ser4~sex,data=diabetes1,xlab="sex",ylab="ser4")

# Corrélations entre les variables, y compris sex
round(cor(diabetes1),2)

# A compléter

# Question 5
# A compléter


####################

### Question 6: recherche exhaustive
# Eventuellement si pas déjà installé:
# install.packages("leaps")
library(leaps)

## Ajuste tous les modeles possibles,
## avec jusqu'a 11 covariables (nvmax),
## en ne retenant que les 2 meilleurs (nbest) pour chaque nombre de covariables,
## en forcant l'utilisation d'un intercept (intercept=TRUE).
rech.ex2<-regsubsets(prog~.,data=diabetes1,nvmax=11,nbest=2,intercept=TRUE,method="exhaustive")
summary(rech.ex2)

# Demandez-vous pourquoi est-ce que le choix du "meilleur" modèle ne
# dépend pas du critère de sélection... En d'autres termes, comment se
# fait-il que peu importe le critère choisi, le "meilleur" modèle retenu
# est le même ? (cf Question 6a)

## Idem avec le meilleur (nbest=1)
rech.ex1<-regsubsets(prog~.,data=diabetes1,nvmax=11,nbest=1,intercept=TRUE,method="exhaustive")

## 6a.  Visualisation des meilleurs modeles (1 par dimension), classes selon la valeur du R^2
plot(rech.ex1,scale="r2")
## Autres options possibles pour 'scale': adjr2", "Cp" et "BIC"

sum.rech.ex1 <- summary(rech.ex1)
str(sum.rech.ex1)## permet de visualiser la structure de l'objet sum.rech.ex2

nb.param1<-as.numeric(rownames(sum.rech.ex1$which))## nombre de parametres de la liste des modeles choisis
sum.rech.ex1$rss  ## valeur de la SCR pour la liste des modeles choisis
sum.rech.ex1$cp   ## valeur du critere Cp pour la liste des modeles choisis
sum.rech.ex1$bic  ## valeur du critere BIC pour la liste des modeles choisis
sum.rech.ex1$adjr2## valeur du R_a^2 pour la liste des modeles choisis


# A compléter

## 6b. Le resultat de la commande summary contient des informations interessantes

## Idem avec les 500 meilleurs (nbest=500)
## l'option "really.big=TRUE" precise qu'il y a beaucoup (trop) de modeles pour les enumerer simplement
rech.ex500<-regsubsets(prog~.,data=diabetes1,nvmax=11,nbest=500,intercept=TRUE,method="exhaustive",really.big=TRUE)

# Quelques plots intéressants
plot(nb.param1,log(sum.rech.ex1$rss),type="p",pch=20, col="gray",cex=0.7,xlab="Nombre de parametres",ylab="log(SCR)")
# A compléter...

## 6c.

#### Quelques commandes qui peuvent être utiles
## Et on peut acceder aux coefficients de n'importe lequel des modeles de la liste (icie, le 5e)
coef(rech.ex1,5) 
## On peut aussi visualiser les covariables retenues dans un modele de la liste (ici, le 7eme)
sum.rech.ex1$which[7,]
## Et l'utiliser pour ajuster ce modele
modele<-lm(prog~.,data=diabetes1[,sum.rech.ex1$which[7,]])

#### Retour à la question
## Quel est le modèle sélectionné si on choisit comme critère le R^2 ajusté ?
indRa2<-which.max(sum.rech.ex1$adjr2)
modele.Ra2<-lm(prog~., data=diabetes1[,sum.rech.ex1$which[indRa2,]])
modele.Ra2$rank
summary(modele.Ra2)
plot(modele.Ra2)

# A compléter


##### REMARQUE
##### la recherche exhaustive peut etre très lourde car le nombre
##### de modèles possible est potentiellement grand: ici 2^10 = 1024
##### modèles possibles. C'est pour cela que dès que le nombre de
##### variables devient important, une recherce exhaustive n'est pas
##### raisonnable. Dans ce cas, on utilise d'autres algorithmes.
##### Une possibilité est d'utiliser des algorithmes "greedy",
##### dont quelques exemples sont donnés dans les questions suivantes.

###### Question 7: 

## Pour retirer une variable dans le modele complet,
## on peut utiliser une formule comme celle-ci:
lm(prog~.-ser3,data=diabetes1)


###### Question 8: recherche stepwise

## Premiere methode: avec regsubsets
rech.forward<-regsubsets(prog~.,data=diabetes1,method="forward",nvmax=11,nbest=1,intercept=TRUE)


###### Question 9: erreur de prevision

diabetes2 <- read.table(file="diabetes2.txt")

y.test<-diabetes2$prog

## Calcul de l'erreur de prevision moyenne avec le modele "nul"
err.nul<-mean((y.test - predict(nul, diabetes2))^2)
