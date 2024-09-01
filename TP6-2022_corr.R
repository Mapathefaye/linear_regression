### TP6 - Selection de variables
### Code R pour commencer

#### Question 1
diabetes1 <- read.table(file="diabetes1.txt")
head(diabetes1)
summary(diabetes1)

## Distribution de la variable d'intérêt
par(mfrow=c(1,3))
plot(ecdf(diabetes1$prog),main="Fonction de repartition",xlab="")
hist(diabetes1$prog,main="Histogramme",xlab="")
plot(density(diabetes1$prog),main="Estimation de densite",xlab="")

## Graph pair à pair
## On enlevèe la variable "sex" qui est qualitative
pairs(prog~.,data=diabetes1[,c(-3)])

## Un peu difficile à lire, on peut sélectionner un sous ensemble
pairs(prog~.,data=diabetes1[,c(1,6:11)])

## Etude des corrélations
heatmap(abs(cor(diabetes1[,c(-3)])))
round(cor(diabetes1[,c(-3)]),2)

# Question 3
# Analyse du lien entre sex et les autres variables
par(mfrow=c(1,1))
boxplot(age~sex,data=diabetes1,xlab="sex",ylab="age")
boxplot(bmi~sex,data=diabetes1,xlab="sex",ylab="bmi")
boxplot(map~sex,data=diabetes1,xlab="sex",ylab="map")
boxplot(ser1~sex,data=diabetes1,xlab="sex",ylab="ser1")
boxplot(ser2~sex,data=diabetes1,xlab="sex",ylab="ser2")
boxplot(ser3~sex,data=diabetes1,xlab="sex",ylab="ser3")
boxplot(ser4~sex,data=diabetes1,xlab="sex",ylab="ser4")
boxplot(ser5~sex,data=diabetes1,xlab="sex",ylab="ser5")
boxplot(ser6~sex,data=diabetes1,xlab="sex",ylab="ser6")

## On observe quelques différences entre les
## deux groupes, toutefois il ne semble pas que ces
## différences soient très importantes, ce qui suggère
## d'utiliser le modèle proposé... Toutefois, on pourrait
## se poser la question de savoir si c'est vraiment pertinent
## que ser2 (par exemple) ne dépende pas du sexe: ie
## peut-être qu'il faudrait tenir compte des interactions
## entre les covariables... cela peut s'étudier.
##
## En d'autres termes: il semble satisfaisant de considérer
## que les coefficients correspondants aux variables
## age,bmi,map,ser1-6 ne soient les mêmes pour les deux
## sexes, mais cela pourrait mériter une analyse plus fine.
## En particulier on pourrait considérer un modèle où
## ces coeffs dépendent du sexe pour voir ce qu'il se passe
## (modèle avec interactions).

# Question 5
complet<- lm(prog~.,data=diabetes1)
nul<- lm(prog~1,data=diabetes1)
anova(nul,complet)

## Summary du modèle complet
## On remarque qu'il n'y a qu'un seul coefficient pour "sex" car
## on sait que le modèle est surparamétré, il faut donc que soit
## \mu_F = 0 ou \mu_M = 0...
## On voit que le niveau de référence choisit par R ici est F
## c'est à dire que par default \mu_F = 0
summary(complet)

## Si ce choix ne nous plait pas, on peut changer
## Ici on utilise M comme facteur de référence (donc \mu_M = 0)
factor(diabetes1$sex) # Pour info
diabetes1$sex<-factor(diabetes1$sex,levels=c("M","F"))
factor(diabetes1$sex) # Pour info

# On recommence
complet<- lm(prog~.,data=diabetes1)
summary(complet)

## L'idée ici c'est qu'on compare 2 groupes entre eux (M versus F)
## et on doit donc choisir un groupe de référence.

## On n'oublie pas les diagnostiques
par(mfrow=c(2,2))
plot(complet,which=c(1,2,4,5))

# Un shapiro au passage
shapiro.test(complet$residuals)
# On conserve H0 au risque d'erreur de première espèce 5%

####################

### Question 6: recherche exhaustive
# Eventuellement si pas déjà installé:
# install.packages("leaps")
library(leaps)


## meilleur modèles a p paramètres (nbest=1)
rech.ex1<-regsubsets(prog~.,data=diabetes1,nvmax=11,nbest=1,intercept=TRUE,method="exhaustive")


## 6a.  Visualisation des meilleurs modeles (1 par dimension), classes selon la valeur du R^2
par(mfrow=c(1,1))
plot(rech.ex1,scale="r2")
plot(rech.ex1,scale="adjr2")
plot(rech.ex1,scale="Cp")
plot(rech.ex1,scale="bic")

summary(rech.ex1)

## 6b. Le resultat de la commande summary contient des informations interessantes

## 500 meilleurs modèles à p paramètres (nbest=500)
## l'option "really.big=TRUE" precise qu'il y a beaucoup (trop) de modeles pour les enumerer simplement
rech.ex500<-regsubsets(prog~.,data=diabetes1,nvmax=11,nbest=500,intercept=TRUE,method="exhaustive",really.big=TRUE)

## Quelques plots intéressants

# SCR
# Chaque point correspond à un modèle (il y en a 1024 possibles ici)
# Pour mieux visualiser, on entoure en rouge le meilleur modèle
# pour chaque nombre de paramètre
nb.param1<-as.numeric(rownames(summary(rech.ex1)$which))## nombre de parametres de la liste des modeles choisis
nb.param500<-as.numeric(rownames(summary(rech.ex500)$which))## nombre de parametres de la liste des modeles choisis
plot(nb.param500,log(summary(rech.ex500)$rss),type="p",pch=20, col="gray",cex=0.7,xlab="Nombre de parametres",ylab="log(SCR)")
points(nb.param1,log(summary(rech.ex1)$rss),pch=1,col="red")

# Pour le SCR: on remarque un gain assez important on considérant 2
# variables au lieu de 1, puis une très légère augmentation jusqu'a
# 6 ou 7 variables, puis quasiment plus rien.
#
# En revanche, le SCR ne peux que diminuer lorsqu'on ajoute des
# variables, ce n'est donc pas un très bon critère pour choisir
# le nombre de variables le plus adapté.

# R2: essentiellement la même chose que SCR
# On ignore donc ici

# On passe donc au R^2 ajusté
plot(nb.param500,summary(rech.ex500)$adjr2,type="p",pch=20, col="gray",cex=0.7,xlab="Nombre de parametres",ylab="AdjR2")
points(nb.param1,summary(rech.ex1)$adjr2,pch=1,col="red")
# cbind(nb.param1,summary(rech.ex1)$adjr2)

# Cp
plot(nb.param500,summary(rech.ex500)$cp,type="p",pch=20, col="gray",cex=0.7,xlab="Nombre de parametres",ylab="Cp")
points(nb.param1,summary(rech.ex1)$cp,pch=1,col="red")
# cbind(nb.param1,summary(rech.ex1)$cp)

# Bic
plot(nb.param500,summary(rech.ex500)$bic,type="p",pch=20, col="gray",cex=0.7,xlab="Nombre de parametres",ylab="BIC")
points(nb.param1,summary(rech.ex1)$bic,pch=1,col="red")
cbind(nb.param1,summary(rech.ex1)$bic)

# Concentrons nous sur les "meilleurs modèles" de chaque dimension
# Pour les 3 critères (R^2 ajusté, Cp, BIC):
# - On gagne a ajouter des variables jusqu'a environ D=6
# - Les 3 critères deviennent moins bons au dela de D > 6
# - R^2 ajusté est maximal lorsque D = 6 (presque égal si D=7)
# - Cp et BIC sont minimal pour D = 6
# Donc dans cet exemple, peu importe le critère choisi on
# sélectionne un modèle avec 6 variables explicatives.
# Précisément on choisit le modèle:
indRa2<-which.max(summary(rech.ex1)$adjr2)
names(which(summary(rech.ex1)$which[indRa2,]==TRUE))
summary(rech.ex1)$which[indRa2,]

# Pour être complet:
# On note une différence entre les courbes pour le critère BIC et Cp,
# pour des valeurs de $D>7$, le critère BIC remonte plus vite que le
# critère Cp. Ceci illustre le fait que BIC "pénalise" plus fortement
# que Cp les modèeles de grande dimension.
# (cf. la comparaison entre critères vue en cours).

## 6c.
# On a déjà partiellement répondu puisque tous les critères retiennent
# le meme modèle, qu'on a déjà affiché.
#
# On peut ajuster ce modèle pour être complet.
modele.Ra2<-lm(prog~., data=diabetes1[,summary(rech.ex1)$which[indRa2,]])
modele.Ra2$rank
summary(modele.Ra2)
plot(modele.Ra2)
shapiro.test(modele.Ra2$residuals)

# Exactement la meme chose, car les deux critères ont sélectionné le
# meme modèle... mais ce n'est pas toujours le cas!
indcp<-which.min(summary(rech.ex1)$cp)
modele.cp<-lm(prog~.,data=diabetes1[,summary(rech.ex1)$which[indcp,]])
sum(summary(rech.ex1)$which[indcp,])# nb de param


##### REMARQUE
##### la recherche exhaustive peut etre très lourde car le nombre
##### de modèles possible est potentiellement grand: ici 2^10 = 1024
##### modèles possibles. C'est pour cela que dès que le nombre de
##### variables devient important, une recherce exhaustive n'est pas
##### raisonnable. Dans ce cas, on utilise d'autres algorithmes.
##### Une possibilité est d'utiliser des algorithmes "greedy",
##### dont quelques exemples sont donnés dans les questions suivantes.

###### Question 7: 

# On part du modèle complet
summary(complet)
# La p-value la plus élevée est pour le coefficient correspondant a ser3
# On ajuste donc le modèle sans cette variable
summary(lm(prog~.-ser3,data=diabetes1))
# La p-value la plus élevée est pour le coefficient correspondant a ser6
summary(lm(prog~.-ser3-ser6,data=diabetes1))
# La p-value la plus élevée est pour le coefficient correspondant a age
summary(lm(prog~.-ser3-ser6-age,data=diabetes1))
# La p-value la plus élevée est pour le coefficient correspondant a ser1
summary(lm(prog~.-ser3-ser6-age-ser1,data=diabetes1))

## A ce stade toutes les p-values sont << 0.05, on conserve donc ce modele
## C'est à dire le modèle avec sexF, bmi,map, ser2,ser4,ser5
## On observe que c'est le même modèle qu'avec la recherche exhasutive
## L'algorithme a donc permis de trouver le meilleur modèle.
## Ceci dit, il n'y a aucune garantie que ce soit toujours le cas.
## En revanche: c'est beaucoup moins couteux, surtout quand le nombre
## de covariables est grand.


###### Question 8: recherche stepwise

## Premiere methode: avec regsubsets
rech.forward<-regsubsets(prog~.,data=diabetes1,method="forward",nvmax=11,nbest=1,intercept=TRUE)
summary(rech.forward) # Intéressant: pas le meme modèle qu'avec backward.

plot(rech.forward,scale="adjr2",main="forward")
plot(rech.ex1,scale="adjr2",main="exhaustif")

summary(rech.forward)
summary(rech.ex1)

##
## On observe que la sélection forward et la recherche exhaustive
## ne sélectionnent pas le même modèle pour certaines valeurs de D. 
##
## Par construction, la sélection forward ne fait qu'ajouter des
## variables (on ne peut jamais en retirer).
##
## A l'inverse, avec la recherche exhaustive, certaines variables
## sont ajoutées puis retirées (ser3 et ser2).
##
## Pour D=6, la différence est importante: 
## ser2 et ser4 (obtenues par recherche exhaustive) sont remplacées
## par ser1 et ser3.
## C'est l'une des conséquences des corrélations entre covariables.
##
## On peut comparer les deux modèles
summary(lm(prog~., data=diabetes1[,summary(rech.ex1)$which[6,]]))
summary(lm(prog~., data=diabetes1[,summary(rech.forward)$which[6,]]))
## On constate qu'on perd un peu en term de R^2 ajusté
## avec le modèle sélectionné par méthode forward.
##

# Modèles sélectionnées pour chaque critère
par(mfrow=c(2,2))
plot(rech.forward,scale="r2",main="forward")
plot(rech.forward,scale="Cp",main="forward")
plot(rech.forward,scale="bic",main="forward")
plot(rech.forward,scale="adjr2",main="forward")

##
## Enfin, avec la recherche forward (idem backward), le modèle
## choisi dépend du critère sélectionné (car a chaque étape on ne peut
## que ajouter, ou bien enlever des variables). Par exemple ici,
modele.forward.cp<-lm(prog~.,data=diabetes1[,summary(rech.forward)$which[which.min(summary(rech.forward)$cp),]])
summary(modele.forward.cp)
# cp sélectionne un modèle avec 7 covariables + intercept.
modele.forward.bic<-lm(prog~.,data=diabetes1[,summary(rech.forward)$which[which.min(summary(rech.forward)$bic),]])
summary(modele.forward.bic)
## bic sélectionne un modèle avec 5 covariables + intercept.

###### Question 9: erreur de prevision

diabetes2 <- read.table(file="diabetes2.txt")
y.test<-diabetes2$prog

## Calcul de l'erreur de prevision moyenne avec le modele "nul"
err.nul<-mean((y.test - predict(nul, diabetes2))^2)

## Les autres modèles considérés.
## On sait que la recherche exhaustive a sélectionné le même modèle
## quelque soit le critère choisit, on ne regarde ici donc qu'un seul de ces
## modèles. On regarde aussi les modèles obtenues par recherche forward
## selon les critères Cp et BIC.
err.complet<-mean((y.test - predict(complet, diabetes2))^2)
err.Ra2<-mean((y.test - predict(modele.Ra2, diabetes2))^2)
err.forward.bic<-mean((y.test - predict(modele.forward.bic, diabetes2))^2)
err.forward.cp<-mean((y.test - predict(modele.forward.cp, diabetes2))^2)

resq6 <- data.frame(modele=c("nul","complet","R_a^2","Forward BIC","Forward Cp"), 
                    erreur=c(err.nul,err.complet,err.Ra2,err.forward.bic,err.forward.cp))
print(resq6)

## Etonnamment, on obtient une erreur de prévision largement plus faible
## avec la recherche forward qu'avec la recherche exhaustive. 
##
## Ce n'est absolument pas systématique, mais cela peut arriver: 
## les critèreres Ra^2, Cp, BIC (sur les données d'entrainement, diabetes1) 
## ne sont pas parfaits et ne fournissent pas toujours le meilleur modèle 
## (au sens de l'erreur de prévision).
##
## Ceci montre l'intérêt de disposer de données test (diabetes2) 
## sur lesquelles on peut estimer l'erreur de prévision.
##
## Si le but est la prévision: alors il pourrait être une bonne idée
## de faire une recherche (exhaustive, forward ou backward) en utilisant
## comme critère de sélection l'erreur de prévision sur les données tests.
##
## Aussi, il n'y a aucune raison que le modèle a D variables le mieux au
## sens d'un critère Cp, Ra^2, BIC, etc.. soit le meilleur modèle pour la
## prédiction... Il est donc important de savoir ce que l'on veut faire :)
