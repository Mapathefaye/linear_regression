library(ggplot2)
ozones <- read.table("Dataset_ozone.txt", header=TRUE, sep=";", dec=",")

## Aprés avoir retirer les paramètres qui ne sont pas significativement différents de 0, on obtient le modèle suivant

reg_multi <- lm(maxO3~T12+Ne9+maxO3v,data=ozones)
summary(reg_multi)
# On remarque qu'à présent, tous les paramètres sont significatifs.
## Quant au R2 , il vaut environ 0.75, tout comme le R2 ajusté. 
# On peut donc utiliser ce modèle à des fins de prévisions

# Quelques prévisions
a_prevoir <- data.frame(T12=15,Ne9=2,maxO3v=100)
maxO3_prev <- predict(reg_multi,a_prevoir)
round(maxO3_prev, digits=2)  #### maxO3_prev = 84.08

# ----------------------------------------------------------------
##  Pour aller plus loin analysons les résultats
# ----------------------------------------------------------------

#Nous allons ici réaliser les tests à un niveau α=5%

  
alpha <- 0.05

# Récupérons n le nombre d'individus de l'échantillon, et p le nombre de variables.

n <- dim(ozones)[1]

p <- 4

# Nous allons mener des analyses sur les valeurs atypiques 
# et/ou influentes en travaillant sur un dataframe appelé  analyses  .

analyses <- data.frame(obs=1:n)

# Calculez les leviers

# On peut calculer les leviers comme ceci, en sachant que le seuil des leviers est de 2∗pn

analyses$levier <- hat(model.matrix(reg_multi))

seuil_levier <- 2*p/n 

# On peut visualiser les leviers pour chaque point comme ceci :
  
  ggplot(data=analyses,aes(x=obs,y=levier))+    ## 8 observations dépassent le seuil
  
  geom_bar(stat="identity",fill="steelblue")+
  
  geom_hline(yintercept=seuil_levier,col="red")+
  
  theme_minimal()+
  
  xlab("Observation")+
  
  ylab("Leviers")+
  
  scale_x_continuous(breaks=seq(0,n,by=5))
#Pour sélectionner les points pour lesquels le levier est supérieur au seuil, on exécute ces 2 lignes :
    
idl <- analyses$levier>seuil_levier
analyses$levier[idl]  

## Calcul des résidus studentisés. 
# sachant que le seuil pour les résidus studentisés est une loi de Student à n-p-1 degrés de liberté

analyses$rstudent <- rstudent(reg_multi)
seuil_rstudent <- qt(1-alpha/2,n-p-1)
## Visualisation des résidus studentisés

ggplot(data=analyses,aes(x=obs,y=rstudent))+
  geom_bar(stat="identity",fill="steelblue")+
  geom_hline(yintercept=-seuil_rstudent,col="red")+
  geom_hline(yintercept=seuil_rstudent,col="red")+
  theme_minimal()+
  xlab("observation")+
  ylab("Résidus studentisés")+
  scale_x_continuous(breaks=seq(0,n,by=5))

## détermination de la distance de cook pour déterminer les observations influentes
influence <- influence.measures(reg_multi)
names(influence)
colnames(influence$infmat)
analyses$dcook <- influence$infmat[,"cook.d"]
seuil_dcook <- 4/(n-p)

ggplot(data=analyses,aes(x=obs,y=dcook))+
  geom_bar(stat="identity",fill="steelblue")+
  geom_hline(yintercept=seuil_dcook,col="red")+
  theme_minimal()+
  xlab("Observation")+
  ylab("Distance de cook")+
  scale_x_continuous(breaks=seq(0,n,by=5))

# Vérifions l'éventuelle colinéarité approchées des variables
vif(reg_multi)
#  T12      Ne9      maxO3v   # tous les coefficients sont inférieurs à 10,
                              #il n'y a donc pas de problème de colinéarité
# 2.067833 1.527773 1.474718 

## tester l’homoscédasticité (c'est-à-dire la constance de la variance) des résidus :

bptest(reg_multi) # La p-valeur ici n'est pas inférieure à 5 %, on ne rejette pas l'hypothèse d’homoscédasticité

## 