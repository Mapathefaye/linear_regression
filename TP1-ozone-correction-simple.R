# PARTIE 1: Analyse descriptive
###############################################################################

## Question 1: Importation des données
# Set working directory so that R looks at the right place for data file
setwd("~/Work/Teaching/M1BIBS/2023-2024/TPS/TP1")
# Load Ozone data
ozone <- read.table(file="ozone_simple.txt",sep=";",header=TRUE,dec=".")
n <- nrow(ozone)

## Question 2: Moyenne, mediane, variance
summary(ozone) # Quick way, but do not include variance
var(ozone$O3)  # variance of O3
var(ozone$T12) # variance of T12

# In case we don't want to use summary()
# I do only O3.
mean(ozone$O3)
median(ozone$O3)

## Question 3: Histogrammes
hist(ozone$O3)
hist(ozone$T12)

## Question 4: Nuage de points
plot(ozone$T12,ozone$O3,xlab="T12",ylab="O3")


## PARTIE 2: MOINDRES CARRES
###############################################################################

# Question 5:
# We model O3(i) = beta1 + beta2*T12(i) + eps_i

# Question 6a: Régression linéaire (calculs à la main)
hatbeta2 <- cov(ozone$O3,ozone$T12)/var(ozone$T12)
hatbeta1 <- mean(ozone$O3) - mean(ozone$T12)*hatbeta2
hatsigma <- sqrt(sum((ozone$O3-hatbeta1-hatbeta2*ozone$T12)^2)/(n-2))
abline(hatbeta1, hatbeta2) # Facultatif

var(ozone$T12)
sum((ozone$T12 - mean(ozone$T12))^2)/(n-1)

# Question 6b: Régression linéaire utilisant lm
reg <- lm(O3~T12,data=ozone)
summary(reg)

# Question 7: Tracer la droite de régression
# Exploite le polymorphisme
abline(reg)

# Question 8: Graph des résidus (à la main)
res <- ozone$O3-hatbeta1-hatbeta2*ozone$T12
plot(res)

# Question 8: Graph des résidus (pas à la main)
plot(residuals(reg))
abline(a=0,b=0,col="red") ## materialise la valeur 0

# On rappelle que les résidus sont une "estimation" du bruit
# et que donc si le modèle est correct, le graph des résidus doit
# ressembler à du "bruit"
#
# voici à quoi un "bruit" eps_1,...,eps_n réel ressemble quand on le trace
# tracé en fonction de l'indice de l'observation:
# (on peut executer la commande plusieurs fois pour obtenir
# plusieurs exemples, c'est aléatoire!)
plot(rnorm(mean=0,sd=20.5,n=50))


# Il est aprfois intéressant de tracer en fonction d'autre chose
# que l'indice de l'observation. Par exemple, on peut tracer en fonction
# de la covariable T12 pour vérifier l'hypothèse d'homoscedasticité:
# si l'hypothèse est vraie, le bruit ne dépend pas de T12, et cela devrait
# se voir sur les résidus.

# on trace les résidus en fonction de T12: on constatera qu'il y a une tendance
# et que donc les résidus semblent dépendre de T12. Cela peut arriver
# soit car le bruit est heteroscedastique, ou bien que le modèle est
# incomplet (il manque des variables explicatives), ou bien la relation
# entre O3 et T12 n'est pas exactement linéaire (ou bien tout ça à la fois, ce qui
# est le cas ici...)
plot(ozone$T12,residuals(reg))

# Quelques diagnostiques pour vérifier la normalité des résidus
qqnorm(residuals(reg)) # Facultatif
hist(res)

# On ira plus loin dans l'analyse des résidus lors des cours suivants!

## PARTIE 3: Questions subsidiaires
###############################################################################

# Question 9: Transformation de T12 pour meilleur explication
# CF TP3

# Question 10: 