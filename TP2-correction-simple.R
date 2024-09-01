# Q1
set.seed(2)
n <- 100; beta1 <- -1; beta2 <- 3; sigma <-sqrt(2);
X <- runif(n,min=0,max=1)
Y <- beta1 + beta2*X + rnorm(n,mean=0,sd=sigma)

# Avant tout, on trace les données!
plot(X,Y)

# Q2 / Q3
reg <- lm(Y~X)
summary(reg)

# Q4
Nrep <- 200

betah1vect<-rep(NA,Nrep)
betah2vect<-rep(NA,Nrep)
sigmahvect<-rep(NA,Nrep)

for (i in 1:Nrep) {
  set.seed(i)
  X<-runif(n,min=0,max=1)
  Y<-beta2*X+beta1+rnorm(n,mean=0,sd=sigma)
  reg<-lm(Y~X)
  betah1vect[i]<-coefficients(reg)[["(Intercept)"]]
  betah2vect[i]<-coefficients(reg)[["X"]]
  sigmahvect[i]<-summary(reg)[["sigma"]]
}

## Using replicate
#library(dplyr)
#lms.out <- replicate(Nrep, lm(beta2*X+beta1+rnorm(n,mean=0,sd=sigma) ~ runif(n,min=0,max=1)))
#coeffs <- lapply(lms.out,coefficients)

# 4a)
summary(betah1vect)
var(betah1vect)

# 4b)
# On remarque le caractère aléatoire de l'estimateur, et aussi que sa variance
# n'est pas négligeable...
hist(betah1vect)

# 4c)
summary(betah2vect)
var(betah2vect)
hist(betah2vect)

summary(sigmahvect)
var(sigmahvect)
hist(sigmahvect)


# Remarque a propos de la Q4: ce n'est pas fait ici, mais
# c'est une bonne idée de comparer avec ce qui est attendu
# cf. la Proposition 6 du cours... On s'attend dans les conditions
# de la Proposition 6 a ce que la loi des estimateurs (beta) soit
# normale avec des paramètres connus.
# 
# Il serait bien de comparer en superposant la densité de la loi;
# encore mieux comparer la FDR empirique versus théorique..
#
# On a toutefois un problème ici: la simulation effectuée en Q4
# ne correspond pas au modèle du cours... ici on regenère les x_i
# a chaque fois (ie, les x sont random), alors que la Propsition 6
# suppose x deterministe, et fixé une bonne fois pour toute. La loi
# des estimateurs dans le cas de x aléatoire est un peu plus compliquée...
# On peut toutefois comparer à la loi Normale dont les paramètres
# sont les vrais beta, et prendre pour variance la variance empirique
# des beta (qui estime donc la "vraie" variance des estimateurs, dont
# l'expression est compliquée et hors programme).
#
# idem pour sigma en utilisant la loi du chi^2.
#
# La Q suivante est donc bien plus pertinente pour comprendre
# la Proposition 6 du cours, puisqu'elle simule exactement le modèle
# du cours (X fixé, Y aléatoire).

# Q5
# On remarquera ici l'importance de la question:
# Dans le cours, on suppose que les x_i sont déterministes, ce qui ne correspond
# pas au cadre étudié dans les questions précédentes du TP...
#
# La Q5 est alors le vrai cadre étudié en cours.
# On notera qu'il y a peu de différence, si ce n'est pour les variances observées
# (c'est normal, il y a moins d'aléa!)
X<-runif(n,min=0,max=1)
for (i in 1:Nrep) {
  set.seed(i)
  Y<-beta2*X+beta1+rnorm(n,mean=0,sd=sigma)
  reg<-lm(Y~X)
  betah1vect[i]<-coefficients(reg)[["(Intercept)"]]
  betah2vect[i]<-coefficients(reg)[["X"]]
  sigmahvect[i]<-summary(reg)[["sigma"]]
}

summary(betah1vect)
var(betah1vect)
hist(betah1vect)

summary(betah2vect)
var(betah2vect)
hist(betah2vect)

summary(sigmahvect)
var(sigmahvect)
hist(sigmahvect)
