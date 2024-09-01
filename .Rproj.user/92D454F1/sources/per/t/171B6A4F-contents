# Preliminaries
setwd("~/Work/Teaching/M1BIBS/2019-2020/TPS/TP4")

# Q1: Import data
###############################################################################
donnees <- read.table(file="glaces.txt",sep=";",header=TRUE,dec=".")

# Q2:
# First things first:
# Have a look at the data (not as easy as simple linear regression!)
###############################################################################

# First preview, basic
head(donnees)
summary(donnees)

# The interesting variable is conso, so we shall look at its distribution
par(mfrow=c(1,3))
plot(ecdf(donnees$Conso),main="Fonction de repartition",xlab="")
hist(donnees$Conso,main="Histogramme",xlab="")
plot(density(donnees$Conso),main="Estimation de densite",xlab="")

# Show the data plotted as pairs
pairs(donnees)
pairs(Conso~Periode+Prix,data=donnees) # pour visualiser seulement certaines covariables

# Correlation matrix
cor(donnees)
heatmap(abs(cor(donnees)))

# Consumption as function of the temperature (intuitively important factor)
# Does it look linear ?
par(mfrow=c(1,1))
plot(Conso~Temp,data=donnees)

# Temperature as function of time
# Are they correlated ?
# Is it linear ? Do we care ?
plot(Temp~Periode,data=donnees)

# Wage as function of time
# Are they correlated ?
plot(Salaire~Periode,data=donnees)

# Q3
# Simpler linear regression
# Explain the icecream consumption as function of temprature only
###############################################################################
regT <- lm(Conso~Temp,data=donnees)
summary(regT)

summary(regT)$coefficients # How do you interpret this ? is there a link
summary(regT)$r.squared # fraction of consumption explained by temperature

# Trend lines + confidence intervals + prediction intervals
plot(donnees$Temp,donnees$Conso,xlab="Temp",ylab="Consommation")
grille <- seq(min(donnees$Temp),max(donnees$Temp),length=100)
grille.df <- data.frame(grille)
dimnames(grille.df)[[2]] <- "Temp"
ICconf <- predict(regT,new=grille.df,interval="confidence",level=0.95)
ICprev <- predict(regT,new=grille.df,interval="prediction",level=0.95)
matlines(grille,cbind(ICconf,ICprev[,-1]),lty=c(1,2,2,3,3),col=1)
legend("topleft",lty=1:3,c("droite de regression","intervalles de confiance pour E[y]", "intervalles de prevision"))


# Q4
# Simple linear regression
# Consumption versus price
###############################################################################
regPrix <- lm(Conso~Prix,data=donnees)
summary(regPrix)

# Trend lines
par(mfrow=c(1,1))
variable<-donnees$Prix
reg<-regPrix
nomvariable<-"Prix"
plot(variable,donnees$Conso,xlab=nomvariable,ylab="Consommation")
grille <- seq(min(variable),max(variable),length=100)
grille.df <- data.frame(grille)
dimnames(grille.df)[[2]] <- nomvariable
droitereg <- predict(reg,new=grille.df,interval="none")
lines(grille,droitereg,col="red")

# Do a test to conclude something ?
summary(regPrix)$coefficients
# We can see that the price does not affect the consumption
# Maybe one explanation: the salaries increase over time, so the price of the
# ice creams is not a good indicator (does it increase faster than the salaries ??)

# Bonus: pouvoir d'achat!
donnees$PouvoirAchat<-donnees$Salaire/donnees$Prix
regPA <- lm(Conso~PouvoirAchat,data=donnees)
summary(regPA) # Not relevant either...

par(mfrow=c(1,1))
variable<-donnees$PouvoirAchat
reg<-regPA
nomvariable<-"PouvoirAchat"
plot(variable,donnees$Conso,xlab=nomvariable,ylab="Consommation")
grille <- seq(min(variable),max(variable),length=100)
grille.df <- data.frame(grille)
dimnames(grille.df)[[2]] <- nomvariable
droitereg <- predict(reg,new=grille.df,interval="none")
lines(grille,droitereg,col="red")

# Q5
# Which variable explains the best the ice cream consumption ?
###############################################################################

# In term of period ?
regPer <- lm(Conso~Periode,data=donnees)
summary(regPer) # bof..

# In term of salary ?
regS <- lm(Conso~Salaire,data=donnees)
summary(regS) # bof...

# We can compare all the R^2...
# This shows that the temprature is by far the most realistic model
cat("\nR^2 (Temp):   ",summary(regT)$r.squared,
    "\nR^2 (Prix):   ",summary(regPrix)$r.squared,
    "\nR^2 (Periode):",summary(regPer)$r.squared,
    "\nR^2 (Salaire):",summary(regS)$r.squared)


# Q6
# Cf blackboard.
###############################################################################

# Q7
###############################################################################
# ajustement d'un modele
regComplet <- lm(Conso~.,data=donnees)
regComplet <- lm(Conso~Periode+Prix+Salaire+Temp,data=donnees)# autre option
summary(regComplet)

# modele sans la constante
regCompletSansCte <- lm(Conso~Periode+Prix+Salaire+Temp-1,data=donnees)
summary(regCompletSansCte)

# Test to choose between the two
# See Next lessons
anova(regCompletSansCte,regComplet)

# Might also want to compare the model with intercept + temp only
# versus complete model ?
anova(regT,regComplet)

# Confidence intervals for the temperature
confint(regComplet,"Temp",level=0.95)


# Q8
###############################################################################


cat("\nR^2 (Complet):",summary(regComplet)$r.squared,
    "\nR^2 (Temp):   ",summary(regT)$r.squared,
    "\nR^2 (Prix):   ",summary(regPrix)$r.squared,
    "\nR^2 (Periode):",summary(regPer)$r.squared,
    "\nR^2 (Salaire):",summary(regS)$r.squared)

# Indeed, the R^2 is better for the complete model, but it uses more
# variables, so it is not relevant to compare R^2...

cat("\nRa^2 (Complet):",summary(regComplet)$adj.r.squared,
    "\nRa^2 (Temp):   ",summary(regT)$adj.r.squared,
    "\nRa^2 (Prix):   ",summary(regPrix)$adj.r.squared,
    "\nRa^2 (Periode):",summary(regPer)$adj.r.squared,
    "\nRa^2 (Salaire):",summary(regS)$adj.r.squared)

# To fully anser this question we need to do tests
# see the anova() command for instance...
# This will be done in next lessons!
#
# Note that we shall also consider intermediary model too (with only some of the covariates)