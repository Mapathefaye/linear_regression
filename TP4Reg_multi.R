## Régression linéaire multiple (1) : premiers pas
####################################
# Analyse descriptive

## 1) importation 
glaces <- read.table(file="glaces.txt",header = T, sep = ";", dec = ".")

summary(glaces)

# histogramme pour visualiser la distribution des variables

par(mfrow=c(2,2))
hist(glaces$Conso, main="Consommation de glaces", xlab="Consommation (pintes par habitant)")
hist(glaces$Prix, main="Prix des glaces", xlab="Prix (dollars)")
hist(glaces$Salaire, main="Salaire hebdomadaire", xlab="Salaire (dollars)")
hist(glaces$Temp, main="Température", xlab="Température (Fahrenheit)")

# Boxplot pour avoir une vue d'ensemble des valeurs abérantes et 

par(mfrow=c(2,2))
boxplot(glaces$Conso, main="Consommation de glaces")
boxplot(glaces$Prix, main="Prix des glaces")
boxplot(glaces$Salaire, main="Salaire hebdomadaire")
boxplot(glaces$Temp, main="Température")

# Matrice de corrélation

correlation_matrix <- cor(glaces[, c("Conso", "Prix", "Salaire", "Temp")])
print(correlation_matrix)
heatmap(abs(cor(glaces[, c("Conso", "Prix", "Salaire", "Temp")])))

#######################################################################
#Etude de quelques modèles univariés
########################################################################

# 3) expliquons la consommation de glaces à l’aide de la température uniquement

reg_simple <- lm(glaces$Conso~glaces$Temp, data = glaces)
summary(reg_simple)
library(ggplot2)
ggplot(glaces,aes(x=Temp,y=Conso))+
  geom_point()+
  stat_smooth(method="lm",se=FALSE)+
  xlab("Temp")+
  ylab("Conso")
#Le coefficient associé à la température (glaces$Temp) est de 0.0031074. 
#Cela signifie qu'une augmentation d'une unité de température est associée
#à une augmentation de 0.0031074 unités de consommation de glaces.
#Le p-value très faible indique que cette relation est statistiquement significative donc slope significtativement différent de 0.
#Le coefficient de détermination R² ajusté est de 0.5874, ce qui signifie que 58.74 % de 
#la variabilité de la consommation de glaces peut être expliquée par la température.

# 4) Même question en remplaçant la température par le prix des glaces
reg_simple1 <- lm(glaces$Conso~glaces$Prix, data = glaces)
summary(reg_simple1)
ggplot(glaces,aes(x=Prix,y=Conso))+
  geom_point()+
  stat_smooth(method="lm",se=FALSE)+
  xlab("Prix")+
  ylab("Conso")

# 
