
### TP7: grande dimension, tests multiples

### 1. Chargement des donnees "liver"

load("donnees_liver.rda")

### Premiere visualisation des donnees: 
### on se limite aux 5 premieres variables

summary(liver[,c(1:5)])
head(liver[,c(1:5)])

### Distribution de la variable d'int\'er\^et

par(mfrow=c(1,3))
plot(ecdf(liver$cholesterol),main="Fonction de repartition",xlab="")
hist(liver$cholesterol,main="Histogramme",xlab="")
plot(density(liver$cholesterol),main="Estimation de densite",xlab="")

### Distribution de quelques covariables
par(mfrow=c(2,2))
hist(liver[,2],main="Histogramme",xlab="")
hist(liver[,3],main="Histogramme",xlab="")
hist(liver[,4],main="Histogramme",xlab="")
hist(liver[,5],main="Histogramme",xlab="")


### Valeurs moyennes des covariables
moyennes<-apply(liver[,-1],2,mean)
plot(moyennes,type="l",xlab="numero covariable",ylab="moyenne")


### Quelques graphes paire a paire
pairs(liver[,c(1:5)])

### Quelques correlations
cor(liver[,c(1:5)])
heatmap(abs(cor(liver[,c(1:5)])))

### Matrice de correlations complete (un peu grosse, ne pas la visualiser...)
cormat<-cor(liver)
dim(cormat)
max(abs(cormat[1,-1]))

## Variables les plus corrélées à la réponse
which.max(abs(cormat[1,-1]))
cormat[1,1158]
#colnames(liver)[1158]
which.max(abs(cormat[1,c(-1,-1158)]))
cormat[1,1837]
#colnames(liver)[1837]

## Graphes vs. la réponse correspondants
par(mfrow=c(1,2))
plot(liver$cholesterol~liver[,1158])
plot(liver$cholesterol~liver[,1837])


### 3. Tests multiples: quelles variables ont un lien avec la reponse? 

## 3b. Calcul des p-valeurs
nbcovar<-dim(liver)[2]-1
pvaleurs<-rep(0,nbcovar)

for (ivar in seq(1,nbcovar)) {
pvaleurs[ivar]<-summary(lm(liver$cholesterol~liver[[ivar+1]]))$coefficients[2,4]
}

## ranger les p-valeurs par ordre croissant
pvaleurs.croiss<-matrix(sort(pvaleurs),nrow=1)
colnames(pvaleurs.croiss)<-colnames(liver[,-1])[order(pvaleurs)]

pvaleurs.croiss[,1:10]

## visualisation (2)
plot(pvaleurs.croiss[1,],type="l",xlab="rang",ylab="p-valeurs ordonnees")
abline(0,1/nbcovar,col="blue")
abline(0.05,0,col="red")


## 3f. Correction de Benjamini-Yekutieli

H=sum(1/seq(1,nbcovar))
indicemax=max(seq(1,nbcovar)[pvaleurs.croiss[1,]<=(0.05/(nbcovar*H))*seq(1,nbcovar)])
indicemax# nb de rejets
seuilBY<-(0.05/(nbcovar*H))*indicemax# on rejette lorsque la p-valeur est sous ce seuil

plot(pvaleurs.croiss[1,1:1250],type="l",xlab="rang",ylab="p-valeurs ordonnees")
abline(0.05,0,col="red")# sans correction
abline(0,0.05/(nbcovar*H),col="blue")# correction Benjamini-Yekutieli
abline(0.05/nbcovar,0,col="green")# correction Bonferroni

# idem en zoomant un peu
plot(pvaleurs.croiss[1,1:750],type="l",xlab="rang",ylab="p-valeurs ordonnees")
abline(0.05,0,col="red")# sans correction
abline(0,0.05/(nbcovar*H),col="blue")# correction Benjamini-Yekutieli
abline(0.05/nbcovar,0,col="green")# correction Bonferroni
lines(pvaleurs.croiss[1,1:indicemax],lwd=2,col="blue")



