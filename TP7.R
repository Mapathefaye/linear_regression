# 1) chargement et analyse descriptive
load("donnees_liver.rda")
# on se limite sur les 5 premières colonnes pour la 1e visualisation des données
summary(liver[,c(1:5)])
head(liver[,c(1:5)])
# analyse descriptive des données
### Distribution de la variable d'intéret

par(mfrow=c(1,3))
plot(ecdf(liver$cholesterol),main="Fonction de repartition",xlab="")
hist(liver$cholesterol,main="Histogramme",xlab="")
plot(density(liver$cholesterol),main="Estimation de densite",xlab="")
# domaine de valeurs entre 4à et 100
# 

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
dim(cormat)  ## [1] 3117 3117
max(abs(cormat[1,-1])) ## [1] 0.7320148

## Variables les plus corrélées à la réponse
which.max(abs(cormat[1,-1]))  ## A_42_P631473 1157 
cormat[1,1158]
#colnames(liver)[1158]
which.max(abs(cormat[1,c(-1,-1158)])) ## A_42_P700288 1835  
cormat[1,1837]
#colnames(liver)[1837]

## Graphes vs. la réponse correspondants
par(mfrow=c(1,2))
plot(liver$cholesterol~liver[,1158])
plot(liver$cholesterol~liver[,1837])

# 2. Sélection forward
library(leaps)
rech.forward<-regsubsets(cholesterol~.,data=liver,method="forward",nvmax=6,nbest=1,intercept=TRUE)
summary(rech.forward)
plot(rech.forward,scale="adjr2",main="forward")
summary(rech.forward)
# 4) Tests multiples (quelles ont un lien avec la variable réponse)
# 4-a) H0: Bj = 0 contre H1: Bj != 0
# 4-b) alculer une p-valeur̂ pj pour chacun des tests ci-dessus (j ∈ {1, . . . , p})
nbcovar<-dim(liver)[2]-1
pvaleurs<-rep(0,nbcovar)

for (ivar in seq(1,nbcovar)) {
  pvaleurs[ivar]<-summary(lm(liver$cholesterol~liver[[ivar+1]]))$coefficients[2,4]
}

## ranger les p-valeurs par ordre croissant
pvaleurs.croiss<-matrix(sort(pvaleurs),nrow=1)
colnames(pvaleurs.croiss)<-colnames(liver[,-1])[order(pvaleurs)]

pvaleurs.croiss[,1:10]

#  Visualiser le résultat avec un ou deux graphiques.
plot(pvaleurs.croiss[1,],type="l",xlab="rang",ylab="p-valeurs ordonnees")
abline(0,1/nbcovar,col="blue")
abline(0.05,0,col="red")

# 4-c) On décide de rejeter H0,j pour tous les j tels quê pj ≤ α = 0.05. Combien de gènes
#a-t-on ainsi sélectionné ?
significant_genes <- sum(pvaleurs.croiss <= 0.05)
print(paste("Nombre de gènes sélectionnés :", significant_genes))
# [1] "Nombre de gènes sélectionnés : 1239"
# 4-d)  En moyenne, avec la procédure de sélection à un seuil de α=0.05, 
#nous sélectionnerions (à tort) environ 5% des gènes, même s'ils n'ont aucun lien avec la réponse,
# soit 0.05 * 3116 ([1] 155.8).
# 4-e) Correction de Bonferroni
Bonf_significant_genes <- sum(pvaleurs.croiss <= 0.05/nbcovar)
print(paste("Nombre de gènes sélectionnés :", Bonf_significant_genes))
colnames(liver[Bonf_significant_genes])
# [1] "Nombre de gènes sélectionnés : 220"
# la correction de Bonferroni donne moins de rejets de H0 donc moins de fausses découvertes

# 4-f) Correction de Benjamini-Yekutieli
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
