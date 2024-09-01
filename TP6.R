# Question 1

data <- read.table(file="diabetes1.txt")

# graphe pair pair 
avisualiser<-c("prog","age","bmi","map","ser1","ser2","ser3","ser4","ser5","ser6")
pairs(prog~.,data=diabetes1[,avisualiser])
# commmentaire : prog semble etre lié à plusieurs variables. les variables ser1 et ser2 semblent 
#etre fortement corrélées. Il faudra faire attention si on les met ensemble de le modèle
# matrice de corrélation
cor(diabetes1[,avisualiser])
heatmap(abs(cor(diabetes1[,avisualiser])))

# 2) sex est une variable qualiative à deux niveaux
# 3) boxplot
par(mfrow=c(2,4))
boxplot(ser3~sex,data=diabetes1,xlab="sex",ylab="ser3")
boxplot(ser4~sex,data=diabetes1,xlab="sex",ylab="ser4")
boxplot(ser1~sex,data=diabetes1,xlab="sex",ylab="ser1")
boxplot(ser2~sex,data=diabetes1,xlab="sex",ylab="ser2")
boxplot(ser5~sex,data=diabetes1,xlab="sex",ylab="ser5")
boxplot(ser6~sex,data=diabetes1,xlab="sex",ylab="ser6")
boxplot(age~sex,data=diabetes1,xlab="sex",ylab="age")
boxplot(bmi~sex,data=diabetes1,xlab="sex",ylab="bmi")
boxplot(map~sex,data=diabetes1,xlab="sex",ylab="map")

# 5)
complet<- lm(prog~., data=diabetes1)
summary(complet)
# 6
rech.ex2<-regsubsets(prog~.,data=diabetes1,nvmax=11,nbest=2,intercept=TRUE,method="exhaustive")
summary(rech.ex2)
rech.ex1<-regsubsets(prog~.,data=diabetes1,nvmax=11,nbest=1,intercept=TRUE,method="exhaustive")
summary(rech.ex1)

# 7
complet<- lm(prog~age + sex + bmi + map + ser1 + ser2 +ser3 + ser4 + ser5 + ser6, data=data)
complet1<- lm(prog~age + sex + bmi + map + ser1 + ser2 + ser4 + ser5 + ser6, data=data)
anova(complet, complet1)
complet2<- lm(prog~age + sex + bmi + map + ser1 + ser4 + ser5 + ser6, data=data)
anova(complet, complet2)
complet3<- lm(prog~age + sex + bmi + map + ser1 + ser4 + ser5, data=data)
anova(complet, complet3)
complet4<- lm(prog~sex + bmi + map + ser1 + ser4 + ser5, data=data)
anova(complet, complet4)
