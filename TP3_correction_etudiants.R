# Q1
###############################################################################
ozone <- read.table(file="ozone_simple.txt",sep=";", header=TRUE,dec=".")
reg <- lm(O3~T12,data=ozone)

plot(ozone$T12,ozone$O3)

# Q2
###############################################################################
n <- nrow(ozone)
betah1 <- coef(reg)[["(Intercept)"]]
betah2 <- coef(reg)[["T12"]]
sigmah <- summary(reg)$sigma

sigmahbetah1 <- sqrt(sigmah^2*sum(ozone$T12^2)/(n*sum((ozone$T12-mean(ozone$T12))^2)))
sigmahbetah2 <- sqrt(sigmah^2/sum((ozone$T12-mean(ozone$T12))^2))

# Avec les formules de la Lesson 2
X <- cbind(rep(1,n),ozone$T12)
var <- sigmah^2 * solve(t(X) %*% X)

var
c(sigmahbetah1^2,sigmahbetah2^2)

##
## IC pour beta1
## (qt donne les quantiles de la loi de Student)
betah1+sigmahbetah1*c(qt(0.025,df=n-2),qt(0.975,df=n-2))

## IC pour beta2
betah2+sigmahbetah2*c(qt(0.025,df=n-2),qt(0.975,df=n-2))
betah2+sigmahbetah2*c(-1*qt(0.975,df=n-2),qt(0.975,df=n-2))

## IC pour sigma
## (qchisq donne les quantiles de la loi du chi deux)
sqrt((n-2)*sigmah^2*c(1/qchisq(0.975,df=n-2),1/qchisq(0.025,df=n-2)))

# Q2: using confint
###############################################################################
confint(reg,level=0.95)

## Et pour extraire des valeurs: 
## IC pour beta2
confint(reg,level=0.95)["T12",]
confint(reg,level=0.95)["T12",1]
confint(reg,level=0.95)["T12",2]
## IC pour beta1
confint(reg,level=0.95)["(Intercept)",]

# Q3
###############################################################################
plot(O3~T12,data=ozone,ylim=c(30,140))
grilleT12 <- seq(min(ozone$T12),max(ozone$T12),length=100)
grilleT12.df <- data.frame(grilleT12)
dimnames(grilleT12.df)[[2]] <- "T12"

ICconf <- predict(reg,new=grilleT12.df,interval="confidence",level=0.95)
ICprev <- predict(reg,new=grilleT12.df,interval="prediction",level=0.95)

matlines(grilleT12,cbind(ICconf,ICprev[,-1]),lty=c(1,2,2,3,3),col=1)
legend("topleft",lty=1:3,c("droite de regression","intervalles de confiance pour E[y]", "intervalles de prevision"))

# Q4
###############################################################################
summary(reg)
# 1st line: H_0: \beta_1=0 versus H_1: \beta_1 \neq 0
# 2nd line: H_0: \beta_2=0 versus H_1: \beta_2 \neq 0

# Q5, ANOVA
###############################################################################
SCR <- sum(residuals(reg)^2)
SCM <- sum((fitted(reg)-mean(ozone$O3))^2)
SCT <- sum((ozone$O3-mean(ozone$O3))^2)
SCT-SCM-SCR # on verifie le Thme de Pythagore
(SCM/1) / (SCR/(n-2)) # statistique F
SCM/SCT # coefficient R^2

# We can compare with lm() et anova()
anova(reg)
summary(reg)


# Q6
###############################################################################

# Outliers ?
plot(residuals(reg))
abline(0,0,lty=1,col="blue")
lines(lowess(residuals(reg)),col="red")

# Studentized residuals
# Outliers are point above 2 or below -2 (roughly)
plot(rstudent(reg),xlab="index",ylab="residu studentise")
abline(h=c(-2,0,2),lty=c(2,1,2),col="blue")
lines(lowess(rstudent(reg)),col="red")

# Studentized residuals versus adjusted values
# We see that the model can be improved as we obsevre some tendancy in the residuals
plot(fitted(reg),rstudent(reg),xlab="valeur ajustee", ylab="residu studentise")
lines(lowess(rstudent(reg)~fitted(reg)),col="red")


# en fonction de T12
plot(ozone$T12,rstudent(reg),xlab="valeur ajustee", ylab="residu studentise")
lines(lowess(rstudent(reg)~ozone$T12),col="red")

# Homoscedasticity ?
plot(reg,which=3)

# Normality ?
plot(reg,which=2)
shapiro.test(residuals(reg))


# Q7
###############################################################################

# We can see that ideally it would be better to have two explanatory variables:
# - One variable for temperatures smaller than ~20--22 degrees
# - One variable for temperatures greater than ~20--22 degrees
plot(fitted(reg),rstudent(reg),xlab="valeur ajustee", ylab="residu studentise")
lines(lowess(rstudent(reg)~fitted(reg)),col="red")

# Better way to see the previous remark:
# Partial residuals :  ie plot residual_i + \beta_1 = f(Temperature_i)
# If the model is correct, this should be linear in the temperature
# We can see that below ~20-22 degrees, the temperature has no real effect on the pollution (not linear, and then linear for above 20 degrees)
residpart<-resid(reg,type="partial")
plot(ozone$T12,residpart[,"T12"],xlab="T12", ylab="residu partiel")
lines(lowess(residpart[,"T12"]~ozone$T12),col="red")

# The previous suggests to replace T12 by max(T12 -c, 0) for c between 20 and 22 (say c=21)
# We observe this is better (look at R^2 for instance)
ozone$T12transfo<-pmax(ozone$T12,21)
reg2<-lm(O3~T12transfo,data=ozone)
summary(reg2)

# Some plots
plot(ozone$T12,ozone$O3,xlab="T12",ylab="O3")
grilleT12 <- seq(min(ozone$T12),max(ozone$T12),length=100)
grilleT12transfo <- pmax(grilleT12,21)
grilleT12transfo.df <- data.frame(grilleT12transfo)
dimnames(grilleT12transfo.df)[[2]] <- "T12transfo"
droitereg <- predict(reg2,new=grilleT12transfo.df,interval="none")
lines(grilleT12,droitereg,col="red")

# Some CI
plot(ozone$T12,ozone$O3,xlab="T12",ylab="O3")
ICconf <- predict(reg2,new=grilleT12transfo.df,
                  interval="confidence",level=0.95)
ICprev <- predict(reg2,new=grilleT12transfo.df,
                  interval="prediction",level=0.95)
matlines(grilleT12,cbind(ICconf,ICprev[,-1]),
         lty=c(1,2,2,3,3),col=c(1,"red","red","blue","blue"))
legend("topleft",lty=1:3,col=c(1,"red","blue"),c("droite de regression",
                                                 "intervalles de confiance pour E[y]", "intervalles de prevision"))
