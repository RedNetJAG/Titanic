rm(list=ls())
Titanic<-read.csv('Titanic.csv')
attach(Titanic)
head(Titanic)
#Num de hombres y mujeres en el Titanic
summary(Sex)
table(Survived)
prop.table(table(Survived)) ##El 38% de los pasajeros sobrevivieron
#Proporción de supervivientes por genero
table(Sex) ##lo mismo que Summary(Sex)
table(Survived) ##Num de supervivientes
table (Sex, Survived) ##Supervivientes por sexo
prop.table(table(Sex, Survived))  ##Proporción de supervivientes según sexo
## El 52% de las victimas fueron hombres
prop.table(table(Sex, Survived), 1)  ##Proporción de supervivientes agrupado por sexo
## El 81% de los hombres y el 25% de las mujeres murieron

summary(Age)
mean(Age, na.rm=TRUE)
sd(Age, na.rm=TRUE)
edad <- Age
edad[is.na(edad)] <- 0
tapply(edad, Survived, mean) ##edad media de los supervivientes: 24 años
aggregate(edad ~ Survived + Sex, FUN = "mean") #edad media por genero y superv
cor(Survived, edad) #Correlación edad - superv
# Analizamos menores de 18 años
Titanic$Child <- 0
Titanic$Child[Age<18] <- 1
# Num de supervivientes por sexo y mayor o menor de edad:
aggregate(Survived ~ Child + Sex, data=Titanic, FUN = sum)
#Para sacar la proporción, tenemos que dividir la suma de supervivientes entre el total
aggregate(Survived ~ Child + Sex, data=Titanic, FUN=function(x) {sum(x)/length(x)})
# El 75% de las mujeres menores sobrevivieron y tan sólo el 16% de los hombres menores

## Variables de clase en que viajaban y precio que pagaron
table(Pclass)
#Meteremos las tarifas en tramos
Titanic$Fare2 <- '30+'
Titanic$Fare2[Titanic$Fare < 30 & Titanic$Fare >= 20] <- '20-30'
Titanic$Fare2[Titanic$Fare < 20 & Titanic$Fare >= 10] <- '10-20'
Titanic$Fare2[Titanic$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Sex, data=Titanic, FUN=function(x) {sum(x)/length(x)})
#El 86% de las mujeres y el 33% de los hombres que mas pagaron, sobrevivieron
#Los que menos pagaron, son los que tienen menores cifras dee supervivencia
aggregate(Survived ~ Fare2 + Pclass + Sex, data=Titanic, FUN=function(x) {sum(x)/length(x)})

chisq.test(Pclass, Fare, simulate.p.value = TRUE)
qchisq(0.95,1)
cor(Pclass, as.numeric(Fare))
cor(SibSp, Parch)
chisq.test(SibSp, Parch, simulate.p.value = TRUE)
chisq.test(SibSp, Parch, simulate.p.value = TRUE)



