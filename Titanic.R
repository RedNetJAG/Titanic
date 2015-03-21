rm(list=ls())
Titanic<-read.csv('Titanic.csv', header=TRUE)
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
summary(Fare)
sd(Fare)
boxplot(Titanic$Fare) ##Hay demasiados valores atipicos que quizás convendría eliminar
kurtosis(Fare) ##leptocurtica
#Meteremos las tarifas en tramos
Titanic$Fare2 <- '30+'
Titanic$Fare2[Titanic$Fare < 30 & Titanic$Fare >= 20] <- '20-30'
Titanic$Fare2[Titanic$Fare < 20 & Titanic$Fare >= 10] <- '10-20'
Titanic$Fare2[Titanic$Fare < 10] <- '<10'
table(Fare2)
aggregate(Survived ~ Fare2 + Sex, data=Titanic, FUN=function(x) {sum(x)/length(x)})
#El 86% de las mujeres y el 33% de los hombres que mas pagaron, sobrevivieron
#Los que menos pagaron, son los que tienen menores cifras de supervivencia
aggregate(Survived ~ Fare2 + Pclass + Sex, data=Titanic, FUN=function(x) {sum(x)/length(x)})

chisq.test(Pclass, Fare, simulate.p.value = TRUE)
qchisq(0.95,1)
cor(Pclass, as.numeric(Fare))
cor(SibSp, Parch)
chisq.test(SibSp, Parch, simulate.p.value = TRUE)
chisq.test(SibSp, Parch, simulate.p.value = TRUE)


## 20/03/15
######################################################################
## Var Dependiente - Survived
## Var Independientes - Pclass, Sex, Age, SibSp, Parch, Fare, Embarked
nob <- dim(Titanic)
nobs <- nob[1]
nobs

## Analisis de las variables

## Cualitativa Sex
## Frec absolutas y relativas
table(Sex)
table(Sex)/nobs
cumsum(table(Sex))
cumsum(table(Sex))/nobs

## Cuantitativas
## Edad en años
summary(Age) #media 29,7, hay 177 NA´s
sd(Age, na.rm=TRUE)
q1 <- quantile(Age, 0.25, na.rm=TRUE)
q3 <- quantile(Age, 0.75, na.rm=TRUE)
#Calculo el rango interquantilico
RIC <- q3-q1
edad<-subset(Age, Age>65) #Son sólo 8 casos, no es significativo
edad<-subset(cbind(Age, Survived), Age>0) # para descartar NAs
cor(edad)
###########CORRELACIÓN Age/Survived -0.07 BAJISIMA

summary(SibSp)
table(SibSp)
boxplot(SibSp)
########### SibSp >2 son el 5%

summary(Parch)
Mayor <- subset (cbind(SibSp, Parch), SibSp>=3)
table(Mayor)
cor(Mayor)
###########CORRELACIÓN SibSp/Parch -0.4177 BAJA


summary(Fare)
q3 <- quantile(Fare, 0.75)
q1 <- quantile(Fare, 0.25)
RIC <- q3-q1
bigote <-q3+1.5*RIC #valores del bigote de boxplot, al sumar, es el bigote superior
## el bigote es a partir de 65.64, por lo que nos quedamos con lo valores mayores de 
## de 66
f <- subset(Fare, Fare>66)
summary(f)
length(f)/length(Fare) # Son el 13% de los valores los que están por encima del bigote
boxplot(f)
## Fare <=66 y >66


table(Embarked) # 2 casos sin dato
pie(table(Embarked))
table(Pclass)
table(Fare,Pclass)
tapply(Fare, Pclass, mean)
cor(Fare, Pclass) #Correlac de -0.5494 IMPORTANTE


cor(Survived, as.numeric(Sex)) # -0.5433 salen negativas porque son inversas

## Sustituimos los valores NA de Age, por la media de la variable
Age[is.na(Age)] <- mean(Age, na.rm=TRUE)
## Creamos una matriz con las vars que nos interesan, pasando Text a numeric
Sex2 <- as.numeric(Sex)
Embarked2 <- as.numeric(Embarked)
datos <- cbind(Survived, Pclass, Sex2, Age, SibSp, Parch, Fare, Embarked2)
head(datos)
## Hacemos la matriz de correlaciones
cor(datos)
## Las mayores cor de Survived son con Sex (-0.5433) y Pclass (-0.3384)
## No son muy altas, observamos otras posibles vars dependientes
## Pclass con Fare (-0.5494) y Parch con SibSp (0.4148)
## seguimos tratando las vars por si lo mejoramos
datos <- as.data.frame(datos) #convertimos en dataframe para poder hacer el attach
detach(Titanic)
attach(datos)
head(datos)
## eliminamos los datos atipicos de Age
datosF <- subset(datos, Fare<=66)
head(datosF)
datosFa <- subset(datos, Fare>66)
cor(datosF)
## empeora la corr de Survived con Sex y Pclass, y Parch con SibSp
## mejora la de Pclass con Fare
cor(datosFa)
## Aparece una alta corr de Pclass con SibSp(0.8759)

## Hacemos la regresión logaritmica al tener var dicotomica (Survived)
regr<-glm(datosF$Survived ~ datosF$Pclass + datosF$Sex2 + datosF$Age + datosF$SibSp + datosF$Parch + datosF$Fare + datosF$Embarked2, family = binomial)
## distribución binomial por ser Survived dicotomica, 0 sucede, 1 no sucede
summary(regr)
## Hay q fijarse en los asteriscos, cuantos mas mejor, sube el nivel significativo
## los que no lo tienen no me valen, se descartan
## lanzamos de nuevo, quitando Embarked2 y Parch
regr<-glm(datosF$Survived ~ datosF$Pclass + datosF$Sex2 + datosF$Age + datosF$SibSp + datosF$Fare, family = binomial)
summary(regr)
fitted.values(regr) ##los q tengan un 50 o menos, se pone a valor 0, y los > a 1
fit1 <- fitted.values(regr) ## creamos vector con los valores
fit1[fit1 < 0.5] <- 0
fit1[fit1 >= 0.5] <- 1
## queremos comprobar si los valores de Survived predicha están a 0 y 1, 
## están bien o mal clasificados con respecto a la Survived observada
table(datosF$Survived, fit1)/length(datosF)
## 54.75% + 21.75% =  76.5% de los casos están bien clasif, predichos
## intentamos mejorar estos datos
cor(datosF)
##volvemos a hacer la regresión eliminando Parch
regrF<-glm(datosF$Survived ~ datosF$Pclass + datosF$Sex2 + datosF$Age + datosF$SibSp + datosF$Fare + datosF$Embarked2, family = binomial)
summary(regrF)
fitted.values(regrF) ##los q tengan un 50 o menos, se pone a valor 0, y los > a 1
fit1 <- fitted.values(regrF) ## creamos vector con los valores
fit1[fit1 < 0.5] <- 0
fit1[fit1 >= 0.5] <- 1
## queremos comprobar si los valores de Survived predicha están a 0 y 1, 
## están bien o mal clasificados con respecto a la Survived observada
table(datosF$Survived, fit1)/length(datosF)
cor(datosF)
