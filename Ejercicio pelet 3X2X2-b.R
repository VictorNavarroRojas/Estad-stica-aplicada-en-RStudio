
#__________________________ Factorial 3x2x2 ________________________

datos<-read.csv(file.choose(),header=T)
datos
getwd()
Pelet<-as.factor(datos$Pelet) 
Temperatura <- as.factor(datos$Temperatura)

Salinidad<-as.factor(datos$Salinidad)

#Definir el modelo lineal a utilizar, en nuestro caso un factorial de 3 factores (para ello se utilizan " * ")
#la variable tiempo es la variable de respuesta
m1<-lm(datos$Tiempo~Pelet*Temperatura*Salinidad)


#ANOVA
anova(m1)

#Tukey
TukeyHSD(aov(datos$Tiempo~Pelet*Temperatura*Salinidad), "Pelet:Salinidad")

# Medias
medias1 <- tapply(datos$Tiempo, Pelet:Salinidad, mean)
medias1
sort(medias1)

TukeyHSD(aov(datos$Tiempo~Pelet*Temperatura*Salinidad), "Temperatura")
medias2 <- tapply(datos$Tiempo, Temperatura, mean)
medias2


#Hacer gr醘ica de interacci髇 de dos factores
interaction.plot(Pelet,Salinidad,datos$Tiempo,type="l",main="Gr谩fica de interacci贸n",xlab="Pelet",ylab="tiempo",col=1:3)
interaction.plot(Salinidad,Pelet,datos$Tiempo,type="l",main="Gr谩fica de interacci贸n",xlab="Salinidad",ylab="tiempo",col=1:3)

#Las siguientes interacciones no resultaron significativas
interaction.plot(Pelet, Temperatura,datos$Tiempo, type="l",main ="Gr谩fica de interacci贸n ", xlab="Pelet",ylab="tiempo", col=2:4)
interaction.plot(Temperatura, Salinidad,datos$Tiempo, type="l",main ="Gr谩fica de interacci贸n ", xlab="Temperatura",ylab="tiempo", col=2:4)


#Gr醘ica de interacci髇 de tres factores 
#es necesario separar los datos para cada nivel de uno de los factores
datosSalinidad30<-subset(datos, Salinidad==30)
datosSalinidad30
datosSalinidad35<-subset(datos, Salinidad==35)
datosSalinidad35

#Volver a correr los modelos con cada uno de los niveles del factor seleccionado 
m2<-lm(datosSalinidad30$Tiempo~datosSalinidad30$Pelet*datosSalinidad30$Temperatura)

m3 <-lm(datosSalinidad35$Tiempo~datosSalinidad35$Pelet*datosSalinidad35$Temperatura)

par(mfrow=c(1,2))
interaction.plot(datosSalinidad30$Pelet, datosSalinidad30$Temperatura, datosSalinidad30$Tiempo, type="l",main ="Grafica de interaccion salinidad 30ppm ", xlab="Pelet",ylab="tiempo", col=2:4)

interaction.plot(datosSalinidad35$Pelet, datosSalinidad35$Temperatura, datosSalinidad35$Tiempo, type="l",main ="Grafica de interaccion salinidad 35ppm ", xlab="Pelet",ylab="tiempo", col=5:6)

par(mfrow=c(1,1))

install.packages("dae")
library(dae)
install.packages("ggplot2")
library(ggplot2)

#Gr醘ica de 3 factores = 3 niveles de pelet .: son tres gr醘icas
interaction.ABC.plot(Tiempo, x.factor = Salinidad, groups.factor = Temperatura, trace.factor = Pelet, data = datos, fun = "mean", title = "Gr谩fica de interacci贸n de tres factores", xlab = "Salinidad", ylab = "Tiempo", lwd = 4, columns = 2)
#esta es otra menera de graficar lo mismo que en la linea 63
interaction.ABC.plot(Tiempo, x.factor = Pelet, groups.factor = Temperatura, trace.factor = Salinidad, data = datos, fun = "mean", title = "Gr谩fica de interacci贸n de tres factores", xlab = "Pelet", ylab = "Tiempo", lwd = 4, columns = 2)

interaction.ABC.plot(Tiempo, x.factor = Temperatura, groups.factor = Salinidad, trace.factor = Pelet, data = datos, fun = "mean", title = "Gr谩fica de interacci贸n de tres factores", xlab = "Temperatura", ylab = "Tiempo", lwd = 4, columns = 2)

interaction.ABC.plot(Tiempo, x.factor = Pelet, groups.factor = Salinidad, trace.factor = Temperatura, data = datos, fun = "mean", title = "Gr谩fica de interacci贸n de tres factores", xlab = "Pelet", ylab = "Tiempo", lwd = 4, columns = 2)

# Residuos
residuos<-resid(m1)
residuos

residuos1<-m1$res
residuos1

# Valores estimados del modelo original
estimados<-m1$fitted.values
estimados

par(mfrow=c(1,1))

# Normalidad de los errores
qqnorm(m1$res)
qqline(m1$res, col="red")

#Histograma
hist(m1$res, prob=T,ylim= c(0,0.8), col="gray", xlim=c(-1.5,1.5))
x=m1$res
m<-mean(x)
s<-sd(x)
curve(dnorm(x,mean=m,sd=s), col="red", add=TRUE)

boxplot(m1$res, col="red")

shapiro.test(residuos)

#Homocedasticidad de varianza de los errores
plot(estimados,residuos)
abline(h=0,col="red")
bartlett.test(residuos,Pelet:Temperatura:Salinidad)


#Prueba de independencia e los errores
plot(residuos)
abline(h=0,col="red")
#Istalar en paqueter?a tseries
library(tseries)


runs.test(as.factor(m1$residual>0))



