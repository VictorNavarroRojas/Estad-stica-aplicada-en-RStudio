
#________________________Bloques al azar____________________________

#Hacer los vectores para las diferentes variables.
setwd("C:/Users/vicon/Documents/Bioestadística 2")
barra<-rep(c(1,2,3,4), c(4))
barra
punta<-rep(c(1,2,3,4), c(4,4,4,4)) 
punta
Y<-c(9.3,9.4,9.2,9.7,9.4,9.3,9.4,9.6,9.6,9.8,9.5,10,10,9.9,9.7,10.2) 

datos<-data.frame("Punta"=punta, "Barra"=barra, "Respuesta"=Y)
datos

#Variables categóricas
punta<-as.factor(punta) 
barra<-as.factor(barra)

str(datos)

class(barra)

is.factor(barra)
is.numeric(Y)
is.numeric(punta)

#Modelo lineal
modelo1<-lm(Y~punta+barra)

#ANOVA
anova(modelo1)

#Tukey
TukeyHSD(aov(Y~punta+barra))
TukeyHSD(aov(Y~punta+barra), "punta")

#Gráfica de comparaciones múltiples
plot(TukeyHSD(aov(Y~punta+barra), "punta"))

#Gráfica de int.conf. para la media de cada trat.
#calcualr ls medias para cada grupo
medias <- tapply(Y,punta,mean)
medias
sort(medias)

#calcular el número de repeticiones por grupo
n <- tapply(Y,punta,length)
n
#Obtener el cuadrado medio de error 
CME <- anova(modelo1)["Residuals", "Mean Sq"]
CME
# a partir de los CME se calcula el error estándar de las medias(EE)
#En ANOVA se calcula un mismo Error estándar para todos los tratamientos
EE <- sqrt(CME/n)
EE
#Grados de libertad del error
gle <- anova(modelo1)["Residuals", "Df"]
gle

#se obtiene el cuantil de la distribución de t
#el valor de dicho cuantil se obtiene con un alfa/2 y los grados de libertad del error
#Si confianza = 95% en I.C se utiliza alfa/2 o 1-alfa/2
cuantil <- qt(0.975,gle)
cuantil

#Gráfica
plot.new()
stripchart(Y~punta,pch=16,vert=T)
arrows(1:4,medias+cuantil*EE,1:4,medias-cuantil*EE,angle=90,code=3,length=.1)

#Unir los tratamientos con una línea
lines(1:4,medias,pch=4,type="b",cex=2, col="blue")

#Disgnóstico del modelo
residuos <- modelo1$res
residuos

#Generar un vector de 1 a n para hacer la gráfica de los residuos
orden <-1:16
orden
plot(orden,residuos)

plot(residuos)
abline(h=0, lty=2, col="red")
#Probar normalidad de los errores
#diagrama de caja de los residuos
boxplot(residuos)

#histograma de los resiudos
hist(residuos,xlim=c(-0.15,0.15))
x=residuos
m<-mean(x)
s<-sd(x)
curve(dnorm(x,mean=m,sd=s), col="blue", add=TRUE)


#Gráfica de Normalidad de los residuos
qqnorm(residuos)

qqline(residuos, col="red")

#Prueba de Normalidad de los residuos
shapiro.test(residuos)


#Homogeneidad de varianza
#calcular los valores estimados
estimados<-modelo1$fitted.values
estimados

plot(punta,residuos)

plot.new()
stripchart(residuos~punta,pch=16,vert=T)
abline(h=0,col="red")

#Prueba de homogeneidad de varianzas
bartlett.test(residuos,punta)

#Prueba de independencia de los errorres
plot(residuos)

library(tseries)
#Ho: Los errores son independientes    vs
#Ha: Los errores NO son independientes
runs.test(as.factor(residuos>0))
