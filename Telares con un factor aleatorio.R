

#_____________________________________ ANOVA de Efectos Aleatorios _________________-

Telar<- c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
Telar
Telar<-c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4)
y<-c(98,97,99,96,91,90,93,92,96,95,97,95,95,96,99,98) 

#Factor categórico
Telar<-as.factor(Telar)

##Para factores aleatorios
library(Matrix)

# Ahora con GAD para generar la tabla de ANOVA
install.packages("GAD")
library(GAD)
#Declarar que Telar es un factor aleatorio, si no lo declaro aleatorio no me corre el modelo
Telar<- as.random (Telar)
modelo <- lm(y~Telar)

gad(modelo)


library(lme4)
# estimar los componentes de varianza
m1<-lmer(y~(1|Telar))
#summary nos da los componentes de varianza para el modelo m1
summary(m1)
#Según la fórmula el Componente de varianza para el telar es
#CMtelar-CMerror/n
componente_de_varianza<-(29.7292-1.8958)/4
componente_de_varianza
#Y el componente aleatorio para el error es 
#CME
#Para calcular el porcetaje se hace un vector con cada componente de varianza, la de los telares y la variación total
varianza<-c(6.958, 1.896)
#para calcular los valores de los componentes de varianza en porcentaje
#La suma entre la variacion total y la del total es el 100%
sum(varianza)
#a partir del 100% calculamos con regla de 3 los porcentajes de cada componente de varianza
100*varianza/sum(varianza)
#el telar aporta un 78% de la variación mientras que la variación aleatoria (o también llamada varianza del error)

#En términos de porcentaje el telar aporta un 78.6 % de la varianza total
#Mientras que el error sólo aporta el 21.4%
porcentajes<-c(78.59, 21.41)

#Opción 2
#Creamos una etiqueta para cada porcentaje
Telares<-(78.59)
Error<-(21.41)
porcentajes<-c(Telares, Error)

etiquetas<-paste(porcentajes, "%")
colores<-c("blue", "green")
pie(porcentajes, labels=etiquetas, col=colores, radius = 1, main="Componenetes de varianza")
legend ("topright", c("Telares", "Error"), cex=.7, fill=colores)


#Residuos
residuos<-c(modelo$res)
residuos

#Para comprobar supuesto de independencia de los errores
plot(residuos)

library(tseries)
runs.test(as.factor(modelo$residual>0))

#Probar normalidad
qqnorm(residuos)
qqline(residuos)
hist(residuos, prob=T, col="gray", ylim = c(0,0.5))
x=residuos
m<-mean(x)
s<-sd(x)
curve(dnorm(x,mean=m,sd=s), col="blue", add=TRUE)

boxplot(residuos)
shapiro.test(residuos)


