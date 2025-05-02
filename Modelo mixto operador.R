

#_________________________ Modelo mixto Operador ___________________


datos<-read.csv(file.choose(),header=T)
datos

library(GAD)

#Definir factores fijos y aleatorios
duracion <- as.factor(datos$duracion)
duracion <- as.fixed(duracion)
temperatura <- as.factor(datos$temperatura)
temperatura <- as.fixed(temperatura)
operador <- as.factor(datos$operador)
operador <- as.random(operador)

#Definir el modelo factorial de temperatura, duración y operador

m1 <- lm(datos$tenido~duracion*temperatura*operador)

#Con gad se obtienen la tabla de ANOVA
gad(m1)  

# Estimaciones de los componentes de varianza
library(Matrix)
library(lme4)

#Se declara el modelo para hacer las estimaciones de varianza
m2 <- lmer(datos$tenido~ duracion*temperatura + (1|operador ) + (1|duracion:operador)+(1|temperatura:operador)+(1|duracion:temperatura:operador))
summary(m2)

# Porcentaje de varianza total de cada componente

vars <- c(2.099,13.224,2.319,3.278)
porcentajes<-100*vars/sum(vars)
porcentajes
sort(porcentajes)

# Grafico de pastel
#carga la paqueteria plotrixpara hacer una gráfica de pie3D
library(plotrix) 
porcentajes2<-c(10.03, 63.21, 11.09, 15.67)
etiqueta<-paste(porcentajes2,"%",sep = "")
colores<-c("purple","red","541","blue")
pie3D(porcentajes2,radius=0.9,explode=0.1,main="Componentes de varianza",labels=etiqueta, labelcex=0.8,col=colores)

#Leyenda
legend("topright",c("Duracion&Temperatura&Operador","Duracion&Operador","Operador","Error"),cex=0.7,fill=colores, bty="n")

#Residuos
residuos<- m1$res
estimados<- m1$fitted.values

qqnorm(residuos)
qqline(residuos, col = "blue")

# Histograma de los residuales 

hist(residuos,col="blue")
hist(residuos, prob=T, col="gray", xlim=c(-4,4), ylim=c(0,0.3))
x=residuos
m<-mean(x)
s<-sd(x)
curve(dnorm(x,mean=m,sd=s), col="blue", add=TRUE)

#Prueba de normalidad
shapiro.test(residuos)
# p-value = 0.3978 --> se cumple la normalidad


plot(residuos)
abline(h=0, col = "red")
library(tseries)

runs.test(as.factor(m1$residual>0))



