
#______________________________________ Modelo Factorial 2x2 _________________________----

TIEMPO<-c(5,7,6,8,4,6,9,7,10,8,8,6,4,6,5,7,3,5,8,7,9,8,7,6,12,13,13,14,11,12,7,10,8,11,6,9,14,11,15,12,13,10,8,9,9,10,7,8,12,15,13,16,11,14)
METODO<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3)
MEDICAMENTO<-c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3)

datos<-data.frame(cbind(TIEMPO,METODO,MEDICAMENTO))
datos

#Variables categóricas
METODO <- as.factor(METODO)
MEDICAMENTO <- as.factor(MEDICAMENTO)

#IMPORTANTE: Se especifica el modelo. ~ En el modelo de bloques al azar se usaba +; en este se usa *
modelo<-lm(TIEMPO~METODO*MEDICAMENTO)

#ANOVA
anova(modelo)

#Tukey
TukeyHSD(aov(TIEMPO~MEDICAMENTO*METODO))

TukeyHSD(aov(TIEMPO~MEDICAMENTO*METODO), "MEDICAMENTO:METODO")

#Calcular las medias de los tratamientos
medias <- tapply(TIEMPO,MEDICAMENTO:METODO,mean)
medias

mediasord #para que nos de las medias ordenadas

#vector de colores
colores<-c("red","purple","blue")

#Gráficas de interacción tiempo y metodo, 

interaction.plot(MEDICAMENTO,METODO,TIEMPO,type="l",main="Gráfica de interacción",xlab="MEDICAMENTO",ylab="TIEMPO",col=1:3)
interaction.plot(MEDICAMENTO,METODO,TIEMPO,type="l",main="Gráfica de interacción",xlab="MEDICAMENTO",ylab="TIEMPO",col=colores) #col=colores fue el nombre del vector que creamos  
interaction.plot(METODO,MEDICAMENTO,TIEMPO,type="l",main="Gráfica de interacción",xlab="METODO",ylab="TIEMPO",col=4:6)

#Residuos
residuos<-resid(modelo)
residuos
estimados<-fitted(modelo)
estimados

#Probar normalidad
qqnorm(modelo$res)
qqline(modelo$res, col="red")
hist(modelo$res,prob=T, col="orange", ylim=c(0.0,0.30))

x=modelo$res
m<-mean(x)
s<-sd(x)
curve(dnorm(x,mean=m,sd=s), col="black", add=TRUE)
boxplot(residuos,col = "23")

#Prueba de normalidad


#Homocedasticidad
plot(estimados,residuos)
abline(h=0,col="red")

plot(residuos)
abline(h=0,col="red")


bartlett.test(residuos,MEDICAMENTO:METODO) #Bartlett es para los residuos

#Prueba de independencia de los errores
plot(residuos) 
abline(h=0, col="red")
abline(h=0.5, col="red")
abline(h=-0.5, col="red")

##Istalar en paquetería tseries
library(tseries)
runs.test(as.factor(residuos>0))

