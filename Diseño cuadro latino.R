
#_______________________Cuadro latino____________________________


#Obejtivo: Evaluar el Efecto del operador 
#La variable respuesta (y) es el tiempo de ensamblado
trat<-c("c","b","a","d","d","c","b","a","a","d","c","b","b","a","d","c")
operador<-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
orden<-c(rep(1:4,4))
y<-c(10,7,5,10,14,18,10,10,7,11,11,12,8,8,9,14)

#Factores = var categ�rica
trat<-as.factor(trat)
orden<-as.factor(orden)
operador<-as.factor(operador)

#Organizar los vectores en una base de datos
datos<-data.frame(cbind(trat,orden,operador,y))
datos

#Definir el modelo
#En en modelo de cuadro latino los efectos van "sumados"
#NOTA: Los efectos van "sumados" por el supuesto de no interacci�n entre factores 
m1<-lm(y~trat+ orden+operador)

#ANOVA
anova(m1)

#Prueba de Tukey
TukeyHSD(aov(y~trat+ orden+operador))

#Tukey para el factor Trat
TukeyHSD(aov(y~trat+ orden+operador),"trat")
plot(TukeyHSD(aov(y~trat+ orden+operador),"trat"))

#Medias de los tratamientos
medias <- tapply(y,trat,mean)
medias

#Medias ordenadas de menor a mayor
ordenando <- sort(medias)
ordenando

#Gr�fica de intervalos de confianza para las medias de cada tratamiento
ybar=medias
ybar <- tapply(y,trat,mean)
ybar

n <- tapply(y,trat,length)
n

CME <- anova(m1)["Residuals", "Mean Sq"]
CME

# error est�ndar de las medias
sem <- sqrt(CME/n)
sem

#Residuales del modelo
gle <- anova(m1)["Residuals", "Df"]
gle

cuantil <- qt(0.975,gle)
cuantil

plot.new()

stripchart(y~trat,pch=16,vert=T, col="blue", main= "Title")

arrows(1:4,medias+cuantil*sem,1:4,medias-cuantil*sem,angle=90,code=3,length=.1)

lines(1:4,medias,pch=4,type="b",cex=2, col="red")



#Diagnóstico del modelo

#Análisis de residuos
residuos<-m1$res

estimados<-m1$fitt

#Probar normalidad de los errores

#histograma de los residuos
hist(residuos, prob=T, col="gray")
x=residuos
m<-mean(x)
s<-sd(x)
curve(dnorm(x,mean=m,sd=s), col="blue", add=TRUE)

#diagrama de caja
boxplot(residuos)

#grafica normal
qqnorm(residuos)
qqline(residuos, col="red")

#Diferentes gráficas del modelo
plot(m1)

#Prueba de normalidad
#Ho:Los datos(residuos) se ajustan a una distribución normal  vs 
#Ha: Los datos(residuos) no se ajustan a una distribución normal

shapiro.test(residuos)

#Probar independencia de los errores
plot(residuos)

#Instalar en paquetería tseries
library(tseries)
#La prueba de rachas se utiliza para probar autocorrelación de datos
#Prueba que el signo de los datos es aleatorio

#Ho: No hay correlación serial vs Ha: Sí hay correlación serial
runs.test(as.factor(m1$residual<0))


#Probar homogeneidad de varianza

#Gráfica de residuos vs estimados
plot(estimados,residuos,xlab="Estimados",ylab="Residuos",main="Gráfica de residuos vs estimados")

#Prueba de homogeneidad de varianza, (homocedasticidad)
#Ho: las varianzas de los tratamientos son homogénesas vs 
#Ha: las varianzas de los trat. no son Homogéneas
bartlett.test(residuos,trat)

library(agricolae)
library(foreign)
library(multcomp)
library(LSD)
     
#Prueba de Duncan
model<-aov(y~trat+orden+operador)
out <- duncan.test(model,"trat",main="Metodos de ensamblado")
     
duncan.test(model,"trat",alpha=0.05,console=TRUE)
     
     
#Prueba se Scheffe
comparison <- scheffe.test(model,"trat", group=TRUE,console=TRUE,main="metodos")
     
     
     
     
     
     
     