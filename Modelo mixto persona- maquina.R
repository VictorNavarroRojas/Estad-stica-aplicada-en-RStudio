

#__________________________ Modelo mixto (Persona, Operador)_____________________

#Factor fijo: 3 tipos de máquinas y son operadas por diferentes personas/operadores
maquina <- rep(c(1,2,3),each=18)
maquina
#Factor aleatorio: personas/operadores 
persona <- rep(rep(c(1,2,3,4,5,6),each=3),3)
persona
#variable de respuesta: "y" que es la calificación del producto
y <- c(52,52.8,53.1,51.8,52.8,53.1,
       60,60.2,58.4,51.1,52.3,50.3,
       50.9,51.8,51.4,46.4,44.8,49.2,
       62.1,62.6,64,59.7,60,59,
       68.6,65.8,69.7,63.2,62.8,62.2,
       64.8,65,65.4,43.7,44.2,43,
       67.5,67.2,66.9,61.5,61.7,62.3,
       70.8,70.6,71,64.1,66.2,64,
       72.1,72,71.1,62,61.4,60.5)
data.frame(maquina,persona,y)
#GAD para generar la tabla de ANOVA
#cargar paqueteria GAD

library(Matrix)
library(GAD)

#maquina es el factor fijo por eso se le indica: fixed
maquina <- as.factor(maquina)
maquina <- as.fixed(maquina)
#persona es el factor aleatorio por eso se le indica: random
persona <- as.factor(persona)
persona <- as.random(persona)

#  Calcula la tabla de ANOVA para el modelo
mod1 <- lm(y~ maquina*persona)
gad(mod1) 

# Estimaciones de los componentes de varianza
library(lme4)

#el 1| indica que es aleatorio
mod2 <- lmer(y ~ maquina + (1|persona) + (1|maquina:persona))
summary(mod2)

# Porcentaje de varianza total de cada componente

vars <- c(13.9095,22.8584,0.9246)
100*vars/sum(vars)

#Grafico de pastel
#En un vector coloco los porcentajes
porcentaje<-c(36.90,60.64,2.45)
etiqueta<-paste(porcentaje,"%",sep ="")
colores<-c("purple","orange","blue")
pie(porcentaje,labels=etiqueta,col=colores,radius=1, main="Porcentaje de varianza total de cada componente")
legend("bottomleft",c("Maquina&Persona","Persona","Error"),cex=0.6,fill=colores, bty="n")
#en cex modificas el tamaño de las leyendas y bty para quitar el marco de las leyendas


#supuestos
residuos<-c(mod1$res)
residuos
plot(residuos)

library(tseries)

runs.test(as.factor(mod1$residual>0))

qqnorm(residuos)
qqline(residuos)
hist(residuos)
boxplot(residuos)
caja<-boxplot(residuos, main="Diagrama de caja de los residuos del modelo mixto")
cat("Valores atÃ­picos", caja$out)
shapiro.test(residuos)
