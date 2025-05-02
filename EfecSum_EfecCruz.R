
#__________________ Efectos Sumados y Cruzados ____________________

datos<-read.csv(file.choose(),header=T)
datos

#Problema 1----------------------------------------------------------------------
# Se aislaron los valores de cada variable en objetos para analizar y graficar
# Se declaran aquellos que son variables categoricas

datos$calidad = datos$Calidad
datos$temperatura = as.factor(datos$Temperatura)
datos$ciclo = as.factor(datos$Ciclo)
datos$fijador = as.factor(datos$Fijador)
str(datos)

    
#no hay interacción van sumados los efectos "+"
#hay interacción --> efectos cruzados  "*"
 modelo = lm(datos$calidad~datos$temperatura*datos$ciclo*datos$fijador)
 
 #ANOVA
 anova(modelo)
 
 # de los resultados de ANOVA:
 # temperatura:ciclo p(0.0001002)
 # ciclo:fijador p(1.982e-10)
 #ciclo:fijador p(0.0158701) 
 
 #Tukey
    TukeyHSD(aov(modelo), "datos$temperatura:datos$fijador")
 media1 <- tapply(datos$calidad, datos$temperatura:datos$fijador, mean)    
 sort(media1)
 
    #La inrteracción entre temperatura 90 y fijador 2 obtuvo la calidad más alta = 36

    TukeyHSD(aov(modelo), "datos$fijador")
 medias12 <- tapply(datos$calidad, datos$fijador, mean)  
 sort(medias12)
 
  #El fijador 2 obtuvo los mejores resultados en calidad = 34.4
    TukeyHSD(aov(modelo), "datos$ciclo:datos$fijador")
 media2 <- tapply(datos$calidad, datos$ciclo:datos$fijador, mean)    
 sort(media2)
 
 
# Gráficas de las interacciones 
 
 interaction.plot(datos$ciclo,datos$temperatura,datos$Calidad,type="l",main="Gráfica de interacción",xlab="Ciclo",ylab="Calidad",col=1:3)
 #interaction.plot(temperatura,ciclo,datos$Calidad,type="l",main="Gráfica de interacción",xlab="Fijador",ylab="Calidad",col=1:3)
 
 interaction.plot(datos$fijador,datos$temperatura,datos$Calidad,type="l",main="Gráfica de interacción",xlab="Fijador",ylab="Calidad",col=1:3)
 # interaction.plot(temperatura,fijador,datos$Calidad,type="l",main="Gráfica de interacción",xlab="Temperatura",ylab="Calidad",col=1:3)
 
 #---------------------------------------------
 interaction.ABC.plot(calidad, x.factor = Pelet, groups.factor = Temperatura, trace.factor = Salinidad, data = datos, fun = "mean", title = "GrÃ¡fica de interacción de tres factores", xlab = "Pelet", ylab = "Tiempo", lwd = 4, columns = 2)
 
 interaction.ABC.plot(calidad, x.factor = Temperatura, groups.factor = Salinidad, trace.factor = Pelet, data = datos, fun = "mean", title = "GrÃ¡fica de interacción de tres factores", xlab = "Temperatura", ylab = "Tiempo", lwd = 4, columns = 2)
 
 interaction.ABC.plot(calidad, x.factor = Pelet, groups.factor = Salinidad, trace.factor = Temperatura, data = datos, fun = "mean", title = "GrÃ¡fica de interacción de tres factores", xlab = "Pelet", ylab = "Tiempo", lwd = 4, columns = 2)
 
 #------------------------------------------
 library(ggplot2)
 library(dae)
 interaction.ABC.plot(calidad, x.factor = fijador, groups.factor = temperatura, trace.factor = ciclo, data = datos, fun = "mean", title = "Gráfica de interacción de tres factores", xlab = "Fijador", ylab = "Calidad", lwd = 4)

 interaction.ABC.plot(calidad, x.factor = temperatura, groups.factor = ciclo, trace.factor = fijador, data = datos, fun = "mean", title = "Gráfica de interacción de tres factores", xlab = "Temperatura", ylab = "Calidad", lwd = 4)
 
 interaction.ABC.plot(calidad, x.factor = fijador, groups.factor = ciclo, trace.factor = temperatura, data = datos, fun = "mean", title = "Gráfica de interacción de tres factores", xlab = "Fijador", ylab = "Calidad", lwd = 4, columns = 3)
 
# Mostrar varias gráficas al mismo tiempo
 par(mfrow=c(1,2))
 par(mfrow=c(1,1))
 install.packages("dae")
 library(dae)
 install.packages("ggplot2")
 library(ggplot2)
 
 #3 Factores = 3 niveles de fijador, son tres gráficas
 interaction.ABC.plot(calidad, x.factor = ciclo, groups.factor = temperatura, trace.factor = fijador, data = datos, fun = "mean", title = "Gráfica de interacción de tres factores", xlab = "Temperatura", ylab = "Tiempo", lwd = 4, columns = 2)
 #esta es otra menera de graficar lo mismo que en la linea 63
 interaction.ABC.plot(calidad, x.factor = fijador, groups.factor = temperatura, trace.factor = ciclo, data = datos, fun = "mean", title = "Gráfica de interacción de tres factores", xlab = "Pelet", ylab = "Tiempo", lwd = 4, columns = 2)
 
 interaction.ABC.plot(calidad, x.factor = fijador, groups.factor = ciclo, trace.factor = temperatura, data = datos, fun = "mean", title = "Gráfica de interacción de tres factores", xlab = "Temperatura", ylab = "Tiempo", lwd = 4, columns = 2)
 
 #Problema 2----------------------------------------------------------------------------------------------

 datos2<-read.csv(file.choose("Matriz2"),header=T)
 
 datos2
 
 # variables
 
 rendimiento = datos2$Rendimiento
 posición = as.factor(datos2$Posicion)
 fuente_N = as.factor (datos2$Fuente_N)
 humedad = as.factor(datos2$ï..Humedad)
 
 # Modelo SE USA + PORQUE
 
 modelo1 = lm(rendimiento~posición+fuente_N+humedad)
 
 # ANOVA  #¿cuándo es Gad y cuándo es ANOVA?
 anova(modelo1)
 
 residuos<-modelo1$res
 residuos
 plot(residuos)
 abline(h=0, col = "red")
 
 library(tseries)
 runs.test(as.factor(residuos>0))

 TukeyHSD(aov(modelo1), "fuente_N")

 media1 <- tapply(calidad, temp:fijador, mean)    
 sort(media1)
TukeyHSD()

