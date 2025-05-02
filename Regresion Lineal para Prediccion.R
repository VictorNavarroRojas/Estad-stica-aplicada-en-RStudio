
#________________________ Modelo de Regresión lineal para PREDICCIÓN ________________________
datos<-read.csv("embarazo.csv")
embarazo<-read.csv(file.choose(),header=T)
embarazo
str(datos)
attach(embarazo)  
 
Modelo0<-lm(Wgt~Gest)
summary(Modelo0)

#En este modelo tenemos 2 betas
#B0 = -2037.00
#B1 = 130.82

#Grafica de dispersión con recta ajustada de acuerdo a M1
plot(Gest,Wgt, pch=16, xlab="semanas de gestacion", ylab="Peso al nacer (g)")
curve(-2037 + 130.82*x, add=TRUE, col="red", lwd=2)

embarazo$CodeSmoke<-as.factor(embarazo$CodeSmoke)
str(embarazo)
Modelo1<-lm(Wgt~Gest+CodeSmoke)
summary(Modelo1)


library("ggplot2")
install.packages("ggpmisc")
library("ggpmisc")
install.packages("ggpubr")
library("ggpubr")

a<-ggscatter(
  embarazo, x = "Gest", y = "Wgt",
  color = "Smoke", size = 3, alpha = 0.6,
  palette = c("#FF007F", "#E7B800"),
     add="none",
   ggtheme = theme_bw(),  
)
a

b<-ggscatter(
  embarazo, x = "Gest", y = "Wgt",
  color = "Smoke", size = 3, alpha = 0.6,
  palette = c("#FF007F", "#E7B800"),
     add="reg.line",
   ggtheme = theme_bw(),  
)
b

c<-ggscatterhist(
  embarazo, x = "Gest", y = "Wgt",
  color = "Smoke", size = 3, alpha = 0.6,
  palette = c("#00AFBB", "#E7B800"),
  margin.plot = "boxplot",
   add="reg.line",
   ggtheme = theme_bw(),  
)
c


-2389.57 +143.1 *40  -244.54*0

-2389.57 +143.1 *40

-2389.57 +143.1 *40  -244.54*1

3334.43 - 2491.01 
