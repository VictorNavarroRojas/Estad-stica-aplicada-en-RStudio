#__________________________Componentes Principales_______________

datos2<-read.csv("database5.csv")
datos2<-read.csv(file.choose(),header=T)

variable.names(datos2)
row.names(datos2)<-datos2$Province

#Generar datos sin el nombre como variable 
datos_trabajo2<-datos2[,-grep(c("Province"),colnames(datos2))]
variable.names(datos_trabajo2)

str(datos_trabajo2)

#Calcular Matriz de Correlaciones y almacenarla en el directorio de trabajo
#Al calcularla, se elimina la variable Health Index con [-c(3)]). No tiene caso mantenerla para explicar un modelo de educación, no es relevante
MatrizR1<-cor(datos_trabajo2[-c(3)])
#Al quitar una variable, nos quedamos con 5 variables, nos queda una matriz de 5x5

#Componentes Principales
CP<-princomp(datos_trabajo2[-c(3)], cor=T)

#Proporción de varianza explicada de  cada componentes
summary(CP)
#Con 2 componentes es suficiente para explicar el 84.5% de varianza

#Gráfica de codo (screeplot). Componentes en el eje x y varianzas de los componentes en eje y
#De acuerdo con el criterio de Kaiser, se recomienda retener los componentes con varianza mayor a 1
screeplot(CP, main="Grafica de codo")
abline(h=1,col=4)
#Los componentes 1 y 2, varianza > 1

#Eigen valores
eigen(MatrizR1)$values

#Eigen vectores
eigen(MatrizR1)$vectors

#Cargas de las variables en cada uno de los componentes 
CP$loadings

#Biplot
library(factoextra)
fviz_pca_biplot(CP,col.var="#2E9FDF", col.ind="#696969")

