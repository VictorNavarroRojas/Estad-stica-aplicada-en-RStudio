#____________________ Clusters_________________

#Leer datos desde un archivo csv directorio de trabajo
datos1<-read.csv("datbas_cluster.csv")

#Buscar archivo csv en documentos
datos1<-read.csv(file.choose(),header=T)

variable.names(datos1)
row.names(datos1)<-datos1$Province

#Generar datos sin el nombre como variable
datos_trabajo1<-datos1[,-grep(c("Province"),colnames(datos1))]

install.packages("reshape")
library(reshape)

#Estandarización, type="sd" por default (Xi - Mu)/desves
base.std1 <- rescaler(datos1)

ó

base.std1 <- rescaler(datos1, type="sd")

#librerias que se ocupan según el método; comando{librería}
#rescaler{reshape}
#dist {stats}
#daisy {cluster}

install.packages("xlsx")
library("xlsx")

write.xlsx(base.std1, "base_std1.xlsx") #para llevar a una base de datos en excel

#Matriz de distancias por método euclideano (todas las variables son numéricas)
base.dist1 <- dist(base.std1[,-c(1)], method = "euclidean")

#ya que tengo mi matriz de distancias "base.dist1"
#se hace un Cluster por liga de Ward
base.hc51 <- hclust(base.dist1, method="ward") 
plot(base.hc51, hang=-1,labels=datos1[,1], main="Dendogram using Ward Linkage and Euclidean distances", xlab="Geographical Jusrisdictions", ylab="Distances", font.lab=2, font=2)
abline(h=15, col="red", lty=2)

#Cluster por liga sencilla
base.hc51 <- hclust(base.dist1, method="single")
plot(base.hc51, hang=-1,labels=datos1[,1], main="Dendogram using Single Linkage and Euclidean distances", xlab="Geographical Jusrisdictions", ylab="Distances", font.lab=2, font=2)
abline(h=5, col="black", lty=2)

#Cluster por liga completa
base.hc51 <- hclust(base.dist1, method="complete")
plot(base.hc51, hang=-1,labels=datos1[,1], main="Dendogram using Complete Linkage and Euclidean distances", xlab="Geographical Jusrisdictions", ylab="Distances", font.lab=2, font=2)
abline(h=7, col="black", lty=2)

#Metodo no Jerárquico de K medias
#4 grupos
nojerarquico41<-kmeans(base.dist1,4, iter.max = 10)
nojerarquico41
datos1[nojerarquico41$cluster==1, 1:2]
datos1[nojerarquico41$cluster==2, 1:2]
datos1[nojerarquico41$cluster==3, 1:2]
datos1[nojerarquico41$cluster==4, 1:2]
palette(c("black","black","black","black"))
nombres<-c(1:63)

palette(c("red","black","blue","orange","#28E2E5"))
#Generar la gráfica con los nombres de los estados
plot(cmdscale(base.dist1), xlab="First Dimension", ylab="Second Dimension", main="K-means clustering, k=4",
     sub= "Total variation explained by First and Second dimensions: 63%", cex.sub=0.7, font.lab=2, pch="*")

text(cmdscale(base.dist1),labels=datos1[,1],cex=0.8,col=nojerarquico41$cluster)

#Generar la gráfica sólo con números
plot(cmdscale(base.dist1), xlab="First Dimension", ylab="Second Dimension", main="K-means clustering, k=4",
     sub= "Total variation explained by First and Second dimensions: 65.4%", cex.sub=0.7, font.lab=2, pch=".")

text(cmdscale(base.dist1),labels=nombres,cex=1.1,col=nojerarquico41$cluster)

#5 grupos
palette(c("red","black","blue","orange","green"))
nojerarquico51<-kmeans(base.dist1,5, iter.max =30)
nojerarquico51
datos1[nojerarquico51$cluster==1, 1:2]
datos1[nojerarquico51$cluster==2, 1:2]
datos1[nojerarquico51$cluster==3, 1:2]
datos1[nojerarquico51$cluster==4, 1:2]
datos1[nojerarquico51$cluster==5, 1:2]

#Generar la gráfica con los nombres de los estados
plot(cmdscale(base.dist1), xlab="First Dimension", ylab="Second Dimension", main="K-means clustering, k=5",
     sub= "Total variation explained by First and Second dimensions: 77.6%", cex.sub=0.7, font.lab=2, pch="*")

text(cmdscale(base.dist1),labels=datos1[,1],cex=1, font=1, font.axis=2,col=nojerarquico51$cluster)

#Generar la gráfica sólo con números
plot(cmdscale(base.dist1), xlab="First Dimension", ylab="Second Dimension", main="K-means clustering, k=5", sub= "Total variation explained by First and Second dimensions: 77.6%", cex.sub=0.7, font.lab=2, pch=".")
text(cmdscale(base.dist1),labels=nombres,cex=1.1, font=1, font.axis=2,col=nojerarquico51$cluster)



