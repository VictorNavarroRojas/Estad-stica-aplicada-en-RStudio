#-------------------Análisis discriminante-------------------------

# Paquetes necesarios
install.packages("MASS")
install.packages("ggplot2")

# Cargar los paquetes
library(MASS)
library(ggplot2)

# Cargar los datos de ejemplo (puedes reemplazarlos con tus propios datos)
data(iris)

# Se dividen los datos en variables predictoras (X) y la variable de respuesta (Y)
X <- iris[, 1:4]
Y <- iris[, 5]

# Para realizar el análisis discriminante
modelo_lda <- lda(X, Y)

# Obtener las coordenadas discriminantes para los datos de entrenamiento
coord_discriminantes <- predict(modelo_lda)$x

# Data frame con las coordenadas discriminantes y la variable respuesta
df_resultados <- data.frame(Discriminante1 = coord_discriminantes[, 1],
                            Discriminante2 = coord_discriminantes[, 2],
                            Clase = Y)
par(mfrow=c(2,2))

# Plots
ggplot(df_resultados, aes(x = Discriminante1, y = Discriminante2, color = Clase)) +
  geom_point() +
  labs(x = "Discriminante 1", y = "Discriminante 2", title = "Análisis Discriminante") +
  scale_color_discrete(name = "Clase")

ggplot(df_resultados, aes(x = Discriminante1, y = Discriminante2, color = Clase)) +
  geom_point() +
  labs(x = "Canónica 1", y = "Canónica 2", title = "Análisis Discriminante") +
  scale_color_discrete(name = "Especie")

#-----------Para la función discriminante

# Definir la función discriminante
mi_funcion_discriminante <- function(X, Y) {
  # Se calculan las medias de cada clase
  medias <- tapply(X, Y, colMeans)
  
  # Calcular las matrices de dispersión dentro y entre clases
  dispersion_within <- matrix(0, ncol = ncol(X), nrow = ncol(X))
  dispersion_between <- matrix(0, ncol = ncol(X), nrow = ncol(X))
  
  for (clase in unique(Y)) {
    X_clase <- X[Y == clase, ]
    n_clase <- nrow(X_clase)
    dispersion_within <- dispersion_within + (n_clase - 1) * cov(X_clase)
    dispersion_between <- dispersion_between + n_clase * crossprod(medias[clase, ] - colMeans(X))
  }
  
  # Calcular la matriz inversa de la matriz de dispersión dentro de las clases
  dispersion_within_inv <- solve(dispersion_within)
  
  # Calcular los coeficientes de la función discriminante
  coeficientes <- dispersion_within_inv %*% as.matrix(colMeans(X) - medias)
  
  # Crear la función discriminante
  funcion_discriminante <- function(x) {
    return(as.numeric(x %*% coeficientes))
  }
  
  return(funcion_discriminante)
}

