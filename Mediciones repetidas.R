
#________________________________ Mediciones Repetidas ________________________

# Cargar los paquetes
library(Matrix)
library(lme4)
library(ggplot2)


data(sleepstudy)

# Realizar el análisis de medidas repetidas
modelo_lme <- lmer(Reaction ~ Days + (1|Subject), data = sleepstudy)

# Obtener los valores ajustados
datos_ajustados <- data.frame(Days = rep(unique(sleepstudy$Days), length(unique(sleepstudy$Subject))),
                              Ajustados = predict(modelo_lme))

# Graficar los resultados utilizando ggplot2
ggplot(data = sleepstudy, aes(x = Days, y = Reaction, color = Subject)) +
  geom_point() +
  geom_line(data = datos_ajustados, aes(x = Days, y = Ajustados), color = "blue") +
  labs(x = "Días", y = "Reacción", title = "Análisis de Medidas Repetidas") +
  theme_minimal()
