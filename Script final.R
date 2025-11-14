# Blanca Escobar Rodríguez_Trabajo2.R
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)
if(!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if(!require("dplyr"))   install.packages("dplyr",   dependencies = TRUE)
library(ggplot2)
library(dplyr)
#Cargar los datos del archivo
datos <- read.csv(file.choose(), header = TRUE)


# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)
dim(datos)
str(datos)
summary(datos)
unique(datos$Tratamiento)
length(unique(datos$Tratamiento))

# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)
ggplot(datos, aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  labs(title = "Niveles de glucosa por tratamiento",
    x = "",
    y = "Glucosa (mg/dL)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))


# 4. Realiza un violin plot (investiga qué es). (1 pt)
ggplot(datos, aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +
  labs(title = "Distribución de glucosa por tratamiento (Violin Plot)", 
    x = "", 
    y = "Glucosa (mg/dL)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))


# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)
colores <- c("FarmacoA" = "steelblue", "FarmacoB" = "tomato", "Placebo" = "darkgreen")
plot(x = datos$Glucosa, y = datos$Presion, col = colores[datos$Tratamiento],
     pch = 19, cex = 1.2,
     xlab = "Glucosa (mg/dL)", ylab = "Presión (mmHg)",
     main = "Relación entre Glucosa y Presión")
legend("bottomright", legend = names(colores), col = colores, pch = 19, title = "Tratamiento", bty = "n")


# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)
ggplot(datos, aes(x = Colesterol, y = Presion, color = Tratamiento)) +
  geom_point(size = 3, alpha = 0.7) +
  facet_grid(~ Tratamiento) +
  labs(title = "Colesterol vs Presión por tratamiento",
       x = "Colesterol (mg/dL)",
       y = "Presión (mmHg)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


# 7. Realiza un histogramas para cada variable. (0.5 pts)
# Histograma para la glucosa.
ggplot(datos, aes(x = Glucosa, fill = Tratamiento)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 15) +
  labs(title = "Distribución de Glucosa", 
    x = "Glucosa (mg/dL)", 
    y = "Frecuencia") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
#Histograma para la presión.
ggplot(datos, aes(x = Presion, fill = Tratamiento)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 15) +
  labs(title = "Distribución de Presión", 
    x = "Presión (mmHg)", 
    y = "Frecuencia") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
# Histograma para el colesterol
ggplot(datos, aes(x = Colesterol, fill = Tratamiento)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 15) +
  labs(title = "Distribución de Colesterol", 
    x = "Colesterol (mg/dL)", 
    y = "Frecuencia") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))


# 8. Crea un factor a partir del tratamiento. Investifa factor(). (1 pt)
datos$Tratamiento <- factor(datos$Tratamiento, levels = c("Placebo", "FarmacoA", "FarmacoB"))
str(datos$Tratamiento)


# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)
aggregate(Glucosa ~ Tratamiento, data = datos, FUN = mean)
aggregate(Glucosa ~ Tratamiento, data = datos, FUN = sd)

# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)
# Los datos del ejemplo para crear las variables.
placebo <- subset(datos, Tratamiento == "Placebo")
farmacoA <- subset(datos, Tratamiento == "FarmacoA")
farmacoB <- subset(datos, Tratamiento == "FarmacoB")
#La comprobación de las variables.
head(placebo)
head(farmacoA)
head(farmacoB)

# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)
shapiro.test(placebo$Glucosa)
shapiro.test(farmacoA$Glucosa)
shapiro.test(farmacoB$Glucosa)
#Los tres tratamientos (placebo, fármacoA y fármacoB) presentan p-values mayores de 0.05, en el test de shapiro. Esto indica que los niveles de glucosa siguen una distribución normal en los tres grupos. Por tanto, la comparativa de medias apropiada es ANOVA que lo hago en el ejercicio 12.


# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)
anova_glucosa <- aov(Glucosa ~ Tratamiento, data = datos)
summary(anova_glucosa)
# Hacemos una comparación post-hoc (tukey) después de ANOVA.
tukey <- TukeyHSD(anova_glucosa)
tukey
# Si el p-value ajustado es > 0.05 en todas las comparaciones, entonces no hay diferencias estadísticamente significativas entre tratamientos.
# Gráfica final de comparación de las medias.
ggplot(datos, aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  labs(title = "Comparación de niveles de glucosa por tratamiento (ANOVA)",
       x = "", 
       y = "Glucosa (mg/dL)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))


