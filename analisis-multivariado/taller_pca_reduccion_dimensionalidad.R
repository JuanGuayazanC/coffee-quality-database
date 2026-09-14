# ============================================================
# Taller práctico de Análisis Multivariado
# PCA y métodos alternativos de reducción de dimensionalidad
# Dataset: Coffee Quality Institute - arabica_data_cleaned.csv
# ============================================================

# ------------------------------------------------------------
# 2. Selección del conjunto de datos
# ------------------------------------------------------------
# Fuente: Coffee Quality Institute, vía repositorio
#   https://github.com/jldbc/coffee-quality-database
#   archivo: data/arabica_data_cleaned.csv
# Observaciones: 1311 lotes de café arábica evaluados por catadores certificados.
#
# Variables cuantitativas utilizadas (puntajes de catación, escala 0-10,
# más variables físicas del lote):
#   Aroma, Flavor, Aftertaste, Acidity, Body, Balance, Uniformity,
#   Clean.Cup, Sweetness, Cupper.Points   -> puntajes sensoriales (0-10)
#   Moisture                              -> % de humedad del grano
#   Category.One.Defects                  -> conteo de defectos primarios
#   Category.Two.Defects                  -> conteo de defectos secundarios
#   altitude_mean_meters                  -> altitud media de la finca (metros)
#
# Se excluye Total.Cup.Points del conjunto activo del PCA porque es,
# por construcción, la suma de los diez puntajes sensoriales anteriores:
# incluirla generaría una variable redundante (colinealidad perfecta con
# el resto) en vez de aportar información nueva. Se conserva aparte como
# variable ilustrativa para contrastar con las componentes obtenidas.
#
# Justificación de la selección: son las únicas variables genuinamente
# cuantitativas y continuas del dataset; el resto de columnas son
# identificadores, certificaciones o metadatos administrativos sin
# contenido analítico (Owner, Farm.Name, Lot.Number, Certification.Body, etc.).

library(dplyr)

data <- read.csv("data/arabica_data_cleaned.csv")

vars_cuantitativas <- c("Aroma", "Flavor", "Aftertaste", "Acidity", "Body",
                         "Balance", "Uniformity", "Clean.Cup", "Sweetness",
                         "Cupper.Points", "Moisture", "Category.One.Defects",
                         "Category.Two.Defects", "altitude_mean_meters")

# --- Datos faltantes ---
colSums(is.na(data[, vars_cuantitativas]))
# Único NA relevante: altitude_mean_meters (227 filas). Se documenta y se
# trata más abajo junto con el outlier conocido de esa misma variable.

# --- Outlier conocido de altitud ---
summary(data$altitude_mean_meters)
# Valor corrupto (~190000 m, imposible para una finca cafetera). Se filtra
# a un rango plausible de altitud de cultivo (0 - 3000 msnm) y se eliminan
# los NA restantes solo para las variables usadas en este taller.

datos <- data %>%
  filter(is.na(altitude_mean_meters) | (altitude_mean_meters > 0 & altitude_mean_meters <= 3000)) %>%
  filter(!is.na(altitude_mean_meters)) %>%
  select(all_of(vars_cuantitativas))

dim(datos)  # filas remanentes tras el tratamiento de NA/outlier

# ------------------------------------------------------------
# 3. Exploración inicial
# ------------------------------------------------------------
str(datos)
dim(datos)
head(datos)
summary(datos)
colSums(is.na(datos))

cor(datos)

# Puntos a discutir en la presentación (a partir de la salida anterior):
#  - diferencias de escala: altitude_mean_meters (cientos-miles de metros)
#    frente a los puntajes sensoriales (rango ~6-10) y Moisture (%).
#  - variables con mayor variabilidad: altitude_mean_meters y los conteos
#    de defectos, frente a los puntajes de catación que son mucho más
#    homogéneos entre lotes.
#  - correlaciones importantes: los puntajes sensoriales están fuertemente
#    correlacionados entre sí (Flavor-Aftertaste, Balance-Aftertaste, etc.),
#    reflejando que evalúan dimensiones relacionadas de la experiencia de cata.
#  - Category.One/Two.Defects y Moisture están poco correlacionadas con el
#    bloque sensorial: aportan información distinta (calidad física del grano).

# ------------------------------------------------------------
# 4. Matriz de varianzas-covarianzas y medidas de variabilidad
# ------------------------------------------------------------
S <- cov(datos)
S

# 4.1. Varianza total
varianza_total <- sum(diag(S))
varianza_total
# Dominada casi por completo por altitude_mean_meters, dado que su varianza
# está en una escala de miles de metros^2 frente a puntajes en escala 0-10.

# 4.2. Varianza generalizada
varianza_generalizada <- det(S)
varianza_generalizada
# Mide la variabilidad conjunta (volumen del elipsoide de dispersión).
# Un valor cercano a cero indicaría multicolinealidad severa o redundancia
# casi perfecta entre variables (lo que aquí no ocurre de forma extrema
# tras excluir Total.Cup.Points, pero sí hay colinealidad moderada-alta
# entre los puntajes sensoriales).

p <- ncol(datos)
varianza_total_media <- varianza_total / p
varianza_generalizada_media <- varianza_generalizada^(1 / p)
varianza_total_media
varianza_generalizada_media
# Las versiones "promedio" (dividiendo por p o tomando la raíz p-ésima)
# permiten comparar variabilidad entre conjuntos de variables de distinto
# tamaño p, cosa que la varianza total y el determinante crudo no permiten
# al depender de la dimensión y de las unidades de cada variable.

# ------------------------------------------------------------
# 5. Análisis de Componentes Principales
# ------------------------------------------------------------
# Decisión: ESTANDARIZAR. Las variables tienen unidades y escalas muy
# distintas (metros vs. puntaje 0-10 vs. porcentaje vs. conteo de defectos).
# Sin estandarizar, altitude_mean_meters dominaría por completo la varianza
# total y, por tanto, el primer componente, ocultando la estructura de los
# puntajes sensoriales que es el objeto real de interés del análisis.

pca <- prcomp(datos, center = TRUE, scale. = TRUE)

# ------------------------------------------------------------
# 6. Selección del número de componentes
# ------------------------------------------------------------
summary(pca)
pca$sdev^2

screeplot(pca, type = "lines", main = "Gráfico de codo - PCA café")

# Criterio de decisión (ajustar la lectura una vez ejecutado en clase):
#  - varianza acumulada explicada por las primeras 3-4 componentes;
#  - codo visible en el screeplot;
#  - interpretabilidad de cada componente en términos de las variables
#    originales (ver sección 7).
# El número final de componentes a conservar (k) debe fijarse con base en
# estos criterios y no de forma arbitraria.

# ------------------------------------------------------------
# 7. Interpretación de las componentes
# ------------------------------------------------------------
pca$rotation

# Para cada componente conservada, identificar en pca$rotation:
#  - variables con mayor carga absoluta (mayor peso);
#  - variables con cargas del mismo signo (comportamiento similar);
#  - variables con cargas de signo opuesto (relación inversa);
#  - propuesta de nombre/significado del componente en el contexto cafetero
#    (p. ej., un componente dominado por Aroma/Flavor/Aftertaste/Balance
#    puede leerse como un eje general de "calidad sensorial"; un componente
#    dominado por Category.One/Two.Defects y Moisture puede leerse como un
#    eje de "calidad física del grano").

# ------------------------------------------------------------
# 8. Matriz de gráficos de dispersión de las componentes
# ------------------------------------------------------------
k <- 4  # ajustar según la decisión tomada en la sección 6

pca$x[1:6, 1:k]

pairs(pca$x[, 1:k],
      main = "Matriz de dispersión - Componentes principales (café)",
      pch = 19, col = adjustcolor("steelblue", alpha.f = 0.4))

# Revisar si aparecen agrupamientos (p. ej., por Processing.Method o
# Country.of.Origin, no usados como variables activas del PCA pero
# disponibles en `data` para colorear los puntos si se desea profundizar),
# patrones no lineales, separación entre observaciones y posibles atípicos
# multivariados. Analizar si estos patrones se repiten entre distintos
# pares de componentes o son específicos de un par en particular.

# ------------------------------------------------------------
# 9. Biplot
# ------------------------------------------------------------
biplot(pca, choices = c(1, 2), cex = 0.6,
       main = "Biplot PCA - Componentes 1 y 2 (café)")

# El biplot añade, sobre la matriz de dispersión, la dirección y magnitud
# de cada variable original en el espacio de componentes: permite ver
# simultáneamente qué variables "empujan" hacia dónde a las observaciones,
# información que la matriz de dispersión (solo puntos) no muestra por sí sola.
