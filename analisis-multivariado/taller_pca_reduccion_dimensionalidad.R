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
