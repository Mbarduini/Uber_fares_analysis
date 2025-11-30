# =============================================================================
# SCRIPT 3: ESTADISTICA DESCRIPTIVA DE VARIABLES
# Proyecto: uber_fares_analysis
# Autor: Mariela Arduini, Adriel Morrone
# Fecha creación: 2025-11-25
# Última modificación: 2025-11-25
# Descripción: Análisis de tarifas de viajes en Uber en NYC (2009-2015)
# Inputs: uber_fares_dataset_variables.csv
# 
# Outputs: estadisticas_descriptivas_tendencia central_unifiltered.csv, 
#          estadisticas_descriptivas_posicion y forma_unifiltered.csv,
#          tabla_anios_unifiltered.csv, tabla_dias_semana_unifiltered.csv,
#          tabla_horas_unifiltered.csv, tabla_pasajeros_unifiltered.csv
#          matriz_correlacion_unfiltered.csv
# =============================================================================

# Cargar librerias

library(tidyverse)
library(dplyr)
library(moments)
library(scales)
library(geosphere)
library(lubridate)
library(reshape2)
library(gridExtra)
library(grid)
library(corrplot)

#Cargar funciones de visualización

source(file.path("functions", "visualizacion_funciones.R"))

# =============================================================================
# 1. CARGA DE LA BASE DE DATOS
# =============================================================================


uber_fares_dataset_variables <- read.csv("data/processed/uber_fares_dataset_variables.csv")

# Ver estructura básica
cat("Estructura del dataset:\n")
str(uber_fares_dataset_variables)
cat("\n\nPrimeras filas:\n")
head(uber_fares_dataset_variables)


# ============================================================================
# 2. ESTADÍSTICAS DESCRIPTIVAS - VARIABLES NUMÉRICAS SELECCIONADAS
# ============================================================================


cat("\n\n========== ESTADÍSTICAS DESCRIPTIVAS - VARIABLES NUMÉRICAS SELECCIONADAS ==========\n\n")

# Variables numéricas de interés
variables_numericas <- c("fare_amount",
                         "distance_km")


# Función para calcular la moda
calcular_moda <- function(x) {
  x <- x[!is.na(x)]
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Función para crear tabla de estadísticas descriptivas
crea_tabla_estadistica_descriptiva <- function(data, variable) {
  x <- data[[variable]]
  x <- x[!is.na(x)]
  
  estadistica_descriptiva <- data.frame(
    Variable = variable,
    Media = mean(x),
    Mediana = median(x),
    Moda = calcular_moda(x),
    Desviacion_Estandar = sd(x),
    Varianza = var(x),
    Minimo = min(x),
    Q1 = quantile(x, 0.25),
    Q3 = quantile(x, 0.75),
    Maximo = max(x),
    Rango = max(x) - min(x),
    Rango_Intercuartilico = IQR(x),
    Coef_Variacion = (sd(x) / mean(x)) * 100,
    Asimetria = skewness(x),
    Curtosis = kurtosis(x)
  )
  
  return(estadistica_descriptiva)
}

# ============================================================================
# 3. TABLAS DE ESTADÍSTICAS DESCRIPTIVAS - VARIABLES NUMÉRICAS SELECCIONADAS
# ============================================================================

# Crear tabla completa de estadísticas
tabla_completa <- do.call(rbind, lapply(variables_numericas, 
                                        function(v) crea_tabla_estadistica_descriptiva(uber_fares_dataset_variables, v)))

tabla_completa_formatted <- tabla_completa %>%
  mutate(
    Variable = case_when(
      Variable == "fare_amount" ~ "Tarifa",
      Variable == "distance_km" ~ "Distancia (km)",
      TRUE ~ Variable
    )
  ) %>%
  # Redondear solo columnas numéricas
  mutate(across(where(is.numeric), ~round(., 4)))

print(tabla_completa_formatted)

# TABLA 1: Medidas de tendencia central y dispersión básica
tabla_parte1 <- tabla_completa_formatted %>%
  select(Variable, Media, Mediana, Moda, Desviacion_Estandar, 
         Varianza, Coef_Variacion)

# TABLA 2: Medidas de posición y forma
tabla_parte2 <- tabla_completa_formatted %>%
  select(Variable, Minimo, Q1, Q3, Maximo, Rango, 
         Rango_Intercuartilico, Asimetria, Curtosis)


# GUARDAR TABLA 1: Tendencia Central y Dispersión

write.csv(tabla_parte1, "data/processed/tabla_estadisticas_descriptivas_tendencia central_unfiltered.csv", row.names = FALSE)

png("outputs/tables/estadisticas_descriptivas_tendencia central_unfiltered.png", 
    width = 900, height = 100, res = 120)

par(mar = c(1, 1, 1, 1))

grid.table(tabla_parte1, 
           rows = NULL,
           theme = theme_uber_table)

dev.off()


# GUARDAR TABLA 2: Posición y Forma

write.csv(tabla_parte2, "data/processed/tabla_estadisticas_descriptivas_posicion y forma_unfiltered.csv", row.names = FALSE)

png("outputs/tables/estadisticas_descriptivas_posicion y forma_unfiltered.png", 
    width = 1000, height = 100, res = 120)

grid.table(tabla_parte2, 
           rows = NULL,
           theme = theme_uber_table)

dev.off()

# ============================================================================
# 4. DISTRIBUCIÓN DE FRECUENCIAS - VARIABLES CATEGÓRICAS
# ============================================================================

cat("\n\n========== DISTRIBUCIÓN DE FRECUENCIAS - VARIABLES CATEGÓRICAS ==========\n\n")

# Frecuencias de passenger_count
cat("FRECUENCIA DE PASAJEROS:\n")

tabla_pasajeros <- uber_fares_dataset_variables %>%
  count(passenger_count, name = "Frecuencia") %>% 
  rename(`Cantidad de Pasajeros` = passenger_count) %>%
  mutate(
    Porcentaje = round((`Frecuencia` / sum(`Frecuencia`)) * 100, 3),
    Porcentaje_Acumulado = round(cumsum(Porcentaje),3)
  )

print(tabla_pasajeros)

write.csv(tabla_pasajeros, "data/processed/tabla_frecuencia_pasajeros_unfiltered.csv", row.names = FALSE)

png("outputs/tables/tabla_pasajeros_unfiltered.png", width = 800, height = 300, res = 120)
grid.table(tabla_pasajeros, rows = NULL, theme = theme_uber_table)
dev.off()


# Frecuencias por hora del día
cat("\n\nFRECUENCIA POR HORA DEL DÍA:\n")
tabla_horas <- uber_fares_dataset_variables %>%
  count(hour) %>%
  arrange(desc(n)) %>%
  mutate(
    Porcentaje = round((n / sum(n)) * 100,3),
    Porcentaje_Acumulado = round(cumsum(Porcentaje),3)
  ) %>%
  rename(
  `Hora del día` = hour,  # Renombrar hour para mejor presentación
  Frecuencia = n          # Renombrar n para mejor presentación
)

head(tabla_horas, 10)

write.csv(tabla_horas, "data/processed/tabla_frecuencia_horas_unfiltered.csv", row.names = FALSE)

png("outputs/tables/tabla_horas_unfiltered.png", width = 800, height = 1000, res = 120)
grid.table(tabla_horas, rows = NULL, theme = theme_uber_table)
dev.off()

# Frecuencias por día de la semana
cat("\n\nFRECUENCIA POR DÍA DE LA SEMANA:\n")
tabla_dias <- uber_fares_dataset_variables %>%
  count(weekday) %>%
  arrange(desc(n)) %>%
  mutate(
    Porcentaje = round((n / sum(n)) * 100,3),
    Porcentaje_Acumulado = round(cumsum(Porcentaje),3)
  ) %>%
  rename(
  `Día de la semana` = weekday,  # Renombrar weekdya para mejor presentación
  `Frecuencia` = n          # Renombrar n para mejor presentación
)

print(tabla_dias)

write.csv(tabla_dias, "data/processed/tabla_frecuencia_dias_semana_unfiltered.csv", row.names = FALSE)

png("outputs/tables/tabla_dias_semana_unfiltered.png", width = 800, height = 400, res = 120)
grid.table(tabla_dias, rows = NULL, theme = theme_uber_table)
dev.off()

# Frecuencias por año
cat("\n\nFRECUENCIA POR ANIO:\n")
tabla_anios <- uber_fares_dataset_variables %>%
  count(year) %>%
  arrange(year) %>%
  mutate(
    Porcentaje = round((n / sum(n)) * 100,3),
    Porcentaje_Acumulado = round(cumsum(Porcentaje),3)
  ) %>%
  rename(
    `Año` = year,           # Renombrar año para mejor presentación
    `Frecuencia` = n        # Renombrar n para mejor presentación
  )

print(tabla_anios)
write.csv(tabla_anios, "data/processed/tabla_frecuencia_anios_unfiltered.csv", row.names = FALSE)

png("outputs/tables/tabla_anios_unfiltered.png", width = 800, height = 400, res = 120)
grid.table(tabla_anios, rows = NULL, theme = theme_uber_table)
dev.off()


# ============================================================================
# 5. ANÁLISIS DE CORRELACIÓN
# ============================================================================

cat("\n\n========== MATRIZ DE CORRELACIÓN ==========\n\n")

# Seleccionar variables numéricas para correlación
vars_correlacion <- uber_fares_dataset_variables %>%
  select(fare_amount, pickup_longitude, pickup_latitude, 
         dropoff_longitude, dropoff_latitude, passenger_count,
         distance_km, fare_per_km) %>%
  na.omit()

# Calcular matriz de correlación
matriz_cor_unfil <- cor(vars_correlacion)
print(round(matriz_cor_unfil, 3))


write.csv(matriz_cor_unfil, "data/processed/matriz_correlacion_unfiltered.csv", row.names = FALSE)
