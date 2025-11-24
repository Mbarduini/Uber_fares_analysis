# =============================================================================
# SCRIPT 5: ANÁLISIS DE OUTLIERS Y DATOS FALTANTES
# Proyecto: uber_fares_analysis
# Autor: Mariela Arduini, Adriel Morrone
# Fecha creación: 2025-11-25
# Última modificación: 2025-11-25
# Descripción: Análisis de tarifas de viajes en Uber en NYC (2009-2015)
# Inputs: uber_fares_dataset_variables.csv
# Outputs: uber_fares_dataset_variables.csv
# =============================================================================

# Cargar librerias

library(tidyverse)
library(dplyr)
library(moments)
library(gridExtra)
library(scales)



# =============================================================================
# 1. CARGA DE LA BASE DE DATOS
# =============================================================================

uber_fares_dataset_variables <- read.csv("data/processed/uber_fares_dataset_variables.csv")

#Cargar funciones de visualización

source(file.path("funciones", "visualizacion_funciones.R"))


# Verificar dimensiones de la base
cat("\nDimensiones de la base:", dim(uber_fares_dataset_variables), "\n")
cat("Filas:", nrow(uber_fares_dataset_variables), "\nColumnas:", ncol(uber_fares_dataset_variables), "\n")


# =============================================================================
# 2. ANÁLISIS DE DATOS FALTANTES
# =============================================================================

datos_faltantes <- data.frame(
  Variable = names(uber_fares_dataset_variables),
  N_Faltantes = sapply(uber_fares_dataset_variables, function(x) sum(is.na(x))),
  Porcentaje = sapply(uber_fares_dataset_variables, function(x) round(sum(is.na(x))/length(x)*100, 2))
)
datos_faltantes <- datos_faltantes %>% arrange(desc(N_Faltantes))


cat("=== DATOS FALTANTES ===\n")
print(datos_faltantes)
cat("\nTotal de valores faltantes en el dataset:", sum(datos_faltantes$N_Faltantes), "\n")
cat("Porcentaje total de datos faltantes:", 
    round(sum(datos_faltantes$N_Faltantes)/(nrow(uber_fares_dataset_variables)*ncol(uber_fares_dataset_variables))*100, 2), "%\n\n")

#Funcion de respuesta para el patrón de datos faltantes
if(sum(datos_faltantes$N_Faltantes) > 0) {
  cat("### 2.2. FILAS CON DATOS FALTANTES ###\n")
  filas_con_na <- uber_fares[!complete.cases(uber_fares_dataset_variables), ]
  cat("Número de filas con al menos un dato faltante:", nrow(filas_con_na), "\n")
  cat("Porcentaje de filas con datos faltantes:", 
      round(nrow(filas_con_na)/nrow(uber_fares_dataset_variables)*100, 2), "%\n\n")
} else {
  cat("### 2.2. NO SE DETECTARON DATOS FALTANTES EN LA BASE DE DATOS ###\n\n")
}

# =============================================================================
# 2. ANÁLISIS DE VALORES ANÓMALOS ESPECÍFICOS (OUTLIERS)
# =============================================================================

cat("### 5.1. VALORES NEGATIVOS O CERO (OUTLIERS)###\n")
cat("Tarifas <= 0:", sum(uber_fares_dataset_variables$fare_amount <= 0, na.rm = TRUE), 
    "(", round(sum(uber_fares_dataset_variables$fare_amount <= 0, 
                   na.rm = TRUE)/nrow(uber_fares_dataset_variables)*100, 2), "%)\n")
cat("Distancias = 0:", sum(uber_fares_dataset_variables$distance_km == 0, na.rm = TRUE),
    "(", round(sum(uber_fares_dataset_variables$distance_km == 0, 
                   na.rm = TRUE)/nrow(uber_fares_dataset_variables)*100, 2), "%)\n")
cat("Pasajeros = 0:", sum(uber_fares_dataset_variables$passenger_count == 0, na.rm = TRUE),
    "(", round(sum(uber_fares_dataset_variables$passenger_count == 0, 
                   na.rm = TRUE)/nrow(uber_fares_dataset_variables)*100, 2), "%)\n\n")

cat("### 5.2. VALORES EXTREMOS (OUTLIERS) ###\n")
cat("Tarifas > $200:", sum(uber_fares_dataset_variables$fare_amount > 200, na.rm = TRUE),
    "(", round(sum(uber_fares_dataset_variables$fare_amount > 200, 
                   na.rm = TRUE)/nrow(uber_fares_dataset_variables)*100, 2), "%)\n")
cat("Distancias > 100 km:", sum(uber_fares_dataset_variables$distance_km > 100, na.rm = TRUE),
    "(", round(sum(uber_fares_dataset_variables$distance_km > 100, 
                   na.rm = TRUE)/nrow(uber_fares_dataset_variables)*100, 2), "%)\n")
cat("Pasajeros > 6:", sum(uber_fares_dataset_variables$passenger_count > 6, na.rm = TRUE),
    "(", round(sum(uber_fares_dataset_variables$passenger_count > 6, 
                   na.rm = TRUE)/nrow(uber_fares_dataset_variables)*100, 2), "%)\n\n")


viajes_200 <- uber_fares_dataset_variables %>% filter(fare_amount > 200)

cat(sprintf("Se elige el valor $200 para el corte de tarifas por viaje, que representa el: %.4f%% de la muestra\ncon una distancia de viaje promedio de %.2f km\n",
            nrow(viajes_200) / nrow(uber_fares_dataset_variables) * 100,
            mean(viajes_200$distance_km, na.rm = TRUE)))


cat("### 5.3. COORDENADAS INVÁLIDAS O FUERA DE RANGO NYC ###\n")
# NYC aproximadamente: Longitud [-74.3, -73.7], Latitud [40.5, 40.9]
coords_invalidas <- uber_fares_dataset_variables %>%
  filter(pickup_longitude < -74.3 | pickup_longitude > -73.7 |
           pickup_latitude < 40.5 | pickup_latitude > 40.9 |
           dropoff_longitude < -74.3 | dropoff_longitude > -73.7 |
           dropoff_latitude < 40.5 | dropoff_latitude > 40.9 |
           pickup_longitude == 0 | pickup_latitude == 0 |
           dropoff_longitude == 0 | dropoff_latitude == 0)

cat("Viajes con coordenadas fuera del rango de NYC:", nrow(coords_invalidas),
    "(", round(nrow(coords_invalidas)/nrow(uber_fares_dataset_variables)*100, 2), "%)\n\n")


# =============================================================================
# 3. LIMPIEZA DE LA BASE DE DATOS
# =============================================================================

#Total de valores registrados previo a la limpieza
total_registros <- nrow(uber_fares_dataset_variables)
cat("Total de viajes registrados sin limpieza:", total_registros)

    # Crear dataset filtrado 
uber_dataset_limpio <- uber_fares_dataset_variables %>%
  filter(fare_amount > 0,                                   # Eliminar tarifas <= 0
         fare_amount < 200,                                 # Eliminar tarifas muy altas y poco representativas
         distance_km != 0,                                  # Eliminar distancia 0, viajes cancelados y/o error de reporte
         pickup_longitude >= -74.3 & pickup_longitude <= -73.7,   # Coordenadas válidas para NYC
         pickup_latitude >= 40.5 & pickup_latitude <= 40.9,
         dropoff_longitude >= -74.3 & dropoff_longitude <= -73.7,
         dropoff_latitude >= 40.5 & dropoff_latitude <= 40.9,
         pickup_longitude != 0 & pickup_latitude != 0,           # Coordenadas lógicas
         dropoff_longitude != 0 & dropoff_latitude != 0,         # Coordenadas lógicas
         passenger_count != 0, passenger_count <= 6)        # Eliminar errores de reporte para pasajeros


write.csv(uber_dataset_limpio, "data/processed/uber_dataset_limpio.csv", row.names = FALSE)


# ============================================================================
# 4. IMPACTO DE LA LIMPIEZA Y NUEVAS ESTADISTICAS DESCRIPTIVAS
# ============================================================================


cat("\n\n===== IMPACTO GENERAL DE LA LIMPIEZA =====\n")
cat(sprintf("Observaciones originales: %d\n", nrow(uber_fares_dataset_variables)))
cat(sprintf("Observaciones finales: %d\n", nrow(uber_dataset_limpio)))
cat(sprintf("Observaciones eliminadas: %d\n", nrow(uber_fares_dataset_variables) - nrow(uber_dataset_limpio)))
cat(sprintf("Porcentaje retenido: %.2f%%\n", (nrow(uber_dataset_limpio) / nrow(uber_fares_dataset_variables)) * 100))
cat("==============================\n")


cat("\n\n========== ESTADÍSTICAS DESCRIPTIVAS LUEGO DE LIMPIEZA- VARIABLES NUMÉRICAS SELECCIONADAS ==========\n\n")

# Variables numéricas de interés
variables_numericas <- c("fare_amount",
                         "distance_km", "fare_per_km")


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


# Crear tabla completa de estadísticas
tabla_completa_clean <- do.call(rbind, lapply(variables_numericas, 
                                        function(v) crea_tabla_estadistica_descriptiva(uber_dataset_limpio, v)))

# Renombrar variables a español
tabla_completa_formatted_clean <- tabla_completa_clean %>%
  mutate(
    Variable = recode(Variable,
                      "fare_amount" = "Tarifa",
                      "distance_km" = "Distancia (km)",
                      "fare_per_km" = "Tarifa por Km")
  ) %>%
  #Redondear solo columnas numéricas, excluir nombre variables
  mutate(across(where(is.numeric), ~round(., 4)))


print(tabla_completa_formatted_clean)

# TABLA 1: Medidas de tendencia central y dispersión básica
tabla_parte1_clean <- tabla_completa_formatted_clean %>%
  select(Variable, Media, Mediana, Moda, Desviacion_Estandar, 
         Varianza, Coef_Variacion)

# TABLA 2: Medidas de posición y forma
tabla_parte2_clean <- tabla_completa_formatted_clean %>%
  select(Variable, Minimo, Q1, Q3, Maximo, Rango, 
         Rango_Intercuartilico, Asimetria, Curtosis)


# GUARDAR TABLA 1: Tendencia Central y Dispersión

write.csv(tabla_parte1_clean, "data/processed/tabla_estadisticas_descriptivas_tendencia central.csv", row.names = FALSE)

png("outputs/tables/estadisticas_descriptivas_tendencia central.png", 
    width = 1200, height = 500, res = 120)

grid.table(tabla_parte1_clean, 
           rows = NULL,
           theme = theme_uber_table)

dev.off()


# GUARDAR TABLA 2: Posición y Forma

write.csv(tabla_parte2_clean, "data/processed/tabla_estadisticas_descriptivas_posicion y forma.csv", row.names = FALSE)

png("outputs/tables/estadisticas_descriptivas_posicion y forma.png", 
    width = 1200, height = 500, res = 120)

grid.table(tabla_parte2_clean, 
           rows = NULL,
           theme = theme_uber_table)

dev.off()

cat("\n\n========== COMPARATIVA ESTADÍSTICAS DESCRIPTIVAS - VARIABLES NUMÉRICAS SELECCIONADAS ==========\n\n")

cat("\n\n========== ESTADÍSTICAS DESCRIPTIVAS PREVIO A LIMPIEZA - TENDENCIA CENTRAL Y DISPERSIÓN ==========\n\n")
read.csv("data/processed/tabla_estadisticas_descriptivas_tendencia central_unfiltered.csv")

cat("\n\n========== ESTADÍSTICAS DESCRIPTIVAS LUEGO DE LIMPIEZA - TENDENCIA CENTRAL Y DISPERSIÓN ==========\n\n")
print(tabla_parte1_clean)

cat("\n\n========== ESTADÍSTICAS DESCRIPTIVAS PREVIO A LIMPIEZA - POSICIÓN Y FORMA ==========\n\n")
read.csv("data/processed/tabla_estadisticas_descriptivas_posicion y forma_unfiltered.csv")

cat("\n\n========== ESTADÍSTICAS DESCRIPTIVAS LUEGO DE LIMPIEZA - POSICIÓN Y FORMA ==========\n\n")
print(tabla_parte2_clean)



cat("\n\n========== ESTADÍSTICAS DESCRIPTIVAS LUEGO DE LIMPIEZA- VARIABLES CATEGORICAS ==========\n\n")

# Frecuencias de passenger_count
cat("FRECUENCIA DE PASAJEROS:\n")

tabla_pasajeros_clean <- uber_dataset_limpio %>%
  count(passenger_count, name = "Frecuencia") %>% 
  rename(`Cantidad de Pasajeros` = passenger_count) %>%
  mutate(
    Porcentaje = round((`Frecuencia` / sum(`Frecuencia`)) * 100, 3),
    Porcentaje_Acumulado = round(cumsum(Porcentaje),3)
  )
print(tabla_pasajeros_clean)

write.csv(tabla_pasajeros_clean, "data/processed/tabla_frecuencia_pasajeros.csv", row.names = FALSE)

png("outputs/tables/tabla_pasajeros_clean.png", width = 800, height = 300, res = 120)
grid.table(tabla_pasajeros_clean, rows = NULL, theme = theme_uber_table)
dev.off()


# Frecuencias por hora del día
cat("\n\nFRECUENCIA POR HORA DEL DÍA:\n")
tabla_horas_clean <- uber_dataset_limpio %>%
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

head(tabla_horas_clean, 10)

write.csv(tabla_horas_clean, "data/processed/tabla_frecuencia_horas.csv", row.names = FALSE)

png("outputs/tables/tabla_horas_clean.png", width = 800, height = 1000, res = 120)
grid.table(tabla_horas_clean, rows = NULL, theme = theme_uber_table)
dev.off()

# Frecuencias por día de la semana
cat("\n\nFRECUENCIA POR DÍA DE LA SEMANA:\n")
tabla_dias_clean <- uber_dataset_limpio %>%
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

print(tabla_dias_clean)

write.csv(tabla_dias_clean, "data/processed/tabla_frecuencia_dias_semana.csv", row.names = FALSE)

png("outputs/tables/tabla_dias_clean_semana.png", width = 800, height = 400, res = 120)
grid.table(tabla_dias_clean, rows = NULL, theme = theme_uber_table)
dev.off()

# Frecuencias por año
cat("\n\nFRECUENCIA POR ANIO:\n")
tabla_anios_clean <- uber_dataset_limpio %>%
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

print(tabla_anios_clean)
write.csv(tabla_anios_clean, "data/processed/tabla_frecuencia_anios.csv", row.names = FALSE)

png("outputs/tables/tabla_anios_clean.png", width = 800, height = 400, res = 120)
grid.table(tabla_anios_clean, rows = NULL, theme = theme_uber_table)
dev.off()

#============================================================================================

cat("\n\n========== COMPARATIVA ESTADÍSTICAS DESCRIPTIVAS - VARIABLES CATEGORICAS ==========\n\n")


cat("\n\n========== TABLAS DE FRECUENCIA PREVIO A LIMPIEZA - PASAJEROS ==========\n\n")
read.csv("data/processed/tabla_frecuencia_pasajeros_unfiltered.csv")

cat("\n\n========== TABLAS DE FRECUENCIA LUEGO DE LIMPIEZA - PASAJEROS ==========\n\n")
print(tabla_pasajeros_clean)

#============================================================================================

cat("\n\n========== TABLAS DE FRECUENCIA PREVIO A LIMPIEZA - HORAS DEL DÍA ==========\n\n")
read.csv("data/processed/tabla_frecuencia_horas_unfiltered.csv")

cat("\n\n========== TABLAS DE FRECUENCIA LUEGO DE LIMPIEZA - HORAS DEL DÍA ==========\n\n")
print(tabla_horas_clean)

#============================================================================================

cat("\n\n========== TABLAS DE FRECUENCIA PREVIO A LIMPIEZA - DÍAS DE LA SEMANA ==========\n\n")
read.csv("data/processed/tabla_frecuencia_dias_semana_unfiltered.csv")

cat("\n\n========== TABLAS DE FRECUENCIA LUEGO DE LIMPIEZA - DÍAS DE LA SEMANA ==========\n\n")
print(tabla_dias_clean)

#============================================================================================

cat("\n\n========== TABLAS DE FRECUENCIA PREVIO A LIMPIEZA - ANIOS ==========\n\n")
read.csv("data/processed/tabla_frecuencia_anios_unfiltered.csv")

cat("\n\n========== TABLAS DE FRECUENCIA LUEGO DE LIMPIEZA - ANIOS ==========\n\n")
print(tabla_anios_clean)

#============================================================================================

