# =============================================================================
# SCRIPT 2: CREACION DE VARIABLES
# Proyecto: uber_fares_analysis
# Autores: Mariela Arduini, Adriel Morrone
# Fecha creación: 2025-11-25
# Última modificación: 2025-11-25
# Descripción: Análisis de tarifas de viajes en Uber en NYC (2009-2015)
# Inputs: uber_fares_dataset.csv
# Outputs: uber_fares_variables.csv
# =============================================================================

#Cargar librerías

library(tidyverse)
library(dplyr)
library(scales)
library(geosphere)
library(lubridate)

# ============================================================================
# 1. CARGA Y PREPARACIÓN DE DATOS TEMPORALES
# ============================================================================

uber_fares_dataset <- read.csv("data/processed/uber_fares_dataset.csv")

# Convertir fecha a formato date time con lubridate
uber_fares_dataset$pickup_datetime <- ymd_hms(uber_fares_dataset$pickup_datetime)

# Crear variables derivadas para el analisis temporal

uber_fares_dataset_time <- uber_fares_dataset %>%
    mutate(
      hour = hour(pickup_datetime),
      weekday = wday(pickup_datetime, label = TRUE),
      month = month(pickup_datetime),
      year = year(pickup_datetime),
      
# Definir períodos del día
      time_period = case_when(
        hour >= 6 & hour < 10 ~ "morning_rush",
        hour >= 10 & hour < 16 ~ "noon",
        hour >= 16 & hour < 20 ~ "afternoon_rush",
        hour >= 20 & hour < 24 ~ "night",
        TRUE ~ "dawn"
      )
    )

glimpse(uber_fares_dataset_time)

# ============================================================================
# 2. PREPARACIÓN DE DATOS DE DISTANCIA
# ============================================================================

uber_fares_dataset_variables <- uber_fares_dataset_time %>%
  filter(
    # Filtrar por las coordenadas válidas
    pickup_longitude >= -180 & pickup_longitude <= 180,
    pickup_latitude >= -90 & pickup_latitude <= 90,
    dropoff_longitude >= -180 & dropoff_longitude <= 180,
    dropoff_latitude >= -90 & dropoff_latitude <= 90
  ) %>%
  mutate(
    distance_km = distHaversine(
      cbind(pickup_longitude, pickup_latitude),
      cbind(dropoff_longitude, dropoff_latitude)
    ) / 1000,
    # Crear categorías de distancia
    rango_distancia = cut(distance_km,
                          breaks = c(0, 2, 5, 10, 20, Inf),
                          labels = c("0-2 km", "2-5 km", "5-10 km", 
                                     "10-20 km", ">20 km"),
                          include.lowest = TRUE),
    # Calcular tarifa por kilómetro
    fare_per_km = fare_amount / (distance_km + 0.1)  # +0.1 para evitar división por 0
  )

glimpse(uber_fares_dataset_variables)

write.csv(uber_fares_dataset_variables, file.path("data", "processed/uber_fares_dataset_variables.csv"), row.names = FALSE)
