# =============================================================================
# SCRIPT 1: CARGA Y VALIDACIÓN INICIAL DE DATOS
# Proyecto: uber_fares_analysis
# Autor: Mariela Arduini, Adriel Morrone
# Fecha creación: 2025-11-25
# Última modificación: 2025-11-25
# Descripción: Análisis de tarifas de viajes en Uber en NYC (2009-2015)
# Inputs: uber.csv
# Outputs: uber_fares_dataset.csv
# =============================================================================

# Cargar librerias

library(tidyverse)

# =============================================================================
# CARGA Y EXPLORACION INICIAL DE LA BASE DE DATOS
# =============================================================================

# Cargar base de datos Uber Fares Dataset, con datos para 200,000 viajes de la 
# aplicacion en la ciudad de Nueva York para los años 2009-2015

uber_fares <- read.csv("data/raw/uber.csv")

# Convertir a tibble para mejor manejo con tidyverse
uber_fares_dataset <- as_tibble(uber_fares)

# Exploracion inicial de la estructura
cat("=========== ESTRUCTURA DE LA BASE ===========\n")
glimpse(uber_fares_dataset)

# Verificar dimensiones de la base
cat("\nDimensiones de la base:", dim(uber_fares_dataset), "\n")
cat("Filas:", nrow(uber_fares_dataset), "\nColumnas:", ncol(uber_fares_dataset), "\n")

# Ver las primeras observaciones
cat("\n=========== PRIMERAS 10 OBSERVACIONES ===========\n")
head(uber_fares_dataset, 10)

# Resumen estadistico general
cat("\n=========== RESUMEN ESTADISTICO ===========\n")
summary(uber_fares_dataset)

# Identificar tipos de variables
cat("\n=========== TIPOS DE VARIABLES ===========\n")
sapply(uber_fares_dataset, class)

# Guardar el dataset
write.csv(uber_fares_dataset, file.path("data", "processed/uber_fares_dataset.csv"), row.names = FALSE)
