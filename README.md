# Proyecto: Análisis de Tarifas de Viajes en Uber en NYC (2009-2015)

# Resumen del Proyecto

Este proyecto realiza un análisis completo de un dataset de 200,000 viajes de Uber en la ciudad de Nueva York entre 2009 y 2015. 

El objetivo principal es explorar la relación entre la distancia, la hora del día y la tarifa cobrada, 
y determinar si existe una diferencia estadísticamente significativa en las tarifas entre los períodos de alta y baja demanda (*surge pricing*).

El análisis está completamente desarrollado en **R** y se centra en:

la **limpieza de datos**, la **estadística descriptiva**, la **visualización** y la **inferencia estadística** (Regresión Lineal y T-Test).

Las variables originales del Dataset consisten en:

   Key: un identificador único para cada viaje

   fare_amount: la tarifa de cada viaje en dólares

   pickup_datetime: fecha y hora exactas en que se registró el inicio del viaje en la App.

   passenger_count: el número de pasajeros en el vehículo (valor ingresado manualmente por el/la conductor/a)

   pickup_longitude: la longitud donde se recogió a pasajero/s según GPS de la App

   pickup_latitude: la latitud donde se recogió a pasajero/s según GPS de la App

   dropoff_longitude: la longitud donde finaliza el trayecto según GPS de la App

   dropoff_latitude: la latitud donde finaliza el trayecto según GPS de la App

---

## Entorno y Dependencias

El proyecto fue desarrollado y probado en el entorno **RStudio**.

### Paquetes de R

Los siguientes paquetes son esenciales:

* `tidyverse` (incluye `dplyr` y `ggplot2`)
* `lubridate` (para manejo de fechas y tiempo)
* `geosphere` (para el cálculo de distancias Haversine)
* `moments` (para asimetría y curtosis)
* `car`, `stats`, `broom` (para tests de hipótesis y regresión)

---

## Estructura del Repositorio

La estructura del repositorio sigue la metodología de análisis, lo que facilita la comprensión del flujo de trabajo:

| Archivo/Carpeta | Descripción |
| :--- | :--- |
| `data/raw/` | Contiene el *dataset* original (`uber.csv`). |
| `data/processed/` | Archivos de datos generados en cada etapa (`uber_dataset_limpio.csv`, tablas de estadísticas). |
| `scripts/` | Contiene los **scripts de R** para el análisis, numerados en orden de ejecución. |
| &nbsp;&nbsp;`01_cargar_datos.R` | Carga inicial, validación y exploración del *dataset*. |
| &nbsp;&nbsp;`02_creacion_de_variables.R` | Creación de variables clave (distancia en km, hora, día de la semana, período del día). |
| &nbsp;&nbsp;`03_estadistica_descriptiva.R` | Cálculo de medidas de tendencia central, dispersión, posición y frecuencias. |
| &nbsp;&nbsp;`04_visualizaciones_unfiltered.R` | Gráficos iniciales **antes** de la limpieza de datos. |
| &nbsp;&nbsp;`05_manejo_outliers_y_datos_faltantes.R` | Detección y eliminación de *outliers* y datos inválidos. |
| &nbsp;&nbsp;`06_visualizaciones_limpio.R` | Gráficos finales **después** de la limpieza de datos. |
| &nbsp;&nbsp;`07_estadistica inferencial.R` | Regresión Log-Log y T-Test de Welch para la prueba de hipótesis del *surge pricing*. |
| `outputs/figures/` | Figuras y gráficos generados por los scripts de visualización. |
| `outputs/tables/` | Tablas de estadísticas generadas en formato PNG. |

---

## Instrucciones para la Reproducción del Análisis

Link para la base de datos en Kaggle:

https://www.kaggle.com/datasets/yasserh/uber-fares-dataset

**ACLARACIÓN**: Es necesario crearse un usuario en la página para poder descargar la base.

Para ejecutar el flujo de trabajo completo y replicar todos los resultados:

1.  **Descargar el Repositorio** y asegurarse de que el *dataset* original esté ubicado en `data/raw/uber.csv`.
2.  **Abrir el Proyecto** en RStudio.
3.  **Instalar las Dependencias** de R mencionadas.
4.  **Ejecutar los *scripts* secuencialmente** en el siguiente orden:

    `01_cargar_datos.R`  
    
    `02_creacion_de_variables.R`  
    
    `03_estadistica_descriptiva.R`  
    
    `04_visualizaciones_unfiltered.R`  
    
    `05_manejo_outliers_y_datos_faltantes.R`  
    
    `06_visualizaciones_limpio.R`  
    
    `07_estadistica inferencial.R`

---

## Contacto

* **Autores:** Mariela Arduini, Adriel Morrone
* **Fecha de Creación:** 25-11-2025
* **Repositorio:** https://github.com/Mbarduini/Uber_fares_analysis