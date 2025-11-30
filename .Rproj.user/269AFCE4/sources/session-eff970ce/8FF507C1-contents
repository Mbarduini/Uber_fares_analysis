# =============================================================================
# SCRIPT 6: VISUALIZACIONES DE LOS DATOS DESPUES DE LIMPIEZA
# Proyecto: uber_fares_analysis
# Autor: Mariela Arduini, Adriel Morrone
# Fecha creación: 2025-11-25
# Última modificación: 2025-11-25
# Descripción: Análisis de tarifas de viajes en Uber en NYC (2009-2015)
# Inputs: uber_dataset_limpio.csv
# Outputs: heatmap_correlacion_uber.png, histograma_tarifas.png, 
#          histograma_distancia.png, histograma_hora.png,
#          boxplot_tarifas_por_distancia.png
#          barras_dias_semana.png
# =============================================================================

# Cargar librerias

library(tidyverse)
library(dplyr)
library(scales)
library(lubridate)
library(reshape2)
library(gridExtra)
library(grid)
library(corrplot)

# =============================================================================
# 1. CARGA DE LA BASE DE DATOS
# =============================================================================

uber_dataset_limpio <- read.csv("data/clean/uber_dataset_limpio.csv")

#Cargar funciones de visualización

source(file.path("functions", "visualizacion_funciones.R"))

# =============================================================================
# 2. VISUALIZACIÓN CORRELACIÓN: HEATMAP DE ESTILO SEABORN
# =============================================================================

# Seleccionar variables numéricas para correlación
vars_correlacion <- uber_dataset_limpio %>%
  select(distance_km, pickup_latitude, dropoff_latitude,
         pickup_longitude, dropoff_longitude, 
         hour, fare_amount) %>%
  na.omit()

# Calcular matriz de correlación
matriz_cor_unfil <- cor(vars_correlacion)
print(round(matriz_cor_unfil, 3))


#Corroborar matriz
matriz_cor_unfil <- as.matrix(matriz_cor_unfil)

# Convertir matriz a formato largo para ggplot2
cor_melted <- melt(matriz_cor_unfil)
colnames(cor_melted) <- c("Var1", "Var2", "value")

# Diccionario para renombrar variables a español
nombres_espanol <- c(
  "distance_km" = "Distancia (km)",
  "pickup_latitude" = "Lat. Origen",
  "dropoff_latitude" = "Lat. Destino",
  "pickup_longitude" = "Long. Origen",
  "dropoff_longitude" = "Long. Destino",
  "hour" = "Hora del día",
  "fare_amount" = "Tarifa"
)

# Renombrar variables manteniendo el orden original
cor_melted$Var1 <- factor(nombres_espanol[as.character(cor_melted$Var1)],
                          levels = unname(nombres_espanol)) 
cor_melted$Var2 <- factor(nombres_espanol[as.character(cor_melted$Var2)],
                          levels = unname(nombres_espanol))


# Crear heatmap estilo seaborn
heatmap_correlacion <- ggplot(cor_melted, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white", size = 0.8) +  
  geom_text(aes(label = round(value, 2),
                color = abs(value) > 0.5),  
            size = 3.5,
            fontface = "bold") +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "#2d3e40"), guide = "none") +
  scale_fill_gradient2(
    low = "#1fbad6",        # Turquesa para correlación negativa
    mid = "#f4f4f4",        # Gris claro para correlación cercana a 0
    high = "#0d7377",       # Verde petróleo para correlación positiva
    midpoint = 0,
    limit = c(-1, 1),
    name = "Correlación",
    breaks = c(-1, -0.5, 0, 0.5, 1),
    labels = c("-1.0", "-0.5", "0.0", "0.5", "1.0")
  ) +
  theme_uber() +  # Aplica el theme con funcion de visualizacion
  theme(
    # Ajustes específicos para el heatmap
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_blank(),      
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(), 
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.background = element_blank()
  ) +
  coord_fixed() +
  labs(
    title = "Matriz de Correlación - Variables Viajes en Uber",
    subtitle = "Coeficiente de Correlación de Pearson"
  )


print(heatmap_correlacion)

# Guardar como PNG
ggsave("outputs/figures/heatmap_correlacion_uber.png", 
       plot = heatmap_correlacion,
       width = 10, 
       height = 9, 
       dpi = 300,
       bg = "white")


# =============================================================================
# 3. VISUALIZACIÓN VARIABLES NUMERICAS: HISTOGRAMA PARA TARIFAS
# =============================================================================

histograma_fare <- ggplot(uber_dataset_limpio %>% 
                            filter(fare_amount > 0), 
                          aes(x = fare_amount)) +
  geom_histogram(bins = 50, 
                 fill = colores_uber["primario"], 
                 color = "white", 
                 alpha = 0.8) +
  geom_vline(aes(xintercept = mean(fare_amount, na.rm = TRUE)), 
             color = colores_uber["acento"],    # Naranja para media
             linetype = "dashed", 
             linewidth = 1) +
  geom_vline(aes(xintercept = median(fare_amount, na.rm = TRUE)), 
             color = colores_uber["secundario"], # Verde petróleo para mediana
             linetype = "dashed", 
             linewidth = 1) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA),labels = scales::dollar_format(prefix = "$")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, NA), labels = scales::comma)+
  theme_uber() +
  theme(
    plot.margin = margin(40, 25, 20, 20),  # más espacio superior
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", 
                              color = "#14535f", margin = margin(b = 8)),  # Más espacio después del título
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.text.y = element_text(margin = margin(r = 5))
  ) + 
  labs(
    title = "Distribución de Tarifas - Viajes Uber",
    subtitle = "Línea naranja: Media | Línea verde: Mediana",
    x = "Tarifa (USD)",
    y = "Frecuencia"
  )

print(histograma_fare)

ggsave("outputs/figures/histograma_tarifas.png", histograma_fare, width = 10, height = 6, dpi = 300)


# =============================================================================
# 4. VISUALIZACIÓN VARIABLES NUMERICAS: HISTOGRAMA PARA HORAS DEL DIA
# =============================================================================


# Agrupar los datos por hora para que queden las horas centradas en barras
datos_hora <- uber_dataset_limpio %>%
  count(hour, name = "Frecuencia")

# Definir las horas a resaltar
horas_pico_manana <- c(7, 8, 9, 10)
horas_pico_tarde <- c(17, 18, 19, 20)


# Iniciar el gráfico de barras
histograma_hora <- ggplot(datos_hora, aes(x = hour, y = Frecuencia)) + 
  geom_bar(stat = "identity", 
           fill = colores_uber["primario"], 
           color = "white",             
           width = 1,              
           alpha = 0.8) 

# Agregar resaltados para la maniana con su borde
for(h in horas_pico_manana) {
  histograma_hora <- histograma_hora +
    annotate("rect", xmin = h - 0.5, xmax = h + 0.5, ymin = 0, ymax = Inf,
             fill = "#ffa726", alpha = 0.3,
             color = "#e68900", size = 0.5)  # Borde oscuro para maniana
}

# Agregar resaltados para la tarde con su borde
for(h in horas_pico_tarde) {
  histograma_hora <- histograma_hora +
    annotate("rect", xmin = h - 0.5, xmax = h + 0.5, ymin = 0, ymax = Inf,
             fill = "#ffa726", alpha = 0.3,
             color = "#e68900", size = 0.5)  # Borde oscuro para tarde
}

# Agregar el restode las capas
histograma_hora <- histograma_hora +
  scale_x_continuous(breaks = seq(0, 23, 1),
                     labels = paste0(seq(0, 23, 1), "h"),  # Agregar "h" a cada hora
                     expand = c(0, 0),
                     limits = c(-0.5, 23.5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                     labels = scales::comma) +
  theme_uber() +
  theme(
    plot.margin = margin(20, 25, 20, 20),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", 
                              color = "#14535f", margin = margin(b = 8)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.text.y = element_text(margin = margin(r = 5))
  ) +
  labs(
    title = "Demanda de Viajes por Hora del Día",
    subtitle = "Horas pico destacadas: Mañana (7-10h) y Tarde (17-20h)",
    x = NULL,
    y = "Número de viajes"
  )

print(histograma_hora)

ggsave("outputs/figures/histograma_hora.png", histograma_hora, width = 10, height = 6, dpi = 300)


# =============================================================================
# 5. VISUALIZACIÓN VARIABLES NUMERICAS: HISTOGRAMA PARA DISTANCIA
# =============================================================================

histograma_distancia <- ggplot(uber_dataset_limpio %>% 
                                 filter(distance_km > 0), 
                               aes(x = distance_km)) +
  geom_histogram(bins = 50, 
                 fill = colores_uber["primario"], 
                 color = "white", 
                 alpha = 0.8) +
  geom_vline(aes(xintercept = mean(distance_km, na.rm = TRUE)), 
             color = colores_uber["acento"],
             linetype = "dashed", 
             linewidth = 1) +
  geom_vline(aes(xintercept = median(distance_km, na.rm = TRUE)), 
             color = colores_uber["secundario"],
             linetype = "dashed", 
             linewidth = 1) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 30),
                     labels = scales::comma) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                     labels = scales::comma) +
  theme_uber() +
  theme(
    plot.margin = margin(20, 25, 20, 20),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", 
                              color = "#14535f", margin = margin(b = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.text.y = element_text(margin = margin(r = 5))
  ) +
  labs(
    title = "Distribución de Distancia de Viajes en Uber",
    subtitle = "Línea naranja: Media | Línea verde: Mediana",
    x = "Distancia (km)",
    y = "Frecuencia"
  )

print(histograma_distancia)
ggsave("outputs/figures/histograma_distancia.png", histograma_distancia, width = 10, height = 6, dpi = 300)

# =============================================================================
# 6. VISUALIZACIÓN VARIABLES NUMERICAS: BOXPLOT PARA TARIFA
# =============================================================================

boxplot_tarifas <- ggplot(uber_dataset_limpio %>% 
                            filter(fare_amount > 0), 
                          aes(x = "", y = fare_amount)) +
  geom_boxplot(fill = colores_uber["primario"], 
               color = colores_uber["secundario"],
               alpha = 0.7, 
               outlier.color = colores_uber["acento"],
               outlier.size = 1.5) +
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 23, 
               size = 4, 
               fill = colores_uber["acento"], 
               color = colores_uber["secundario"]) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60),
                     breaks = seq(0, 60, by = 5),
                     labels = scales::dollar_format(prefix = "$")) +
  theme_uber() +
  theme(
    plot.margin = margin(20, 25, 20, 20),
    axis.ticks.x = element_blank()
  ) +
  labs(
    title = "Boxplot de Tarifas - Viajes Uber",
    subtitle = "Análisis de dispersión y valores atípicos\nRombo naranja = Media | Puntos naranja = Outliers",
    x = NULL,
    y = "Tarifa (USD)"
  ) +
  coord_flip()

print(boxplot_tarifas)
ggsave("outputs/figures/boxplot_tarifas.png", 
       boxplot_tarifas, 
       width = 10, height = 4, dpi = 300)


# ===================================================================================
# 7. VISUALIZACIÓN: BOXPLOT PARA TARIFAS POR RANGO DISTANCIAS
# ===================================================================================


boxplot_tarifas_distancia <- ggplot(uber_dataset_limpio %>% 
                                      mutate(rango_distancia = factor(rango_distancia,
                                                                    levels = c("0-2 km", "2-5 km", "5-10 km", 
                                                                               "10-20 km", ">20 km"))), 
                                    aes(x = rango_distancia, 
                                        y = fare_amount, 
                                        fill = rango_distancia)) +
  geom_boxplot(alpha = 0.7, 
               outlier.color = colores_uber["acento"], 
               outlier.size = 0.8,
               color = colores_uber["oscuro"]) +
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 23, 
               size = 3, 
               fill = colores_uber["acento"],
               color = colores_uber["secundario"]) +
  scale_fill_manual(values = colorRampPalette(c(colores_uber["primario"], 
                                                colores_uber["secundario"], 
                                                colores_uber["oscuro"]))(5)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$"),
                     limits = c(0, 60)) +
  theme_uber() +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 25, 20, 20),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.text.y = element_text(margin = margin(r = 5))
  ) +
  labs(
    title = "Distribución de Tarifas por Distancia de Viaje",
    subtitle = "Rombo naranja = Media por rango de distancia | Puntos naranja = Outliers",
    x = NULL,
    y = "Tarifa (USD)"
  )

print(boxplot_tarifas_distancia)
ggsave("outputs/figures/boxplot_tarifas_por_distancia.png", 
       boxplot_tarifas_distancia, width = 10, height = 6, dpi = 300)


# ===================================================================================
# 8. VISUALIZACIÓN: BOXPLOT PARA DISTANCIA
# ===================================================================================



boxplot_distancia <- ggplot(uber_dataset_limpio %>% 
                              filter(distance_km > 0), 
                            aes(x = "", y = distance_km)) +
  geom_boxplot(fill = colores_uber["primario"], 
               color = colores_uber["secundario"],
               alpha = 0.7, 
               outlier.color = colores_uber["acento"],
               outlier.size = 1.5) +
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 23, 
               size = 4, 
               fill = colores_uber["acento"], 
               color = colores_uber["secundario"]) +
  scale_y_continuous(breaks = seq(0, 30, by = 2)) +
  theme_uber() +
  theme(
    plot.margin = margin(20, 25, 20, 20),
    axis.ticks.x = element_blank()
  ) +
  labs(
    title = "Boxplot de Distancia - Viajes Uber",
    subtitle = "Análisis de dispersión y valores atípicos\nRombo naranja = Media | Puntos naranja = Outliers",
    x = NULL,
    y = "Distancia (en Km)"
  ) +
  coord_flip()

print(boxplot_distancia)
ggsave("outputs/figures/boxplot_distancia.png", 
       boxplot_distancia, 
       width = 10, height = 4, dpi = 300)

# ===================================================================================
# 9. VISUALIZACIÓN: GRÁFICO DE FRECUENCIAS POR DÍA DE SEMANA
# ===================================================================================


uber_dataset_limpio$dia_semana <- factor(uber_dataset_limpio$weekday, 
                                         levels = c("lun", "mar", "mié", 
                                                    "jue", "vie", "sáb", 
                                                    "dom"))

# Preparar datos agregados
datos_dias <- uber_dataset_limpio %>%
  count(dia_semana) %>%
  mutate(tipo_dia = ifelse(dia_semana %in% c("sáb", "dom"), "Fin de semana", "Día laboral"))

barras_dias_semana <- ggplot(datos_dias, aes(x = dia_semana, y = n)) +
  # Barras con gradiente de colores para días laborables vs fin de semana
  geom_bar(aes(fill = dia_semana), 
           stat = "identity",
           color = NA,
           alpha = 0.85,
           width = 0.8) +
  
  # Línea conectando los puntos máximos
  geom_line(aes(group = 1), 
            color = colores_uber["secundario"], 
            linewidth = 2) +
  
  # Puntos en el máximo de cada barra
  geom_point(size = 6, 
             shape = 21, 
             fill = "#ffa726", 
             color = "white", 
             stroke = 2) +
  
  # Etiquetas con mejor posicionamiento
  geom_text(aes(label = scales::comma(n, big.mark = ".")), 
            vjust = -0.8, 
            size = 3.8,
            fontface = "bold",
            color = colores_uber["oscuro"]) +
  
  # Paleta de colores - TODAS LAS BARRAS EN #1fbad6
  scale_fill_manual(
    values = c(
      "lun" = "#1fbad6",
      "mar" = "#1fbad6",
      "mié" = "#1fbad6",
      "jue" = "#1fbad6",
      "vie" = "#1fbad6",
      "sáb" = "#1fbad6",
      "dom" = "#1fbad6"
    )
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12)),
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  
  theme_uber(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      margin = margin(t = 8),
      size = 11,
      face = "bold",
      color = colores_uber["oscuro"]
    ),
    axis.text.y = element_text(
      margin = margin(r = 8),
      size = 10
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),
      size = 12,
      face = "bold"
    ),
    plot.title = element_text(
      hjust = 0.5,
      size = 15,
      face = "bold",
      color = colores_uber["oscuro"],
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 10,
      color = colores_uber["texto"],
      margin = margin(b = 15)
    ),
    plot.margin = margin(20, 30, 20, 25),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "#e8e8e8", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  
  labs(
    title = "Demanda de Viajes por Día de la Semana",
    subtitle = "Tendencia semanal de viajes en Uber (2009-2015)",
    x = NULL,
    y = "Número de viajes"
  )

print(barras_dias_semana)

ggsave("outputs/figures/barras_dias_semana.png", barras_dias_semana, width = 10, height = 6, dpi = 300)



# ===================================================================================
# 10. VISUALIZACIÓN: GRÁFICO DE FRECUENCIAS POR CANTIDAD DE PASAJEROS
# ===================================================================================

# Convertir passenger_count a factor
uber_dataset_limpio$passenger_count <- factor(uber_dataset_limpio$passenger_count)
table_pasajeros <- uber_dataset_limpio %>%
  count(passenger_count) %>%
  mutate(porcentaje = n / sum(n) * 100)

barras_pasajeros <- ggplot(table_pasajeros, aes(x = passenger_count, y = n)) +
  
  # Barras con color uniforme Uber
  geom_bar(stat = "identity",
           fill = "#1fbad6",
           color = NA,
           alpha = 0.85,
           width = 0.7) +
  
  # Línea conectando los puntos máximos
  geom_line(aes(group = 1), 
            color = colores_uber["secundario"], 
            linewidth = 2) +
  
  # Puntos en el máximo de cada barra
  geom_point(size = 6, 
             shape = 21, 
             fill = "#ffa726", 
             color = "white", 
             stroke = 2) +
  
  # Etiquetas con cantidad y porcentaje
  geom_text(aes(label = paste0(scales::comma(n, big.mark = "."), 
                               "\n(", round(porcentaje, 1), "%)")), 
            vjust = -0.8, 
            size = 3.5,
            fontface = "bold",
            color = colores_uber["oscuro"]) +
  
  scale_y_continuous(
    breaks = seq(0, max(table_pasajeros$n) + 10000, by = 10000),
    expand = expansion(mult = c(0.02, 0.15)),
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  scale_x_discrete(expand = expansion(add = c(0.7, 0.7))) +
  
  theme_uber(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      margin = margin(t = 8),
      size = 11,
      face = "bold",
      color = colores_uber["oscuro"]
    ),
    axis.text.y = element_text(
      margin = margin(r = 8),
      size = 10
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),
      size = 12,
      face = "bold"
    ),
    axis.title.x = element_text(
      margin = margin(t = 15),
      size = 12,
      face = "bold"
    ),
    plot.title = element_text(
      hjust = 0.5,
      size = 15,
      face = "bold",
      color = colores_uber["oscuro"],
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 10,
      color = colores_uber["texto"],
      margin = margin(b = 25)
    ),
    plot.margin = margin(20, 40, 20, 20),  
    legend.position = "none",
    panel.grid.major.y = element_line(color = "#e8e8e8", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  
  labs(
    title = "Distribución de Viajes por Número de Pasajeros",
    subtitle = "Frecuencia y porcentaje de viajes según cantidad de pasajeros",
    x = NULL,
    y = NULL
  ) +
  coord_cartesian(clip = "off")  # Para que los elementos sobresalgan del área de plot

print(barras_pasajeros)

ggsave("outputs/figures/barras_pasajeros.png", barras_pasajeros, width = 10, height = 6, dpi = 300)

