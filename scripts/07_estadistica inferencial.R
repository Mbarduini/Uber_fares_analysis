# =============================================================================
# SCRIPT 7: ESTADISTICA INFERENCIAL
# Proyecto: uber_fares_analysis
# Autor: Mariela Arduini, Adriel Morrone
# Fecha creación: 2025-11-25
# Última modificación: 2025-11-25
# Descripción: Análisis de tarifas de viajes en Uber en NYC (2009-2015)
# Inputs:  uber_dataset_limpio.csv
# Outputs: uber_dataset_limpio_hipotesis.csv
#          tabla_coeficientes_regresion.png
        
# =============================================================================

# Cargar librerías necesarias

library(tidyverse)
library(readr)
library(dplyr)
library(broom)
library(knitr)
library(car)    # Para el Test de Levene
library(stats)  # Para Regresión, T-test y Kruskal-Wallis
library(ggplot2)
library(gridExtra)
library(grid)

# =============================================================================
# 1. CARGA Y PREPARACIÓN DE DATOS
# =============================================================================

file_path <- "data/processed/uber_dataset_limpio.csv"
uber_dataset_limpio <- read.csv(file_path)

# Asegurar que time_period es un factor
uber_dataset_limpio$time_period <- as.factor(uber_dataset_limpio$time_period)

source(file.path("funciones", "visualizacion_funciones.R"))

# ============================================================================
# 2. REGRESIÓN LINEAL: TARIFA vs. DISTANCIA
# ============================================================================

cat("\n========================================================\n")
cat(" ANÁLISIS EXPLORATORIO Y DIAGNÓSTICO DE ASIMETRÍA         \n")
cat("==========================================================\n")

cat("\n💡 INTERPRETACIÓN DE ASIMETRÍA:\n")

# Cargar y preparar datos extraidos del script 03_estadistica_descriptiva
estadisticas_posicion_forma <- read_csv("data/processed/tabla_estadisticas_descriptivas_posicion_y_forma.csv")

# Extraer asimetrías directamente
asimetrias <- estadisticas_posicion_forma %>%
  filter(Variable %in% c("Tarifa", "Distancia (km)")) %>%
  select(Variable, Asimetria)


asimetrias %>%
  mutate(
    nivel = case_when(
      abs(Asimetria) < 0.5 ~ "BAJA (aceptable)",
      abs(Asimetria) < 1   ~ "MODERADA",
      abs(Asimetria) < 2   ~ "ALTA (requiere transformación)",
      TRUE                 ~ "MUY ALTA (transformación crítica)"
    ),
    direccion = ifelse(Asimetria > 0, 
                       "positiva (cola derecha)", 
                       "negativa (cola izquierda)")
    
  )

# Determinación del modelo de regresion en base a los resultados del análisis de asimetría

cat("\n========================================================\n")
cat(" CONCLUSIÓN DEL ANÁLISIS                                  \n")
cat("==========================================================\n")


if (all(abs(asimetrias$Asimetria) >= 1)) {
  cat("\nMODELO RECOMENDADO: REGRESIÓN LOG-LOG\n\n")
  cat("Razón: Ambas variables muestran asimetría ", 
      ifelse(any(abs(asimetrias$Asimetria) >= 2), "MUY ALTA", "ALTA"), 
      " que requiere\n")
  cat("   corrección mediante transformación logarítmica para:\n")
  cat("   • Cumplir supuestos de normalidad\n")
  cat("   • Estabilizar varianza de residuos\n")
  cat("   • Mejorar capacidad predictiva del modelo\n\n")
  cat("   Ecuación: log(Tarifa) = β₀ + β₁ × log(Distancia)\n")
  cat("   Interpretación: β₁ = elasticidad (cambio % en tarifa por 1% en distancia)\n")
} else {
  cat("\n MODELO RECOMENDADO: REGRESIÓN LINEAL SIMPLE\n")
  cat("   Asimetría aceptable, no requiere transformación.\n")
}


# ========== MODELO LOG-LOG (ELASTICIDAD) ==========
cat("MODELO LOG-LOG (ambas variables transformadas)\n")
cat("────────────────────────────────────────────────────────\n")
modelo_loglog <- lm(log(fare_amount) ~ log(distance_km), data = uber_dataset_limpio)
cat(sprintf("log(Tarifa) = %.4f + %.4f × log(Distancia)\n", 
            coef(modelo_loglog)[1], coef(modelo_loglog)[2]))


# ============= COEFICIENTES Y SIGNIFICANCIA ============
cat("RESUMEN DE COEFICIENTES DEL MODELO:\n")
cat("────────────────────────────────────────────────────────\n")
coef_tabla <- tidy(modelo_loglog, conf.int = TRUE) %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercepto (β₀)",
      term == "distance_km" ~ "Distancia (β₁)",
      TRUE ~ term
    ),
    Significancia = case_when(
      p.value < 0.001 ~ "Extremadamente Significativo",
      p.value < 0.01 ~ "Muy Significativo",
      p.value < 0.05 ~ "Significativo",
      p.value > 0.05 ~ "No Significativo",
      TRUE ~ ""
    )
  ) %>%
  select(term, estimate, std.error, statistic, p.value, conf.low, conf.high, Significancia)

print(kable(coef_tabla, 
            col.names = c("Término", "Coeficiente", "Error Est.", "t-valor", 
                          "p-valor", "IC 95% Inf", "IC 95% Sup", "Sig."),
            digits = 4,
            format = "simple"))

cat("\nNota: No Significativo p>0.05, Significativo p<0.05, Muy Significativo p<0.01, Extremadamente Significativo p<0.001\n\n")


write_csv(coef_tabla, "data/processed/tabla_coeficientes_regresion.csv")

png("outputs/tables/tabla_coeficientes_regresion.png", width = 1500, height = 400, res = 150)

grid.table(coef_tabla, 
           rows = NULL,
           theme = theme_uber_table)
dev.off()

cat("────────────────────────────────────────────────────────\n")
cat("BONDAD DE AJUSTE:\n")
cat(sprintf("R² = %.4f\n", summary(modelo_loglog)$r.squared))
cat(sprintf("\nINTERPRETACIÓN: Un aumento del 1%% en distancia → %.2f%% de\n", 
            coef(modelo_loglog)[2]))
cat("aumento en tarifa (elasticidad constante)\n\n")
cat(sprintf("• R² Ajustado = %.4f\n", summary(modelo_loglog)$adj.r.squared))
cat(sprintf("• Error Estándar Residual = $%.4f\n", summary(modelo_loglog)$sigma))
cat(sprintf("• N = %d observaciones\n", nobs(modelo_loglog)))
cat("────────────────────────────────────────────────────────\n")

# =============================================================================
# 3. TEST DE HIPOTESIS - SURGE PRICING DE TARIFAS POR PERIODO DEL DÍA
# =============================================================================

cat("\n========================================================\n")
cat(" ANÁLISIS EXPLORATORIO Y SUPUESTOS                        \n")
cat("==========================================================\n")

# ---   CREACIÓN DE LA VARIABLE DICOTÓMICA TIME_GROUP, PARA IDENTIFICAR
# PERIODOS DE RELATIVA ALTA DEMANDA (PICO) Y BAJA DEMANDA (NO-PICO)  ---

# Pico: morning_rush, afternoon_rush
# Valle: dawn, noon, night

uber_dataset_limpio_hipotesis <- uber_dataset_limpio %>%
  mutate(
    time_group = case_when(
      time_period %in% c("morning_rush", "afternoon_rush") ~ "Pico",
      time_period %in% c("dawn", "noon", "night") ~ "Valle",
      TRUE ~ NA_character_
    ),
    log_fare = log(fare_amount) # Transformación logarítmica de tarifa por asimetria extrema
  ) %>%
  filter(!is.na(time_group))

# --- TEST DE LEVENE (Homogeneidad de Varianzas) ---
cat("HIPÓTESIS:\n")
cat("────────────────────────────────────────────────────────\n")

cat("H₀: La variabilidad de las tarifas en horas pico es similar a\n")
cat("la variabilidad de las tarifas fuera de pico, hay homocedasticidad\n")

cat("\n=== 2.1. TEST DE LEVENE (Homocedasticidad) ===\n")
levene_fare <- leveneTest(log_fare ~ as.factor(time_group), 
                          data = uber_dataset_limpio_hipotesis, 
                          center = median)

print(kable(levene_fare, 
            col.names = c("Df", "F value", "Pr(>F) ."),
            digits = 4,
            format = "simple"))

#Aislar el p-value para conclusiones y comparar con nivel de significancia
p_value <- levene_fare$`Pr(>F)`[1]

cat(paste0("\nNivel de Significancia (alpha): ", 0.05, "\n"))
cat(paste0("Valor p (Pr(>F)): ", round(p_value, 4), "\n"))

# Aplicar la regla de decisión: Rechazo si p < 0.05
if (p_value < 0.05) {
  cat("Conclusión: Rechazo de H0 -> Hay Heterocedasticidad.\n")
} else {
  cat("Conclusión: No rechazo de H0 -> Asumimos Homocedasticidad.\n")
}


png("outputs/tables/tabla_levene_fare.png", width = 1000, height = 400, res = 200)

grid.table(levene_fare, 
           rows = NULL,
           theme = theme_uber_table)
dev.off()



# --- QQ-plot para evaluar normalidad de tarifas ---

qqplot_tarifas <- uber_dataset_limpio %>%
                  ggplot(aes(sample = fare_amount)) +
  stat_qq(color = colores_uber["primario"], 
          alpha = 0.6,
          shape = 21,
          size = 2) +
  stat_qq_line(color = colores_uber["acento"], 
               linewidth = 1.2) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_uber() +
  theme(
    # Eliminar cuadrícula completamente
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Agregar líneas de eje elegantes
    axis.line = element_line(color = colores_uber["oscuro"], linewidth = 0.6),
    
    # Mejorar texto de ejes
    axis.text.x = element_text(margin = margin(t = 8), size = 11, face = "bold"),
    axis.text.y = element_text(margin = margin(r = 8), size = 11, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    
    # Márgenes y títulos
    plot.margin = margin(40, 35, 30, 30),
    plot.title = element_text(size = 15, face = "bold", 
                              color = colores_uber["oscuro"],
                              margin = margin(b = 8)),
    plot.subtitle = element_text(size = 11, 
                                 color = colores_uber["texto"],
                                 margin = margin(b = 15))
  ) +
  labs(
    title = "Q-Q Plot de Tarifas - Viajes Uber",
    subtitle = "Evaluación de normalidad | Línea naranja = Distribución normal teórica",
    x = "Cuantiles Teóricos",
    y = "Cuantiles de la Muestra"
  )

print(qqplot_tarifas)
ggsave("outputs/figures/qqplot_tarifas.png", qqplot_tarifas, width = 8, height = 8, dpi = 300)

# Conclusión del QQ-plot
cat("\n=== CONCLUSIÓN: TEST DE NORMALIDAD (QQ-Plot) ===\n")
cat("Se rechaza normalidad en base a la evidencia gráfica:\n")
cat("  • Cola derecha: Desviación extrema (outliers >$150)\n")
cat("  • Patrón en 'S': Asimetría positiva severa\n")
cat("  • Puntos NO siguen la línea teórica (naranja)\n")
cat("\nImplicancias:\n")
cat("  → Justifica uso de transformación log de la variable Tafira (fare_amount)\n")
cat("  → Válida elección de tests no paramétricos como el Mann-Whitney o\n")
cat("  → el T test parametricO con corrección de Welch\n")
cat("  → Confirma asimetría extrema detectada por metodos previos (skewness ≈ 2.6)\n")


# =============================================================================
# 4. TEST DE MUESTRAS INDEPENDIENTES CON LOG (WELCH)
# =============================================================================

cat("\n========================================================\n")
cat("  T-TEST DE MUESTRAS INDEPENDIENTES (PICO vs. VALLE)      \n")
cat("==========================================================\n")

# --- EJECUCIÓN DEL T-TEST DE WELCH CON LOG DE TARIFA ---
cat("Medias por Grupo:\n")
uber_dataset_limpio_hipotesis %>%
  group_by(time_group) %>%
  summarise(
    N = n(),
    Media_Original = mean(fare_amount, na.rm = TRUE),  #Media original de la muestra post-limpieza
    Media_Log = mean(log_fare, na.rm = TRUE)           #Media despues de la transformacion logaritmica
  ) %>%
  print()


cat("\nEjecutar T-Test (Welch):\n")
# var.equal = FALSE aplica la corrección de Welch (no asume varianzas iguales).
t_test_result <- t.test(log_fare ~ time_group, 
                        data = uber_dataset_limpio_hipotesis, 
                        var.equal = FALSE,
                        alternative = "two.sided") 

print(t_test_result)

cat("\n=== CONCLUSIÓN T-TEST DE WELCH ===\n")

# Diferencia porcentual (correcta para logs de tarifa)
pct_diff <- (exp(diff(t_test_result$estimate)) - 1) * 100
ci_lower <- (exp(t_test_result$conf.int[1]) - 1) * 100
ci_upper <- (exp(t_test_result$conf.int[2]) - 1) * 100

# Medias originales del dataset (para emplear en el contexto descriptivo)
mean_pico <- mean(uber_dataset_limpio_hipotesis$fare_amount[
  uber_dataset_limpio_hipotesis$time_group == "Pico"], na.rm = TRUE)
mean_valle <- mean(uber_dataset_limpio_hipotesis$fare_amount[
  uber_dataset_limpio_hipotesis$time_group == "Valle"], na.rm = TRUE)

cat(sprintf("p-valor = %.4f %s\n", t_test_result$p.value,
            ifelse(t_test_result$p.value < 0.05, 
                   "(p < 0.05): SIGNIFICATIVO", 
                   "(p >= 0.05): NO SIGNIFICATIVO")))

cat("\nEstadísticas descriptivas (contexto):\n")
cat(sprintf("• Media aritmética Pico: $%.2f | Valle: $%.2f\n", 
            mean_pico, mean_valle))

cat("\nResultado del test (escala log):\n")
cat(sprintf("• Diferencia porcentual: Pico es %.1f%% %s que Valle\n", 
            abs(pct_diff), ifelse(pct_diff > 0, "más caro", "más barato")))
cat(sprintf("• IC 95%%: [%.1f%%, %.1f%%]\n", ci_lower, ci_upper))

if (t_test_result$p.value < 0.05) {
  cat("\n• Se RECHAZA H₀: Las tarifas Pico y Valle difieren significativamente\n")
  cat("• El horario pico/valle es un predictor significativo del precio\n")
} else {
  cat("\n• NO se rechaza H₀: No hay diferencias estadísticamente significativas\n")
  cat("• El horario pico/valle no es un predictor significativo del precio\n")
}

#=================================================================================