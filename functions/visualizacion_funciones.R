
# Theme personalizado Uber
theme_uber <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      # Texto general
      text = element_text(color = "#2d3e40"),
      
      # Ejes
      axis.text = element_text(color = "#14535f", face = "bold"),
      axis.title = element_text(color = "#14535f", face = "bold", size = base_size + 1),
      axis.line = element_line(color = "#0d7377", size = 0.5),
      
      # Panel y fondo
      panel.background = element_rect(fill = "#fafafa", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      
      # Títulos
      plot.title = element_text(hjust = 0.5, size = base_size + 3, 
                                face = "bold", color = "#14535f", 
                                margin = margin(b = 5)),
      plot.subtitle = element_text(hjust = 0.5, size = base_size, 
                                   color = "#2d3e40", margin = margin(b = 10)),
      
      # Leyenda
      legend.title = element_text(size = base_size + 1, face = "bold", color = "#14535f"),
      legend.text = element_text(size = base_size - 2, color = "#2d3e40"),
      legend.background = element_rect(fill = "white", color = "#e0e0e0"),
      legend.key = element_rect(fill = "#fafafa"),
      
      # Márgenes
      plot.margin = margin(20, 20, 20, 20)
    )
}

# Paleta de colores Uber
colores_uber <- c(
  primario = "#1fbad6",
  secundario = "#0d7377",
  oscuro = "#14535f",
  texto = "#2d3e40",
  claro = "#f4f4f4",
  acento = "#ffa726",
  medio = "#7ec8d4",
  gris = "#8b9fa1"
)

# Función para escala de relleno continua
scale_fill_uber <- function(...) {
  scale_fill_gradient2(
    low = colores_uber["primario"],
    mid = colores_uber["claro"],
    high = colores_uber["secundario"],
    midpoint = 0,
    ...
  )
}

# Tema personalizado para tablas
theme_uber_table <- ttheme_minimal(
  base_size = 11,
  core = list(
    fg_params = list(col = "#2d3e40", fontface = "bold"),
    bg_params = list(fill = c("#fafafa", "white"), col = "#e0e0e0", lwd = 0.5)
  ),
  colhead = list(
    fg_params = list(col = "white", fontface = "bold", fontsize = 12),
    bg_params = list(fill = "#1fbad6", col = "#0d7377", lwd = 0.5)
  ),
  rowhead = list(
    fg_params = list(col = "#14535f", fontface = "bold"),
    bg_params = list(fill = "#f4f4f4", col = "#e0e0e0")
  )
)
