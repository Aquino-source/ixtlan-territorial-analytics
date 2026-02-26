# scripts/00_config.R
# -------------------------------------------------------------------------
# Configuración estética global para el proyecto Ixtlán
# -------------------------------------------------------------------------

# 1. Cargar librerías base
library(ggplot2)
library(extrafont)

# 2. Definición de Paleta de Colores "Ixtlán-Territorial"
paleta_ixtlan <- c(
  "principal" = "#2D5A27", # Verde bosque profundo
  "secundario" = "#1B4F72", # Azul económico
  "enfasis"   = "#D4AC0D", # Dorado/Ocre para destacar puntos clave
  "neutral"   = "#7F8C8D", # Gris para comparativas
  "fondo"     = "#F4F6F6"  # Gris casi blanco para fondos
)

# 3. Definición de Tema Personalizado para ggplot2
tema_ixtlan <- function() {
  theme_minimal(base_size = 12) +
    theme(
      text = element_text(color = "#333333"),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "#666666", hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E5E7E9"),
      axis.title = element_text(face = "italic"),
      legend.position = "bottom",
      strip.background = element_rect(fill = paleta_ixtlan["principal"], color = "white"),
      strip.text = element_text(color = "white", face = "bold")
    )
}

# 4. Función útil para exportar gráficas con las mismas dimensiones
guardar_grafica <- function(plot, nombre_archivo) {
  ggsave(
    filename = paste0("output/figures/", nombre_archivo),
    plot = plot,
    width = 10, 
    height = 6, 
    units = "in", 
    dpi = 300
  )
}

message(">>> Configuración de estilo cargada correctamente.")
