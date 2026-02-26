# -------------------------------------------------------------------------
# Visualización: Nivel de Escolaridad Comparativo (2010 vs. 2020)
# -------------------------------------------------------------------------

# 1. Cargar configuración y librerías
source("scripts/00_config.R")
library(tidyverse)

# 2. Cargar datos procesados generados por scripts/01_limpieza.R
# Este archivo contiene las columnas P15YM... procesadas
escolaridad_data <- readRDS("data/processed/clean_educacion.rds")

# Definición de caption optimizado
fuente_iter <- "Fuente: Elaboración propia con datos de INEGI, ITER (2010, 2020)."

# =========================================================================
# A. COMPARATIVA DE NIVEL DE ESCOLARIDAD (PORCENTAJES)
# =========================================================================

# Asegurar el orden de los niveles para la gráfica
niveles_ordenados <- c(
  "Analfabetismo (15+)", "Sin escolaridad/Preescolar",
  "Primaria Incompleta", "Primaria Completa",
  "Secundaria Incompleta", "Secundaria Completa",
  "Media Superior y Superior"
)

plot_educacion <- escolaridad_data %>%
  mutate(Nivel_Escolaridad = factor(Nivel_Escolaridad, levels = niveles_ordenados)) %>%
  ggplot(aes(x = Nivel_Escolaridad, y = Poblacion_Porcentaje, fill = Anio)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  # Usamos los colores de tu paleta institucional
  scale_fill_manual(
    values = c("2010" = paleta_ixtlan["neutral"], "2020" = paleta_ixtlan["principal"]),
    labels = c("2010" = "Censo 2010", "2020" = "Censo 2020")
  ) +
  # Etiquetas abreviadas para mejor lectura en el eje X
  scale_x_discrete(labels = c(
    "Analfabetismo (15+)" = "Analfabetismo",
    "Sin escolaridad/Preescolar" = "Sin Esc./Preesc.",
    "Primaria Incompleta" = "Prim. Inc.",
    "Primaria Completa" = "Prim. Comp.",
    "Secundaria Incompleta" = "Sec. Inc.",
    "Secundaria Completa" = "Sec. Comp.",
    "Media Superior y Superior" = "Media Sup. y Sup."
  )) +
  tema_ixtlan() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Nivel de Escolaridad en Ixtlán de Juárez",
    subtitle = "Población de 15 años y más (Distribución porcentual)",
    x = "Nivel Alcanzado",
    y = "Porcentaje (%)",
    fill = "Año del Censo",
    caption = fuente_iter
  )

# Exportación automática
guardar_grafica(plot_educacion, "educacion_01_niveles.png")

message(">>> Script 04_educacion.R finalizado con éxito.")
