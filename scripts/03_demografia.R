# -------------------------------------------------------------------------
# Visualización: Dinámica Demográfica y Migratoria (2010 vs. 2020)
# -------------------------------------------------------------------------

# 1. Cargar configuración y librerías
source("scripts/00_config.R")
library(tidyverse)
library(scales)

# 2. Cargar datos procesados generados por scripts/01_limpieza.R
piramide_data <- readRDS("data/processed/clean_piramide.rds")
migracion_data <- readRDS("data/processed/clean_migracion.rds")
nym_data       <- readRDS("data/processed/clean_natalidad_mortalidad.rds")

# Definición de caption estándar para fuentes de ITER/INEGI
fuente_iter <- "Fuente: Elaboración propia con datos de INEGI, ITER (2010, 2020)."
fuente_vitales <- "Fuente: Elaboración propia con datos de INEGI (Estadísticas Vitales)."

# =========================================================================
# A. PIRÁMIDE POBLACIONAL COMPARATIVA
# =========================================================================

plot_piramide <- ggplot(piramide_data, aes(x = Valor_Grafico, y = Rango_Edad, fill = Anio)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  scale_x_continuous(
    labels = function(x) paste0(abs(x), "%"), # Muestra valores absolutos porcentuales
    breaks = pretty_breaks(n = 5)
  ) +
  scale_fill_manual(
    values = c("2010" = paleta_ixtlan["neutral"], "2020" = paleta_ixtlan["secundario"]),
    labels = c("2010" = "Censo 2010", "2020" = "Censo 2020")
  ) +
  facet_wrap(~ Sexo, scales = "free_x") +
  tema_ixtlan() +
  labs(
    title = "Pirámide Poblacional de Ixtlán de Juárez",
    subtitle = "Distribución porcentual por edad y sexo",
    x = "Porcentaje de la Población Total",
    y = "Grupo de Edad",
    fill = "Año del Censo",
    caption = "Fuente: Elaboración propia con datos de INEGI, ITER (2010, 2020)."
  )

guardar_grafica(plot_piramide, "demografia_01_piramide.png")

# =========================================================================
# B. TENDENCIA DE NATALIDAD Y MORTALIDAD (2010-2020)
# =========================================================================

tendencia_vital <- nym_data %>% 
  filter(Indicador %in% c("Nacimientos Totales", "Defunciones Totales"))

plot_vitales <- ggplot(tendencia_vital, aes(x = Año, y = Valor, color = Indicador)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("Nacimientos Totales" = paleta_ixtlan["principal"], 
               "Defunciones Totales" = paleta_ixtlan["enfasis"])
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2)) +
  tema_ixtlan() +
  labs(
    title = "Tendencia de Nacimientos y Defunciones Registradas",
    subtitle = "Ixtlán de Juárez, Oaxaca (Periodo 2010-2020)",
    x = "Año",
    y = "Número de Registros",
    color = "Indicador",
    caption = fuente_vitales
  )

guardar_grafica(plot_vitales, "demografia_02_vitales.png")

# =========================================================================
# C. MIGRACIÓN RECIENTE (PORCENTAJE DE LA POBLACIÓN)
# =========================================================================

plot_migracion <- migracion_data %>%
  filter(Tipo_Migracion == "Residentes Anteriores en Otra Entidad (Reciente)", 
         Sexo != "Total") %>%
  ggplot(aes(x = Sexo, y = Poblacion_Porcentaje, fill = Anio)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = paste0(round(Poblacion_Porcentaje, 1), "%")),
    position = position_dodge(width = 0.8), 
    vjust = -0.5, 
    size = 3
  ) +
  scale_fill_manual(
    values = c("2010" = paleta_ixtlan["neutral"], "2020" = paleta_ixtlan["principal"])
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  tema_ixtlan() +
  labs(
    title = "Población Migrante Reciente en Ixtlán de Juárez",
    subtitle = "Residentes anteriores en otra entidad (últimos 5 años)",
    x = "Sexo",
    y = "Porcentaje de la Población Total",
    fill = "Año de Censo",
    caption = fuente_iter
  )

guardar_grafica(plot_migracion, "demografia_03_migracion.png")

message(">>> Script 03_demografia.R finalizado: 3 gráficas generadas.")
