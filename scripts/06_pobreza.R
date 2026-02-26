# -------------------------------------------------------------------------
# Visualización: Evolución de la Pobreza y Carencias Sociales (2010-2020)
# -------------------------------------------------------------------------

# 1. Cargar configuración y librerías
source("scripts/00_config.R")
library(tidyverse)

# 2. Cargar datos procesados generados por scripts/01_limpieza.R
# Este archivo contiene los indicadores de CONEVAL en formato largo
pob_long <- readRDS("data/processed/clean_pobreza.rds")

# Definición de caption optimizado para CONEVAL
fuente_coneval <- "Fuente: Elaboración propia con datos de CONEVAL (2010-2020)."

# =========================================================================
# A. EVOLUCIÓN DE LA POBREZA (PORCENTAJE)
# =========================================================================

# Filtramos las categorías principales de pobreza
pobreza_evol <- pob_long %>%
  filter(Indicador %in% c("Pobreza", "Pobreza extrema", "Pobreza moderada"), 
         Tipo == "Porcentaje")

plot_pobreza_pct <- ggplot(pobreza_evol, aes(x = factor(Año), y = Valor, color = Indicador, group = Indicador)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("Pobreza" = paleta_ixtlan["principal"], 
               "Pobreza extrema" = paleta_ixtlan["enfasis"], 
               "Pobreza moderada" = paleta_ixtlan["secundario"])
  ) +
  tema_ixtlan() +
  labs(
    title = "Evolución de la Pobreza en Ixtlán de Juárez",
    subtitle = "Porcentaje de la población en situación de pobreza",
    x = "Año de Medición",
    y = "Porcentaje (%)",
    color = "Categoría",
    caption = fuente_coneval
  )

guardar_grafica(plot_pobreza_pct, "pobreza_01_evolucion_pct.png")

# =========================================================================
# B. NÚMERO DE PERSONAS EN SITUACIÓN DE POBREZA
# =========================================================================

plot_pobreza_abs <- pob_long %>%
  filter(Indicador %in% c("Pobreza", "Pobreza extrema", "Pobreza moderada"), 
         Tipo == "Personas") %>%
  ggplot(aes(x = factor(Año), y = Valor, fill = Indicador)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = c("Pobreza" = paleta_ixtlan["principal"], 
               "Pobreza extrema" = paleta_ixtlan["enfasis"], 
               "Pobreza moderada" = paleta_ixtlan["secundario"])
  ) +
  tema_ixtlan() +
  labs(
    title = "Población en Situación de Pobreza (Valores Absolutos)",
    subtitle = "Número de personas según estimaciones municipales",
    x = "Año de Medición",
    y = "Número de Personas",
    fill = "Categoría",
    caption = fuente_coneval
  )

guardar_grafica(plot_pobreza_abs, "pobreza_02_personas_abs.png")

# =========================================================================
# C. EVOLUCIÓN DE LAS CARENCIAS SOCIALES
# =========================================================================

# Seleccionamos los indicadores de carencias sociales
carencias_plot <- pob_long %>%
  filter(Indicador %in% c("Rezago educativo", 
                          "Carencia por acceso a los servicios de salud", 
                          "Carencia por acceso a la seguridad social", 
                          "Carencia por calidad y espacios de la vivienda"),
         Tipo == "Porcentaje")

plot_carencias <- ggplot(carencias_plot, aes(x = factor(Año), y = Valor, color = Indicador, group = Indicador)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set2") + # Usamos una paleta divergente para las carencias
  tema_ixtlan() +
  labs(
    title = "Evolución de las Carencias Sociales",
    subtitle = "Porcentaje de la población con carencias detectadas",
    x = "Año de Medición",
    y = "Porcentaje (%)",
    color = "Tipo de Carencia",
    caption = fuente_coneval
  )

guardar_grafica(plot_carencias, "pobreza_03_carencias.png")

# =========================================================================
# D. PROMEDIO DE CARENCIAS SOCIALES
# =========================================================================

plot_promedio_carencias <- pob_long %>%
  filter(Indicador %in% c("Pobreza", "Pobreza extrema"), 
         Tipo == "Carencias promedio") %>%
  ggplot(aes(x = factor(Año), y = Valor, color = Indicador, group = Indicador)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("Pobreza" = paleta_ixtlan["principal"], 
               "Pobreza extrema" = paleta_ixtlan["enfasis"])
  ) +
  tema_ixtlan() +
  labs(
    title = "Intensidad de la Pobreza: Promedio de Carencias",
    subtitle = "Número promedio de carencias sociales por persona",
    x = "Año de Medición",
    y = "Promedio de Carencias",
    color = "Categoría",
    caption = fuente_coneval
  )

guardar_grafica(plot_promedio_carencias, "pobreza_04_intensidad.png")

message(">>> Script 06_pobreza.R finalizado: 4 gráficas sociales generadas.")
