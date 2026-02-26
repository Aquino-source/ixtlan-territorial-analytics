# -------------------------------------------------------------------------
# Visualización: Análisis de Flujo de Remesas (2013-2020)
# -------------------------------------------------------------------------

# 1. Cargar configuración y librerías
source("scripts/00_config.R")
library(tidyverse)
library(scales)
library(forecast) # Para descomposición de series de tiempo

# 2. Cargar datos procesados generados por scripts/01_limpieza.R
# Este archivo contiene los montos trimestrales de SE42246
remesas_data <- readRDS("data/processed/clean_remesas.rds")

# Definición de caption optimizado para Banxico
fuente_banxico <- "Fuente: Elaboración propia con datos del Banco de México (Banxico)."

# =========================================================================
# A. EVOLUCIÓN TRIMESTRAL CON TENDENCIA (LOESS)
# =========================================================================

plot_remesas_tendencia <- ggplot(remesas_data, aes(x = Fecha, y = Monto)) +
  geom_line(color = paleta_ixtlan["secundario"], linewidth = 1) +
  # Añadimos una línea de tendencia suave para ver el comportamiento a largo plazo
  geom_smooth(method = "loess", se = FALSE, color = paleta_ixtlan["enfasis"], 
              linetype = "dashed", linewidth = 1) +
  tema_ixtlan() +
  labs(
    title = "Evolución de Remesas Trimestrales en Ixtlán de Juárez",
    subtitle = "Montos en millones de dólares (USD) con tendencia suavizada",
    x = "Año",
    y = "Monto (USD Millones)",
    caption = fuente_banxico
  )

guardar_grafica(plot_remesas_tendencia, "remesas_01_tendencia.png")

# =========================================================================
# B. VARIACIÓN ESTACIONAL (BOXPLOT POR TRIMESTRE)
# =========================================================================

plot_remesas_estacional <- remesas_data %>%
  mutate(Trimestre_Lab = factor(Trimestre, levels = 1:4, 
                                labels = c("T1 (Ene-Mar)", "T2 (Abr-Jun)", 
                                           "T3 (Jul-Sep)", "T4 (Oct-Dic)"))) %>%
  ggplot(aes(x = Trimestre_Lab, y = Monto, fill = Trimestre_Lab)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_brewer(palette = "Blues") +
  tema_ixtlan() +
  theme(legend.position = "none") +
  labs(
    title = "Variación Estacional de Remesas",
    subtitle = "Distribución de montos recibidos por trimestre (2013-2020)",
    x = "Trimestre del Año",
    y = "Monto (USD Millones)",
    caption = fuente_banxico
  )

guardar_grafica(plot_remesas_estacional, "remesas_02_estacionalidad.png")

# =========================================================================
# C. DESCOMPOSICIÓN DE LA SERIE DE TIEMPO
# =========================================================================

# Creamos el objeto ts para la descomposición
remesas_ts <- ts(remesas_data$Monto, start = c(2013, 1), frequency = 4)
decomp <- decompose(remesas_ts, type = "multiplicative")

# Transformamos a formato largo para ggplot
decomp_df <- tibble(
  Fecha = remesas_data$Fecha,
  Observado = as.numeric(decomp$x),
  Tendencia = as.numeric(decomp$trend),
  Estacionalidad = as.numeric(decomp$seasonal),
  Aleatorio = as.numeric(decomp$random)
) %>%
  pivot_longer(cols = -Fecha, names_to = "Componente", values_to = "Valor")

# Visualización por facetas de los componentes
plot_remesas_decomp <- ggplot(decomp_df, aes(x = Fecha, y = Valor)) +
  geom_line(color = paleta_ixtlan["principal"]) +
  facet_wrap(~ Componente, scales = "free_y", ncol = 1) +
  tema_ixtlan() +
  labs(
    title = "Descomposición Multiplicativa de Remesas",
    subtitle = "Análisis de tendencia, estacionalidad e irregularidad",
    x = "Año",
    y = "Valor del Componente",
    caption = fuente_banxico
  )

guardar_grafica(plot_remesas_decomp, "remesas_03_descomposicion.png")

message(">>> Script 07_remesas.R finalizado: Análisis de series de tiempo generado.")
