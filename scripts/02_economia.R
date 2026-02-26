# -------------------------------------------------------------------------
# Visualización: Análisis Económico y de Salud (2010 vs. 2020)
# -------------------------------------------------------------------------

# 1. Cargar configuración y librerías
source("scripts/00_config.R")
library(tidyverse)

# 2. Cargar datos procesados
# Este archivo fue generado por scripts/01_limpieza.R
df_econ <- readRDS("data/processed/clean_economia.rds")

# =========================================================================
# A. POBLACIÓN ECONÓMICAMENTE ACTIVA (PEA) VS INACTIVA (PNEA)
# =========================================================================

plot_pea_pnea <- df_econ %>% 
  filter(Variable %in% c("PEA", "PE_INAC")) %>%
  ggplot(aes(x = factor(Año), y = Valor, fill = Variable)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c("PEA" = paleta_ixtlan["principal"], "PE_INAC" = paleta_ixtlan["neutral"]),
    labels = c("PEA" = "PEA (Activa)", "PE_INAC" = "PNEA (Inactiva)")
  ) +
  tema_ixtlan() +
  labs(
    title = "Evolución de la Población Económicamente Activa e Inactiva",
    subtitle = "Ixtlán de Juárez, Oaxaca (Periodo 2010-2020)",
    x = "Censo de Población",
    y = "Número de Personas",
    fill = "Segmento",
    caption = "Fuente: Elaboración propia con datos de INEGI, Censo de Población y Vivienda (2010, 2020)."
  )

guardar_grafica(plot_pea_pnea, "economia_01_pea_pnea.png")

# =========================================================================
# B. COMPOSICIÓN POR GÉNERO DE LA PEA
# =========================================================================

plot_pea_genero <- df_econ %>% 
  filter(Variable %in% c("PEA_M", "PEA_F")) %>%
  ggplot(aes(x = factor(Año), y = Valor, fill = Variable)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c("PEA_M" = paleta_ixtlan["principal"], "PEA_F" = paleta_ixtlan["secundario"]),
    labels = c("PEA_M" = "Hombres", "PEA_F" = "Mujeres")
  ) +
  tema_ixtlan() +
  labs(
    title = "Composición por Género de la Población Económicamente Activa",
    subtitle = "Ixtlán de Juárez, Oaxaca (Censos 2010 y 2020)",
    x = "Año del Censo",
    y = "Número de Personas",
    fill = "Género",
    caption = "Fuente: Elaboración propia con datos de INEGI, Censo de Población y Vivienda (2010, 2020)."
  )

guardar_grafica(plot_pea_genero, "economia_02_pea_genero.png")

# =========================================================================
# C. POBLACIÓN OCUPADA Y DESOCUPADA
# =========================================================================

plot_ocupacion <- df_econ %>% 
  filter(Variable %in% c("POCUPADA", "PDESOCUP")) %>%
  ggplot(aes(x = factor(Año), y = Valor, fill = Variable)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c("POCUPADA" = paleta_ixtlan["principal"], "PDESOCUP" = paleta_ixtlan["enfasis"]),
    labels = c("POCUPADA" = "Ocupada", "PDESOCUP" = "Desocupada")
  ) +
  tema_ixtlan() +
  labs(
    title = "Evolución de la Población Ocupada y Desocupada",
    subtitle = "Ixtlán de Juárez, Oaxaca (Periodo 2010-2020)",
    x = "Año del Censo",
    y = "Número de Personas",
    fill = "Estado Laboral",
    caption = "Fuente: Elaboración propia con datos de INEGI, Censo de Población y Vivienda (2010, 2020)."
  )

guardar_grafica(plot_ocupacion, "economia_03_ocupacion.png")

# =========================================================================
# D. DERECHOHABIENCIA A SERVICIOS DE SALUD
# =========================================================================

plot_salud_base <- df_econ %>% 
  filter(Variable %in% c("PSINDER", "PDER_SS")) %>%
  ggplot(aes(x = factor(Año), y = Valor, fill = Variable)) +
  geom_col(position = "stack", width = 0.7) + 
  scale_fill_manual(
    values = c("PDER_SS" = paleta_ixtlan["principal"], "PSINDER" = paleta_ixtlan["neutral"]),
    labels = c("PDER_SS" = "Con Derechohabiencia", "PSINDER" = "Sin Derechohabiencia")
  ) +
  tema_ixtlan() +
  labs(
    title = "Evolución de la Derechohabiencia a Servicios de Salud",
    subtitle = "Ixtlán de Juárez, Oaxaca (Periodo 2010-2020)",
    x = "Año del Censo",
    y = "Número de Personas",
    fill = "Condición",
    caption = "Fuente: Elaboración propia con datos de INEGI, Censo de Población y Vivienda (2010, 2020)."
  )

guardar_grafica(plot_salud_base, "economia_04_salud_cobertura.png")

# =========================================================================
# E. COMPOSICIÓN POR INSTITUCIÓN DE SALUD
# =========================================================================

plot_salud_tipo <- df_econ %>% 
  filter(Variable %in% c("PDER_IMSS", "PDER_ISTE", "PDER_ISTEE", "PDER_SEGP")) %>%
  ggplot(aes(x = factor(Año), y = Valor, fill = Variable)) +
  geom_col(position = "stack", width = 0.7) + 
  scale_fill_brewer(
    palette = "Blues", # Usamos una paleta de azules consistente
    direction = -1,
    labels = c(
      "PDER_IMSS"  = "IMSS", 
      "PDER_ISTE"  = "ISSSTE", 
      "PDER_ISTEE" = "ISSSTE Estatal", 
      "PDER_SEGP"  = "Seguro Popular / Otros"
    )
  ) +
  tema_ixtlan() +
  labs(
    title = "Composición de la Derechohabiencia por Tipo de Servicio",
    subtitle = "Ixtlán de Juárez, Oaxaca (Periodo 2010-2020)",
    x = "Año del Censo",
    y = "Número de Personas",
    fill = "Institución",
    caption = "Fuente: Elaboración propia con datos de INEGI, Censo de Población y Vivienda (2010, 2020)."
  )

guardar_grafica(plot_salud_tipo, "economia_05_salud_institucion.png")

message(">>> Script 02_economia.R ejecutado: 5 gráficas generadas.")
