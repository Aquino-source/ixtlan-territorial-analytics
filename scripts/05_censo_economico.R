# -------------------------------------------------------------------------
# Visualización: Análisis de Censos Económicos (2014 vs. 2019)
# -------------------------------------------------------------------------

# 1. Cargar configuración y librerías
source("scripts/00_config.R")
library(tidyverse)
library(scales)

# 2. Cargar datos procesados

ce_data <- readRDS("data/processed/clean_censo_economico.rds")

fuente_ce <- "Fuente: Elaboración propia con datos de INEGI, Censos Económicos (2014, 2019)."

# =========================================================================
# A. VALOR AGREGADO POR SECTOR (TOP 10)
# =========================================================================

# =========================================================================
# A. ANÁLISIS DE CAMBIO ESTRUCTURAL (TOP SECTORES 2014 vs 2019)
# =========================================================================

# 1. Identificar los sectores líderes en CADA año para no omitir cambios
top_2014 <- ce_data %>%
  filter(Año == 2014) %>%
  group_by(DESC_CODIGO) %>%
  summarise(VA = sum(`Valor agregado censal bruto (millones de pesos)`, na.rm = TRUE)) %>%
  slice_max(VA, n = 10) %>%
  pull(DESC_CODIGO)

top_2019 <- ce_data %>%
  filter(Año == 2019) %>%
  group_by(DESC_CODIGO) %>%
  summarise(VA = sum(`Valor agregado censal bruto (millones de pesos)`, na.rm = TRUE)) %>%
  slice_max(VA, n = 10) %>%
  pull(DESC_CODIGO)

# 2. Unir ambos grupos para la comparativa (evitamos duplicados con unique)
sectores_comparar <- unique(c(top_2014, top_2019))

# 3. Graficar la comparativa
plot_cambio_estructural <- ce_data %>%
  filter(DESC_CODIGO %in% sectores_comparar) %>%
  group_by(Año, DESC_CODIGO) %>%
  summarise(VA_Total = sum(`Valor agregado censal bruto (millones de pesos)`, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = reorder(DESC_CODIGO, VA_Total), y = VA_Total, fill = factor(Año))) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() + 
  scale_fill_manual(
    values = c("2014" = paleta_ixtlan["neutral"], "2019" = paleta_ixtlan["secundario"]),
    labels = c("2014" = "Censo 2014", "2019" = "Censo 2019")
  ) +
  tema_ixtlan() +
  labs(
    title = "Cambio Estructural: Valor Agregado por Sector",
    subtitle = "Comparativa de los principales sectores económicos (2014-2019)",
    x = "Sector Económico",
    y = "Valor Agregado (Millones de Pesos)",
    fill = "Año del Censo",
    caption = fuente_ce
  )

guardar_grafica(plot_cambio_estructural, "censo_01_cambio_estructural.png")

plot_va_sectores <- ce_data %>%
  filter(DESC_CODIGO %in% top_sectores_va) %>%
  group_by(Año, DESC_CODIGO) %>%
  summarise(VA_Total = sum(`Valor agregado censal bruto (millones de pesos)`, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = reorder(DESC_CODIGO, VA_Total), y = VA_Total, fill = factor(Año))) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() + 
  scale_fill_manual(
    values = c("2014" = paleta_ixtlan["neutral"], "2019" = paleta_ixtlan["secundario"]),
    labels = c("2014" = "Censo 2014", "2019" = "Censo 2019")
  ) +
  tema_ixtlan() +
  labs(
    title = "Sectores Predominantes por Valor Agregado",
    subtitle = "Top 10 sectores económicos en Ixtlán de Juárez",
    x = "Sector Económico",
    y = "Valor Agregado (Millones de Pesos)",
    fill = "Año del Censo",
    caption = fuente_ce
  )

guardar_grafica(plot_va_sectores, "censo_02_valor_agregado.png")

# =========================================================================
# B. DISTRIBUCIÓN DE LA PRODUCCIÓN BRUTA
# =========================================================================

plot_produccion_dist <- ggplot(ce_data, aes(x = factor(Año), y = `Producción bruta total (millones de pesos)`, fill = factor(Año))) +
  geom_boxplot(alpha = 0.8, outlier.color = paleta_ixtlan["enfasis"]) +
  scale_fill_manual(values = c("2014" = paleta_ixtlan["neutral"], "2019" = paleta_ixtlan["principal"])) +
  scale_y_log10(labels = comma) + 
  tema_ixtlan() +
  labs(
    title = "Distribución de la Producción Bruta Total",
    subtitle = "Comparativa de todas las unidades económicas (Escala logarítmica)",
    x = "Año del Censo",
    y = "Producción Bruta (MDP)",
    caption = fuente_ce
  ) +
  theme(legend.position = "none")

guardar_grafica(plot_produccion_dist, "censo_02_dist_produccion.png")

# =========================================================================
# C. CONTRIBUCIÓN POR ESTRATO ECONÓMICO (Evolución 2014 vs 2019)
# =========================================================================

# Usamos 'sectores_comparar' que definimos en la Sección A para consistencia
plot_estratos_evolucion <- ce_data %>%
  filter(DESC_CODIGO %in% sectores_comparar) %>%
  mutate(DESC_CODIGO = factor(DESC_CODIGO, levels = sectores_comparar)) %>%
  ggplot(aes(x = DESC_CODIGO, y = `Valor agregado censal bruto (millones de pesos)`, fill = factor(ID_ESTRATO))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(
    palette = "GnBu", 
    name = "Tamaño de Unidad",
    labels = c("1" = "0-5 pers.", "2" = "6-10 pers.", "3" = "11-30 pers.", 
               "4" = "31-50 pers.", "5" = "51-250 pers.", "6" = "251+ pers.")
  ) +
  coord_flip() +
  facet_wrap(~ Año) + # <-- Esta es la clave para la comparativa temporal
  tema_ixtlan() +
  labs(
    title = "Evolución del Valor Agregado por Estrato",
    subtitle = "Contribución según tamaño de empresa en sectores líderes",
    x = "Sector Económico",
    y = "Valor Agregado (MDP)",
    caption = fuente_ce
  )

guardar_grafica(plot_estratos_evolucion, "censo_03_estratos_comparativo.png")

message(">>> Script 05_censo_economico.R ejecutado con éxito.")
