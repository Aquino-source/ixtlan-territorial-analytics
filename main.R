# =========================================================================
# PROYECTO: Ixtlán Territorial Analytics
# OBJETIVO: Orquestación completa del Pipeline (ETL + Visualización)
# AUTOR: Alan Aquino
# =========================================================================

# 1. Preparación del Entorno ----------------------------------------------
message(">>> Iniciando Pipeline de Ixtlán Territorial Analytics...")

# Cargar configuración estética y funciones globales
if (file.exists("scripts/00_config.R")) {
  source("scripts/00_config.R")
} else {
  stop("Error: No se encuentra scripts/00_config.R")
}

# 2. Fase ETL: Extracción y Limpieza (Raw -> Processed) -------------------
message(">>> Ejecutando Fase 01: Limpieza de datos...")
source("scripts/01_limpieza.R") 
# Al finalizar este paso, data/processed/ debería estar llena de .rds

# 3. Fase de Visualización (Processed -> Figures) -------------------------
message(">>> Ejecutando Fase 02: Generación de visualizaciones...")

scripts_visualizacion <- c(
  "scripts/02_economia.R",
  "scripts/03_demografia.R",
  "scripts/04_educacion.R",
  "scripts/05_censo_economico.R",
  "scripts/06_pobreza.R",
  "scripts/07_remesas.R"
)

for (script in scripts_visualizacion) {
  if (file.exists(script)) {
    message(paste(">>> Procesando:", script))
    source(script)
  } else {
    warning(paste("Advertencia: El script", script, "no fue encontrado."))
  }
}

message("=========================================================")
message(">>> PIPELINE FINALIZADO CON ÉXITO")
message(">>> Revisa la carpeta 'output/figures/' para ver los resultados.")
message("=========================================================")
