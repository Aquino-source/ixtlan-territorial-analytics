# -------------------------------------------------------------------------
# Pipeline de Limpieza: Ixtlán de Juárez Territorial Analytics
# -------------------------------------------------------------------------

# 0. Cargar librerías necesarias
library(tidyverse)
library(readxl)
library(lubridate)

# 1. CARGA Y FILTRADO INICIAL (ITER INEGI) --------------------------------
# Cargamos las bases crudas desde data/raw/
D10 <- read_xls("data/raw/ITER_20XLS10.xls")
D20 <- read_xlsx("data/raw/ITER_20XLSX20.xlsx")

# Filtrado específico para la localidad de Ixtlán de Juárez
ix10 <- D10 %>% filter(NOM_LOC == "Ixtlán de Juárez")
ix20 <- D20 %>% filter(MUN == "042", NOM_LOC == "Ixtlán de Juárez")

# 2. ECONOMÍA (Población Ocupada y Derechohabiencia) ----------------------
vars_econ <- c(
  "PEA", "PEA_M", "PEA_F", "PE_INAC", "PE_INAC_M", "PE_INAC_F", 
  "POCUPADA", "POCUPADA_M", "POCUPADA_F", "PDESOCUP", "PDESOCUP_M", "PDESOCUP_F",
  "PSINDER", "PDER_SS", "PDER_IMSS", "PDER_ISTE", "PDER_ISTEE", "PDER_SEGP"
)

clean_economia <- bind_rows(
  ix10 %>% select(all_of(vars_econ)) %>% mutate(Año = 2010),
  ix20 %>% select(all_of(vars_econ)) %>% mutate(Año = 2020)
) %>%
  pivot_longer(cols = -Año, names_to = "Variable", values_to = "Valor") %>%
  mutate(Valor = as.numeric(Valor))

saveRDS(clean_economia, "data/processed/clean_economia.rds")

# 3. PIRÁMIDE POBLACIONAL -------------------------------------------------
procesar_piramide <- function(df, year) {
  df %>%
    pivot_longer(
      cols = starts_with("P_") & ends_with(c("_M", "_F")), 
      names_to = "Variable", 
      values_to = "Pob_Texto"
    ) %>%
    mutate(
      Poblacion = suppressWarnings(as.numeric(Pob_Texto)),
      Grupo_Edad = case_when(
        grepl("3A5", Variable)    ~ "03-05 años",
        grepl("6A11", Variable)   ~ "06-11 años",
        grepl("12A14", Variable)  ~ "12-14 años",
        grepl("15A17", Variable)  ~ "15-17 años",
        grepl("18A24", Variable)  ~ "18-24 años",
        grepl("60YMAS", Variable) ~ "60+ años",
        TRUE ~ NA_character_
      ),
      Sexo = ifelse(grepl("_M", Variable), "Hombres", "Mujeres"),
      Anio = as.character(year)
    ) %>%
    filter(!is.na(Grupo_Edad) & !is.na(Poblacion)) %>%
    group_by(Anio, Grupo_Edad, Sexo) %>%
    summarise(Poblacion_Abs = sum(Poblacion), .groups = 'drop') %>%
    mutate(
      # Valor negativo para hombres para crear la forma de pirámide
      Valor_Grafico = ifelse(Sexo == "Hombres", -Poblacion_Abs, Poblacion_Abs)
    )
}

piramide_final <- bind_rows(procesar_piramide(ix10, 2010), procesar_piramide(ix20, 2020))
saveRDS(piramide_final, "data/processed/clean_piramide.rds")

# 4. MIGRACIÓN (Porcentajes sobre población total) ------------------------
pobtot_10 <- as.numeric(ix10$POBTOT)
pobtot_20 <- as.numeric(ix20$POBTOT)

procesar_migracion <- function(df, year, total_pop) {
  df %>%
    select(starts_with("PNACOE"), starts_with("PRESOE")) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor_Texto") %>%
    mutate(
      Absoluto = replace_na(suppressWarnings(as.numeric(Valor_Texto)), 0),
      Tipo_Migracion = case_when(
        grepl("PNACOE", Variable) ~ "Nacidos en Otra Entidad (Acumulado)",
        grepl("PRESOE", Variable) ~ "Residentes Anteriores en Otra Entidad (Reciente)"
      ),
      Sexo = case_when(
        grepl("_M", Variable) ~ "Hombres",
        grepl("_F", Variable) ~ "Mujeres",
        TRUE ~ "Total"
      ),
      Anio = as.character(year),
      Poblacion_Porcentaje = (Absoluto / total_pop) * 100
    )
}

migracion_final <- bind_rows(
  procesar_migracion(ix10, 2010, pobtot_10),
  procesar_migracion(ix20, 2020, pobtot_20)
)
saveRDS(migracion_final, "data/processed/clean_migracion.rds")

# 5. EDUCACIÓN ------------------------------------------------------------
cols_edu <- c("P15YM_AN", "P15YM_SE", "P15PRI_IN", "P15PRI_CO", "P15SEC_IN", "P15SEC_CO", "P18YM_PB")
total_15_10 <- as.numeric(ix10$P_15YMAS)
total_15_20 <- as.numeric(ix20$P_15YMAS)

clean_educacion <- bind_rows(
  ix10 %>% select(all_of(cols_edu)) %>% mutate(Año = 2010, Denom = total_15_10),
  ix20 %>% select(all_of(cols_edu)) %>% mutate(Año = 2020, Denom = total_15_20)
) %>%
  pivot_longer(cols = -c(Año, Denom), names_to = "Nivel_Escolaridad", values_to = "Pob_Texto") %>%
  mutate(
    Poblacion_Abs = replace_na(suppressWarnings(as.numeric(Pob_Texto)), 0),
    Poblacion_Porcentaje = (Poblacion_Abs / Denom) * 100,
    Anio = as.character(Año)
  )

saveRDS(clean_educacion, "data/processed/clean_educacion.rds")

# 6. CENSO ECONÓMICO (2014-2019) ------------------------------------------
ce14 <- read_csv("data/raw/ce2014_oax.csv")
ce19 <- read_csv("data/raw/ce2019_oax.csv")
diccionario <- read_csv("data/raw/diccionario_de_datos_ce.csv", skip = 5)
codigo_act <- read_csv("data/raw/tc_codigo_actividad.csv")

# Mapa de renombrado dinámico
mapa_ce <- diccionario %>% 
  select(nuevo = `Clave de la  unidad economica`, viejo = UE) %>% 
  tibble::deframe()

ce_clean <- bind_rows(
  ce14 %>% filter(MUNICIPIO == "042") %>% mutate(Año = 2014),
  ce19 %>% filter(MUNICIPIO == "042") %>% mutate(Año = 2019)
) %>%
  rename(any_of(mapa_ce)) %>%
  left_join(codigo_act, by = "CODIGO")

saveRDS(ce_clean, "data/processed/clean_censo_economico.rds")

# 7. NATALIDAD Y MORTALIDAD -----------------------------------------------
nym_raw <- read_xls("data/raw/Indicadores20250521170015, natalidad y mortalidad.xls", sheet = "pagina2")

clean_nym <- nym_raw %>%
  mutate(Indicador = case_when(
    grepl("Nacimientos registrados$", Indicador) ~ "Nacimientos Totales",
    grepl("Defunciones registradas$", Indicador) ~ "Defunciones Totales",
    grepl("Hombres.*Nacimientos", Indicador) ~ "Nacimientos Hombres",
    grepl("Mujeres.*Nacimientos", Indicador) ~ "Nacimientos Mujeres",
    TRUE ~ Indicador
  )) %>%
  pivot_longer(cols = -Indicador, names_to = "Año", values_to = "Valor") %>%
  mutate(Año = as.numeric(Año)) %>%
  filter(Año >= 2010 & Año <= 2020)

saveRDS(clean_nym, "data/processed/clean_natalidad_mortalidad.rds")

# 8. POBREZA (CONEVAL) ----------------------------------------------------
pob_raw <- read_xlsx("data/raw/Concentrado_indicadores_de_pobreza_2020.xlsx", sheet = "Hoja2")

clean_pobreza <- pob_raw %>%
  pivot_longer(
    cols = -Indicador,
    names_to = c("Tipo", "Año"),
    names_pattern = "(.*) (\\d{4})",
    values_to = "Valor"
  ) %>%
  mutate(Año = as.numeric(Año))

saveRDS(clean_pobreza, "data/processed/clean_pobreza.rds")

# 9. REMESAS (BANXICO) ----------------------------------------------------
clean_remesas <- read_xlsx("data/raw/Consulta_20250521-160242759, remesas.xlsx") %>%
  rename(Monto = SE42246) %>%
  mutate(
    Anio = year(Fecha),
    Trimestre = quarter(Fecha)
  )

saveRDS(clean_remesas, "data/processed/clean_remesas.rds")

message("=========================================================")
message(">>> PIPELINE DE LIMPIEZA COMPLETADO")
message(">>> Archivos generados en data/processed/")
message("=========================================================")
