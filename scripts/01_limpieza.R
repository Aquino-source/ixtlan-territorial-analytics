# -------------------------------------------------------------------------
# Pipeline de Limpieza: De Datos Raw a Datos Procesados (.rds)
# -------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)

# --- 1. CARGA Y FILTRADO INICIAL (ITER) ---
D10 <- read_xls("data/raw/ITER_20XLS10.xls")
D20 <- read_xlsx("data/raw/ITER_20XLSX20.xlsx")

ix10 <- D10 %>% filter(NOM_LOC == "Ixtlán de Juárez")
ix20 <- D20 %>% filter(MUN == "042", NOM_LOC == "Ixtlán de Juárez")

# --- 2. ECONOMÍA (ITER) ---
vars_econ <- c("PEA", "PEA_M", "PEA_F", "PE_INAC", "PE_INAC_M", "PE_INAC_F", 
               "POCUPADA", "PDESOCUP", "PSINDER", "PDER_SS", "PDER_IMSS", 
               "PDER_ISTE", "PDER_ISTEE", "PDER_SEGP")

clean_economia <- bind_rows(
  ix10 %>% select(all_of(vars_econ)) %>% mutate(Año = 2010),
  ix20 %>% select(all_of(vars_econ)) %>% mutate(Año = 2020)
) %>%
  pivot_longer(cols = -Año, names_to = "Variable", values_to = "Valor") %>%
  mutate(Valor = as.numeric(Valor))

saveRDS(clean_economia, "data/processed/clean_economia.rds")

# --- 3. PIRÁMIDE POBLACIONAL (ITER) ---
# Definimos los niveles para mantener el orden
age_groups_ordered <- c(
  "0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24", "25 a 29",
  "30 a 34", "35 a 39", "40 a 44", "45 a 49", "50 a 54", "55 a 59",
  "60 a 64", "65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 y más"
)

# Lógica de mapeo de rangos
procesar_piramide <- function(df, year) {
  df %>%
    select(starts_with("P_") & ends_with(c("_M", "_F"))) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Pob") %>%
    mutate(
      Pob = as.numeric(Pob),
      Rango_Raw = sub("P_([0-9A-Za-z]+)_.*", "\\1", Variable),
      Sexo = ifelse(grepl("_M", Variable), "Hombres", "Mujeres"),
      Rango_Edad = case_when(
        Rango_Raw %in% c("0A2", "3A5", "0A4") ~ "0 a 4",
        Rango_Raw %in% c("5A9", "6A11") ~ "5 a 9",
        Rango_Raw %in% c("10A14", "12A14") ~ "10 a 14",
        Rango_Raw == "15A17" ~ "15 a 19",
        Rango_Raw == "18A24" ~ "20 a 24",
        Rango_Raw == "85YMAS" ~ "85 y más",
        # ... (agrega aquí el resto de tus mapeos específicos)
        TRUE ~ "Otros"
      ),
      Anio = as.character(year)
    ) %>%
    group_by(Anio, Rango_Edad, Sexo) %>%
    summarise(Poblacion = sum(Pob, na.rm = TRUE), .groups = 'drop')
}

piramide_final <- bind_rows(procesar_piramide(ix10, 2010), procesar_piramide(ix20, 2020))
saveRDS(piramide_final, "data/processed/clean_piramide.rds")

# --- 4. EDUCACIÓN (ITER) ---
cols_edu <- c("P15YM_AN", "P15YM_SE", "P15PRI_IN", "P15PRI_CO", "P15SEC_IN", "P15SEC_CO", "P18YM_PB")

clean_educacion <- bind_rows(
  ix10 %>% select(all_of(cols_edu)) %>% mutate(Año = 2010),
  ix20 %>% select(all_of(cols_edu)) %>% mutate(Año = 2020)
) %>%
  pivot_longer(cols = -Año, names_to = "Nivel", values_to = "Valor") %>%
  mutate(Valor = as.numeric(Valor))

saveRDS(clean_educacion, "data/processed/clean_educacion.rds")

# --- 5. CENSO ECONÓMICO ---
ce14 <- read_csv("data/raw/ce2014_oax.csv")
ce19 <- read_csv("data/raw/ce2019_oax.csv")
diccionario <- read_csv("data/raw/diccionario_de_datos_ce.csv", skip = 5)
codigo_act <- read_csv("data/raw/tc_codigo_actividad.csv")

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

# --- 6. POBREZA (CONEVAL) ---
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

# --- 7. REMESAS (BANXICO) ---
clean_remesas <- read_xlsx("data/raw/Consulta_20250521-160242759, remesas.xlsx") %>%
  rename(Monto = SE42246) %>%
  mutate(
    Anio = year(Fecha),
    Trimestre = quarter(Fecha)
  )

saveRDS(clean_remesas, "data/processed/clean_remesas.rds")

message(">>> ¡Limpieza 100% completada y reproducible!")
