source("scripts/00_config.R")
library(tidyverse)

df_econ <- readRDS("data/processed/clean_economia.rds")

# Gráfico de PEA vs PNEA
p1 <- df_econ %>% 
  filter(Variable %in% c("PEA", "PE_INAC")) %>%
  ggplot(aes(x = factor(Año), y = Valor, fill = Variable)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("PEA" = paleta_ixtlan["principal"], 
                                "PE_INAC" = paleta_ixtlan["neutral"])) +
  tema_ixtlan() +
  labs(title = "Evolución de la PEA", y = "Personas")

guardar_grafica(p1, "economia_pea.png")
