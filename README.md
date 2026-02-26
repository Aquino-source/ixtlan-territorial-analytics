# Ixtlán Territorial Analytics: Radiografía Socioeconómica

Este repositorio contiene un análisis integral de la estructura socioeconómica y territorial de **Ixtlán de Juárez, Oaxaca**. A través del procesamiento de fuentes oficiales, este proyecto busca desmenuzar las dinámicas de desarrollo de una de las comunidades forestales más emblemáticas de México, integrando una perspectiva económica y de ciencia de datos.

Originalmente desarrollado como parte del Capítulo 3 de mi tesis de Economía en la **UNAM**, este espacio evoluciona hacia un pipeline de datos automatizado, enfocado en la reproducibilidad y la visualización avanzada.

## 📊 Dimensiones del Análisis
El proyecto realiza un "rayos X" de la comunidad a través de los siguientes módulos:
* **Economía Local:** Análisis del ITER y Censos Económicos (2014, 2019).
* **Demografía:** Dinámicas de natalidad y mortalidad.
* **Bienestar:** Indicadores de pobreza y rezago educativo.
* **Flujos Financieros:** Monitoreo trimestral de remesas (Banxico).
* **Movilidad:** Datos de migración y flujos poblacionales.

## 🛠️ Estructura del Proyecto
El repositorio está organizado de forma modular para garantizar que el análisis sea escalable y fácil de mantener:

```text
ixtlan-territorial-analytics/
├── data/
│   ├── raw/            # Datos originales (INEGI, Banxico, ITER) sin modificar.
│   └── processed/      # Versiones limpias y estructuradas listas para análisis.
├── scripts/
│   ├── 00_config.R     # Configuración global (estética, paleta de colores y temas).
│   ├── 01_limpieza.R   # Pipeline de procesamiento (Raw -> Processed).
│   ├── 02_economia.R   # Scripts sectoriales de análisis y graficación.
│   └── ...             
├── output/
│   └── figures/        # Visualizaciones finales en alta resolución (300 dpi).
├── main.R              # Script orquestador que ejecuta el pipeline completo.
└── README.md           # Bitácora y documentación del proyecto.

```

Autor: Alan Aquino
Economista | Analista de Datos
