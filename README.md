# Modelamiento de Especies: Correlativo y Mecanístico

## Descripción

Este repositorio contiene scripts y recursos desarrollados en **R** para el **Modelamiento de Distribución de Especies (SDM)**, abarcando tanto enfoques correlativos como mecanísticos. El proyecto explora cómo las condiciones ambientales actuales y futuras afectan la distribución de los organismos, analizando también factores físicos y fisiológicos como el intercambio de calor y la sensibilidad térmica a nivel de microambientes.

## 📂 Estructura del Repositorio

El repositorio está organizado en las siguientes carpetas temáticas:

* **`Starting with R/`**: Introducción y conceptos básicos orientados al manejo de datos espaciales y fundamentos de programación en R para ecología.
* **`Correlative Species Distribution Modeling/`**: Implementación de modelos correlativos de nicho ecológico. Estos scripts relacionan datos de presencia/ausencia de especies con variables ambientales macroclimáticas para predecir áreas de idoneidad.
* **`Future Distribution Modeling/`**: Proyecciones de la distribución espacial de especies bajo diferentes escenarios de cambio climático futuros, evaluando el impacto del calentamiento global en la biodiversidad.
* **`Microenvironment/`**: Análisis y modelado de datos microclimáticos. Define los hábitats a una escala de mayor resolución, relevante de manera directa para los organismos.
* **`Heat exchange and mass, organish thermal sensitivity/`**: Enfoque de modelamiento mecanístico (biofísico). Evaluación del equilibrio térmico (intercambio de calor y masa) para comprender la sensibilidad térmica, requerimientos y los límites fisiológicos de las especies.

## 🛠️ Requisitos e Instalación

Para ejecutar los scripts de este repositorio, necesitarás:

1.  **Software:** [R](https://cran.r-project.org/) (versión 4.0 o superior recomendada) y un entorno de desarrollo como [RStudio](https://posit.co/download/rstudio-desktop/).
2.  **Paquetes de R:** El flujo de trabajo requiere las siguientes librerías especializadas. Puedes instalarlas ejecutando este bloque de código en tu consola:

```R
# Definir los paquetes necesarios
paquetes_necesarios <- c(
  "tidyverse",        # paquete para manejar datos (incluye dplyr, ggplot2)
  "terra",            # herramienta para trabajar con mapas (rasters)
  "raster",           # herramienta clásica para mapas (necesaria para microclimas)
  "ncdf4",            # abrir archivos climáticos en formato netCDF (.nc)
  "dismo",            # Para modelos correlativos de distribución de especies
  "geobr",            # Para descargar mapas y fronteras
  "spocc",            # Para descargar datos de ocurrencia de especies (GBIF)
  "CoordinateCleaner",# Para lidiar con coordenadas de las especies
  "corrplot",         # Para hacer gráficos de correlación
  "mgcv",             # Para crear modelos estadísticos (GAMs)
  "rTPC",             # Para crear Curvas de desempeño térmico
  "nls.multstart",    # Para ajustar Curvas de desempeño térmico
  "devtools"          # Necesario para descargar paquetes desde GitHub
)

# Instalar los paquetes que falten
paquetes_nuevos <- paquetes_necesarios[!(paquetes_necesarios %in% installed.packages()[,"Package"])]
if(length(paquetes_nuevos)) install.packages(paquetes_nuevos)
```

## 🚀 Cómo Empezar

1.  Clona este repositorio en tu máquina local:
    ```bash
    git clone https://github.com/dnareyes/Modelamiento-de-especies--Correlativo-y-Mecanistico-.git
    ```
2.  Abre tu IDE de R (ej. RStudio) y establece tu directorio de trabajo en la carpeta descargada.
3.  Ejecuta el script de instalación de paquetes de la sección anterior.
4.  Si no tienes experiencia previa con R en este contexto, comienza explorando los scripts.
5.  Luego, puedes avanzar hacia los modelos correlativos y finalmente integrar los conceptos fisiológicos y microclimáticos de los modelos mecanísticos.
