# ============================================================================== #
#           PREPARACIÓN DEL CURSO: INSTALACIÓN DE PAQUETES                       #
# ============================================================================== #

# Para nuestro curso, usaremos algunos paquetes que precisan ser instalados en R

# INSTRUCCIONES IMPORTANTES ANTES DE EMPEZAR:
# 1. Necesitas estar conectado a internet.
# 2. R va a imprimir MUCHO texto rojo en la consola mientras descarga. 
#    NO TE ASUSTES, el texto rojo es normal, R solo te está contando lo que hace mientras lo hace.
# 3. Este proceso puede tardar entre 5 y 15 minutos, dependiendo de tu internet.
#   

# Si durante la instalación R te hace una pregunta en la consola que dice:
# "Do you want to install from sources the package which needs compilation? (Yes/no/cancel)"
# Escribe la palabra "no" (sin comillas) en la consola (panel de abajo) y presiona Enter.


# ---
# PASO 1: INSTALAR LOS PAQUETES ESTÁNDAR (CRAN)
# ---

# recuerda, para correr cada pedazo de código, presiona Ctrl + Enter.

paquetes_necesarios <- c(
  "tidyverse",    # paquete para manejar datos (incluye dplyr, ggplot2)
  "terra",        # herramienta para trabajar con mapas (rasters)
  "raster",       # herramienta clásica para mapas (necesaria para microclimas)
  "ncdf4",        # abrir archivos climáticos en formato netCDF (.nc)
  "dismo",        # Para modelos correlativos de distribución de especies
  "geobr",        # Para descargar mapas y fronteras
  "spocc",        # Para descargar datos de ocurrencia de especies (GBIF)
  "CoordinateCleaner", # Para lidiar con coordenadas de las especies
  "corrplot",     # Para hacer gráficos de correlación
  "mgcv",         # Para crear modelos estadísticos (GAMs)
  "rTPC",         # Para crear Curvas de desempenho térmico
  "nls.multstart",# Para ajustar Curvas de desempenho térmico
  "devtools"      # Necesario para descargar paquetes desde GitHub
)

# Este código revisa cuáles paquetes te faltan y solo instala esos:
paquetes_faltantes <- paquetes_necesarios[!(paquetes_necesarios %in% installed.packages()[,"Package"])]

if(length(paquetes_faltantes)) {
  cat("\n¡Alerta! Faltan los siguientes paquetes y se van a instalar:", paquetes_faltantes, "\n")
  install.packages(paquetes_faltantes)
} else {
  cat("\nTodos los paquetes ya están instalados.\n")
}


# ---
# PASO 2: INSTALAR NicheMapR
# ---
# NicheMapR es un modelo biofísico y microclimático. 
# Como está en constante actualización, no se descarga de la forma 
# tradicional, sino directamente desde el GitHub de sus creadores.

# Ejecuta esta línea.
devtools::install_github('mrke/NicheMapR')


# NOTA PARA USUARIOS DE WINDOWS:
# Si la instalación de NicheMapR falla y te sale un error mencionando "Rtools",
# significa que tu computadora necesita un programa extra de R para compilar el modelo.
# No te preocupes, sigue estos pasos:
# 1. Ve a esta página: https://cran.r-project.org/bin/windows/Rtools/
# 2. Descarga e instala Rtools como si fuera un programa normal de Windows.
# 3. Cierra R, vuelve a abrirlo e intenta correr la línea (devtools::install_github('mrke/NicheMapR')) de nuevo.


# ---
# PASO 3: VERIFICACIÓN FINAL
# ---
# Vamos a comprobar que los paquetes se hayan instalado bien.
# Si ejecutas estas líneas y NO sale ningún error, estás todo ok

library(tidyverse)
library(terra)
library(NicheMapR)
library(raster)
library(ncdf4)
library(dismo)
library(geobr)
library(spocc)
library(CoordinateCleaner)
library(corrplot)
library(mgcv)
library(rTPC)
library(nls.multstart)
library(devtools)

# end---