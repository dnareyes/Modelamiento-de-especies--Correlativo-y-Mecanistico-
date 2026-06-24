# ============================================================================== #
#            PRE-CURSO DE R: GUÍA DE SUPERVIVENCIA PARA EL DÍA 1                 #
# ============================================================================== #


# ¡Bienvenidos! Estamos muy emocionados de tenerlos en el curso de modelado mecanicista.
# 
# Se que R puede parecer intimidante a primera vista (pero voy a intentar disminuir el impacto inicial) 
# NO necesitan memorizar nada de esto. El objetivo de este script es simplemente 
# presentarles la gramática básica de R para que se sientan cómodos (o talvez menos intimidados) cuando 
# empecemos con las prácticas.
#
# CÓMO USAR ESTE SCRIPT (SCRIPT = líneas de código):
# Lean los comentarios (el texto en gris que comienza con un '#'). 
# Para ejecutar (correr) una línea de código, hagan clic en esa línea y presionen:
#   - Windows: Ctrl + Enter
#   - Mac: Cmd + Return
#   - Alternativamente, pueden clicar en el botón 'Run', arriba, en la esquina derecha de este panel  


# ------------------------------------------------------------------------------ #
# "HABLANDO" CON R (OPERACIONES BÁSICAS Y OBJETOS)
# ------------------------------------------------------------------------------ #

# R es, en el fondo, ES una calculadora gigante y muy potente. Hagamos algo de matemática para enteder la ideia:
2 + 2
10 / 2
5 ^ 2  # El símbolo ^ significa "elevado a" (5 al cuadrado)


# En R, no solo calculamos cosas; las podemos guardar como "Objetos" (esto nos permite usar despues ese valor).
# Usamos el símbolo de flecha ( <- ) para asignar un valor a un nombre (el objeto). 
# Piensen en ello como poner un número dentro de una caja con una etiqueta.
masa_rana <- 57
temp_rana <- 25.5 # pueden ver que estos objetos (masa_rana y temp_rana) ahora aparecen en el panel del lado derecho

# ¡Ahora R recuerda esos valores! Podemos hacer matemáticas usando esos objetos:
masa_rana * 2

# Si cometen un error o su espacio de trabajo está muy desordenado, pueden borrar un 
# objeto usando la función remover: rm()
rm(masa_rana)    # ahora el objeto masa_rana ya no se encuentra más disponible

# los objetos también pueden guardar texto
mi_triste_realidad <- "soy feo"





# ------------------------------------------------------------------------------ #
# INSTALAR Y CARGAR PAQUETES 
# ------------------------------------------------------------------------------ #

# R viene con herramientas básicas, pero para analisis más complejos (como hacer mapas), 
# necesitamos descargar cajas de herramientas adicionales llamadas "Paquetes". 

# Primero, solo se instala un paquete una única vez. 
# Quiten el '#' de las líneas de abajo y ejecútenlas para instalar esos tres paquetes:
# install.packages("terra")
# install.packages("dplyr")
# install.packages("ggplot2")

# Cada vez que abran R, deben cargar los paquetes para poder usarlos.
# Hacemos esto usando la función library(). Ejecuten estas líneas ahora:
library(terra)      # Para mapas y datos espaciales
library(dplyr)      # Para organizar y filtrar datos
library(ggplot2)    # Para hacer gráficos





# ------------------------------------------------------------------------------ #
# EL DIRECTORIO DE TRABAJO (osea ¿DÓNDE ESTÁN MIS ARCHIVOS?)
# ------------------------------------------------------------------------------ #

# R es como si estuviera ciego, no sabe dónde están sus archivos a menos que se lo digan.
# Usamos setwd() para "Configurar el Directorio de Trabajo" (la carpeta que R mirará cuando carquemos [o salvemos] un archivo).

# Windows usa barras invertidas para indicar la ubicación de una carpeta ( C:\Usuarios\mi_carpeta ). 
# Sin embargo, R solamente entiende las barras normales ( C:/Usuarios/mi_carpeta ). 
# Siempre deben cambiar las barras manualmente cuando copien y peguen aqui en R la ruta de su carpeta 


# Por ejemplo, aqui cambien la ruta de abajo a la carpeta donde guardarán (en su computador) los archivos del curso:
path_wd <- "C:/Pongan/Su/Ruta/Aqui/Dia 1 - Junio 15"
setwd(path_wd)

# Para comprobar si funcionó, pregúntenle a R qué carpeta está mirando actualmente:
getwd()





# ------------------------------------------------------------------------------ #
# LAS CUATRO FORMAS DE LOS DATOS
# ------------------------------------------------------------------------------ #
# Durante el curso, usaremos diferentes "formas" de datos. Vamos a conocerlas:

# 1. VECTORES (Una simple línea de datos)
# Usamos c() que significa "combinar" para hacer un vector.
temperaturas_rana <- c(20.1, 22.5, 25.0, 27.3, 30.1)
temperaturas_rana # queda asi

# 2. DATA FRAMES (Como una hoja de cálculo de Excel)
# Este es el formato más común. Tiene filas y columnas.
mis_datos <- data.frame(
  especie = c("Rana_A", "Rana_B", "Rana_C"),
  temp = c(22.5, 25.0, 28.1)
)
print(mis_datos)

# Pueden extraer una sola columna de un data frame usando el signo de dólar ($)
mis_datos$temp 

# 3. MATRICES (Una cuadrícula estricta de números [sip, matrices solo aceptan números])
# si quieren almacenar texto también, tienen que usar Data Frames (que pueden tener texto)
mi_matriz <- matrix(1:9, nrow = 3, ncol = 3)
print(mi_matriz)

# 4. LISTAS
# Una lista es un objeto que puede contener CUALQUIER COSA. Puede contener un data frame, 
# un vector y una matriz, todo al mismo tiempo (es muy versátil)
mi_lista <- list(mis_datos, temperaturas_rana, mi_matriz, mi_triste_realidad)
mi_lista





# ------------------------------------------------------------------------------ #
# EL MÁGICO "PIPE" ( %>% )
# ------------------------------------------------------------------------------ #

# En este curso, verán este símbolo un montón:  %>% 
# Se llama "Pipe" (tubo o pipa), y se traduce literalmente como la frase: "Y LUEGO..."
# Nos permite leer el código como si fuera una receta sin crear mil objetos nuevos para cada paso intermediario en una tarea.

# Ejemplo de cómo leer un código con pipe:
# Toma mis_datos Y LUEGO fíltralos para mostrar solo las ranas con temperaturas arriba de 24 grados
datos_filtrados <- mis_datos %>% 
  filter(temp > 24)

print(datos_filtrados)






# ------------------------------------------------------------------------------ #
# DATOS ESPACIALES (MAPAS Y RASTERS)
# ------------------------------------------------------------------------------ #

# En el modelado mecanicista, trabajamos con mapas llamados "Rasters". Un raster es 
# simplemente una matriz gigante puesta sobre la tierra, donde cada píxel tiene un 
# valor específico (como temperatura o elevación).

# Carguemos el mapa del departamento de Boyacá (en formato raster) usando la función rast().
# (Asegúrense de que su directorio de trabajo esté colocado en su computadora dentro de la carpeta Dia 1 - Junio 15)

mapa_boyaca <- rast("data/boyaca_ras_2_5m.tif")

# Inspeccionemos el mapa. Aqui podemos ver la resolución, la extensión y el tamaño del mapa
print(mapa_boyaca)

# vamos a graficarlo (plot) para verlo visualmente (podemos agregar una paleta de colores).
plot(mapa_boyaca, main = "Raster del departamento de Boyacá", col = terrain.colors(50))

# también podemos aprovechar para cargar (e comparar) las diferencias para un mapa más fino de la misma región
mapa_boyaca_30s <- rast("data/boyaca_ras_30s.tif")
print(mapa_boyaca_30s)
plot(mapa_boyaca_30s, main = "Raster del departamento de Boyacá (alta resolución)", col = terrain.colors(50))




# ------------------------------------------------------------------------------ #
# AUTOMATIZACIÓN (EL 'FOR LOOP')
# ------------------------------------------------------------------------------ #

# Si necesitamos calcular la temperatura corporal de una rana para 10,000 píxeles 
# en un mapa, no escribimos la ecuación 10,000 veces. Usamos un for loop para automatizar ese proceso.

# Osea, el for loop le dice a R: "Para cada elemento en esta secuencia, haz esta acción."
# Corramos un for loop simple que imprima un cálculo sequencial para 6 días:

for (dia in 1:6) {
  cat("Calculando datos para el Día", dia, "...\n")
  
  # Un cálculo falso y simple (apenas con 6 cálculos) para que no demore más de 1 segundo para acabar el for loop
  temperatura_falsa <- 20 + dia 
  cat("La temperatura es de", temperatura_falsa, "grados.\n\n")
}

# En el curso, en lugar de "1:6", usaremos for loop (tambien llamados de loopings) para correr 
# ecuaciones de balance térmico a través de miles de píxeles en el mapa (osea, estos cálculos de loopings pueden demorar 
# mucho para ser concluidos)



# al terminar un analisis, para guardar todo, precisas clicar en el ícono 'save' (ícono en formato de disquete azul),
# arriba, en la esquina isquierda. El código, junto con los objetos creados (.RData) serán guardados en el directorio que especificaste
# al principio (en este caso, en la carpeta Dia 1 - Junio 15 de tu computadora). Aqui:
getwd()



# ============================================================================== #
# Si ejecutaste este script con éxito, ya tienes algunas de las habilidades 
# básicas necesarias para el Día 1 y 2. Nos vemos en clase!
# ============================================================================== #