#
# Body temperature (Tb) and evaporative water loss (EWL) calculation 
#


# working directory
setwd("C:/.../curso/Dia 3 - Junio 17/03_pract") # set your own directory



# library
library(tidyverse)
library(terra)


# load biiophysical function
source("data/extra.R")



# load climate data (https://www.worldclim.org/data/worldclim21.html)
ground_0cm_ras <- rast("data/ambient/ground_0cm.tif")
RH_ras <- rast("data/ambient/RH.tif")
solr_radt_ras <- rast("data/ambient/solr_radt.tif")
tair_ras <- rast("data/ambient/tair.tif")
wind_ras <- rast("data/ambient/wind.tif")


# joint ambient data to data frame
env_stack <- c(ground_0cm_ras, RH_ras, solr_radt_ras, tair_ras, wind_ras)
names(env_stack) <- c("ground_0cm", "RH", "solr_radt", "tair", "wind")
micro_env2 <- as.data.frame(env_stack, cells = TRUE, na.rm = TRUE)

# animal information

body_mass <- as.numeric(57)  # body mass (g)
skin_res <- as.numeric(2.46) # skin resistance (s*cm)


# prepare for calculations 
micro_env2$mass <- body_mass
micro_env2$surface <- round((9.8537 * micro_env2$mass^0.6745/10000), digits = 4) # expected body surface
micro_env2$Rs <- skin_res*100
micro_env2$heat_cap <- as.numeric(3.6)
micro_env2$model_Tb <- as.numeric(25) # column to store estimated body temperatures (25C is just a random Tb for starting the calculation)
micro_env2$model_ewl <- NA            # column to store estimated evaporative water loss



# heat budget model
for (i in 1:nrow(micro_env2)) {
  heat_output <- theatmodel(Tb = micro_env2$model_Tb[i], 
                    A = micro_env2$surface[i], 
                    M = micro_env2$mass[i],      
                    Ta = micro_env2$tair[i], 
                    Tg = micro_env2$ground_0cm[i], 
                    S = micro_env2$solr_radt[i],  
                    v = micro_env2$wind[i], 
                    HR = micro_env2$RH[i], 
                    skin_humidity = 1, 
                    r = micro_env2$Rs[i], 
                    posture = 1, 
                    vent = 1/3,                                # body surface in contact with ground
                    delta = 3600, 
                    C = micro_env2$heat_cap[i])                # default value
  micro_env2$model_Tb[i] <- round(heat_output[1], digits = 2)  # store estimated body temperatures
  micro_env2$model_ewl[i] <- round(heat_output[2], digits = 8) # store estimated evaporative water loss
  # Convertir EWL de gramos por segundo (g s-1) a gramos por hora (g h-1)
  micro_env2$model_ewl_h <- micro_env2$model_ewl * 3600
  rm(heat_output)                                              # remove to save memory space
}


# rasterize the calculated Tb 
mechanistic_Tb <- rast(ground_0cm_ras) # we copy a new raster
values(mechanistic_Tb) <- NA           # now, empty that copied raster
names(mechanistic_Tb) <- "Tb"          # change the layer name
mechanistic_Tb[micro_env2$cell]  <- micro_env2$model_Tb # now fill the empty raster with calculated Tb


# rasterize the calculated ewl
mechanistic_ewl <- rast(ground_0cm_ras)
values(mechanistic_ewl) <- NA
names(mechanistic_ewl) <- "ewl"
mechanistic_ewl[micro_env2$cell] <- micro_env2$model_ewl_h


# plot body temperature
g_mechanistic_Tb <- as.data.frame(mechanistic_Tb, xy = TRUE) 
g_mechanistic_Tb <- na.omit(g_mechanistic_Tb)

ggplot() +
  geom_raster(data = g_mechanistic_Tb, aes(x = x, y = y, fill = Tb)) +
  scale_fill_gradientn(
    name = "Tb (°C)", 
    colours = c("#3E049C", "#2042C0", "#2DB1EA", "#57D460", "#F0E13C", "#E36724", "#900000"),
    na.value = "transparent" 
  ) +
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "Estimated Body Temperature") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5))

# export
ggsave(filename = "mechanistic_Tb.jpg", wi = 14, he = 17, un = "cm", dpi = 200)



# plot evaporative water loss
g_mechanistic_ewl <- as.data.frame(mechanistic_ewl, xy = TRUE) 
g_mechanistic_ewl <- na.omit(g_mechanistic_ewl)

ggplot() +
  geom_raster(data = g_mechanistic_ewl, aes(x = x, y = y, fill = ewl)) +
  scale_fill_gradientn(
    name = "ewl (g h-1)", 
    colours = c("#E1F5FE", "#81D4FA", "#29B6F6", "#0288D1", "#01579B"),
    na.value = "transparent",
    labels = function(x) format(x, scientific = FALSE)
  ) +
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "Estimated Instantaneous Evaporative Water Loss") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5))

# export
ggsave(filename = "mechanistic_ewl.jpg", wi = 14, he = 17, un = "cm", dpi = 200)

# end---