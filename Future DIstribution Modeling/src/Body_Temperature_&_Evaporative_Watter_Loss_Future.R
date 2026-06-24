#
# Body temperature (Tb) and evaporative water loss (EWL) calculation (for Boyaca) for future
#


# working directory
setwd("C:/.../Dia 5 - Junio 19/05_pract/02_my_fut_microclim") # set your own directory



# library
library(tidyverse)
library(terra)


# load biiophysical function
source("data/extra.R")



# load your created microclimatic data (they contain 288 layers, 24 layers per month)
ground_0cm_ras <- rast("fut_microclim/Soil(subst_temp)/D0cm_soil_all.nc")[[19]] # layer 19 = 19:00 h, January
RH_ras <- rast("fut_microclim/RH(relat_hum)/RH1cm_all.nc")[[19]]                # layer 19 = 19:00 h, January
solr_radt_ras <- rast("fut_microclim/SOLR/SOLR_all.nc")[[19]]                   # layer 19 = 19:00 h, January
tair_ras <- rast("fut_microclim/TA(air_Temp)/TA1cm_all.nc")[[19]]               # layer 19 = 19:00 h, January
wind_ras <- rast("fut_microclim/V1cm/V1cm_all.nc")[[19]]                        # layer 19 = 19:00 h, January


# joint ambient data to data frame
env_stack <- c(ground_0cm_ras, RH_ras, solr_radt_ras, tair_ras, wind_ras)
names(env_stack) <- c("ground_0cm", "RH", "solr_radt", "tair", "wind")
micro_env2 <- as.data.frame(env_stack, cells = TRUE, na.rm = TRUE)

# animal information

body_mass <- as.numeric(57)  # body mass (g)
skin_res <- as.numeric(2.46) # skin resistance (s*cm)
CTmin <- 7.24                # critical thermal minimum
CTmax <- 40.02               # critical thermal maximum
B80_lower <- 21.58           # Lower end of B80 (performance breadth)
B80_upper <- 30.08           # upper end of B80 (performance breadth)

# prepare for calculations 
micro_env2$mass <- body_mass
micro_env2$surface <- round((9.8537 * micro_env2$mass^0.6745/10000), digits = 4) # expected body surface
micro_env2$Rs <- skin_res*100
micro_env2$heat_cap <- as.numeric(3.6)
micro_env2$model_Tb <- as.numeric(25) # column to store estimated body temperatures
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
       title = "Estimated Body Temperature at 19:00 h, January, 2041-2060") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5))

# export
ggsave(filename = "mechanistic_Tb_fut.jpg", wi = 16, he = 17, un = "cm", dpi = 200)



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
       title = "Estimated Evaporative Water Loss at 19:00 h, January, 2041-2060") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5))

# export
ggsave(filename = "mechanistic_ewl_fut.jpg", wi = 16, he = 17, un = "cm", dpi = 200)

#












# SUITABILITY BASED ON ORGANISMAL PERFORMANCE (TPC)

# first, we will relativize the organismal thermal performance
micro_env2 <- micro_env2 %>% 
  mutate(perform = case_when(
    model_Tb < CTmin ~ 0,                                 # if Tb < CTmin, then 0% suitability
    model_Tb > CTmax ~ 0,                                 # if Tb > CTmax, then 0% suitability
    model_Tb >= B80_lower & model_Tb <= B80_upper ~ 1,    # if Tb within B80, then 100% suitability
    model_Tb >= CTmin & model_Tb < B80_lower ~            # relative ascending performance TPC from CTmin towards B80
      (model_Tb - CTmin) / (B80_lower - CTmin),           
    model_Tb > B80_upper & model_Tb <= CTmax ~            # relative descending TPC surpassing B80 towards CTmax
      (CTmax - model_Tb) / (CTmax - B80_upper)            
  ))

# rasterize the calculated performance suitabilityhttp://127.0.0.1:30013/graphics/plot_zoom_png?width=760&height=854
mechanistic_perform <- rast(ground_0cm_ras)
values(mechanistic_perform) <- NA
names(mechanistic_perform) <- "suitability"
mechanistic_perform[micro_env2$cell] <- micro_env2$perform
plot(mechanistic_perform$suitability)

# plot performance suitability
g_mechanistic_perform <- as.data.frame(mechanistic_perform, xy = TRUE) 
g_mechanistic_perform <- na.omit(g_mechanistic_perform)

ggplot() +
  geom_raster(data = g_mechanistic_perform, aes(x = x, y = y, fill = suitability)) +
  scale_fill_viridis_c(name = "Suitability", option = "viridis", direction = -1,  
    na.value = "transparent") +
  labs(x = "Longitude", y = "Latitude", 
       title = "Estimated Habitat Suitability (Performance) at 19:00 h, Jan, 2041-2060") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5))

# export
ggsave(filename = "mechanistic_relative_perf_fut.jpg", wi = 16, he = 17, un = "cm", dpi = 200)



# What about if we export relative performance sutability (and the others) as rasters as well

dir.create("physio_rasters")
terra::writeRaster(mechanistic_perform, filename = "physio_rasters/mechanistic_relative_perf_fut.tif", 
                   overwrite = TRUE)
terra::writeRaster(mechanistic_Tb, filename = "physio_rasters/mechanistic_Tb_fut.tif", overwrite = TRUE)
terra::writeRaster(mechanistic_ewl, filename = "physio_rasters/mechanistic_ewl_fut.tif", overwrite = TRUE)

# end---