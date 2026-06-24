# working directory
setwd("C:/Users/luism/OneDrive/Escritorio/curso/Dia 2 - Junio 16/02_pract") # set your own directory


# packages
library(terra)
library(dismo)
library(tibble)
library(ncdf4)
library(tidyverse)
library(geobr)
library(spocc)
library(CoordinateCleaner)
library(corrplot)


# occurrences

sp <- c("Leptodactylus fuscus") # species
db <- c("gbif")                 # repository (https://www.gbif.org/)

occ <- spocc::occ(query = sp, 
                  from = db, 
                  has_coords = TRUE, 
                  limit = 1e4)


# get data
occ <- occ %>%
  spocc::occ2df() %>% 
  dplyr::mutate(species = gsub(" ", "_", sp),
                lon = as.numeric(longitude),
                lat = as.numeric(latitude),
                year = lubridate::year(date),
                base = prov) %>% 
  dplyr::select(species, lon, lat, year) %>% 
  dplyr::filter(year > 1960, !is.na(year)) %>% 
  tidyr::drop_na(lon, lat)
  
# detect duplicate records, errors, outliers, etc
flags <- CoordinateCleaner::clean_coordinates(
  x = occ,
  species = "species",
  lon = "lon", 
  lat = "lat",
  seas_scale = 10, 
  tests = c("equal",       
            "outliers",    
            "seas",        
            "duplicates",
            "validity", # outside reference coordinate system
            "zeros" # plain zeros and lat = lon
  )
)

# exclude records flagged by any test
occ <- occ %>% 
  dplyr::filter(flags$.summary == TRUE) %>% 
  dplyr::select(species,lon,lat)


# spatial background
ras_bg <- terra::rast("data/ambient/macro/wc_bio_2.tif")
ras_bg[!is.na(ras_bg)]<-1

# allow only one point per cell (and now also remove points outside our target area [south America])

val_bg <- terra::extract(ras_bg, occ[, c("lon", "lat")]) # we identify the presence pixels with no ambient data (sea areas)
occ <- occ %>% 
  dplyr::mutate(land = val_bg[, 2]) %>%
  dplyr::mutate(cell_id = terra::cellFromXY(ras_bg, as.matrix(dplyr::select(., lon, lat)))) %>% 
  dplyr::filter(!is.na(cell_id) & !is.na(land)) %>%  # here we delete those points on sea areas 
  dplyr::distinct(species, cell_id, .keep_all = TRUE) %>%
  dplyr::mutate(occ = 1) %>% 
  dplyr::select(species, lon, lat, occ)

# pseudoabsences
occ_bg <- terra::spatSample(ras_bg, size = nrow(occ), method = "random", na.rm = TRUE, xy = TRUE) %>% 
  tibble::as_tibble() %>%
  dplyr::rename(lon=x, lat=y) %>% 
  dplyr::mutate(occ=0) %>% 
  dplyr::mutate(species="Leptodactylus_fuscus") %>% 
  dplyr::select(species, lon, lat, occ)

# merge points
occ <- rbind(occ,occ_bg)
occ$id <- 1:nrow(occ)

# check
plot(ras_bg,col='grey',legend=F)
points(occ[occ$occ==1,2:3],pch=16,col='blue',cex=0.5)
points(occ[occ$occ==0,2:3],pch=16,col='black',cex=0.5)



# load climatic variables (WroldClim; https://www.worldclim.org/data/bioclim.html)
vars <- terra::rast(c(bio02 = "data/ambient/macro/wc_bio_2.tif",    # Mean Diurnal Range temperature
                       bio03 = "data/ambient/macro/wc_bio_3.tif",   # Isothermality (BIO2/BIO7) (×100)
                       bio08 = "data/ambient/macro/wc_bio_8.tif",   # Mean Temperature of Wettest Quarter
                       bio15 = "data/ambient/macro/wc_bio_15.tif",  # Precipitation Seasonality
                       bio18 = "data/ambient/macro/wc_bio_18.tif")) # Precipitation of Warmest Quarter

names(vars) <- c("bio02", "bio03", "bio08", "bio15", "bio18")

# collinearity
corr_ras <- terra::spatSample(vars, size = 1000, na.rm = TRUE)
cor_matrix <- cor(corr_ras, method = "spearman")
print(cor_matrix)
corrplot::corrplot(cor_matrix, method = "color", type = "upper",            
                   addCoef.col = "black", tl.col = "black",          
                   diag = FALSE, number.cex = 0.8)


# extract ambient values
xy <- terra::extract(vars, occ[, c("lon", "lat")])

# merge points with ambient variables
occ <- occ %>% 
  dplyr::bind_cols(xy) %>% 
  dplyr::select(-ID)


# fit model (GLM model)
m_glm <- glm(occ ~ bio02 + bio03 + bio08 + bio15 + bio18,
             family='binomial', data=occ)

# Map predictions 
glm_ras <- terra::predict(vars, m_glm, type = 'response')
names(glm_ras) <- "GLM_pred"

#plot map prediction
terra::plot(glm_ras, main = "GLM Prediction Map", col = rev(hcl.colors(100, "Viridis")))

# ggplot
glm_ras_df <- as.data.frame(glm_ras, xy = TRUE)
ggplot(data = glm_ras_df) +
  geom_raster(aes(x = x, y = y, fill = GLM_pred)) +
  scale_fill_viridis_c(option = "viridis", direction = -1, 
                       name = "Probability", na.value = "transparent") +
  labs(title = "Habitat Suitability (GLM)",
       x = "Longitude", 
       y = "Latitude") +
  theme_bw()

# export
ggsave(filename = "sdm_L_fuscus.jpg", wi = 12.5, he = 15, un = "cm", dpi = 200)

# end---