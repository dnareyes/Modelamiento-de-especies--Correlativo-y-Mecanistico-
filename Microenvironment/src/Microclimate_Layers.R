
# ---

# create microclimate layers based on current climate database

# ---


# directory paths

# working directory
path_wd <- "C:/Users/luism/OneDrive/Escritorio/curso/Dia 4 - Junio 18/04_pract/2_5m"

# reference to target area
path_range_sp <- "data/boyaca_ras_2_5m.tif"

# my created global climate for preset (Boyaca)
path_my_global <- ("data/my_global")

# path to copy my current global climate
path_path <- "path"




# working directory
setwd(path_wd)

# library
library(NicheMapR)
library(raster)
library(ncdf4)
library(dplyr)





# My custom-made extra material

# function to subset hourly microclimates
my_nHour <- function(data, nSubsets, nSkip){
  lapply(1:nSubsets, function(n) data[seq(n, NROW(data), by = nSkip), ])
}

# function to round dataframe values
my_round <- function(x, digits) {
  for(i in 1:ncol(x)){
    if(class(x[,i])=="numeric"){      
      x[i] <-  round(x[i], digits)}}
  return(x)
}





# download the current global climate (this step is necessary to be able to run the 'micro_global()' function later)
get.global.climate(folder="path") # #get.global.climate

# now replace that just downloaded global climate by our own (custom-made) global climate (here for Boyaca only)
file.copy(from = file.path(path_my_global, "global_climate.nc"), to = file.path(path_path, "global_climate.nc"),
  overwrite = TRUE, copy.mode = TRUE)


## Get coordinates to calculate current microclimates

# get a single layer from your global climate just for reference coordinates
global_sp <- raster::stack(list.files(path_path, pattern = "global_climate*.*nc", full.names = TRUE))[[1]]
plot(global_sp$X1)

# mask to our specific target region
ras_mod <- raster::raster(path_range_sp)
global_sp <- raster::mask(global_sp, ras_mod)
plot(global_sp$X1)

# get the map coordinates
sp_coor <- as.data.frame(rasterToPoints(global_sp)[, 1:2])
sp_coor <- as.data.frame(rasterToPoints(global_sp)[, 1:2])
names(sp_coor) <- c("lon","lat")
n_coor <- nrow(sp_coor)

# coordinates including NAs (the whole map)
global_sp_NAs <- global_sp
global_sp_NAs[is.na(global_sp_NAs[])] <- 0 # replacing NA's by zero (this will help us to get all grid coordinates)
coord_sp_NAs <- as.data.frame(rasterToPoints(global_sp_NAs)[, 1:2])
names(coord_sp_NAs) <- c("lon","lat")
coord_sp_NAs <- data.frame(raster::extract(global_sp_NAs, SpatialPoints(coord_sp_NAs), sp = TRUE))
coord_sp_NAs$id <- 1:nrow(coord_sp_NAs)
coord_sp_NAs <- coord_sp_NAs[ ,c("lon","lat","id","X1")]
coord_sp_NAs["X1"][coord_sp_NAs["X1"] == 0] <- NA
rm(global_sp_NAs)

# update species coordinates with the pixel "id"
sp_coor <- merge(coord_sp_NAs, sp_coor, all = TRUE)
sp_coor <- sp_coor[order(sp_coor$id),]
rownames(sp_coor) <- 1:nrow(sp_coor)
sp_coor <- na.omit(sp_coor)
sp_coor <- sp_coor[ ,c("lon","lat","id")]
coord_sp_NAs <- coord_sp_NAs[ ,c("lon","lat","id")]
coord_sp_NAs_id <- coord_sp_NAs[,"id",drop = FALSE]



###
### calculate current microclimate - 25% shadow
###

sp_coor_25 <- sp_coor
list_soil <- vector("list", nrow(sp_coor_25)) # create empty lists exactly the length of coordinates
list_metout <- vector("list", nrow(sp_coor_25))

# calculate microclimate

cat("Starting microclimate calculations for", nrow(sp_coor_25), "pixels...\n")

for (i in 1:nrow(sp_coor_25)){
  
  if(i %% 100 == 0) cat("Calculating pixel", i, "of", nrow(sp_coor_25), "\n")
  
  tryCatch({
    micro <- micro_global(loc = c(sp_coor_25$lon[i], sp_coor_25$lat[i]), 
                          timeinterval = 12,  # 12 months
                          nyear = 1,          # 1 year
                          soiltype = 6,       # sandy-clay-loam
                          minshade = 25,      # minimun % shade (i.e., the target % cover we will use)
                          Usrhyt = 0.01)      # 1 cm for air temp, wind, and humidity      
    
    list_soil[[i]]   <- micro$soil
    list_metout[[i]] <- micro$metout
    
  }, error = function(e) {
  })
  cat("Calculations complete! Formatting data...\n")
  }




# Identify which pixels successfully ran (not NULL)
valid_idx <- !sapply(list_soil, is.null)
cat(sum(!valid_idx), "border pixels were skipped due to NA mismatches.\n")

# Filter out the failed pixels from everything
list_soil <- list_soil[valid_idx]
list_metout <- list_metout[valid_idx]
sp_coor_25_valid <- sp_coor_25[valid_idx, ] 

# Format Soil
sp_coor_25_soil <- lapply(list_soil, function(x) x[, c(1:3)]) 
sp_coor_25_soil <- do.call(rbind, c(lapply(sp_coor_25_soil, data.frame, stringsAsFactors = FALSE), make.row.names = FALSE))
sp_coor_25_soil$lon <- rep(sp_coor_25_valid$lon, each = 288)
sp_coor_25_soil$lat <- rep(sp_coor_25_valid$lat, each = 288)
sp_coor_25_soil$id  <- rep(sp_coor_25_valid$id, each = 288)

# Format Metout
sp_coor_25_metout <- lapply(list_metout, function(x) x[, c(1:3, 5, 7, 13)])
sp_coor_25_metout <- do.call(rbind, c(lapply(sp_coor_25_metout, data.frame, stringsAsFactors = FALSE), make.row.names = FALSE))
sp_coor_25_metout$lon <- rep(sp_coor_25_valid$lon, each = 288)
sp_coor_25_metout$lat <- rep(sp_coor_25_valid$lat, each = 288)
sp_coor_25_metout$id  <- rep(sp_coor_25_valid$id, each = 288)

# cbind the whole microclimate data together
sp_coor_25_final <- cbind(sp_coor_25_soil, sp_coor_25_metout[, c("TALOC", "RHLOC", "VLOC", "SOLR")])
sp_coor_25_final <- sp_coor_25_final[ ,c("lon", "lat", "id", "DOY", "TIME", "D0cm", "TALOC", "RHLOC", "VLOC", "SOLR")]

# Clean up
rm(list_soil, list_metout, sp_coor_25_soil, sp_coor_25_metout)

# export

# create directory
dir.create("cur_microclim", showWarnings = FALSE)
dir.create("cur_microclim/Soil(subst_temp)", showWarnings = FALSE)
dir.create("cur_microclim/TA(air_Temp)", showWarnings = FALSE)
dir.create("cur_microclim/RH(relat_hum)", showWarnings = FALSE)
dir.create("cur_microclim/V1cm", showWarnings = FALSE)
dir.create("cur_microclim/SOLR", showWarnings = FALSE)

# templade raster model
template_map <- global_sp[[1]]

# SUBSTRATE 0cm (D0cm)
sp_coor_25_D0cm <- sp_coor_25_final[, c("id", "D0cm"), drop = FALSE]
sp_coor_25_D0cm <- my_round(sp_coor_25_D0cm, 3)
sp_coor_25_D0cm <- my_nHour(sp_coor_25_D0cm, 288, 288) 

sp_D0cm_25_all <- raster::stack(replicate(288, template_map))

for (i in 1:288){
  sp_coor_i <- data.frame(sp_coor_25_D0cm[[i]])
  sp_coor_i <- merge(sp_coor_i, coord_sp_NAs_id, by="id", all=TRUE)
  sp_coor_i <- sp_coor_i[order(sp_coor_i$id), ] 
  sp_D0cm_25_all[[i]] <- raster::setValues(sp_D0cm_25_all[[i]], sp_coor_i$D0cm)
}

terra::writeCDF(terra::rast(sp_D0cm_25_all), 
                filename = "cur_microclim/Soil(subst_temp)/D0cm_soil_all.nc", 
                varname = "D0cm", overwrite = TRUE)
rm(sp_D0cm_25_all, sp_coor_25_D0cm)


# AIR TEMPERATURE (TALOC)
sp_coor_25_TALOC <- sp_coor_25_final[, c("id", "TALOC"), drop = FALSE]
sp_coor_25_TALOC <- my_round(sp_coor_25_TALOC, 3)
sp_coor_25_TALOC <- my_nHour(sp_coor_25_TALOC, 288, 288)

sp_TALOC_25_all <- raster::stack(replicate(288, template_map))

for (i in 1:288){
  sp_coor_i <- data.frame(sp_coor_25_TALOC[[i]])
  sp_coor_i <- merge(sp_coor_i, coord_sp_NAs_id, by="id", all=TRUE)
  sp_coor_i <- sp_coor_i[order(sp_coor_i$id), ] 
  sp_TALOC_25_all[[i]] <- raster::setValues(sp_TALOC_25_all[[i]], sp_coor_i$TALOC)
}

terra::writeCDF(terra::rast(sp_TALOC_25_all), 
                filename = "cur_microclim/TA(air_Temp)/TA1cm_all.nc", 
                varname = "TA1cm", overwrite = TRUE)
rm(sp_TALOC_25_all, sp_coor_25_TALOC)


# RELATIVE HUMIDITY (RHLOC)
sp_coor_25_RHLOC <- sp_coor_25_final[, c("id", "RHLOC"), drop = FALSE]
sp_coor_25_RHLOC <- my_round(sp_coor_25_RHLOC, 3)
sp_coor_25_RHLOC <- my_nHour(sp_coor_25_RHLOC, 288, 288)

sp_RHLOC_25_all <- raster::stack(replicate(288, template_map))

for (i in 1:288){
  sp_coor_i <- data.frame(sp_coor_25_RHLOC[[i]])
  sp_coor_i <- merge(sp_coor_i, coord_sp_NAs_id, by="id", all=TRUE)
  sp_coor_i <- sp_coor_i[order(sp_coor_i$id), ] 
  sp_RHLOC_25_all[[i]] <- raster::setValues(sp_RHLOC_25_all[[i]], sp_coor_i$RHLOC)
}

terra::writeCDF(terra::rast(sp_RHLOC_25_all), 
                filename = "cur_microclim/RH(relat_hum)/RH1cm_all.nc", 
                varname = "RH1cm", overwrite = TRUE)
rm(sp_RHLOC_25_all, sp_coor_25_RHLOC)



# WIND SPEED (VLOC)
sp_coor_25_VLOC <- sp_coor_25_final[, c("id", "VLOC"), drop = FALSE]
sp_coor_25_VLOC <- my_round(sp_coor_25_VLOC, 3)
sp_coor_25_VLOC <- my_nHour(sp_coor_25_VLOC, 288, 288)

sp_VLOC_25_all <- raster::stack(replicate(288, template_map))

for (i in 1:288){
  sp_coor_i <- data.frame(sp_coor_25_VLOC[[i]])
  sp_coor_i <- merge(sp_coor_i, coord_sp_NAs_id, by="id", all=TRUE)
  sp_coor_i <- sp_coor_i[order(sp_coor_i$id), ] 
  sp_VLOC_25_all[[i]] <- raster::setValues(sp_VLOC_25_all[[i]], sp_coor_i$VLOC)
}

terra::writeCDF(terra::rast(sp_VLOC_25_all), 
                filename = "cur_microclim/V1cm/V1cm_all.nc", 
                varname = "V1cm", overwrite = TRUE)
rm(sp_VLOC_25_all, sp_coor_25_VLOC)



# SOLAR RADIATION (SOLR)
sp_coor_25_SOLR <- sp_coor_25_final[, c("id", "SOLR"), drop = FALSE]
sp_coor_25_SOLR <- my_round(sp_coor_25_SOLR, 3)
sp_coor_25_SOLR <- my_nHour(sp_coor_25_SOLR, 288, 288)

sp_SOLR_25_all <- raster::stack(replicate(288, template_map))

for (i in 1:288){
  sp_coor_i <- data.frame(sp_coor_25_SOLR[[i]])
  sp_coor_i <- merge(sp_coor_i, coord_sp_NAs_id, by="id", all=TRUE)
  sp_coor_i <- sp_coor_i[order(sp_coor_i$id), ] 
  sp_SOLR_25_all[[i]] <- raster::setValues(sp_SOLR_25_all[[i]], sp_coor_i$SOLR)
}

terra::writeCDF(terra::rast(sp_SOLR_25_all), 
                filename = "cur_microclim/SOLR/SOLR_all.nc", 
                varname = "SOLR", overwrite = TRUE)
rm(sp_SOLR_25_all, sp_coor_25_SOLR)

# end---