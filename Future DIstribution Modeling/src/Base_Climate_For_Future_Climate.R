# ---

# prepare the base climate for the construction of the FUTURE microclimate

# ---



# directory paths

# working directory
path_wd <- "C:/Users/luism/OneDrive/Escritorio/curso/Dia 5 - Junio 19/05_pract/01_my_global_climate_fut"
# base climate
folder <- "data/base_clim"
# my base climate
folder_global <- "data/my_global"

# boyacá raster model
boyaca_ras_mod <- "data/boyaca_ras_2_5m.tif"

# WorldClim future climate (2041-2060)
path_tmin <- file.path("data/fut_2041_2060/tn")
path_tmax <- file.path("data/fut_2041_2060/tx")
path_prec <- file.path("data/fut_2041_2060/pr")




# working directory
setwd(path_wd)

# library
library(NicheMapR)
library(ncdf4)
library(terra)






### CMIP6 / WorldClim future climate

# CMIP6 / GCM = ACCESS-CM2 / year = 2041-2060 / SSP scenario = SSP3-70 (intermediate emission scenario) / resolution = 2.5 min 
# (https://www.worldclim.org/data/cmip6/cmip6_clim2.5m.html#2041-2060)

# tn -  monthly average minimum temperature (°C):
ras_tmin_list <- list.files(path = path_tmin, pattern = '\\.tif$', all.files = TRUE, full.names = TRUE) 
ras_tmin <- rast(ras_tmin_list) 

# tx - monthly average maximum temperature (°C)
ras_tmax_list <- list.files(path = path_tmax, pattern = '\\.tif$', all.files = TRUE, full.names = TRUE)
ras_tmax <- rast(ras_tmax_list) 

# prec - pr - monthly total precipitation (mm)
ras_prec_list <- list.files(path = path_prec, pattern = '\\.tif$', all.files = TRUE, full.names = TRUE)
ras_prec <- rast(ras_prec_list) 

# Mean diurnal temperature range (dtr)
my_dtr <- ras_tmax - ras_tmin

# Mean temperature (tmp)
my_tmp <- (ras_tmin + ras_tmax)/2

# pre-treat the data
my_pre <- ras_prec
my_dtr <- my_dtr * 10 # *10 according to the original code 
my_tmp <- my_tmp * 10 # *10 according to the original code 

rm(ras_tmin, ras_tmax, ras_prec)

# gridout for 10-min worldwide dataset
gridout <- rast(ncols=2160, nrows=1080, xmin=-180, xmax=180, ymin=-90, ymax=90)
# gridout for 2.5 min derived dataset
gridout_2_5m <- rast(ncols=65, nrows=57, xmin=-74.66667, xmax=-71.95833, ymin=4.666667, ymax=7.041667)

# laod Boyaca raster model
ras_mod <- rast(boyaca_ras_mod)

# soil moisture
cat('soilw.mon.ltm.v2.nc \n',sep="")
soilmoist0.5deg <- rast(file.path(folder, "soilw.mon.ltm.v2.nc"))
cat('interpolating soilw.mon.ltm.v2.nc from 0.5 deg to 10 min\n')
soilmoist0.5deg <- terra::rotate(soilmoist0.5deg)
soilmoist.10min <- terra::resample(soilmoist0.5deg, gridout)

# cropping area (to Boyacá)
e_buffer <- ext(-76.0, -70.0, 3.0, 9.0) 
soilmoist_buffered <- crop(soilmoist.10min, e_buffer) 
soilmoist.2_5m <- resample(soilmoist_buffered, gridout_2_5m)
rm(soilmoist0.5deg, soilmoist.10min)

# global_climate.nc construction

destfile1 <- "grid_10min_"
destfile2 <- ".dat.gz"
folder <- "data/base_clim"
vars=c('elv','pre','rd0','wnd','tmp','dtr','reh')

global_climate <- rep(gridout, 97)

cat('reading in each variable, rasterising and storing in global_climate stack \n')
for(i in 1:length(vars)){
  
  file_path <- file.path(folder, paste0(destfile1, vars[i], ".dat"))
  data <- read.table(file_path)
  
  # list of co-ordinates (lon, lat)
  coords <- cbind(data[,2], data[,1]) 
  
  if(i == 1){ # elevation ('elv'), just one column
    
    r_temp <- rasterize(coords, gridout, values = data[,3])
    global_climate[[1]] <- round(r_temp * 1000, 0) # convert to m, round off
    cat(paste(vars[i], ' done \n', sep=""))
    
  } else {
    for(j in 1:12){
      layer_idx <- 1 + (i - 2) * 12 + j
      
      r_temp <- rasterize(coords, gridout, values = data[, 2 + j])
      
      if(i == 2 | i == 3){   # 'pre' and 'rd0'
        global_climate[[layer_idx]] <- round(r_temp, 0)
        
      } else {               # 'wnd','tmp','dtr' and 'reh'
        global_climate[[layer_idx]] <- round(r_temp * 10, 0)
      }
      cat(paste(vars[i], ' month ', j, ' done \n', sep=""))
    }
  }
}


# adjust to our target extent (Boyaca)
global_climate <- crop(global_climate, e_buffer)
global_climate <- resample(global_climate, gridout_2_5m)
cat("Dimensions of cropped global_climate:", dim(global_climate), "\n")


# Ensure custom layers perfectly match global_climate before swapping
my_pre <- resample(my_pre, global_climate)
my_tmp <- resample(my_tmp, global_climate)
my_dtr <- resample(my_dtr, global_climate)


# replace current layers with future climate layers
pre_lyrs <- seq(2,13,1) 
tmp_lyrs <- seq(38,49,1)
dtr_lyrs <- seq(50,61,1)

# precipitation
for(ind in seq_along(pre_lyrs)) {
  values(global_climate[[pre_lyrs[ind]]]) <- values(my_pre[[ind]])
}

# Mean temperature
for(ind in seq_along(tmp_lyrs)) {
  values(global_climate[[tmp_lyrs[ind]]]) <- values(my_tmp[[ind]])
}

# Mean diurnal temperature range
for(ind in seq_along(dtr_lyrs)) {
  values(global_climate[[dtr_lyrs[ind]]]) <- values(my_dtr[[ind]])
}


# construct max/min temps
meantemps <- global_climate[[38:49]] 
meandtrs <- global_climate[[50:61]]  
meanhums <- global_climate[[62:73]]  

for(i in 1:12){
  global_climate[[37+i]] = round((meantemps[[i]] - meandtrs[[i]]/2),0)
  global_climate[[49+i]] = round((meantemps[[i]] + meandtrs[[i]]/2),0)
}


# construct max/min relative humidities
for(i in 1:12){
  rh_vals   <- values(meanhums[[i]]) / 10
  db_vals   <- values(meantemps[[i]]) / 10
  tmax_vals <- values(global_climate[[49+i]]) / 10
  tmin_vals <- values(global_climate[[37+i]]) / 10
  
  valid_idx <- which(!is.na(rh_vals) & !is.na(db_vals) & !is.na(tmax_vals) & !is.na(tmin_vals))
  
  minhum_full <- rep(NA, length(rh_vals))
  maxhum_full <- rep(NA, length(rh_vals))
  
  if(length(valid_idx) > 0) {
    e_vals <- WETAIR.rh(rh = rh_vals[valid_idx], db = db_vals[valid_idx])$e
    
    minhum_full[valid_idx] <- round((e_vals / VAPPRS(tmax_vals[valid_idx])) * 100 * 10, 0)
    maxhum_full[valid_idx] <- round((e_vals / VAPPRS(tmin_vals[valid_idx])) * 100 * 10, 0)
  }
  
  values(global_climate[[61+i]]) <- minhum_full
  values(global_climate[[73+i]]) <- maxhum_full
}

# adjust values outside 0-100 range
for(i in 1:12){
  global_climate[[61+i]] <- clamp(global_climate[[61+i]], lower=0, upper=1000)
  global_climate[[73+i]] <- clamp(global_climate[[73+i]], lower=0, upper=1000)
}

# cloud cover
crs(gridout_2_5m) <- "EPSG:4326"

crucloud <- crudat2raster(file="ccld6190.dat", loc=paste0(folder, "/"))
monthly.cld <- crucloud$cru.raster
header <- crucloud$header
crs(monthly.cld) <- "EPSG:4326"

monthly.cld_buffered <- crop(monthly.cld, e_buffer)
monthly.cld_2_5m <- resample(monthly.cld_buffered, gridout_2_5m)

one <- ifel(is.na(global_climate[[1]]), NA, 1)

cloud_scaled <- round(monthly.cld_2_5m * one * 10, 0)

for(i in 1:header$n_months){
  values(global_climate[[85+i]]) <- values(cloud_scaled[[i]])
}

# Soil moisture
soilmoist.2_5m <- round(soilmoist.2_5m * one, 0)

# export
cat("\nWriting global_climate.nc to", folder_global, "...\n")
dir.create("data/my_global", showWarnings = FALSE)

Sys.setenv(GDAL_PAM_ENABLED = "NO")

terra::writeCDF(global_climate, 
                filename = file.path(folder_global, "global_climate.nc"), 
                varname = "climate", 
                overwrite = TRUE)
rm(global_climate)

cat("Writing soilw.mon.ltm.v2.nc to", folder_global, "...\n")
terra::writeCDF(soilmoist.2_5m, 
                filename = file.path(folder_global, "soilw.mon.ltm.v2.nc"), 
                varname = "soilw", 
                overwrite = TRUE)
rm(soilmoist.2_5m)

# end---