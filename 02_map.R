library(tidyverse)
library(ggrepel)
library(maps)
library(ncdf4)
library(sp)
library(raster)
library(sf)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)

#read data------
full_ds <- read_csv("data/full_ds.csv") %>% 
  mutate(lake_name = str_replace_all(lake_name, "_[1-3]", "")) %>% 
  distinct(lake_name, .keep_all = TRUE)

#make a simple map------
simple_map <- ggplot() +
  borders("world", fill = "gray80", colour = "black", size = 0.5) +
  coord_fixed(ratio = 1.25) +
  geom_point(data = full_ds, aes(x = lon, y = lat), color = "red", size = 3) +
  geom_text_repel(data = full_ds, aes(x = lon, y = lat, label = lake_name), size = 4, color = "darkgreen")

#Boreal summer temperature anomaly map------

ekf_names <- list.files(path = "data/ekf400_ens_mem_mean", full.names = TRUE) #get names for data files
nc_data_list <- lapply(ekf_names, nc_open) #data for the reference period 1766-1866

get_temp <- function(nc_file) {
  ncvar_get(nc_file, "air_temperature")
}

extract_summer <- function(arr) {
  summer_temp <- arr[,,6:8]
  return(summer_temp)
}

temp_an <- lapply(nc_data_list, get_temp)

summer_temp_an <- lapply(temp_an, extract_summer)

summer_temp_mean_prep <- Reduce("+", summer_temp_an) / length(summer_temp_an)
summer_temp_mean <- apply(summer_temp_mean_prep, MARGIN = c(1, 2), FUN = mean)

ekf_1816 <- nc_data_list[[51]] #read data for yws

ekf_1816_temp <- get_temp(ekf_1816)

ekf_1816_summer_prep <- extract_summer(ekf_1816_temp)
ekf_1816_summer_mean <- apply(ekf_1816_summer_prep, MARGIN = c(1, 2), FUN = mean)

summer_temp_anomaly_1816 <- ekf_1816_summer_mean - summer_temp_mean

longitude_prep <- ncvar_get(nc_data_list[[1]], "longitude")
longitude <- ifelse(longitude_prep > 180, longitude_prep + (360-max(longitude_prep)), longitude_prep)
latitude <- ncvar_get(nc_data_list[[1]], "latitude")

lapply(nc_data_list, nc_close)

summer_r_prep <- raster(t(summer_temp_anomaly_1816), xmn=min(longitude), xmx=max(longitude), ymn=min(latitude), ymx=max(latitude), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

summer_r <- raster::rotate(summer_r_prep)

countries <- ne_countries(scale = "medium", returnclass = "sf")

coordinates(full_ds) <- ~lon + lat
proj4string(full_ds) <- CRS("+proj=longlat +datum=WGS84")
full_ds_sf <- st_as_sf(full_ds)

tm_shape(summer_r) + 
  tm_raster(style = "cont", palette = "-RdBu", n = 20, title = "Temperature", legend.reverse = TRUE) +
  tm_shape(countries) + 
  tm_borders(lwd = 1, col = "black") +
  tm_shape(full_ds_sf) +
  tm_symbols(size = 0.5, col = "red", border.col = "black") +
  tm_layout(legend.outside = TRUE,
            frame = TRUE,
            main.title = "Boreal summer temperature anomaly in 1816 with respect to 1766-1866 mean",
            main.title.position = 0,
            main.title.size = 1) +
  tm_grid(labels.size = 0.5,  # Adjust label size
    labels.inside.frame = FALSE,  # Labels outside frame
    lines = FALSE,  # Turn off grid lines
    ticks = TRUE,  # Add ticks
    labels.show = TRUE)

#Boreal summer precipitation anomaly-------
ekf_names <- list.files(path = "data/ekf400_ens_mem_mean", full.names = TRUE) #get names for data files
nc_data_list <- lapply(ekf_names, nc_open) #data for the reference period 1766-1866

get_prec <- function(nc_file) {
  ncvar_get(nc_file, "total precipitation")
}

extract_summer_prec <- function(arr) {
  summer_prec <- arr[,,6:8]
  return(summer_prec)
}

prec_an <- lapply(nc_data_list, get_prec)

summer_prec_an <- lapply(prec_an, extract_summer_prec)

summer_prec_mean_prep <- Reduce("+", summer_prec_an) / length(summer_prec_an)
summer_prec_mean <- apply(summer_prec_mean_prep, MARGIN = c(1, 2), FUN = mean)

ekf_1816 <- nc_data_list[[51]] #read data for yws

ekf_1816_prec <- get_prec(ekf_1816)

ekf_1816_summer_prec_prep <- extract_summer_prec(ekf_1816_prec)
ekf_1816_summer_mean_prec <- apply(ekf_1816_summer_prec_prep, MARGIN = c(1, 2), FUN = mean)

summer_prec_anomaly_1816 <- summer_prec_mean - ekf_1816_summer_mean_prec
summer_prec_anomaly_1816_perc_prep <- summer_prec_anomaly_1816/summer_prec_mean
summer_prec_anomaly_1816_perc <- summer_prec_anomaly_1816_perc_prep*(-100)

summer_prec_anomaly_1816_perc[summer_prec_anomaly_1816_perc == min(summer_prec_anomaly_1816_perc)] <- NA #replace outlier with NA
summer_prec_anomaly_1816_perc[summer_prec_anomaly_1816_perc > 210] <- NA #most of the very high values appears erronous

lapply(nc_data_list, nc_close)

summer_prec_r_prep <- raster(t(summer_prec_anomaly_1816_perc), xmn=min(longitude), xmx=max(longitude), ymn=min(latitude), ymx=max(latitude), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

summer_prec_r <- raster::rotate(summer_prec_r_prep)

countries <- ne_countries(scale = "medium", returnclass = "sf")

tm_shape(summer_prec_r) + 
  tm_raster(style = "cont", palette = "BrBG", n = 20, title = "Precipitation anomaly (%)", legend.reverse = TRUE) +
  tm_shape(countries) + 
  tm_borders(lwd = 1, col = "black") +
  tm_shape(full_ds_sf) +
  tm_symbols(size = 0.5, col = "red", border.col = "black") +
  tm_layout(legend.outside = TRUE,
            frame = TRUE,
            main.title = "Boreal summer precipitation anomaly in 1816 with respect to 1766-1866 mean",
            main.title.position = 0,
            main.title.size = 1) +
  tm_grid(labels.size = 0.5,  # Adjust label size
          labels.inside.frame = FALSE,  # Labels outside frame
          lines = FALSE,  # Turn off grid lines
          ticks = TRUE,  # Add ticks
          labels.show = TRUE)

#extract summer temperature anomaly value for the location of the lakes--------
extracted_summer_an <- extract(summer_r, full_ds_sf)
