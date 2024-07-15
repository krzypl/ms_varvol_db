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
library(abind)

full_ds <- read_csv("data/full_ds.csv") %>% 
  mutate(lake_name = str_replace_all(lake_name, "_[1-3]", "")) %>% 
  distinct(lake_name, .keep_all = TRUE) %>% 
  arrange(lake_name)

#Boreal spring 1817 precipitation anomaly-------
ekf_names <- list.files(path = "data/ekf400_ens_mem_mean", full.names = TRUE) #get names for data files
nc_data_list <- lapply(ekf_names, nc_open) #data for the reference period 1766-1866

get_prec <- function(nc_file) {
  ncvar_get(nc_file, "total precipitation")
}

extract_spring_prec <- function(arr) {
  spring_prec <- arr[,,3:5]
  return(spring_prec)
}

prec_an <- lapply(nc_data_list, get_prec)

spring_prec_an <- lapply(prec_an, extract_spring_prec)

spring_prec_mean_prep <- Reduce("+", spring_prec_an) / length(spring_prec_an)
spring_prec_mean <- apply(spring_prec_mean_prep, MARGIN = c(1, 2), FUN = mean)

ekf_1817 <- nc_data_list[[52]] #read data for 1817

ekf_1817_prec <- get_prec(ekf_1817)

ekf_1817_spring_prec_prep <- extract_spring_prec(ekf_1817_prec)
ekf_1817_spring_mean_prec <- apply(ekf_1817_spring_prec_prep, MARGIN = c(1, 2), FUN = mean)


spring_prec_anomaly_1817_perc <- (ekf_1817_spring_mean_prec/spring_prec_mean)*100
spring_prec_anomaly_1817_perc[spring_prec_anomaly_1817_perc < 0] <- NA

spring_prec_anomaly_1817_perc[spring_prec_anomaly_1817_perc > 400] <- NA #most of the very high values appears erronous

longitude_prep <- ncvar_get(nc_data_list[[1]], "longitude")
longitude <- ifelse(longitude_prep > 180, longitude_prep + (360-max(longitude_prep)), longitude_prep)
latitude <- ncvar_get(nc_data_list[[1]], "latitude")

lapply(nc_data_list, nc_close)

spring_prec_r_prep <- raster(t(spring_prec_anomaly_1817_perc), xmn=min(longitude), xmx=max(longitude), ymn=min(latitude), ymx=max(latitude), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

spring_prec_r <- raster::rotate(spring_prec_r_prep)

spring_prec_r_map <- tm_shape(spring_prec_r) + 
  tm_raster(style = "cont", palette = "BrBG", midpoint = 100, n = 20, title = "Precipitation with respect to reference period (%)", legend.reverse = FALSE, legend.is.portrait = FALSE) +
  tm_shape(countries) + 
  tm_borders(lwd = 1, col = "black") +
  tm_shape(full_ds_sf) +
  tm_symbols(size = 0.5, col = "lightgreen", alpha = 0.8, border.col = "black") +
  tm_shape(full_ds_sf) +
  tm_text("no", col = "black", bg.color = "white", bg.alpha = 0.6, shadow = TRUE, auto.placement = 0.01, fontface = "bold") +
  tm_layout(legend.position = c("right", "bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.6,
            frame = TRUE,
            main.title = "Boreal spring precipitation percentages in 1817 with respect to 1766-1866 mean",
            main.title.position = 0.05,
            legend.width = 1,
            main.title.size = 1) +
  tm_grid(labels.size = 0.8,  # Adjust label size
          labels.inside.frame = FALSE,  # Labels outside frame
          lines = FALSE,  # Turn off grid lines
          ticks = TRUE,  # Add ticks
          labels.show = TRUE)

#boreal spring 1817 temperature---------

ekf_names <- list.files(path = "data/ekf400_ens_mem_mean", full.names = TRUE) #get names for data files
nc_data_list <- lapply(ekf_names, nc_open) #data for the reference period 1766-1866

get_temp <- function(nc_file) {
  ncvar_get(nc_file, "air_temperature")
}

extract_spring <- function(arr) {
  spring_temp <- arr[,,3:5]
  return(spring_temp)
}

temp_an <- lapply(nc_data_list, get_temp)

spring_temp_an <- lapply(temp_an, extract_spring)

spring_temp_mean_prep <- Reduce("+", spring_temp_an) / length(spring_temp_an)
spring_temp_mean <- apply(spring_temp_mean_prep, MARGIN = c(1, 2), FUN = mean)

ekf_1817 <- nc_data_list[[52]] #read data for yws

ekf_1817_temp <- get_temp(ekf_1817)

ekf_1817_spring_prep <- extract_spring(ekf_1817_temp)
ekf_1817_spring_mean <- apply(ekf_1817_spring_prep, MARGIN = c(1, 2), FUN = mean)

spring_temp_anomaly_1817 <- ekf_1817_spring_mean - spring_temp_mean

longitude_prep <- ncvar_get(nc_data_list[[1]], "longitude")
longitude <- ifelse(longitude_prep > 180, longitude_prep + (360-max(longitude_prep)), longitude_prep)
latitude <- ncvar_get(nc_data_list[[1]], "latitude")

lapply(nc_data_list, nc_close)

spring_r_prep <- raster(t(spring_temp_anomaly_1817), xmn=min(longitude), xmx=max(longitude), ymn=min(latitude), ymx=max(latitude), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

spring_r <- raster::rotate(spring_r_prep)

countries <- ne_countries(scale = "medium", returnclass = "sf")

full_ds_sf_prep <- full_ds

coordinates(full_ds_sf_prep) <- ~lon + lat
proj4string(full_ds_sf_prep) <- CRS("+proj=longlat +datum=WGS84")
full_ds_sf <- st_as_sf(full_ds_sf_prep) %>% 
  mutate(no = factor(1:length(lake_name)))

spring_r_map <- tm_shape(spring_r) + 
  tm_raster(style = "cont", palette = "-RdBu", n = 20, title = "Temperature anomaly (°C)", legend.reverse = FALSE, legend.is.portrait = FALSE) +
  tm_shape(countries) + 
  tm_borders(lwd = 1, col = "black") +
  tm_shape(full_ds_sf) +
  tm_symbols(size = 0.5, col = "lightgreen", alpha = 0.8, border.col = "black") +
  tm_shape(full_ds_sf) +
  tm_text("no", col = "black", bg.color = "white", bg.alpha = 0.6, shadow = TRUE, auto.placement = 0.01, fontface = "bold") +
  tm_layout(legend.position = c("right", "bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.6,
            frame = TRUE,
            legend.width = 1,
            main.title = "Boreal spring temperature anomaly in 1817 with respect to 1766-1866 mean",
            main.title.position = 0.05,
            main.title.size = 1) +
  tm_grid(labels.size = 0.8,  # Adjust label size
          labels.inside.frame = FALSE,  # Labels outside frame
          lines = FALSE,  # Turn off grid lines
          ticks = TRUE,  # Add ticks
          labels.show = TRUE)

#extracted values 1817----
extracted_spring_temp_an <- raster::extract(spring_r, full_ds_sf)
extracted_spring_prec_an <- raster::extract(spring_prec_r, full_ds_sf)

kunin_anomalies <- full_ds %>% 
  mutate(spring_temp = extracted_spring_temp_an,
         spring_prec = extracted_spring_prec_an) %>% 
  filter(lake_name == "Kuninkaisenlampi" | lake_name == "Nar Gölü") %>% 
  dplyr::select(lake_name, spring_temp, spring_prec)

#Boreal spring 1816 precipitation anomaly-------
ekf_names <- list.files(path = "data/ekf400_ens_mem_mean", full.names = TRUE) #get names for data files
nc_data_list <- lapply(ekf_names, nc_open) #data for the reference period 1766-1866

get_prec <- function(nc_file) {
  ncvar_get(nc_file, "total precipitation")
}

extract_spring_prec <- function(arr) {
  spring_prec <- arr[,,3:5]
  return(spring_prec)
}

prec_an <- lapply(nc_data_list, get_prec)

spring_prec_an <- lapply(prec_an, extract_spring_prec)

spring_prec_mean_prep <- Reduce("+", spring_prec_an) / length(spring_prec_an)
spring_prec_mean <- apply(spring_prec_mean_prep, MARGIN = c(1, 2), FUN = mean)

ekf_1816 <- nc_data_list[[51]] #read data for 1816

ekf_1816_prec <- get_prec(ekf_1816)

ekf_1816_spring_prec_prep <- extract_spring_prec(ekf_1816_prec)
ekf_1816_spring_mean_prec <- apply(ekf_1816_spring_prec_prep, MARGIN = c(1, 2), FUN = mean)


spring_prec_anomaly_1816_perc <- (ekf_1816_spring_mean_prec/spring_prec_mean)*100
spring_prec_anomaly_1816_perc[spring_prec_anomaly_1816_perc < 0] <- NA

spring_prec_anomaly_1816_perc[spring_prec_anomaly_1816_perc > 400] <- NA #most of the very high values appears erronous

longitude_prep <- ncvar_get(nc_data_list[[1]], "longitude")
longitude <- ifelse(longitude_prep > 180, longitude_prep + (360-max(longitude_prep)), longitude_prep)
latitude <- ncvar_get(nc_data_list[[1]], "latitude")

lapply(nc_data_list, nc_close)

spring_prec_r_prep <- raster(t(spring_prec_anomaly_1816_perc), xmn=min(longitude), xmx=max(longitude), ymn=min(latitude), ymx=max(latitude), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

spring_prec_r <- raster::rotate(spring_prec_r_prep)

spring_prec_r_map <- tm_shape(spring_prec_r) + 
  tm_raster(style = "cont", palette = "BrBG", midpoint = 100, n = 20, title = "Precipitation with respect to reference period (%)", legend.reverse = FALSE, legend.is.portrait = FALSE) +
  tm_shape(countries) + 
  tm_borders(lwd = 1, col = "black") +
  tm_shape(full_ds_sf) +
  tm_symbols(size = 0.5, col = "lightgreen", alpha = 0.8, border.col = "black") +
  tm_shape(full_ds_sf) +
  tm_text("no", col = "black", bg.color = "white", bg.alpha = 0.6, shadow = TRUE, auto.placement = 0.01, fontface = "bold") +
  tm_layout(legend.position = c("right", "bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.6,
            frame = TRUE,
            main.title = "Boreal spring precipitation percentages in 1816 with respect to 1766-1866 mean",
            main.title.position = 0.05,
            legend.width = 1,
            main.title.size = 1) +
  tm_grid(labels.size = 0.8,  # Adjust label size
          labels.inside.frame = FALSE,  # Labels outside frame
          lines = FALSE,  # Turn off grid lines
          ticks = TRUE,  # Add ticks
          labels.show = TRUE)

#boreal spring 1816 temperature---------

ekf_names <- list.files(path = "data/ekf400_ens_mem_mean", full.names = TRUE) #get names for data files
nc_data_list <- lapply(ekf_names, nc_open) #data for the reference period 1766-1866

get_temp <- function(nc_file) {
  ncvar_get(nc_file, "air_temperature")
}

extract_spring <- function(arr) {
  spring_temp <- arr[,,3:5]
  return(spring_temp)
}

temp_an <- lapply(nc_data_list, get_temp)

spring_temp_an <- lapply(temp_an, extract_spring)

spring_temp_mean_prep <- Reduce("+", spring_temp_an) / length(spring_temp_an)
spring_temp_mean <- apply(spring_temp_mean_prep, MARGIN = c(1, 2), FUN = mean)

ekf_1816 <- nc_data_list[[51]] #read data for yws

ekf_1816_temp <- get_temp(ekf_1816)

ekf_1816_spring_prep <- extract_spring(ekf_1816_temp)
ekf_1816_spring_mean <- apply(ekf_1816_spring_prep, MARGIN = c(1, 2), FUN = mean)

spring_temp_anomaly_1816 <- ekf_1816_spring_mean - spring_temp_mean

longitude_prep <- ncvar_get(nc_data_list[[1]], "longitude")
longitude <- ifelse(longitude_prep > 180, longitude_prep + (360-max(longitude_prep)), longitude_prep)
latitude <- ncvar_get(nc_data_list[[1]], "latitude")

lapply(nc_data_list, nc_close)

spring_r_prep <- raster(t(spring_temp_anomaly_1816), xmn=min(longitude), xmx=max(longitude), ymn=min(latitude), ymx=max(latitude), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

spring_r <- raster::rotate(spring_r_prep)

countries <- ne_countries(scale = "medium", returnclass = "sf")

full_ds_sf_prep <- full_ds

coordinates(full_ds_sf_prep) <- ~lon + lat
proj4string(full_ds_sf_prep) <- CRS("+proj=longlat +datum=WGS84")
full_ds_sf <- st_as_sf(full_ds_sf_prep) %>% 
  mutate(no = factor(1:length(lake_name)))

spring_r_map <- tm_shape(spring_r) + 
  tm_raster(style = "cont", palette = "-RdBu", n = 20, title = "Temperature anomaly (°C)", legend.reverse = FALSE, legend.is.portrait = FALSE) +
  tm_shape(countries) + 
  tm_borders(lwd = 1, col = "black") +
  tm_shape(full_ds_sf) +
  tm_symbols(size = 0.5, col = "lightgreen", alpha = 0.8, border.col = "black") +
  tm_shape(full_ds_sf) +
  tm_text("no", col = "black", bg.color = "white", bg.alpha = 0.6, shadow = TRUE, auto.placement = 0.01, fontface = "bold") +
  tm_layout(legend.position = c("right", "bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.6,
            frame = TRUE,
            legend.width = 1,
            main.title = "Boreal spring temperature anomaly in 1816 with respect to 1766-1866 mean",
            main.title.position = 0.05,
            main.title.size = 1) +
  tm_grid(labels.size = 0.8,  # Adjust label size
          labels.inside.frame = FALSE,  # Labels outside frame
          lines = FALSE,  # Turn off grid lines
          ticks = TRUE,  # Add ticks
          labels.show = TRUE)

#extracted values 1816----
extracted_spring_temp_an <- raster::extract(spring_r, full_ds_sf)
extracted_spring_prec_an <- raster::extract(spring_prec_r, full_ds_sf)

kunin_and_nar_golu_anomalies <- full_ds %>% 
  mutate(spring_temp = extracted_spring_temp_an,
         spring_prec = extracted_spring_prec_an) %>% 
  filter(lake_name == "Kuninkaisenlampi" | lake_name == "Nar Gölü") %>% 
  dplyr::select(lake_name, spring_temp, spring_prec)