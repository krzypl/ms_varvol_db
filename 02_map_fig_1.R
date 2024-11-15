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

#read data------
full_ds <- read_csv("data/full_ds.csv") %>% 
  mutate(lake_name = str_replace_all(lake_name, "_[1-3]", "")) %>% 
  distinct(lake_name, .keep_all = TRUE) %>% 
  arrange(lake_name)

#make a simple map------
simple_map <- ggplot() +
  borders("world", fill = "gray80", colour = "black", size = 0.5) +
  coord_fixed(ratio = 1.25) +
  geom_point(data = full_ds, aes(x = lon, y = lat), color = "red", size = 3) +
  geom_text_repel(data = full_ds, aes(x = lon, y = lat, label = lake_name), size = 4, color = "darkgreen")

#Boreal summer temperature anomaly map------
#####The data for climate anomaly maps can be obtained from https://www.wdc-climate.de/ui/entry?acronym=EKF400_ens_mem_Mean_v2.0 following registration. Here the time range for the downloaded data was set to 1766-1866
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

full_ds_sf_prep <- full_ds

coordinates(full_ds_sf_prep) <- ~lon + lat
proj4string(full_ds_sf_prep) <- CRS("+proj=longlat +datum=WGS84")
full_ds_sf <- st_as_sf(full_ds_sf_prep) %>% 
  mutate(no = factor(1:length(lake_name)))

summer_r_map <- tm_shape(summer_r) + 
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
#            main.title = "(A) Boreal summer temperature anomaly in 1816 with respect to 1766-1866 mean",
            main.title.position = 0.05,
            main.title.size = 1) +
  tm_grid(labels.size = 0.8,  # Adjust label size
    labels.inside.frame = FALSE,  # Labels outside frame
    lines = FALSE,  # Turn off grid lines
    ticks = TRUE,  # Add ticks
    labels.show = TRUE)

tmap_save(summer_r_map, filename = "figures/summer_temp_anomaly.pdf")
tmap_save(summer_r_map, filename = "figures/summer_temp_anomaly.svg")

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


summer_prec_anomaly_1816_perc <- (ekf_1816_summer_mean_prec/summer_prec_mean)*100
summer_prec_anomaly_1816_perc[summer_prec_anomaly_1816_perc < 0] <- NA

summer_prec_anomaly_1816_perc[summer_prec_anomaly_1816_perc > 400] <- NA #most of the very high values appears erroneous

lapply(nc_data_list, nc_close)

summer_prec_r_prep <- raster(t(summer_prec_anomaly_1816_perc), xmn=min(longitude), xmx=max(longitude), ymn=min(latitude), ymx=max(latitude), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

summer_prec_r <- raster::rotate(summer_prec_r_prep)

summer_prec_r_map <- tm_shape(summer_prec_r) + 
  tm_raster(style = "cont", palette = "BrBG", midpoint = 100, n = 20, title = "Precipitation anomaly (%)", legend.reverse = FALSE, legend.is.portrait = FALSE) +
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
#            main.title = "(C) Boreal summer precipitation percentages in 1816 with respect to 1766-1866 mean",
            main.title.position = 0.05,
            legend.width = 1,
            main.title.size = 1) +
  tm_grid(labels.size = 0.8,  # Adjust label size
          labels.inside.frame = FALSE,  # Labels outside frame
          lines = FALSE,  # Turn off grid lines
          ticks = TRUE,  # Add ticks
          labels.show = TRUE)

tmap_save(summer_prec_r_map, filename = "figures/summer_prec_anomaly.pdf")
tmap_save(summer_prec_r_map, filename = "figures/summer_prec_anomaly.svg")

#Boreal winter temperature anomaly in 1816/1817-----------
ekf_names <- list.files(path = "data/ekf400_ens_mem_mean", full.names = TRUE) #get names for data files
nc_data_list <- lapply(ekf_names, nc_open) #data for the reference period 1766-1866

extract_winter <- function(arr) {
  summer_temp <- arr[,,c(1,2,12)]
  return(summer_temp)
}

winter_temp_an <- lapply(temp_an, extract_winter)

winter_temp_mean_prep <- Reduce("+", winter_temp_an) / length(winter_temp_an)
winter_temp_mean <- apply(winter_temp_mean_prep, MARGIN = c(1, 2), FUN = mean)

ekf_1816_winter_prep <- ekf_1816_temp[,,12]

ekf_1817 <- nc_data_list[[52]]
ekf_1817_temp <- get_temp(ekf_1817)

ekf_1817_winter_prep <- ekf_1817_temp[,,1:2]
ekf_1816_17_winter <- abind(ekf_1817_winter_prep, ekf_1816_winter_prep, along = 3)

ekf_1816_17_winter_mean <- apply(ekf_1816_17_winter, MARGIN = c(1, 2), FUN = mean)

winter_temp_anomaly_1816_17 <- ekf_1816_17_winter_mean - winter_temp_mean

lapply(nc_data_list, nc_close)

winter_r_prep <- raster(t(winter_temp_anomaly_1816_17), xmn=min(longitude), xmx=max(longitude), ymn=min(latitude), ymx=max(latitude), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

winter_r <- raster::rotate(winter_r_prep)

winter_r_map <- tm_shape(winter_r) + 
  tm_raster(style = "cont", palette = "-RdBu", n = 20, title = "Temperature anomaly (°C)", legend.reverse = FALSE, legend.is.portrait = FALSE) +
  tm_shape(countries) + 
  tm_borders(lwd = 1, col = "black") +
  tm_shape(full_ds_sf) +
  tm_symbols(size = 0.5, col = "lightgreen", alpha = 0.8, border.col = "black") +
  tm_shape(full_ds_sf) +
  tm_text("no", col = "black", bg.color = "white", bg.alpha = 0.6, shadow = TRUE, auto.placement = 0.01, fontface = "bold") +
  tm_layout(legend.position = c("right", "bottom"),
            legend.bg.color = "white",
            legend.width = 1,
            frame = TRUE,
#            main.title = "(B) Boreal winter temperature anomaly in 1816/1817 with respect to 1766-1866 mean",
            main.title.position = 0.05,
            main.title.size = 1) +
  tm_grid(labels.size = 0.8,  # Adjust label size
          labels.inside.frame = FALSE,  # Labels outside frame
          lines = FALSE,  # Turn off grid lines
          ticks = TRUE,  # Add ticks
          labels.show = TRUE)

tmap_save(winter_r_map, filename = "figures/winter_temp_anomaly.pdf")
tmap_save(winter_r_map, filename = "figures/winter_temp_anomaly.svg")

#Boreal winter precipitation anomaly in 1816/17--------
ekf_names <- list.files(path = "data/ekf400_ens_mem_mean", full.names = TRUE) #get names for data files
nc_data_list <- lapply(ekf_names, nc_open) #data for the reference period 1766-1866

extract_winter_prec <- function(arr) {
  winter_prec <- arr[,,c(1,11,12)]
  return(winter_prec)
}

winter_prec_an <- lapply(prec_an, extract_winter_prec)

winter_prec_mean_prep <- Reduce("+", winter_prec_an) / length(winter_prec_an)
winter_prec_mean <- apply(winter_prec_mean_prep, MARGIN = c(1, 2), FUN = mean)

ekf_1816_winter_prec_prep <- ekf_1816_prec[,,12]

ekf_1817_prec <- get_prec(ekf_1817)

ekf_1817_winter_prec_prep <- ekf_1817_prec[,,1:2]
ekf_1816_17_winter_prec <- abind(ekf_1817_winter_prec_prep, ekf_1816_winter_prec_prep, along = 3)

ekf_1816_17_winter_prec_mean <- apply(ekf_1816_17_winter_prec, MARGIN = c(1, 2), FUN = mean)

winter_prec_anomaly_1816_17 <- winter_prec_mean - ekf_1816_17_winter_prec_mean
winter_prec_anomaly_1816__17_perc_prep <- winter_prec_anomaly_1816_17/winter_prec_mean
winter_prec_anomaly_1816_17_perc <- winter_prec_anomaly_1816__17_perc_prep*(-100)

winter_prec_anomaly_1816_17_perc <- (ekf_1816_17_winter_prec_mean/winter_prec_mean)*100
winter_prec_anomaly_1816_17_perc[winter_prec_anomaly_1816_17_perc < 0] <- NA

winter_prec_anomaly_1816_17_perc[winter_prec_anomaly_1816_17_perc > 300] <- NA #most of the very high values appears erroneous

lapply(nc_data_list, nc_close)

winter_prec_r_prep <- raster(t(winter_prec_anomaly_1816_17_perc), xmn=min(longitude), xmx=max(longitude), ymn=min(latitude), ymx=max(latitude), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

winter_prec_r <- raster::rotate(winter_prec_r_prep)

winter_prec_r_map <- tm_shape(winter_prec_r) + 
  tm_raster(style = "cont", palette = "BrBG", midpoint = 100, n = 20, title = "Precipitation anomaly (%)", legend.reverse = FALSE, legend.is.portrait = FALSE) +
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
#            main.title = "(D) Boreal winter precipitation percentages in 1816/1817 with respect to 1766-1866 mean",
            main.title.position = 0.05,
            main.title.size = 1) +
  tm_grid(labels.size = 0.8,  # Adjust label size
          labels.inside.frame = FALSE,  # Labels outside frame
          lines = FALSE,  # Turn off grid lines
          ticks = TRUE,  # Add ticks
          labels.show = TRUE)

tmap_save(winter_prec_r_map, filename = "figures/winter_prec_anomaly.pdf")
tmap_save(winter_prec_r_map, filename = "figures/winter_prec_anomaly.svg")

#extract anomaly values for the location of the lakes--------
extracted_summer_temp_an <- raster::extract(summer_r, full_ds_sf)
extracted_summer_prec_an <- raster::extract(summer_prec_r, full_ds_sf)
extracted_winter_temp_an <- raster::extract(winter_r, full_ds_sf)
extracted_winter_prec_an <- raster::extract(winter_prec_r, full_ds_sf)

full_ds_an <- full_ds %>%
  mutate(summer_1816_temp_an = extracted_summer_temp_an,
         summer_1816_prec_an = extracted_summer_prec_an,
         winter_1816_17_temp_an = extracted_winter_temp_an,
         winter_1816_17_prec_an = extracted_winter_prec_an)

write_csv(full_ds_an, "data/full_ds_an.csv")

#extract countries and continents------
world <- ne_countries(scale = "medium", returnclass = "sf")

extract_countries_prep <- st_join(full_ds_sf, world)

extract_countries <- st_drop_geometry(extract_countries_prep) %>% 
  dplyr::select(lake_name, name_long, continent.y)

extract_countries[which(extract_countries$lake_name == "Lake DV09"), "name_long"] <- "Canada"
extract_countries[which(extract_countries$lake_name == "Lake DV09"), "continent.y"] <- "North America"

extract_countries <- extract_countries %>%
  rename(continent = continent.y, 
         country = name_long,
         lake_name_raw = lake_name)

write_csv(extract_countries, "data/lake_countries.csv")
