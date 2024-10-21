library(tidyverse)
library(ncdf4)
library(raster)
library(sf)

full_ds <- read_csv("data/full_ds.csv") %>% 
  mutate(lake_name = str_replace_all(lake_name, "_[1-3]", "")) %>% 
  distinct(lake_name, .keep_all = TRUE) %>% 
  arrange(lake_name)

lake_coords <- full_ds %>% 
  select(lake_name, lat, lon)
# Get names for data files
ekf_names <- list.files(path = "data/ekf400_ens_mem_mean", full.names = TRUE)

lake_coords_sf <- st_as_sf(lake_coords, 
                           coords = c("lon", "lat"), 
                           crs = 4326)

# summer ------------
nc_data_list <- lapply(ekf_names, nc_open) 

# Function to extract temperature data from the netCDF file
get_temp <- function(nc_file) {
  ncvar_get(nc_file, "air_temperature")
}

# Function to extract summer temperatures (June, July, August)
extract_summer <- function(arr) {
  summer_temp <- arr[,,6:8]  # Extract June, July, and August
  return(summer_temp)
}

# Loop through all netCDF files and extract summer temperatures
temp_list <- lapply(nc_data_list, get_temp)

# Extract summer temperatures for each year (1766-1866)
summer_temp_list <- lapply(temp_list, extract_summer)

# Now calculate the mean summer temperature for each grid cell for each year
summer_temp_mean_list <- lapply(summer_temp_list, function(summer_arr) {
  apply(summer_arr, MARGIN = c(1, 2), FUN = mean)  # Mean over June, July, August
})

longitude_prep <- ncvar_get(nc_data_list[[1]], "longitude")
latitude <- ncvar_get(nc_data_list[[1]], "latitude")
longitude <- ifelse(longitude_prep > 180, longitude_prep + (360-max(longitude_prep)), longitude_prep)

# Initialize an empty list to store the rasters for each year
summer_rasters_list <- list()

# Loop through each year's summer temperature and create a raster object
for (i in 1:length(summer_temp_mean_list)) {
  # Prepare a raster from the temperature data
  summer_r_prep <- raster(t(summer_temp_mean_list[[i]]), 
                          xmn = min(longitude), xmx = max(longitude), 
                          ymn = min(latitude), ymx = max(latitude), 
                          crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  
  # Rotate the raster to match the standard global orientation
  summer_r <- raster::rotate(summer_r_prep)
  
  # Add the raster to the list, naming it by the corresponding year (1766 + i - 1)
  year_name <- 1766 + i - 1
  summer_rasters_list[[as.character(year_name)]] <- summer_r
}

# Close the netCDF files after use
lapply(nc_data_list, nc_close)

lake_coords_sf <- st_transform(lake_coords_sf, crs = crs(summer_rasters_list[[1]]))

# Initialize an empty list to store results
results_list <- list()

# Loop through each year (each raster in the list)
for (year in names(summer_rasters_list)) {
  # Extract the temperature values for the lake locations from the raster of the current year
  temperature_values <- raster::extract(summer_rasters_list[[year]], lake_coords_sf)
  
  # Create a tibble with the lake names, year, and extracted temperature values
  year_data <- tibble(
    lake_name = lake_coords_sf$lake_name,
    year = as.numeric(year),
    summer_temperature = temperature_values
  )
  
  # Append the current year's data to the results list
  results_list[[year]] <- year_data
}

# Combine all years into a single tibble
ekf_summer_temperatures <- bind_rows(results_list) %>% 
  mutate(summer_temperature = summer_temperature - 273.15)

# winter temperarue record -----------
# Function to extract summer temperatures (June, July, August)
extract_winter <- function(arr) {
  summer_temp <- arr[,,c(1,2,12)]
  return(summer_temp)
}

# Loop through all netCDF files and extract summer temperatures
temp_list <- lapply(nc_data_list, get_temp)

# Extract winter temperatures for each year (1766-1867)
winter_temp_list <- lapply(temp_list, extract_winter)

ekf_1867 <- nc_open("data/ekf400_ens_mem_mean_1867/EKF400_ens_mem_Mean_v2.0_266.nc")
ekf_1867_temp <- get_temp(ekf_1867)

ekf_1867_winter_prep <- ekf_1817_temp[,,1:2]

merged_data <- list()

# Step 1: Process the first object (remove the 1st vector from the 3rd dimension)
first_obj <- winter_temp_list[[1]][,,2:3]  # Keep only 2nd and 3rd vectors
merged_data[[1]] <- first_obj

# Step 2: Process the successive pairs of objects
for (i in 1:(length(winter_temp_list) - 1)) {
  # Get the current and next objects
  current_obj <- winter_temp_list[[i]]
  next_obj <- winter_temp_list[[i + 1]]
  
  # Extract the 3rd vector from the current object
  third_vec_current <- current_obj[,,3]
  
  # Extract the 1st and 2nd vectors from the next object
  first_second_vec_next <- next_obj[,,1:2]
  
  # Merge them: cbind third vector of current with first two of the next
  merged_vectors <- abind::abind(third_vec_current, first_second_vec_next, along = 3)
  
  # Append the merged data to the list
  merged_data[[i]] <- merged_vectors
}

# Step 3: Handle the last object and additional_obj
last_obj <- winter_temp_list[[length(winter_temp_list)]]
third_vec_last <- last_obj[,,3]

# Merge the 3rd vector of the last object with the two vectors of additional_obj
final_merged <- abind::abind(third_vec_last, ekf_1867_winter_prep, along = 3)

# Append the final merged result to the list
merged_data[[length(winter_temp_list)]] <- final_merged

winter_temp_list <- merged_data

# Now calculate the mean summer temperature for each grid cell for each year
winter_temp_mean_list <- lapply(winter_temp_list, function(winter_arr) {
  apply(winter_arr, MARGIN = c(1, 2), FUN = mean)  # Mean over December, January, February
})

longitude_prep <- ncvar_get(nc_data_list[[1]], "longitude")
latitude <- ncvar_get(nc_data_list[[1]], "latitude")
longitude <- ifelse(longitude_prep > 180, longitude_prep + (360-max(longitude_prep)), longitude_prep)

# Initialize an empty list to store the rasters for each year
winter_rasters_list <- list()

# Loop through each year's winter temperature and create a raster object
for (i in 1:length(winter_temp_mean_list)) {
  # Prepare a raster from the temperature data
  winter_r_prep <- raster(t(winter_temp_mean_list[[i]]), 
                          xmn = min(longitude), xmx = max(longitude), 
                          ymn = min(latitude), ymx = max(latitude), 
                          crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  
  # Rotate the raster to match the standard global orientation
  winter_r <- raster::rotate(winter_r_prep)
  
  # Add the raster to the list, naming it by the corresponding year (1766 + i - 1)
  year_name <- 1766 + i - 1
  winter_rasters_list[[as.character(year_name)]] <- winter_r
}

# Close the netCDF files after use
lapply(nc_data_list, nc_close)

lake_coords_sf <- st_transform(lake_coords_sf, crs = crs(winter_rasters_list[[1]]))

# Initialize an empty list to store results
results_list <- list()

# Loop through each year (each raster in the list)
for (year in names(winter_rasters_list)) {
  # Extract the temperature values for the lake locations from the raster of the current year
  temperature_values <- raster::extract(winter_rasters_list[[year]], lake_coords_sf)
  
  # Create a tibble with the lake names, year, and extracted temperature values
  year_data <- tibble(
    lake_name = lake_coords_sf$lake_name,
    year = as.numeric(year),
    winter_temperature = temperature_values
  )
  
  # Append the current year's data to the results list
  results_list[[year]] <- year_data
}

# Combine all years into a single tibble
ekf_winter_temperatures <- bind_rows(results_list) %>% 
  mutate(winter_temperature = winter_temperature - 273.15)

#summer precipitation --------
nc_data_list <- lapply(ekf_names, nc_open) 

# Function to extract precerature data from the netCDF file
get_prec <- function(nc_file) {
  ncvar_get(nc_file, "total precipitation")
}

prec_list <- lapply(nc_data_list, get_prec)


summer_prec_list <- lapply(prec_list, extract_summer)

summer_prec_mean_list <- lapply(summer_prec_list, function(summer_arr) {
  apply(summer_arr, MARGIN = c(1, 2), FUN = mean)  # Mean over June, July, August
})

longitude_prep <- ncvar_get(nc_data_list[[1]], "longitude")
latitude <- ncvar_get(nc_data_list[[1]], "latitude")
longitude <- ifelse(longitude_prep > 180, longitude_prep + (360-max(longitude_prep)), longitude_prep)

# Initialize an empty list to store the rasters for each year
summer_rasters_list <- list()

for (i in 1:length(summer_prec_mean_list)) {
  # Prepare a raster from the prec data
  summer_r_prep <- raster(t(summer_prec_mean_list[[i]]), 
                          xmn = min(longitude), xmx = max(longitude), 
                          ymn = min(latitude), ymx = max(latitude), 
                          crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  
  # Rotate the raster to match the standard global orientation
  summer_r <- raster::rotate(summer_r_prep)
  
  # Add the raster to the list, naming it by the corresponding year (1766 + i - 1)
  year_name <- 1766 + i - 1
  summer_rasters_list[[as.character(year_name)]] <- summer_r
}

# Close the netCDF files after use
lapply(nc_data_list, nc_close)

lake_coords_sf <- st_transform(lake_coords_sf, crs = crs(summer_rasters_list[[1]]))

# Initialize an empty list to store results
results_list <- list()

# Loop through each year (each raster in the list)
for (year in names(summer_rasters_list)) {
  
  precipitation_values <- raster::extract(summer_rasters_list[[year]], lake_coords_sf)
  
  # Create a tibble with the lake names, year, and extracted values
  year_data <- tibble(
    lake_name = lake_coords_sf$lake_name,
    year = as.numeric(year),
    summer_precipitation = precipitation_values
  )
  
  # Append the current year's data to the results list
  results_list[[year]] <- year_data
}

# Combine all years into a single tibble
ekf_summer_precipitation <- bind_rows(results_list)

#winter precipitation --------
# Function to extract summer temperatures (June, July, August)
extract_winter <- function(arr) {
  summer_temp <- arr[,,c(1,2,12)]
  return(summer_temp)
}

# Loop through all netCDF files and extract summer temperatures
prec_list <- lapply(nc_data_list, get_prec)


winter_prec_list <- lapply(prec_list, extract_winter)

ekf_1867 <- nc_open("data/ekf400_ens_mem_mean_1867/EKF400_ens_mem_Mean_v2.0_266.nc")
ekf_1867_prec <- get_prec(ekf_1867)

ekf_1867_winter_prep <- ekf_1817_prec[,,1:2]

merged_data <- list()

# Step 1: Process the first object (remove the 1st vector from the 3rd dimension)
first_obj <- winter_prec_list[[1]][,,2:3]  # Keep only 2nd and 3rd vectors
merged_data[[1]] <- first_obj

# Step 2: Process the successive pairs of objects
for (i in 1:(length(winter_prec_list) - 1)) {
  # Get the current and next objects
  current_obj <- winter_prec_list[[i]]
  next_obj <- winter_prec_list[[i + 1]]
  
  # Extract the 3rd vector from the current object
  third_vec_current <- current_obj[,,3]
  
  # Extract the 1st and 2nd vectors from the next object
  first_second_vec_next <- next_obj[,,1:2]
  
  # Merge them: cbind third vector of current with first two of the next
  merged_vectors <- abind::abind(third_vec_current, first_second_vec_next, along = 3)
  
  # Append the merged data to the list
  merged_data[[i]] <- merged_vectors
}

# Step 3: Handle the last object and additional_obj
last_obj <- winter_prec_list[[length(winter_prec_list)]]
third_vec_last <- last_obj[,,3]

# Merge the 3rd vector of the last object with the two vectors of additional_obj
final_merged <- abind::abind(third_vec_last, ekf_1867_winter_prep, along = 3)

# Append the final merged result to the list
merged_data[[length(winter_prec_list)]] <- final_merged

winter_prec_list <- merged_data

# Now calculate the mean summer temperature for each grid cell for each year
winter_prec_mean_list <- lapply(winter_prec_list, function(winter_arr) {
  apply(winter_arr, MARGIN = c(1, 2), FUN = mean)  # Mean over December, January, February
})

longitude_prep <- ncvar_get(nc_data_list[[1]], "longitude")
latitude <- ncvar_get(nc_data_list[[1]], "latitude")
longitude <- ifelse(longitude_prep > 180, longitude_prep + (360-max(longitude_prep)), longitude_prep)

# Initialize an empty list to store the rasters for each year
winter_rasters_list <- list()

# Loop through each year's winter temperature and create a raster object
for (i in 1:length(winter_prec_mean_list)) {
  # Prepare a raster from the temperature data
  winter_r_prep <- raster(t(winter_prec_mean_list[[i]]), 
                          xmn = min(longitude), xmx = max(longitude), 
                          ymn = min(latitude), ymx = max(latitude), 
                          crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  
  # Rotate the raster to match the standard global orientation
  winter_r <- raster::rotate(winter_r_prep)
  
  # Add the raster to the list, naming it by the corresponding year (1766 + i - 1)
  year_name <- 1766 + i - 1
  winter_rasters_list[[as.character(year_name)]] <- winter_r
}

# Close the netCDF files after use
lapply(nc_data_list, nc_close)

lake_coords_sf <- st_transform(lake_coords_sf, crs = crs(winter_rasters_list[[1]]))

# Initialize an empty list to store results
results_list <- list()

# Loop through each year (each raster in the list)
for (year in names(winter_rasters_list)) {
  # Extract the temperature values for the lake locations from the raster of the current year
  precipitation_values <- raster::extract(winter_rasters_list[[year]], lake_coords_sf)
  
  # Create a tibble with the lake names, year, and extracted temperature values
  year_data <- tibble(
    lake_name = lake_coords_sf$lake_name,
    year = as.numeric(year),
    winter_precipitation = precipitation_values
  )
  
  # Append the current year's data to the results list
  results_list[[year]] <- year_data
}

# Combine all years into a single tibble
ekf_winter_precipitation <- bind_rows(results_list)

# all data combined -------

ekf_record_final_prep_base <- ekf_summer_temperatures %>% 
  left_join(ekf_winter_temperatures) %>% 
  left_join(ekf_summer_precipitation) %>% 
  left_join(ekf_winter_precipitation) %>% 
  rename(age_CE = year) %>%
  mutate(layer = "varve")

detrended_records <- read_csv("data/detrended_records.csv")

lake_names_detrended <- unique(detrended_records$lake_name)

ekf_record_final_prep_base_east_lake_prep <- ekf_record_final_prep_base %>% 
  mutate(lake_name = gsub("East Lake", "East Lake_1", lake_name)) %>% 
  filter(lake_name == "East Lake_1")

ekf_record_final_prep_base_east_lake <- ekf_record_final_prep_base %>% 
  mutate(lake_name = gsub("East Lake", "East Lake_2", lake_name)) %>% 
  add_row(ekf_record_final_prep_base_east_lake_prep)

ekf_record_final_prep_base_hvi_lake_prep <- ekf_record_final_prep_base %>% 
  mutate(lake_name = gsub("Hvítárvatn", "Hvítárvatn_1", lake_name)) %>% 
  filter(lake_name == "Hvítárvatn_1")

ekf_record_final_prep_base_hvi_lake <- ekf_record_final_prep_base_east_lake %>% 
  mutate(lake_name = gsub("Hvítárvatn", "Hvítárvatn_2", lake_name)) %>% 
  add_row(ekf_record_final_prep_base_hvi_lake_prep)

ekf_record_final_prep_base_iceberg_lake_prep <- ekf_record_final_prep_base %>% 
  mutate(lake_name = gsub("Iceberg Lake", "Iceberg Lake_1", lake_name)) %>% 
  filter(lake_name == "Iceberg Lake_1")

ekf_record_final_prep_base_iceberg_lake <- ekf_record_final_prep_base_hvi_lake %>% 
  mutate(lake_name = gsub("Iceberg Lake", "Iceberg Lake_2", lake_name)) %>% 
  add_row(ekf_record_final_prep_base_iceberg_lake_prep)

ekf_record_final_prep_base_skilak_lake_prep <- ekf_record_final_prep_base %>% 
  mutate(lake_name = gsub("Skilak", "Skilak_1", lake_name)) %>% 
  filter(lake_name == "Skilak_1")

ekf_record_final_prep_base_skilak_lake <- ekf_record_final_prep_base_iceberg_lake %>% 
  mutate(lake_name = gsub("Skilak", "Skilak_2", lake_name)) %>% 
  add_row(ekf_record_final_prep_base_skilak_lake_prep) %>% 
  arrange(lake_name)

unique(ekf_record_final_prep_base_skilak_lake$lake_name) %in% lake_names_detrended


ekf_record_final_prep_base2 <- ekf_record_final_prep_base_skilak_lake %>% 
  mutate(layer = "light layer") %>% 
  add_row(ekf_record_final_prep_base_skilak_lake)

ekf_record_final_prep <- ekf_record_final_prep_base_skilak_lake %>% 
  mutate(layer = "dark layer") %>% 
  add_row(ekf_record_final_prep_base2)

ekf_record_final <- ekf_record_final_prep %>% 
  left_join(detrended_records, by = c("lake_name", "age_CE", "layer")) %>% 
  filter(!is.na(detrended_thickness)) %>% 
  mutate(record_ID = paste(lake_name, layer, sep = " - ")) %>% 
  arrange(record_ID)

write_csv(ekf_record_final, "data/ekf_record_final.csv")

#check distributions
ggplot(ekf_record_final, aes(x = detrended_thickness)) +
  geom_histogram() +
  facet_wrap(.~record_ID, scales = "free")

ggplot(ekf_record_final, aes(x = summer_temperature)) +
  geom_histogram() +
  facet_wrap(.~record_ID, scales = "free")

ggplot(ekf_record_final, aes(x = winter_temperature)) +
  geom_histogram() +
  facet_wrap(.~record_ID, scales = "free")

ggplot(ekf_record_final, aes(x = summer_precipitation)) +
  geom_histogram() +
  facet_wrap(.~record_ID, scales = "free")

ggplot(ekf_record_final, aes(x = winter_precipitation)) +
  geom_histogram() +
  facet_wrap(.~record_ID, scales = "free")

# calculating correlations -------

ekf_record_final <- read_csv("data/ekf_record_final.csv")

ekf_record_final_cor <- ekf_record_final %>% 
  group_by(record_ID) %>% 
  summarise(summer_temp_cor = cor(detrended_thickness, summer_temperature),
            winter_temp_cor = cor(detrended_thickness, winter_temperature),
            summer_prec_cor = cor(detrended_thickness, summer_precipitation),
            winter_prec_cor = cor(detrended_thickness, winter_precipitation)) %>% 
  pivot_longer(cols = summer_temp_cor:winter_prec_cor, names_to = "variable", values_to = "correlation")


correlation_plot <- ggplot(ekf_record_final_cor, aes(x = variable, y = correlation)) +
  geom_col() +
  facet_wrap(.~record_ID, scales = "fixed")


highest_correlation <- function(x, y) {
  ccf_result <- ccf(x, y, lag.max = 10, plot = FALSE)
  correlations <- ccf_result$acf
  max_corr_index <- which.max(abs(correlations))
  max_corr <- correlations[max_corr_index]
  return(max_corr)
}

lag_for_highest_correlation <- function(x, y) {
  ccf_result <- ccf(x, y, lag.max = 10, plot = FALSE)
  correlations <- ccf_result$acf
  lags <- ccf_result$lag
  max_corr_index <- which.max(abs(correlations))
  max_corr <- correlations[max_corr_index]
  best_lag <- lags[max_corr_index]
  return(best_lag)
}

ekf_record_final_cor_lagged <- ekf_record_final %>% 
  group_by(record_ID) %>% 
  summarise(summer_temp_cor = highest_correlation(detrended_thickness, summer_temperature),
            winter_temp_cor = highest_correlation(detrended_thickness, winter_temperature),
            summer_prec_cor = highest_correlation(detrended_thickness, summer_precipitation),
            winter_prec_cor = highest_correlation(detrended_thickness, winter_precipitation)) %>% 
  pivot_longer(cols = summer_temp_cor:winter_prec_cor, names_to = "variable", values_to = "correlation")

correlation_lagged_plot <- ggplot(ekf_record_final_cor_lagged, aes(x = variable, y = correlation)) +
  geom_col() +
  facet_wrap(.~record_ID, scales = "fixed")

# same but with Spearman

ekf_record_final_cor_spearman <- ekf_record_final %>% 
  group_by(record_ID) %>% 
  summarise(summer_temp_cor = cor(detrended_thickness, summer_temperature, method = "spearman"),
            winter_temp_cor = cor(detrended_thickness, winter_temperature, method = "spearman"),
            summer_prec_cor = cor(detrended_thickness, summer_precipitation, method = "spearman"),
            winter_prec_cor = cor(detrended_thickness, winter_precipitation, method = "spearman")) %>% 
  pivot_longer(cols = summer_temp_cor:winter_prec_cor, names_to = "variable", values_to = "correlation")

correlation_plot_spearman <- ggplot(ekf_record_final_cor_spearman, aes(x = variable, y = correlation)) +
  geom_col() +
  facet_wrap(.~record_ID, scales = "fixed")

ekf_record_final_cor_lagged_spearman <- ekf_record_final %>% 
  group_by(record_ID) %>% 
  summarise(summer_temp_cor = highest_correlation(rank(detrended_thickness), rank(summer_temperature)),
            winter_temp_cor = highest_correlation(rank(detrended_thickness), rank(winter_temperature)),
            summer_prec_cor = highest_correlation(rank(detrended_thickness), rank(summer_precipitation)),
            winter_prec_cor = highest_correlation(rank(detrended_thickness), rank(winter_precipitation))) %>% 
  pivot_longer(cols = summer_temp_cor:winter_prec_cor, names_to = "variable", values_to = "correlation") %>% 
  mutate(variable = gsub("_cor", "", variable),
         variable = as.factor(variable))

ekf_record_final_cor_lagged_spearman$variable <- 
  factor(ekf_record_final_cor_lagged_spearman$variable,                                                        levels = c("summer_temp", "winter_temp", "summer_prec", "winter_prec"))

correlation_lagged_plot_spearman <- ggplot(ekf_record_final_cor_lagged_spearman, aes(x = variable, y = correlation, fill = variable)) +
  geom_col() +
  labs(x = "Climate variable", y = "Correlation coefficient") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_manual(values = c("summer_temp" = "darkred",
                               "winter_temp" = "lightcoral",
                               "summer_prec" = "darkblue",
                               "winter_prec" = "lightblue")) +
  facet_wrap(.~record_ID, scales = "fixed", ncol = 4) +
  theme(legend.position = "none")
 
ggsave(filename = "figures/correlation_lagged_plot_spearman.svg",
       plot = correlation_lagged_plot_spearman,
       width = 6.5,
       height = 9,
       device = "svg")
ggsave(filename = "figures/correlation_lagged_plot_spearman.pdf",
       plot = correlation_lagged_plot_spearman,
       width = 6.5,
       height = 9,
       device = "pdf")
