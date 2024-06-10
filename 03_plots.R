library(tidyverse)
library(ggrepel)
#plot anomalies------
full_ds_an <- read_csv("data/full_ds_an.csv") %>% 
  pivot_longer(cols = summer_1816_temp_an:winter_1816_17_prec_an,
               names_to = "anomaly", values_to = "value_of_anomaly") %>% 
  mutate(unit4anomaly = ifelse(anomaly == "summer_1816_temp_an" | anomaly == "winter_1816_17_temp_an", "°C", "%"))

anomalies_plot <- ggplot(full_ds_an, aes(y = lake_name, x = value_of_anomaly)) +
  geom_col() +
  facet_wrap(.~ anomaly, scales = "free")

#plot varve links to climate-------
varve2climate <- tribble(
  ~lake_name, ~climate_var, ~season, ~strength, ~other_drivers,
  "Ayr Lake", "uncertain", NA, NA, NA,
  "Big Round Lake", "temperature", "summer", r = 0.46, NA,
  "Blue Lake", "temperature", "summer", r2 = 0.31, NA,
  "C2", "uncertain", NA, NA, NA,
  
)

#plot varve thickness time series -------

full_ds <- read_csv("data/full_ds.csv") %>% 
  filter(age_CE > 1765 & age_CE < 1867)

varve_thick_plot <- ggplot(full_ds, aes(x = age_CE, y = varve_thick)) +
  geom_line() +
  geom_point(size = 0.4) +
  geom_vline(xintercept = 1816, color = "blue", alpha = 0.8) +
  facet_wrap(.~ lake_name, scales = "free_y")

varve_thick_plot

dark_lamin_thick_plot <- full_ds %>% 
  filter(!is.na(dark_lamin_thick)) %>% 
  ggplot(aes(x = age_CE, y = dark_lamin_thick)) +
  geom_line() +
  geom_point(size = 0.4) +
  geom_vline(xintercept = 1816, color = "blue", alpha = 0.8) +
  facet_wrap(.~ lake_name, scales = "free_y")

dark_lamin_thick_plot

light_lamin_thick_plot <- full_ds %>% 
  filter(!is.na(light_lamin_thick)) %>% 
  ggplot(aes(x = age_CE, y = light_lamin_thick)) +
  geom_line() +
  geom_point(size = 0.4) +
  geom_vline(xintercept = 1816, color = "blue", alpha = 0.8) +
  facet_wrap(.~ lake_name, scales = "free_y")

light_lamin_thick_plot

#plot peak detection results ---------
##total vt-----
#add labels to peaks so that the corresponding year is visible
pd_df_vt_total_mgcv <- read_csv("data/pd_df_vt_total_mgcv.csv")

pd_df_vt_total_mgcv_plot <- ggplot(pd_df_vt_total_mgcv, aes(x = age_CE, y = varve_thick)) +
  geom_line() +
  geom_point(size = 0.4) +
  geom_point(data = filter(pd_df_vt_total_mgcv, peaks_pos == 1),
             shape = 8, color = "red") +
  geom_point(data = filter(pd_df_vt_total_mgcv, peaks_neg == 1), 
             shape = 8, color = "blue") +
  geom_vline(xintercept = 1816, color = "blue", alpha = 0.8) +
  geom_text_repel(data = filter(pd_df_vt_total_mgcv,
                                (peaks_pos == 1 | peaks_neg == 1) & age_CE >= 1805 & age_CE <= 1825),
            aes(label = as.character(round(age_CE, 0))),
             vjust = -0.5) +
  facet_wrap(.~ lake_name, scales = "free_y")



##light laminae thickness-------
pd_df_vt_light_mgcv <- read_csv("data/pd_df_vt_light_mgcv.csv")

pd_df_vt_light_mgcv_plot <- ggplot(pd_df_vt_light_mgcv, aes(x = age_CE, y = light_lamin_thick)) +
  geom_line() +
  geom_point(size = 0.4) +
  geom_point(data = filter(pd_df_vt_light_mgcv, peaks_pos == 1),
             shape = 8, color = "red") +
  geom_point(data = filter(pd_df_vt_light_mgcv, peaks_neg == 1), 
             shape = 8, color = "blue") +
  geom_vline(xintercept = 1816, color = "blue", alpha = 0.8) +
  geom_text_repel(data = filter(pd_df_vt_light_mgcv,
                                (peaks_pos == 1 | peaks_neg == 1) & age_CE >= 1805 & age_CE <= 1825),
                  aes(label = as.character(round(age_CE, 0))),
                  vjust = -0.5) +
  facet_wrap(.~ lake_name, scales = "free_y")

pd_df_vt_light_mgcv + xlim(c(1800, 1840))

##dark laminae thickness-------
pd_df_vt_dark_mgcv <- read_csv("data/pd_df_vt_dark_mgcv.csv")

pd_df_vt_dark_mgcv_plot <- ggplot(pd_df_vt_dark_mgcv, aes(x = age_CE, y = dark_lamin_thick)) +
  geom_line() +
  geom_point(size = 0.4) +
  geom_point(data = filter(pd_df_vt_dark_mgcv, peaks_pos == 1),
             shape = 8, color = "red") +
  geom_point(data = filter(pd_df_vt_dark_mgcv, peaks_neg == 1), 
             shape = 8, color = "blue") +
  geom_vline(xintercept = 1816, color = "blue", alpha = 0.8) +
  geom_text_repel(data = filter(pd_df_vt_dark_mgcv,
                                (peaks_pos == 1 | peaks_neg == 1) & age_CE >= 1805 & age_CE <= 1825),
                  aes(label = as.character(round(age_CE, 0))),
                  vjust = -0.5) +
  facet_wrap(.~ lake_name, scales = "free_y")

pd_df_vt_dark_mgcv + xlim(c(1800, 1840))