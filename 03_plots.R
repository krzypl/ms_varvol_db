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
  ~lake_name, ~climate_var, ~relation, ~season, ~strength, ~other_drivers, ~comment,
  "Ayr Lake", "uncertain", NA, NA, NA, NA, NA,
  "Big Round Lake", "temperature", "positive", "summer", "r = 0.46", NA, NA,
  "Blue Lake", "temperature", "positive", "summer", "r2 = 0.31", NA, NA,
  "C2", "temperature", "positive", "summer", NA, NA, "association of varve thickness with summer temperature follows Hardy et al. (1996)",
  "Chala light", "precipitation", "positive", "summer", NA, "wind", "strong relationship with ENSO; thick light layer = La ninia",
#  "Chala dark", "uncertain", NA, "various", NA, NA, NA,
  "Chevalier dark", "temperature", "positive", "winter", "r = 0.71", "snowmelt", "r corresponds to nival units",
#  "Czechowskie", "uncertain", NA, NA, NA, NA, NA,
  "Donard", "temperature", "positive", "summer", "r = 0.57", NA, NA,
  "DV09", "temperature", "positive", "summer", NA, NA, "correlation with instrumental data was weak, but met station is distant",
  "East Lake", "uncertain", NA, "various", NA, "snow depth", "correlation with many variables was tested; no variable have dominating significance",
  "Elk Lake", "uncertain", NA, NA, NA, NA, "no information on varve thickness data in the text",
  "Green Lake", "hydrological extremes", "positive", "various", NA, "glacier recession", NA, 
  "Holzmaar", "temperature", "negative", "various", "various", "precipitation", "see jpg for correlations",
  "Hvítárvatn", "uncertain", NA, NA, NA, "rate of glacia erosion", NA,
  "Iceberg Lake", "temperature", "positive", "summer", "weak", "lake levels; glacier dynamics", NA,
#  "Kenai", "hydrological extremes", "positive", NA, NA, NA, NA,
  "Kusai light", "temperature", "positive", "summer", NA, NA, NA,
  "Kusai dark", "wind strength", "positive", "summer", NA, NA, "wind strength is modulated by Siberian High",
  "Lagoon Etoliko light", "temperature", "positive", "summer", NA, "productivity", NA,
"Lagoon Etoliko dark", "uncertain", NA, NA, NA, "productivity", NA,
  "Lehmilampi", "temperature", "negative", "various", NA, "erosion", NA,
  "Lower Murray Lake", "temperature", "positive", "summer", NA, NA, NA,
  "Montcortès light", "precipitation", "positive", "autumn", "r = 0.428", "temperature, and precipitation in other months", NA,
  "Nar Gölü light", "precipitation", "positive", "spring", NA, "evaporation", NA,
  "Nar Gölü dark", "uncertain", NA, NA, NA, "productivity", NA,
  "Nautajärvi light", "precipitation", "positive", "winter", NA, "winter temperature", NA,
"Nautajärvi dark", "uncertain", NA, NA, NA, NA, NA,
  "Oeschinen", "precipitation", "positive", "summer", "r = 0.6", NA, NA,
  "Ogac light", "temperature", "positive", "summer", "uncertain", "melting degree days etc.", "many variables related to summer thermal conditions can be important; no regression on instrumental data has been done",
  "Ogac dark", "uncertain", NA, "various", NA, NA, "a consequence of late summer/fall precipitation as well as of spring/early summer temperature",
  "Plomo", "temperature", "positive", "winter", NA, NA, "the lake is glacier fed and depends on amount of material delivered with a meltwater; boreal winter is austral summer!",
  "Sawtooth", "temperature", "positive", "summer", "r = 0.16", "melting degree-days r = 0.27", NA,
#  "Skilak", "hydrological extremes", "positive", NA, NA, NA, NA,
  "Upper Sopper dark", "temperature", "positive", "summer", "r = 0.82", NA, NA,
  "Żabińskie light", "uncertain", NA, NA, NA, NA, NA,
"Żabińskie dark", "uncertain", NA, NA, NA, NA, NA
) %>% #by season I mean boreal season
  mutate(across(1:7, as.factor),
         climate_var = factor(climate_var, levels = c("temperature", "precipitation", "wind strength",
                                                     "hydrological extremes", "uncertain")),
         season = factor(season, levels = c("spring", "summer", "autumn", "winter")),
         lake_name = factor(lake_name, levels = sort(lake_name, decreasing = TRUE))
         )

varve2climate_plot <- ggplot(varve2climate) +
  geom_point(aes(x = climate_var, y = lake_name, color = season), size = 5) +
  geom_point(aes(x = climate_var, y = lake_name, shape = relation), size = 6) +
  scale_color_manual(values = c("summer" = "#FF6666", "autumn" = "#B8860B", "winter" = "lightblue", "spring" = "lightgreen")) +
  scale_shape_manual(values = c("negative" = 45, "positive" = 43)) +
  labs(
    x = "Climate Variable",
    y = "Lake Name",
    color = "Season",
    shape = "Relationship with \n climate variable"
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