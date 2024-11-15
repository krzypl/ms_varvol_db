library(tidyverse)
library(ggrepel)
library(patchwork)
library(gridExtra)
theme_set(theme_bw())
# Fig. 2. anomalies for the lake sites------
full_ds_an <- read_csv("data/full_ds_an.csv") %>% 
  pivot_longer(cols = summer_1816_temp_an:winter_1816_17_prec_an,
               names_to = "anomaly", values_to = "value_of_anomaly") %>% 
  mutate(unit4anomaly = ifelse(anomaly == "summer_1816_temp_an" | anomaly == "winter_1816_17_temp_an", "°C", "%"),
         lake_name = factor(lake_name, levels = sort(unique(lake_name), decreasing = TRUE))) %>% 
  mutate(anomaly = gsub("summer_1816_temp_an", "Summer temperature in 1816", anomaly),
         anomaly = gsub("winter_1816_17_temp_an", "Winter temperature in 1816/1817", anomaly),
         anomaly = gsub("summer_1816_prec_an", "Summer precipitation in 1816", anomaly),
         anomaly = gsub("winter_1816_17_prec_an", "Winter precipitation in 1816/1817", anomaly))

write_csv(full_ds_an, "data/full_ds_an_2plot.csv")

anomalies_plot_temp <- full_ds_an %>% 
  filter(anomaly == "Summer temperature in 1816" | anomaly == "Winter temperature in 1816/1817") %>%
  mutate(anomaly = gsub("Summer temperature in 1816", "Summer of 1816" , anomaly),
         anomaly = gsub("Winter temperature in 1816/1817", "Winter of 1816/1817", anomaly)) %>% 
  ggplot(aes(y = lake_name, x = value_of_anomaly)) +
  geom_col() +
  facet_wrap(.~anomaly, scales = "fixed", nrow = 2) +
  labs(
    x = "Temperature anomaly (°C)",
    y = NULL
  ) 

anomalies_plot_prec <- full_ds_an %>% 
  filter(anomaly == "Summer precipitation in 1816" | anomaly == "Winter precipitation in 1816/1817") %>% 
  mutate(anomaly = gsub("Summer precipitation in 1816", "Summer of 1816" , anomaly),
         anomaly = gsub("Winter precipitation in 1816/1817", "Winter of 1816/1817", anomaly)) %>% 
  ggplot(aes(y = lake_name, x = value_of_anomaly)) +
  geom_col() +
  facet_wrap(.~anomaly, scales = "fixed", nrow = 2) +
  labs(
    x = "Precipitation anomaly (%)",
    y = NULL
  )
  
anomalies_plot <- grid.arrange(anomalies_plot_temp, anomalies_plot_prec, ncol = 2)

ggsave(filename = "figures/anomalies_plot.svg",
       plot = anomalies_plot,
       width = 6.5,
       height = 7.5,
       device = "svg")
ggsave(filename = "figures/anomalies_plot.pdf",
       plot = anomalies_plot,
       width = 6.5,
       height = 7,
       device = "pdf")

# Fig. 3. plot varve links to climate-------
varve2climate <- tribble(
  ~lake_name, ~climate_var, ~relation, ~season, ~strength, ~other_drivers, ~comment,
  "Kassjön - varve", "NA", NA, NA, NA, NA, NA,
  "Linnévatnet - varve", "NA", NA, NA, NA, NA, NA,
  "Skilak - varve", "hydrological extremes", "positive", "summer", NA, NA, "autumn season also important",
  "Kenai - varve", "hydrological extremes", "positive", "summer", NA, NA, "autumn season also important",
  "Ayr Lake - varve", "NA", NA, NA, NA, NA, NA,
  "Big Round Lake - varve", "temperature", "positive", "summer", "r = 0.46", NA, NA,
  "Blue Lake - varve", "temperature", "positive", "summer", "r2 = 0.31", NA, NA,
  "Lake C2 - varve", "temperature", "positive", "summer", NA, NA, "association of varve thickness with summer temperature follows Hardy et al. (1996)",
  "Chala - light layer", "precipitation", "positive", "summer", NA, "wind", "strong relationship with ENSO; thick - light layer layer = La ninia",
#  "Chala - dark layer", "NA", NA, "various", NA, NA, NA,
  "Chevalier - dark layer", "temperature", "positive", "winter", "r = 0.71", "snowmelt", "r corresponds to nival units",
#  "Czechowskie", "NA", NA, NA, NA, NA, NA,
  "Donard - varve", "temperature", "positive", "summer", "r = 0.57", NA, NA,
  "Lake DV09 - varve", "temperature", "positive", "summer", NA, NA, "correlation with instrumental data was weak, but met station is distant",
  "East Lake - varve", "NA", NA, "various", NA, "snow depth", "correlation with many variables was tested; no variable have dominating significance",
  "Elk Lake - varve", "NA", NA, NA, NA, NA, "no information on varve thickness data in the text",
"Etoliko - dark layer", "NA", NA, NA, NA, "productivity", NA,  
"Etoliko - light layer", "temperature", "positive", "summer", NA, "productivity", NA,
  "Green Lake - varve", "hydrological extremes", "positive", "various", NA, "glacier recession", NA, 
  "Holzmaar - varve", "temperature", "negative", "various", "various", "precipitation", "see jpg for correlations",
  "Hvítárvatn - varve", "NA", NA, NA, NA, "rate of glacia erosion", NA,
  "Iceberg Lake - varve", "temperature", "positive", "summer", "weak", "lake levels; glacier dynamics", NA,
#  "Kenai", "hydrological extremes", "positive", NA, NA, NA, NA,
"Kuninkaisenlampi - dark layer", "NA", NA, NA, NA, NA, NA,
"Kuninkaisenlampi - light layer", "hydrological extremes", "positive", "spring", NA, NA, NA,
"Kusai - dark layer", "wind strength", "positive", "summer", NA, NA, "wind strength is modulated by Siberian High",
  "Kusai - light layer", "temperature", "positive", "summer", NA, NA, NA,
  "Lehmilampi - varve", "temperature", "negative", "various", NA, "erosion", NA,
  "Lower Murray Lake - varve", "temperature", "positive", "summer", NA, NA, NA,
  "Montcortès - light layer", "precipitation", "positive", "autumn", "r = 0.428", "temperature, and precipitation in other months", NA,
"Nar Gölü - dark layer", "NA", NA, NA, NA, "productivity", NA,
  "Nar Gölü - light layer", "precipitation", "positive", "spring", NA, "evaporation", NA,
"Nautajärvi - dark layer", "NA", NA, NA, NA, NA, NA,
  "Nautajärvi - light layer", "precipitation", "positive", "winter", NA, "winter temperature", NA,
  "Oeschinen - varve", "precipitation", "positive", "summer", "r = 0.6", NA, NA,
"Ogac - dark layer", "NA", NA, "various", NA, NA, "a consequence of late summer/fall precipitation as well as of spring/early summer temperature",
  "Ogac - light layer", "temperature", "positive", "summer", "NA", "melting degree days etc.", "many variables related to summer thermal conditions can be important; no regression on instrumental data has been done",
   "Plomo - varve", "temperature", "positive", "winter", NA, NA, "the lake is glacier fed and depends on amount of material delivered with a meltwater; boreal winter is austral summer!",
  "Sawtooth - varve", "temperature", "positive", "summer", "r = 0.16", "melting degree-days r = 0.27", NA,
#  "Skilak", "hydrological extremes", "positive", NA, NA, NA, NA,
  "Upper Sopper - dark layer", "temperature", "positive", "summer", "r = 0.82", NA, NA,
"Żabińskie - dark layer", "NA", NA, NA, NA, NA, NA,
  "Żabińskie - light layer", "NA", NA, NA, NA, NA, NA
) %>% #by season I mean boreal season
  mutate(across(1:7, as.factor),
         climate_var = factor(climate_var, levels = c("temperature", "precipitation", "wind strength",
                                                     "hydrological extremes", "NA")),
         season = factor(season, levels = c("spring", "summer", "autumn", "winter")),
         lake_name = factor(lake_name, levels = sort(lake_name, decreasing = TRUE))
         )

write_rds(varve2climate, "data/varve2climate.rds")

varve2climate_plot <- ggplot(varve2climate) +
  geom_point(aes(x = climate_var, y = lake_name, color = season), size = 5) +
  geom_point(aes(x = climate_var, y = lake_name, shape = relation), size = 6) +
  scale_color_manual(values = c("summer" = "#FF6666", "autumn" = "#B8860B", "winter" = "lightblue", "spring" = "lightgreen")) +
  scale_shape_manual(values = c("negative" = 45, "positive" = 43)) +
  labs(
    x = "Climate variable",
    y = NULL,
    color = "Season",
    shape = "Relationship with \n climate variable"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "figures/varve2climate_plot.svg",
       plot = varve2climate_plot,
       width = 6.5,
       height = 7,
       device = "svg")
ggsave(filename = "figures/varve2climate_plot.pdf",
       plot = varve2climate_plot,
       width = 6.5,
       height = 7,
       device = "pdf")
