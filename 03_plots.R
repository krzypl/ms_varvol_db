library(tidyverse)
library(ggrepel)
#plot anomalies------
full_ds_an <- read_csv("data/full_ds_an.csv") %>% 
  pivot_longer(cols = summer_1816_temp_an:winter_1816_17_prec_an,
               names_to = "anomaly", values_to = "value_of_anomaly") %>% 
  mutate(unit4anomaly = ifelse(anomaly == "summer_1816_temp_an" | anomaly == "winter_1816_17_temp_an", "°C", "%"),
         lake_name = factor(lake_name, levels = sort(unique(lake_name), decreasing = TRUE)))

anomalies_plot <- ggplot(full_ds_an, aes(y = lake_name, x = value_of_anomaly)) + #tutaj mozna zrobic osobno ploty dla temperatury i opadu, zeby utrzymac te same skale; inaczej jest wrazenie, ze sie rozjezdza 
  geom_col() +
  facet_wrap(.~ anomaly, scales = "free")

ggsave(filename = "figures/anomalies_plot.svg",
       plot = anomalies_plot,
       device = "svg")
ggsave(filename = "figures/anomalies_plot.pdf",
       plot = anomalies_plot,
       width = 12.5,
       height = 10,
       device = "pdf")

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

ggsave(filename = "figures/varve2climate_plot.svg",
       plot = varve2climate_plot,
       device = "svg")
ggsave(filename = "figures/varve2climate_plot.pdf",
       plot = varve2climate_plot,
       width = 8,
       height = 10,
       device = "pdf")

#Nino3.4 Index Reconstruction------
nino3_4_index <- read_table("https://www.ncei.noaa.gov/pub/data/paleo/treering/reconstructions/enso-li2013.txt", skip = 125)

ggplot(nino3_4_index, aes(x = age_AD, y = sst.anom)) +
  geom_point()+
  geom_line()+
  geom_vline(xintercept = 1816)