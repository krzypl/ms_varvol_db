library(tidyverse)
library(knitr)
library(kableExtra)

full_ds <- read_csv("data/full_ds.csv")

#number of records from databases---------
full_ds %>% 
  distinct(lake_name, .keep_all = TRUE) %>%
  group_by(source) %>% 
  summarise(source_sum = n())

#total number of records
full_ds %>% 
  group_by(layer) %>% 
  distinct(lake_name, .keep_all = TRUE) %>% 
  summarise(n = n())

#number of lakes from which we have records----------
full_ds %>% 
  mutate(lake_name = str_replace_all(lake_name, "_[1-3]", "")) %>% 
  distinct(lake_name, .keep_all = TRUE) %>% 
  summarise(n = n())

#table listing records used in this study; Table 1----------
table_1 <- full_ds %>% 
  group_by(layer) %>% 
  distinct(lake_name, .keep_all = TRUE) %>%
  mutate(ref = sub("^(.*?\\d{4})\\..*", "\\1", ref),
         ref = gsub("\\b[a-zA-Z]+\\.", "", ref),
         ref = gsub(",", "", ref),
         ref = gsub(" et ", " et al. ", ref),
         ref = gsub("&", "and", ref),
         ref = gsub("Cuven  Francus  and Lamoureux  2011", "Cuven et al. 2011", ref),
         ref = gsub("Loso  2008", "Loso 2009", ref),
         ref = gsub("HALTIAHOVI  SAARINEN  and KUKKONEN  2007", "Haltia-hovi et al. 2007", ref),
         ref = gsub("Cook  et al.  2008", "Cook et al. 2009", ref),
         ref = gsub("Amann  Szidat  and Grosjean  2015", "Amann et al. 2015", ref),
         ref = gsub("Hughen  Overpeck  and Anderson  2000", "Hughen et al. 2000", ref),
         ref = gsub("Schiefer  Menounos  Slaymaker  2006", "Schiefer et al. 2006", ref),
         ref = gsub("Ojala  and    2005", "Ojala and Alenius 2005", ref),
         ref = gsub("Zhang  Liu  Feng  2022", "Zhang  et al. 2022", ref),
         ref = gsub("Woodbridge Jessie  thesis University of Plymouth ‘Late Holocene lake diatom-inferred palaeoclimate from central Turkey'. 2009", "Woodbridge 2009", ref),
         ref = gsub("Żarczynski  Wacnik  Tylmann  2019", "Żarczynski et al. 2019", ref),
         ref = gsub("  ", " ", ref),
         ref = str_replace_all(ref, "\\b(\\d{4})\\b", "(\\1)")
         ) %>% 
  ungroup() %>% 
  arrange(lake_name) %>% 
  # group_by(lake_name) %>%
  # summarize(
  #   lat = first(lat),
  #   lon = first(lon),
  #   age_CE = first(age_CE), 
  #   thickness = first(thickness), 
  #   source = first(source), 
  #   ref = first(ref),  
  #   layers_combined = paste(layer, collapse = ", ") 
  # ) %>% 
  mutate(ref = str_replace(ref, ";.*", ""),
         ref = str_replace(ref, "Amann Benjamin", "Amann et al. (2017)"),
         ref = str_replace(ref, "Lapointe François", "Lapointe et al. (2017)"),
         ref = str_replace(ref, "Vegas-Vilarrúbia Teresa", "Vegas-Vilarrúbia et al. (2022)"),
         source = str_replace(source, "author", "Author"),
         ID = 1:length(lake_name),
         lat = round(lat, digits = 5),
         lon = round(lon, digits = 5),
         label = lake_name,
         source = str_replace(source, "NOAA", "WDS-Paleo"),
         No = 1:length(lake_name)) %>%
  select(No, ID, lake_name, label, lat, lon, source, layer, ref) %>%
  mutate(lake_name = str_replace_all(lake_name, "_[1-3]", "")) %>% 
  rename(`No.` = No, "Lake name" = lake_name, "Record label" = label, Latitude = lat, Longitude = lon, "Data source" = source, Reference = ref, "Thickness of" = layer) %>% 
  mutate(ID = dense_rank(`Lake name`)) %>% 
  rename("ID on the maps" = ID, "Latitude (°)" = Latitude, "Longitude (°)" = Longitude) %>% 
  mutate(`Record label` = paste(`Record label`, `Thickness of`, sep = " - "))

kable(table_1, format = "html") %>% 
  kable_styling() %>% 
  save_kable("table/table_1.html")

#Nino3.4 Index Reconstruction------
nino3_4_index <- read_table("https://www.ncei.noaa.gov/pub/data/paleo/treering/reconstructions/enso-li2013.txt", skip = 125)

ggplot(nino3_4_index, aes(x = age_AD, y = sst.anom)) +
  geom_point()+
  geom_line()+
  geom_vline(xintercept = 1816)