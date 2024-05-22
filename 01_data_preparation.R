library(tidyverse)
library(pangaear)

#VARDA-----------

varda_zip <- download.file("https://datapub.gfz-potsdam.de/download/10.5880.GFZ.4.3.2019.003/2019-003_Varda-export-v1.3.zip", destfile = "data/varda_raw.zip", mode = "wb") #downloading raw data in zip

unzip("data/varda_raw.zip", exdir = "data/varda_raw") #unzip data

varda_datasets_names <- list.files(path = "data/varda_raw/csv/records", full.names = TRUE) #get names for data files

varda_datasets <- lapply(varda_datasets_names, read.csv) #create a list containing all available datasets

varda_index <- as_tibble(read.csv("data/varda_raw/csv/index.csv")) %>% 
  arrange(dataset.file) #get metadata

varda_index_red <- varda_index %>% 
  filter(dataset.ageMax > 150 & dataset.ageMin < 100 & dataset.category.name == "Varves" & dataset.samples > dataset.ageSpan/2) #extract data covering at least the period between 150 - 100 yrs BP based on onformation included in metadata

#filter out the datasets not included in varda_index_red 
varda_datasets_red_prep <- varda_datasets 
names(varda_datasets_red_prep) <- varda_index$dataset.file

get_num_rows <- function(df) {
  nrow(df)
}

varda_datasets_red <- tibble(names = names(varda_datasets_red_prep), df = varda_datasets_red_prep)  %>% 
  filter(names %in% varda_index_red$dataset.file) %>% 
  mutate(lake_name = varda_index_red$dataset.core.lake.name,
         n_rows = map_int(df, get_num_rows)) #counting out the number of rows enables checking match between number of observations in a given dataset and the number of samples reported in metadata; this is done to identify potentially the incorrectly assigned records

n_match <- varda_datasets_red %>% 
  filter(!n_rows == varda_index_red$dataset.samples) #dimensions of few records does not match information provided in the metadata; these records are separately uploded in the following lines of the code

varda_datasets_red$df[[which(varda_datasets_red$names == n_match$names[1])]] <- read.csv("data/varda_raw/csv/records/DV09-Core_B-Varves-Courtney_et_al-2013.csv")

varda_datasets_red$df[[which(varda_datasets_red$names == n_match$names[2])]] <- read.csv("data/varda_raw/csv/records/Donard-95DON_B3_95DON_05-Varves-Moore_et_al-2001.csv")

varda_datasets_red$df[[which(varda_datasets_red$names == n_match$names[3])]] <- read.csv("data/varda_raw/csv/records/Hvitarvatn-HVT031-Varves-Larsen_et_al-2011.csv")

varda_datasets_red$df[[which(varda_datasets_red$names == n_match$names[4])]] <- read.csv("data/varda_raw/csv/records/Hvitarvatn-HVT032-Varves-Larsen_et_al-2011.csv")

varda_datasets_red$df[[which(varda_datasets_red$names == n_match$names[5])]] <- read.csv("data/varda_raw/csv/records/Hvitarvatn-HVT033-Varves-Larsen_et_al-2011.csv")

#check if everything is fine after the manuall corection
varda_datasets_red %>% 
mutate(n_rows = map_int(df, get_num_rows)) %>% 
  filter(!n_rows == varda_index_red$dataset.samples) #all records are now correctly assigned

varda_datasets_red[9, ][[2]][[1]] <- varda_datasets_red[9, ][[2]][[1]][-which(varda_datasets_red[9, ][[2]][[1]]$varveThicknessTotal == "Eroded"),] #removing characters from varveThicknessTotal to convert the column to numeric

varda_datasets_red[9, ][[2]][[1]]$varveThicknessTotal <- as.numeric(varda_datasets_red[9, ][[2]][[1]]$varveThicknessTotal)


varda_long <- varda_datasets_red %>%
  mutate(ref = varda_index_red$dataset.publication.citation,
         lon = varda_index_red$dataset.core.lake.longitude,
         lat = varda_index_red$dataset.core.lake.latitude) %>% 
  select(!n_rows) %>% 
  unnest(df) #unnesting data frames to get easy access to all the observations

varda_long$ref[which(varda_long$lake_name == "Ogac")] <- "Hughen, K.A., 2009. NOAA/WDS Paleoclimatology - Ogac Lake, Baffin Island 2,000 Year Varve Thickness Data. [indicate subset used]. NOAA National Centers for Environmental Information. Available at: https://doi.org/10.25921/aqex-w486."

names(varda_long) #check the veriables to select important ones; after screening out availble variables it seems reasonable to retain only columns contatinng varve ages, and varve thickness measurment, full and for light and dark laminaes. However, these require some adjustments:

x_varve_thick <- varda_long %>% 
  group_by(names) %>% 
  summarise(vt_na = max(varveThicknessTotal)) %>% 
  filter(is.na(vt_na)) #identify records lacking varveThicknessTotal; reason for the lack varies. Sometimes it is the matter of missing few NAs. However, in two instances there is a need to calculate the desired values:

varda_long[which(varda_long$names == "Ogac--Varves-Hughen_et_al-2009"), ] <- varda_long %>% 
  filter(names == "Ogac--Varves-Hughen_et_al-2009") %>% 
  mutate(varveThicknessTotal = darkLayerThickness + calcVarveThicknessTotal)

varda_long[which(varda_long$names == "Iceberg_Lake--Varves-Loso_et_al-2008"), ] <- varda_long %>% 
  filter(names == "Iceberg_Lake--Varves-Loso_et_al-2008") %>% 
  mutate(varveThicknessTotal = varveThicknessRaw)

x_varve_age <- varda_long %>% 
  group_by(names) %>% 
  summarise(va_na = max(varveAge)) %>% 
  filter(is.na(va_na)) #identify records lacking varveAge; there are only two records like that and it relates only to short sections of these records

#tidying up the final data frame
varda_long_red_prep <- varda_long %>% 
  filter(!is.na(varveAge) & !is.na(varveThicknessTotal)) %>% 
  mutate(calciteLayerThickness = ifelse(is.na(calciteLayerThickness), 
                                        calcVarveThicknessTotal,
                                        calciteLayerThickness),
         lightLayerThickness = ifelse(is.na(lightLayerThickness),
                                      calciteLayerThickness,
                                      lightLayerThickness),
         darkLayerThickness = ifelse(is.na(darkLayerThickness),
                                     organicLayerThickness,
                                     darkLayerThickness),
         age_CE = 1950 - varveAge ) %>% 
  select(names, lake_name, ref, lat, lon, varveAge, age_CE, varveThicknessTotal, lightLayerThickness,
         darkLayerThickness)

#extract only the data <1600 CE
age_min <- varda_long_red_prep %>% 
  group_by(names) %>% 
  summarise(min_age = min(age_CE))

varda_long_red <- varda_long_red_prep %>% 
  filter(age_CE >= 1600) %>% 
  filter(!lake_name == "Chatyr Kol") %>% 
  rename(age_BP = varveAge,
         varve_thick = varveThicknessTotal,
         light_lamin_thick = lightLayerThickness,
         dark_lamin_thick = darkLayerThickness)

#there are few records from two lakes. Identify these lakes and change the names to easily plot the data separately for each of the records
names2change <- varda_long_red %>% 
  distinct(names) %>%
  mutate(first_word = str_extract(names, "^[^_-]+")) %>% 
  group_by(first_word) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

unique(varda_long_red$names)

varda_long_red$lake_name[which(varda_long_red$names == "Hvitarvatn-HVT031-Varves-Larsen_et_al-2011")] <- "Hvítárvatn_1"

varda_long_red$lake_name[which(varda_long_red$names == "Hvitarvatn-HVT032-Varves-Larsen_et_al-2011")] <- "Hvítárvatn_2"

varda_long_red$lake_name[which(varda_long_red$names == "Hvitarvatn-HVT033-Varves-Larsen_et_al-2011")] <- "Hvítárvatn_3"

varda_long_red$lake_name[which(varda_long_red$names == "Iceberg_Lake--Varves-Loso_et_al-2008")] <- "Iceberg Lake_1"

varda_long_red$lake_name[which(varda_long_red$names == "Iceberg_Lake-P1-Varves-Diedrich_et_al-2012")] <- "Iceberg Lake_2"

varda_long_red$lake_name[which(varda_long_red$names == "Iceberg_Lake-P2-Varves-Diedrich_et_al-2012")] <- "Iceberg Lake_3"

#check if everything is fine now
unique(varda_long_red$lake_name)

varda_long_red <- varda_long_red %>% 
  filter(!lake_name == "Hvítárvatn_3") %>% #there is no data for 1816 CE
  select(!names) %>% 
  mutate(lake_name = ifelse(lake_name == "East Lake", "East Lake_1", lake_name)) #there is another record from East Lake available from Pangaea. To distinguish between these two the number is given to each


#write the final data frames
write_csv(varda_long_red, "data/varda_long_red.csv")


#PANGAEA----------

pang_search <- pg_search_es("varve thickness", default_operator = "AND", size = 100, ignore.case = TRUE)

pang_doi_prep <- gsub("https://doi.org/", "", pang_search$`_source.URI`)
pang_doi <- gsub("https://doi.pangaea.de/", "", pang_doi_prep) #change the names containing pangea.de separately

pang_raw_list <- list()

for (doi in pang_doi) {
  # Extract data using pg_data function for the current DOI
  data <- pg_data(doi)
  
  # Append the extracted data to the list
  pang_raw_list[[doi]] <- data
}

flattened_pang_raw_list <- flatten(pang_raw_list) #the list is manually searched for appropriate records

pang_selected_doi <- c("10.1594/PANGAEA.874664",
                       "10.1594/PANGAEA.924199",
                       "10.1594/PANGAEA.895170",
                       "10.1594/PANGAEA.949593")  

pang_selected <- flattened_pang_raw_list[pang_selected_doi]

pang_meta_tibble <- tibble(
  ref = c(
    pang_selected$`10.1594/PANGAEA.874664`$citation,
    pang_selected$`10.1594/PANGAEA.924199`$citation,
    pang_selected$`10.1594/PANGAEA.895170`$citation,
    pang_selected$`10.1594/PANGAEA.949593`$citation
  ),
  lake_name = c(
    pang_selected$`10.1594/PANGAEA.874664`$metadata$events$LOCATION,
    pang_selected$`10.1594/PANGAEA.924199`$metadata$events$LOCATION,
    pang_selected$`10.1594/PANGAEA.895170`$metadata$events$LOCATION,
    "Montcortès"
  ),
  lat = as.numeric(c(
    varda_long_red$lat[which(varda_long_red$lake_name == "East Lake_1")][[1]], #coordinates in Pangaea are less precise than the ones from in Varda
    pang_selected$`10.1594/PANGAEA.924199`$metadata$events$LATITUDE, 
    pang_selected$`10.1594/PANGAEA.895170`$metadata$events$LATITUDE,
    42.331580
  )),
  lon = as.numeric(c(
    varda_long_red$lon[which(varda_long_red$lake_name == "East Lake_1")][[1]], #same as above
    pang_selected$`10.1594/PANGAEA.924199`$metadata$events$LONGITUDE,
    pang_selected$`10.1594/PANGAEA.895170`$metadata$events$LONGITUDE,
    0.994910
  ))
) #data from Lake Montecortes appeared to have different strucure. I had somme issues when creating the tibble, and therefore it was finally done manually

pang_meta_tibble <- pang_meta_tibble %>%
  mutate(lake_name = case_when(
    grepl("Cape Bounty East Lake, Canada", lake_name) ~ "East Lake_2",
    grepl("Lake Gościąż, Poland", lake_name) ~ "Gościąż",
    grepl("Melville Island", lake_name) ~ "Chevalier",
    TRUE ~ lake_name  # Retain the original lake_name if none of the conditions match
  ))

pang_long_prep <- pang_meta_tibble %>% 
  mutate(df = list(l1 = pang_selected$`10.1594/PANGAEA.874664`$data,
                l2 = pang_selected$`10.1594/PANGAEA.924199`$data,
                l3 = pang_selected$`10.1594/PANGAEA.895170`$data,
                l4 = pang_selected$`10.1594/PANGAEA.949593`$data)) %>% 
  unnest(df) %>% 
  select(ref, lake_name, lat, lon, "Age [a AD/CE]", "Age [ka BP]", "Depth sed [m]", "Varve thick [mm]", "Varve thick [mm] (of the nival sedimentary unit...)", "Date/Time (Year, Age model, varve counting)", "Varve thick calc [mm] (CaL - Raw calcite values: cal...)") %>%
  mutate(age_CE = ifelse(is.na(`Age [a AD/CE]`), `Date/Time (Year, Age model, varve counting)`, `Age [a AD/CE]`),
         age_BP = `Age [ka BP]`,
         varve_thick = `Varve thick [mm]`,
         light_lamin_thick = `Varve thick calc [mm] (CaL - Raw calcite values: cal...)`,
         dark_lamin_thick = `Varve thick [mm] (of the nival sedimentary unit...)`
         ) %>% 
  select(names(varda_long_red))

gosciaz_ages <- pg_data("10.1594/PANGAEA.924198")[[1]]$data
ga <- gosciaz_ages$`Age [ka BP]`

pang_long_prep$age_BP[which(pang_long_prep$lake_name == "Gościąż")] <- ga

pang_long <- pang_long_prep %>% 
  mutate(age_BP = ifelse(is.na(age_BP), 1950 - age_CE, age_BP * 1000),
         age_CE = ifelse(is.na(age_CE), 1950 - age_BP, age_CE)) %>% 
  filter(age_CE >= 1600)

#NOAA----------
#the database was searched manually for records missing from both VARDA and PANGEA databases. The following records were found: 

upper_sopper_prep <- read_table("https://www.ncei.noaa.gov/pub/data/paleo/paleolimnology/northamerica/canada/baffin/soper_2000.txt", skip = 81)

upper_sopper <- tibble(age_CE = as.numeric(c(names(upper_sopper_prep[1]), upper_sopper_prep$`1992`)),
                       dark_lamin_thick = as.numeric(c(names(upper_sopper_prep[2]), upper_sopper_prep$`0.233285`))) %>% 
  mutate(ref = "Hughen, K.A., Overpeck, J.T. and Anderson, R.F., 2000. Recent Warming in a 500-Year Paleotemperature Record from Varved Sediments: Upper Soper Lake, Baffin Island, Canada, The Holocene, 10(1), 9-19. Availible at: https://doi.org/10.1191/095968300676746202.",
         lake_name = "Upper Sopper",
         lat = 62.9166667, #coordinates from NOAA
         lon = -69.53)

green_lake <- read_table("https://www.ncei.noaa.gov/pub/data/paleo/paleolimnology/northamerica/canada/bc/menounos2006-green.txt", skip = 105) %>% 
  select(year, thick_nlog) %>% 
  mutate(varve_thick = (exp(1))^thick_nlog,
         ref = "Schiefer, E., Menounos, B., Slaymaker, O., 2006. Extreme sediment delivery events recorded in the contemporary sediment record of a montane lake, southern Coast Mountains, British Columbia. Canadian Journal of Earth Sciences, 43(12), 1777-1790. Available at: https://doi.org/10.1139/e06-056",
         lake_name = "Green Lake",
         lat = 50.2, #coordinates from NOAA
         lon = -122.9) %>% 
  rename(age_CE = year) %>% 
  select(!thick_nlog)

lake_nautajarvi <- read_table("https://www.ncei.noaa.gov/pub/data/paleo/paleolimnology/europe/finland/nautajarvi2005.txt", skip = 100-7) %>% 
  mutate(dark_lamin_thick = Organic * 10e-4,
         light_lamin_thick = Mineral * 10e-4,
         varve_thick = dark_lamin_thick + light_lamin_thick,
         age_CE = Year,
         lake_name = "Nautajärvi",
         ref = "Ojala, A.E.K. and T. Alenius.  2005. 10 000 years of interannual sedimentation recorded in the Lake Nautajärvi (Finland) clastic–organic varves.  
Palaeogeography, Palaeoclimatology, Palaeoecology, 219(3-4), 285-302, available at: doi:10.1016/j.palaeo.2005.01.002",
         lon = 24.6833,
         lat = 61.8000) %>% 
  select(!Year & !`X-ray` & !density & !Mineral & !Organic)

noaa_long <- upper_sopper %>% 
  left_join(green_lake) %>%
  left_join(lake_nautajarvi)

#Geological Society of America (GSA) Data Repository -------------
#Data from three alaskan lakes is available at https://gsapubs.figshare.com/articles/journal_contribution/Supplemental_material_Varve_formation_during_the_past_three_centuries_in_three_large_proglacial_lakes_in_south-central_Alaska/12535784; data is available only in pdf and required some steps to get the final records (I use stacked records from each lake):
gsa_kenai <- read_csv("data/boes_et_al_2017_kenai.csv")

gsa_skilak <- read_csv("data/boes_et_al_2017_skilak.csv")

gsa_long <- gsa_kenai %>% 
  left_join(gsa_skilak)

#Data obtained from authors ---------
au_czechowskie <- read_csv("data/slowinski_et_al_2021_czechowskie.csv")

au_long <- au_czechowskie

#Combining datasets --------------
full_ds <- varda_long_red %>% 
  left_join(pang_long) %>% 
  left_join(noaa_long) %>% 
  left_join(gsa_long) %>% 
  left_join(au_long)
  
