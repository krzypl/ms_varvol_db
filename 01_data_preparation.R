library(tidyverse)

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
  select(!n_rows) %>% 
  unnest(df) #unnesting data frames to get easy access to all the observations

names(varda_long) #check the veriable to select important ones; after screening out availble variables it seems reasonable to retain only columns contatinng varve ages, and varve thickness measurment, full and for light and dark laminaes. However, these require some adjustments:

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
  select(names, lake_name, varveAge, age_CE, varveThicknessTotal, lightLayerThickness,
         darkLayerThickness)

#extract only the data <1600 CE
age_min <- varda_long_red_prep %>% 
  group_by(names) %>% 
  summarise(min_age = min(age_CE))

varda_long_red <- varda_long_red_prep %>% 
  filter(age_CE >= 1600) %>% 
  filter(!lake_name == "Chatyr Kol") %>% 
  rename(reference = names,
         age_BP = varveAge,
         varve_thick = varveThicknessTotal,
         light_lamin_thick = lightLayerThickness,
         dark_lamin_thick = darkLayerThickness)

#there are few records from two lakes. Identify these lakes and change the names to easily plot the data separately for each of the records
names2change <- varda_long_red %>% 
  distinct(reference) %>%
  mutate(first_word = str_extract(reference, "^[^_-]+")) %>% 
  group_by(first_word) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

unique(varda_long_red$reference)

varda_long_red$lake_name[which(varda_long_red$reference == "Hvitarvatn-HVT031-Varves-Larsen_et_al-2011")] <- "Hvítárvatn_1"

varda_long_red$lake_name[which(varda_long_red$reference == "Hvitarvatn-HVT032-Varves-Larsen_et_al-2011")] <- "Hvítárvatn_2"

varda_long_red$lake_name[which(varda_long_red$reference == "Hvitarvatn-HVT033-Varves-Larsen_et_al-2011")] <- "Hvítárvatn_3"

varda_long_red$lake_name[which(varda_long_red$reference == "Iceberg_Lake--Varves-Loso_et_al-2008")] <- "Iceberg Lake_1"

varda_long_red$lake_name[which(varda_long_red$reference == "Iceberg_Lake-P1-Varves-Diedrich_et_al-2012")] <- "Iceberg Lake_2"

varda_long_red$lake_name[which(varda_long_red$reference == "Iceberg_Lake-P2-Varves-Diedrich_et_al-2012")] <- "Iceberg Lake_3"

#check if everything is fine now
unique(varda_long_red$lake_name)

varda_long_red <- varda_long_red %>% 
  filter(!lake_name == "Hvítárvatn_3") #there is no data for 1816 CE

#write the final data frames
write_csv(varda_long_red, "data/varda_long_red.csv")

varda_index_red <- varda_index_red %>% 
  filter(dataset.file %in% unique(varda_long_red$reference)) #remove lakes that were deleted from varda_long_red
write_csv(varda_index_red, "data/varda_index_red.csv") #seve the file with extracted dataset
