library(tidyverse)
library(tapas)

#read and filter the data so that all the records are continuous
varda_long_red <- read_csv("data/varda_long_red.csv") %>% 
  group_by(lake_name) %>% 
  mutate(cmBot = cumsum(varve_thick)/10,
         cmTop = cmBot - varve_thick/10,
         AgeTop = age_BP,
         AgeBot = age_BP + 1,
         Volume = 1) %>% 
  filter(!(lake_name == "Iceberg Lake_3" & age_CE < 1739 & age_CE > 1834)) %>%
  filter(!(lake_name == "Sawtooth" & age_CE > 1980)) %>% 
  filter(!(lake_name == "Lower Murray Lake" & age_CE > 1969)) %>%
  filter(!(lake_name == "Lagoon Etoliko" & age_CE < 1715)) %>%
  filter(!(lake_name == "East Lake" & age_CE > 1960)) %>% 
  filter(!lake_name == "Iceberg Lake_3") %>%
  filter(!lake_name == "Hvítárvatn_2") %>%
  mutate(d = c(1, diff(age_CE))) %>% #check if age difference consistently equals 1 in each lake
  filter(!lake_name == "Iceberg Lake_1") %>% #there are zeros in varve_thick; including this dataset causes problems
  filter(!(age_CE > 1816+50 | age_CE < 1816 - 50)) %>% 
  select(lake_name, cmTop, cmBot, AgeTop, AgeBot, Volume, varve_thick,
         light_lamin_thick, dark_lamin_thick) %>% 
  filter(!cmTop - cmBot == 0)

#prepare data for peak detection in total varve thickness records
varda_long_vt_total <- varda_long_red %>% 
  select(!light_lamin_thick & !dark_lamin_thick)

#create a list where each record is assigned to separate data frame
lake_names <- unique(varda_long_vt_total$lake_name)

varda4tapas_vt_total <- list()

for (value in lake_names) {
  subset_df <- as.data.frame(varda_long_vt_total[varda_long_vt_total$lake_name == value, ])
  varda4tapas_vt_total[[value]] <- subset_df
}

remove_lake_name_column <- function(df) {
  df <- df[, !(names(df) %in% "lake_name")]  
  return(df)
} #removing lake_names make the analysis easier with tapas

varda4tapas_vt_total_clean <- lapply(varda4tapas_vt_total, remove_lake_name_column)

#selecting proper numbers here require discussion 
analyze_record <- function(record_data) {
  pd <- peak_detection(
    series = record_data,
    yrInterp = 1,
    thresh_value = 0.95,
    detr_type = "mov.median",
    thresh_type = "local",
    min_CountP = 0.05,
    MinCountP_window = 50,
    keep_consecutive = FALSE,
    #     out_dir = "/figures",
    plotit = FALSE,
    sens = FALSE,
    smoothing_yr = 50,
    smoothing_yr_seq = c(10, 30, 50, 100)
  )
  
  # Return the result
  return(pd)
}

#apply the function peak_detection() to each data frame
pd_results <- lapply(varda4tapas_vt_total_clean, analyze_record)

#extract the most relevant data from the peak detection analysis
make_pd_df <- function(pd_data) {
  detr_ser <- as_tibble(pd_data$detr$detr)
  peaks_pos <- pd_data$thresh$peaks.pos[,1]
  peaks_neg <- pd_data$thresh$peaks.neg[,1]
  filtered_pd <- detr_ser %>% 
    mutate(peaks_pos = peaks_pos,
           peaks_neg = peaks_neg)
  return(filtered_pd)
}

pd_df_prep <- lapply(pd_results, make_pd_df)

add_lake_name <- function(tibble, name) {
  tibble <- tibble %>% 
    mutate(lake_name = name)
  return(tibble)
}

# Combine tibbles into a single long tibble
pd_df_vt_total_mow_median <- pd_df_prep %>%
  imap_dfr(~ add_lake_name(.x, .y)) %>% 
  mutate(age_CE = 1950-age) %>% 
  rename(age_BP = age, varve_thick = varve_thickAR)

write_csv(pd_df_vt_total_mow_median, "data/pd_df_vt_total_mow_median.csv")