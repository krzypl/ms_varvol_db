library(tidyverse)
library(tapas)
library(mgcv)
source("mgcv2tapas_krzypl2.R")

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
  mutate(d = c(0, diff(age_CE))) %>% #check if age difference consistently equals 1 in each lake
  filter(!lake_name == "Iceberg Lake_1") %>% #there are zeros in varve_thick; including this dataset causes problems
  filter(!(age_CE > 1816+50 | age_CE < 1816 - 50)) %>% #remove this filter to get full results
  select(lake_name, cmTop, cmBot, AgeTop, AgeBot, Volume, varve_thick,
         light_lamin_thick, dark_lamin_thick) %>% 
  filter(!cmTop - cmBot == 0)

#total varve thickness----------
#prepare data for peak detection in total varve thickness records
varda_long_vt_total <- varda_long_red %>% 
  select(!light_lamin_thick & !dark_lamin_thick)

lake_names <- unique(varda_long_vt_total$lake_name)

#create a list where each record is assigned to separate data frame
varda4tapas_vt_total <- list()

for (value in lake_names) {
  subset_df <- as.data.frame(varda_long_vt_total[varda_long_vt_total$lake_name == value, ])
  varda4tapas_vt_total[[value]] <- subset_df
}

remove_lake_name_column <- function(df) {
  df <- df[, !(names(df) %in% "lake_name")]
  return(df)
} #removing lake_names make the analysis easier with tapas

#fitting GAMs requires a bit more coding. Thus there are few more steps comparing to other methods. To make analysis possible for all the data frames within the list the original function mgcv2tapas() was slightly modified (source function is loaded at the start of the script)

varda4tapas_vt_total_clean <- lapply(varda4tapas_vt_total, remove_lake_name_column)

pretreat_vt <- function(raw4tapas) {
  pretreated <- pretreatment_data(raw4tapas, yrInterp = 1)
  gam_pre <- tapas2mgcv(pretreated)
  gam_pre <- transform(gam_pre, age = age_top - ((age_top-age_bot)/2))
  gam_pre <- gam_pre[-length(gam_pre$cm_top), ] #remove the last row containing sample of thickness = 0
  age_span <- max(gam_pre$age) - min(gam_pre$age)
  set.seed(12)
  gam_vt <- gam(varve_thickAR ~ s(age, k = 100), data = gam_pre, method = "REML")
  gam_post <- mgcv2tapas_krzypl2(series = gam_vt)

  return(gam_post)
  } 

vt_mgcv <- lapply(varda4tapas_vt_total_clean, pretreat_vt)

#need to think about numbers yet
analyze_record <- function(record_data) {
  pd <- local_thresh(
    series = record_data,
    thresh.value = 0.999,
    thresh.yr = 30,
    smoothing.yr = 30,
    keep_consecutive = FALSE,
    minCountP = 0.001,
    MinCountP_window =30
  )

  return(pd)
}

pd_results <- lapply(vt_mgcv, analyze_record)

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
pd_df_vt_total_mgcv <- pd_df_prep %>%
  imap_dfr(~ add_lake_name(.x, .y)) %>% 
  mutate(age_CE = 1950-age) %>% 
  rename(age_BP = age, varve_thick = varve_thickAR)

write_csv(pd_df_vt_total_mgcv, "data/pd_df_vt_total_mgcv.csv")