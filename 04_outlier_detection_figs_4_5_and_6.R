library(tidyverse)
source("functions/moving_median.R")
source("functions/median_absolute_distance.R")
source("functions/calculate_C1.R")

#median cross-validation criterion for selecting optimum k (window_size = 2*k+1) based on Zheng and Yang (1998); the minimum k is set to 6 and maximum k is 14. The range of possible k is selected to enable for reasonable decadal variations

#plots--------
full_ds <- read_csv("data/full_ds.csv")

full_ds <- full_ds %>% 
  filter(age_CE >= 1766-50 & age_CE <= 1866+50) %>% 
  group_by(lake_name, layer) %>% 
  mutate(k = calculate_C1(thickness),
         window_size = 2*k+1,
         mm = moving_median(thickness, window_size = mean(window_size)),
         mad = median_absolute_distance(thickness, window_size = mean(window_size))) %>% 
  mutate(tresh_pos_z2 = mm + mad*2,
         tresh_pos_z3_5 = mm + mad*3.5,
         tresh_pos_z5 = mm + mad*5,
         tresh_neg_z1 = mm - mad*1,
         tresh_neg_z1_75 = mm - mad*1.75,
         tresh_neg_z2_5 = mm - mad*2.5,
  ) %>% 
  ungroup() %>% 
  filter(age_CE >= 1766 & age_CE <= 1866) %>% 
  arrange(lake_name, layer)

detrended_records <- full_ds %>% 
  mutate(detrended_thickness = thickness - mm) %>% 
  select(lake_name, age_CE, layer, detrended_thickness)

write_csv(detrended_records, "data/detrended_records.csv")

win_size_lab <- paste("(",
                      2*(distinct(full_ds, lake_name, layer,
                                  .keep_all = TRUE))$k + 1, ")", sep = "")



custom_labeller <- function(labels) {
  labels <- as.data.frame(labels)
  labels$combined <- paste(paste(labels$lake_name, labels$layer, sep = " - "), win_size_lab)
  labels <- labels %>% dplyr::select(combined)
  return(labels)
}

full_ds_2plot <- full_ds %>% 
  pivot_longer(cols = c(thickness, mm, tresh_pos_z2:tresh_neg_z2_5),
               names_to = "thickness_var", values_to = "thickness") %>% 
  mutate(xmin_yws = 1816-7,
         xmax_yws = 1816+7,
         yws_range = factor('blue'),
         yws = factor('darkblue'),
         yws_year = 1816,
         xmin_kunin = 1783,
         xmax_kunin = 1797,
         kunin_range = factor('red')
         )

full_ds_2plot$thickness_var <- factor(full_ds_2plot$thickness_var, levels = c("thickness", "mm", "tresh_neg_z1", "tresh_neg_z1_75", "tresh_neg_z2_5", "tresh_pos_z2", "tresh_pos_z3_5", "tresh_pos_z5"))


treshold_plot_prep <- full_ds_2plot %>%
  ggplot() +
  geom_line(aes(x = age_CE, y = thickness, color = thickness_var), linewidth = 0.2) +
#  geom_point(data = filter(full_ds_2plot, thickness_var == "thickness"), 
#             aes(x = age_CE, y = thickness)) +
  labs(x = "Age (year CE)", y = "Thickness (mm)") +
  facet_wrap(vars(lake_name, layer),
             scales = "free_y", ncol = 4, labeller = custom_labeller)

treshold_plot <- treshold_plot_prep + 
  geom_rect(data = distinct(full_ds_2plot, lake_name, layer, .keep_all = TRUE), 
            aes(xmin = xmin_yws, xmax = xmax_yws, ymin = -Inf, ymax = Inf,
                fill = yws_range), alpha = 0.3) +
  geom_rect(data = filter(
    distinct(full_ds_2plot, lake_name, layer, .keep_all = TRUE),
    lake_name == "Kuninkaisenlampi"),
            aes(xmin = xmin_kunin, xmax = xmax_kunin, ymin = -Inf, ymax = Inf, fill = kunin_range), alpha = 0.3) +
  geom_vline(aes(xintercept = yws_year, color = yws), linewidth = 0.1) +
  scale_fill_manual(
    name = NULL,
    values = c(
      'blue' = 'blue',
      'red' = 'red'),
    labels = c(
      'blue' = '1816±7',
      'red' = "not considered in \n the analysis"
    )) +
  scale_color_manual(
    name = NULL,
    values = c(
      'thickness' = 'black',
      'mm' = "red",
      "tresh_pos_z2" = "orange",
      "tresh_pos_z3_5" = "darkorange2",
      "tresh_pos_z5" = "darkorange4",
      "tresh_neg_z1" = "lightblue",
      "tresh_neg_z1_75" = "blue",
      "tresh_neg_z2_5" = "darkblue",
      "darkblue" = "darkblue"
      ),
    labels = c(
      'thickness' = 'raw thickness',
      'mm' = "moving median",
      "tresh_pos_z2" = "positive threshold, z = 2",
      "tresh_pos_z3_5" = "positive threshold, z = 3.5",
      "tresh_pos_z5" = "positive threshold, z = 5",
      "tresh_neg_z1" = "negative treshold, z = 1",
      "tresh_neg_z1_75" = "negative threshold, z = 2",
      "tresh_neg_z2_5" = "negative threshold, z = 2.5",
      'darkblue' = '1816 CE'
      )) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 5.5),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.key.height = unit(0.5, "lines")) +
  guides(color = guide_legend(nrow = 4),
         fill = guide_legend(nrow = 2))


ggsave(filename = "figures/treshold_plot.svg",
       plot = treshold_plot,
       width = 6.5,
       height = 9,
       device = "svg")
ggsave(filename = "figures/treshold_plot.pdf",
       plot = treshold_plot,
       width = 6.5,
       height = 9,
       device = "pdf")

n_of_out <- full_ds %>%
  filter(!is.na(mm)) %>% 
  filter(!(lake_name == "Kuninkaisenlampi" & age_CE >= 1783 & age_CE <= 1797)) %>% 
  group_by(lake_name, layer) %>% 
  summarise(n_pos_z2 = sum(thickness > tresh_pos_z2),
            n_pos_z3_5 = sum(thickness > tresh_pos_z3_5),
            n_pos_z5 = sum(thickness > tresh_pos_z5),
            n_neg_z1 = sum(thickness < tresh_neg_z1),
            n_neg_z1_75 = sum(thickness < tresh_neg_z1_75),
            n_neg_z2_5 = sum(thickness < tresh_neg_z2_5)) %>% 
  pivot_longer(cols = n_pos_z2:n_neg_z2_5, names_to = "treshold_prep",
               values_to = "n_of_extremes") %>% 
  mutate(treshold = as.factor(ifelse(grepl("neg", treshold_prep), "neg", "pos")),
         z = as.factor(case_when(
           grepl("z1$", treshold_prep) ~ "1",
           grepl("z1_75$", treshold_prep) ~ "1.75",
           grepl("z2$", treshold_prep) ~ "2",
           grepl("z2_5$", treshold_prep) ~ "2.5",
           grepl("z3_5$", treshold_prep) ~ "3.5",
           grepl("z5$", treshold_prep) ~ "5",
           TRUE ~ NA_character_))) 

write_rds(n_of_out, "data/n_of_out.rds")

n_of_out_plot <- ggplot(n_of_out,
                        aes(x = treshold, y = n_of_extremes, fill = z)) +
  geom_col(position = position_dodge(width = 0.9)) +
  labs(x = "Type of treshold", y = "Number of outliers identified", fill = "Value of z") +
  facet_wrap(vars(lake_name, layer), ncol = 4, labeller = custom_labeller) +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 7))

ggsave(filename = "figures/n_of_outliers_plot.svg",
       plot = n_of_out_plot,
       width = 6.5,
       height = 9,
       device = "svg")
ggsave(filename = "figures/n_of_outliers_plot.pdf",
       plot = n_of_out_plot,
       width = 6.5,
       height = 9,
       device = "pdf")


scales_out <- full_ds %>% 
  filter(thickness < tresh_neg_z2_5 | thickness > tresh_pos_z5) %>%
  filter(!(lake_name == "Kuninkaisenlampi" & age_CE >= 1783 & age_CE <= 1797)) %>%
  dplyr::select(lake_name, layer, age_CE, tresh_pos_z5,
                tresh_neg_z2_5, thickness, mm, mad) %>% 
  mutate(scaled_magnitude = (thickness - mm)/mad,
         scaled_magnitude_sign = as.factor(ifelse(scaled_magnitude > 0, "positive", "negative")),
         record_label = paste(lake_name, layer, sep = " - "))

write_csv(scales_out, "data/scales_out.csv")

custom_labeller2 <- function(labels) {
  labels <- as.data.frame(labels)
  labels$combined <- labels$record_label
  labels <- labels %>% dplyr::select(combined)
  return(labels)
}

scales_out_plot_prep <- ggplot(scales_out, aes(x = age_CE, y = scaled_magnitude, fill = scaled_magnitude_sign)) +
  annotate("rect",
           xmin = 1816 - 7, xmax = 1816 + 7, ymin = -Inf, ymax = Inf,
           alpha = 0.1, fill = "blue") +
  geom_vline(xintercept = 1816, color = "blue", linewidth = 0.1) +
  geom_col() +
  labs(x = "Age (year CE)", y = "Scaled magnitude of anomaly") +
  facet_wrap(vars(record_label, layer), labeller = custom_labeller2, scales = "free_y", ncol = 4) +
  theme(legend.position = "bottom")

records_red <- scales_out %>% # used later to extract records clearly missing imprint of yws
  filter(age_CE >= 1816 - 7 & age_CE <= 1816 + 7) %>% 
  group_by(record_label) %>%
  summarise(n = n()) 

records_orange <- scales_out %>% # used later to extract records for which imprint of yws is unlikely
  distinct(record_label, .keep_all = TRUE) %>% 
  filter(record_label %in% c("Chala - light layer",
                             "Donard - varve",
                             "Lake DV09 - varve",
                             "East Lake_1 - varve",
                             "East Lake_2 - varve",
                             "Green Lake - varve",
                             "Iceberg Lake_1 - varve",
                             "Iceberg Lake_2 - varve",
                             "Kusai - light layer",
                             "Linnévatnet - varve",
                             "Lower Murray Lake - varve",
                             "Plomo - varve",
                             "Sawtooth - varve")) 

records_yellow <- scales_out %>% 
  distinct(record_label, .keep_all = TRUE) %>% 
  filter(record_label %in% c("Ayr Lake - varve",
                             "Kuninkaisenlampi - dark layer",
                             "Etoliko - dark layer",
                             "Nautajärvi - dark layer",
                             "Ogac - dark layer",
                             "Żabińskie - dark layer",
                             "Żabińskie - light layer"
                             ))

records_green <- scales_out %>% 
  distinct(record_label, .keep_all = TRUE) %>% 
  filter(record_label %in% c("Holzmaar - varve",
                             "Kuninkaisenlampi - light layer",
                             "Kusai - dark layer",
                             "Nar Gölü - light layer",
                             "Oeschinen - varve"
  ))

scales_out3 <- scales_out %>% 
  mutate(red_category = ifelse(!record_label %in% records_red$record_label, 
                               "red", NA),
         ro_cat = ifelse(record_label %in% records_orange$record_label,
                         "darkorange", red_category),
         roy_cat = ifelse(record_label %in% records_yellow$record_label,
                          "lightyellow", ro_cat),
         royg_cat = ifelse(record_label %in% records_green$record_label,
                           "green", roy_cat),
         yws_cat = as.factor(royg_cat)) %>% 
  dplyr::select(!c(red_category, ro_cat, roy_cat, royg_cat)) %>% 
  mutate(xmin = -Inf,
         xmax = Inf,
         ymin = -Inf,
         ymax = Inf) %>% 
  distinct(record_label, .keep_all = TRUE) %>% 
  mutate(xmin_yws = 1816-7,
         xmax_yws = 1816+7,
         yws_range = factor('blue'),
         yws = factor('darkblue'),
         yws_year = 1816)

scales_out3$yws_cat <- factor(scales_out3$yws_cat, levels = c(
  "red", "darkorange", "lightyellow", "green"
))

scales_out_plot <- scales_out_plot_prep +
  geom_vline(data = scales_out3, aes(xintercept = yws_year, color = yws), linewidth = 0.1) +
  geom_rect(data = scales_out3, aes(xmin = xmin_yws, xmax = xmax_yws, ymin = ymin, ymax = ymax, fill = yws_range), alpha = 0.3) +
  geom_rect(data = scales_out3, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = yws_cat), alpha = 0.1) +
  geom_col(data = scales_out, aes(x = age_CE, y = scaled_magnitude, fill = scaled_magnitude_sign)) +
  scale_color_manual(name = NULL,
                     values = c('darkblue' = 'darkblue'), 
                     labels = c('darkblue' = '1816 CE')) +
  scale_fill_manual(
    name = NULL,
    values = c(
    'negative' = 'blueviolet',
    'positive' = 'red4',
    'red' = "red",
    'darkorange' = 'darkorange',
    'lightyellow' = 'lightyellow',
    'green' = 'green',
    'blue' = 'blue'),
    labels = c(
      'negative' = 'negative anomaly',
      'positive' = 'positive anomaly',
      'red' = 'YWS imprint unlikely A',
      'darkorange' = 'YWS imprint unlikely B',
      'lightyellow' = 'YWS imprint questionable',
      'green' = 'YWS imprint likely',
      'blue' = '1816±7'
    )) +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 6.5),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.key.height = unit(0.5, "lines")) +
  guides(fill = guide_legend(nrow = 3))
  

ggsave(filename = "figures/scales_outlier_plot.svg",
       plot = scales_out_plot,
       width = 6.5,
       height = 9,
       device = "svg")
ggsave(filename = "figures/scales_outlier_plot.pdf",
       plot = scales_out_plot,
       width = 6.5,
       height = 9,
       device = "pdf")
