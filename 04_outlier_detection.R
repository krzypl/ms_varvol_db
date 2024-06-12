library(tidyverse)

#functions--------
moving_median <- function(x, window_size = 15) {
  n <- length(x)
  y <- vector("numeric", n)
  y[] <- NA  # Initialize with NAs
  
  for (i in 1:(n - window_size + 1)) {
    y[i + floor(window_size / 2)] <- median(x[i:(i + window_size - 1)], na.rm = TRUE)
  }
  
  return(y)
}

#median absolute distance (MAD)
median_absolute_distance <- function(x, s_i, window_size = 15) {
  n <- length(x)
  y <- vector("numeric", n)
  y[] <- NA  # Initialize with NAs
  
  x_s_i_diff <- abs(x - s_i)
  
  for (i in 1:(n - window_size + 1)) {
    y[i + floor(window_size / 2)] <- median(x_s_i_diff[i:(i + window_size - 1)], na.rm = TRUE)
  }
  
  return(y)
}

#plots--------
full_ds <- read_csv("data/full_ds.csv")

full_ds <- full_ds %>% 
  filter(age_CE >= 1766-7-7 & age_CE <= 1866+7+7) %>% 
  group_by(lake_name, layer) %>% 
  mutate(mm = moving_median(thickness),
         mad = median_absolute_distance(thickness, mm)) %>% 
  mutate(tresh_pos_z2 = mm + mad*2,
         tresh_pos_z3_5 = mm + mad*3.5,
         tresh_pos_z5 = mm + mad*5,
         tresh_neg_z1 = mm - mad*1,
         tresh_neg_z1_75 = mm - mad*1.75,
         tresh_neg_z2_5 = mm - mad*2.5,
         ) %>% 
  ungroup() %>% 
  filter(age_CE >= 1766 & age_CE <= 1866)

custom_labeller <- function(labels) {
  labels <- as.data.frame(labels)
  labels$combined <- paste(labels$lake_name, labels$layer, sep = " - ")
  labels <- labels %>% dplyr::select(combined)
  return(labels)
}

treshold_plot <- full_ds %>%
  ggplot() +
  geom_line(aes(x = age_CE, y = thickness), linewidth = 0.2) +
  geom_point(aes(x = age_CE, y = thickness)) +
  geom_line(aes(x = age_CE, y = mm), color = "red") +
  geom_line(aes(x = age_CE, y = tresh_pos_z2), color = "orange") +
  geom_line(aes(x = age_CE, y = tresh_pos_z3_5), color = "darkorange2") +
  geom_line(aes(x = age_CE, y = tresh_pos_z5), color = "darkorange4") +
  geom_line(aes(x = age_CE, y = tresh_neg_z1), color = "lightblue") +
  geom_line(aes(x = age_CE, y = tresh_neg_z1_75), color = "blue") +
  geom_line(aes(x = age_CE, y = tresh_neg_z2_5), color = "darkblue") +
  annotate("rect",
           xmin = 1816 - 7, xmax = 1816 + 7, ymin = -Inf, ymax = Inf,
           alpha = 0.1, fill = "blue") +
  geom_vline(xintercept = 1816, color = "blue") +
  labs(x = "Age (year CE)", y = "Thickness (mm)") +
  facet_wrap(vars(lake_name, layer), scales = "free_y", ncol = 3, labeller = custom_labeller)

ggsave(filename = "figures/treshold_plot.svg",
       plot = treshold_plot,
       width = 12.5,
       height = 17,
       device = "svg")
ggsave(filename = "figures/treshold_plot.pdf",
       plot = treshold_plot,
       width = 12.5,
       height = 17,
       device = "pdf")
 
n_of_out <- full_ds %>%
  filter(!is.na(mm)) %>% 
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
           grepl("z1_75$", treshold_prep) ~ "1_75",
           grepl("z2$", treshold_prep) ~ "2",
           grepl("z2_5$", treshold_prep) ~ "2_5",
           grepl("z3_5$", treshold_prep) ~ "3_5",
           grepl("z5$", treshold_prep) ~ "5",
           TRUE ~ NA_character_))) 

n_of_out_plot <- ggplot(n_of_out,
                            aes(x = treshold, y = n_of_extremes, fill = z)) +
  geom_col(position = position_dodge(width = 0.9)) +
  labs(x = "Type of treshold", y = "Number of extremes identified") +
  facet_wrap(vars(lake_name, layer), ncol = 9)

ggsave(filename = "figures/n_of_outliers_plot.svg",
       plot = n_of_out_plot,
       width = 12.5,
       height = 17,
       device = "svg")
ggsave(filename = "figures/n_of_outliers_plot.pdf",
       plot = n_of_out_plot,
       width = 12.5,
       height = 6,
       device = "pdf")

scales_out <- full_ds %>% 
  filter(thickness < tresh_neg_z2_5 | thickness > tresh_pos_z5) %>%
  dplyr::select(lake_name, layer, age_CE, tresh_pos_z5,
         tresh_neg_z2_5, thickness, mm, mad) %>% 
  mutate(scaled_magnitude = (thickness - mm)/mad,
         scaled_magnitude_sign = as.factor(ifelse(scaled_magnitude > 0, "positive", "negative")))

scales_out_plot <- ggplot(scales_out, aes(x = age_CE, y = scaled_magnitude, fill = scaled_magnitude_sign)) +
  geom_col() +
  annotate("rect",
           xmin = 1816 - 7, xmax = 1816 + 7, ymin = -Inf, ymax = Inf,
           alpha = 0.1, fill = "blue") +
  geom_vline(xintercept = 1816, color = "blue") +
  labs(x = "Age (year CE)", y = "Scaled magnitude of anomaly") +
  facet_wrap(vars(lake_name, layer), labeller = custom_labeller, scales = "free_y", ncol = 4) +
  theme(legend.position = "bottom")

ggsave(filename = "figures/scales_outlier_plot.svg",
       plot = scales_out_plot,
       width = 12.5,
       height = 17,
       device = "svg")
ggsave(filename = "figures/scales_outlier_plot.pdf",
       plot = scales_out_plot,
       width = 12.5,
       height = 12,
       device = "pdf")
  
