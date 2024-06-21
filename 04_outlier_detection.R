library(tidyverse)

#functions--------
moving_median <- function(x, window_size = 21) {
  n <- length(x)
  y <- vector("numeric", n)
  y[] <- NA  # Initialize with NAs
  
  for (i in 1:(n - window_size + 1)) {
    y[i + floor(window_size / 2)] <- median(x[i:(i + window_size - 1)], na.rm = TRUE)
  }
  
  return(y)
}

#median absolute distance (MAD)
median_absolute_distance <- function(x, window_size = 21) {
  n <- length(x)
  y <- vector("numeric", n)
  y[] <- NA  # Initialize with NAs
  
  for (i in 1:(n - window_size + 1)) {
    y[i + floor(window_size / 2)] <- 
      median(abs(median(x[i:(i + window_size - 1)])-x[i:(i + window_size - 1)]), na.rm = TRUE)
  }
  
  return(y)
}

#median cross-validation criterion for selecting optimum k (window_size = 2*k+1) based on Zheng and Yang (1998); the minimum k is set to 6 and maximum k is 14. The range of possible k is selected to enable for reasonable decadal variations
calculate_C1 <- function(x) {
  n <- length(x)
  k_start <- 6
  k_end <- 14
  
  # Initialize variables to store results
  C1_values <- numeric(k_end - k_start + 1)  # To store C1 values for each k
  min_index <- NA  # Initialize variable to store index of minimum C1 value
  
  for (k in k_start:k_end) {
    sum_abs_diff <- 0
    
    for (i in 1:n) {
      # Define the indices for the neighborhood
      start_index <- max(1, i - k)
      end_index <- min(n, i + k)
      neighborhood <- x[start_index:end_index]
      
      # Exclude the current element x[i]
      neighborhood <- neighborhood[neighborhood != x[i]]
      
      # Calculate the delete-one median
      if (length(neighborhood) > 0) {
        delete_one_median <- median(neighborhood)
      } else {
        delete_one_median <- NA
      }
      
      # Compute the absolute difference
      abs_diff <- abs(x[i] - delete_one_median)
      sum_abs_diff <- sum_abs_diff + abs_diff
    }
    
    # Calculate the final value of C1(k)
    C1_k <- sum_abs_diff / n
    C1_values[k - k_start + 1] <- C1_k
  }
  
  # Find the index of the minimum non-NA C1 value
  valid_indices <- which(!is.na(C1_values))
  if (length(valid_indices) > 0) {
    min_index <- valid_indices[which.min(C1_values[valid_indices])] + k_start - 1
  }
  
  return(min_index)
}

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

win_size_lab <- paste("(", 2*(distinct(full_ds, lake_name, layer, .keep_all = TRUE))$k + 1, ")", sep = "")

custom_labeller <- function(labels) {
  labels <- as.data.frame(labels)
  labels$combined <- paste(paste(labels$lake_name, labels$layer, sep = " - "), win_size_lab)
  labels <- labels %>% dplyr::select(combined)
  return(labels)
}

treshold_plot_prep <- full_ds %>%
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

treshold_plot <- treshold_plot_prep + 
  geom_rect(data = filter(full_ds, lake_name == "Kuninkaisenlampi" & age_CE == 1766), 
            aes(xmin = 1783, xmax = 1797, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.5)

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
  filter(!(lake_name == "Kuninkaisenlampi" & age_CE >= 1783 & age_CE <= 1797)) %>%
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
