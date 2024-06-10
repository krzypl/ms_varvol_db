library(tidyverse)

#In the functions: for ti < window_width and ti > tn - window width, the moving median and mad the "missing" data necessery to calculate the parameters were substituted with pseudodata. The genaration of pseudodata followed the method desribed by Cowling and Hall (1996) that relies on mirroring data that are within the range of [T(1);T(n)]. Eq. 6.35 in Mudelsee, p. 237
#functions--------
moving_median_extended <- function(x, window_size = 15) {
  y <- vector("numeric", length(x))
  window_half <- floor(window_size / 2)
  x2 <- c(x[(1+((window_size/2)-0.5)):2], x, x[(length(x)-1):(length(x)-((window_size/2)-0.5))])
  
  # Calculate moving median for middle elements
  for (i in 1:(length(x2)-window_size+1)) {
    y[i] = median(x2[i:(i + window_size - 1)])
  }
  
  return(y)
}

#median absolute distance (MAD)
median_absolute_distance <- function(x, s_i, window_size = 5) {
  y <- vector("numeric", length(x))
  window_half <- floor(window_size / 2)
  x2 <- c(x[(1+((window_size/2)-0.5)):2], x, x[(length(x)-1):(length(x)-((window_size/2)-0.5))]) #x is varve thickncess
  s_i2 <- c(s_i[(1+((window_size/2)-0.5)):2], s_i, s_i[(length(s_i)-1):(length(s_i)-((window_size/2)-0.5))]) #s_i is moving average
  x_s_i_diff <- abs(x2 - s_i2)
  
  for (i in 1:(length(x_s_i_diff)-window_size+1)) {
    y[i] = median(x_s_i_diff[i:(i + window_size - 1)])
  }
  
  return(y)
}

#
full_ds <- read_csv("data/full_ds.csv") %>% 
  filter(age_CE > 1765 & age_CE < 1867) %>%
  group_by(lake_name) %>% 
  mutate(moving_median_tvt = moving_median_extended(varve_thick, window_size = 15),
         moving_median_llt =
           moving_median_extended(light_lamin_thick, window_size = 15),
         moving_median_dlt = 
           moving_median_extended(dark_lamin_thick, window_size = 15),
         mad_tvt = median_absolute_distance(x = varve_thick, s_i = moving_median_tvt,
                                  window_size = 15),
         mad_llt = 
           median_absolute_distance(x = light_lamin_thick, s_i = moving_median_llt,
                                            window_size = 15),
         mad_dlt = 
           median_absolute_distance(x = dark_lamin_thick, s_i = moving_median_dlt,
                                            window_size = 15),
         tresh_pos_tvt_z2 = moving_median_tvt + mad_tvt*2,
         tresh_pos_llt_z2 = moving_median_llt + mad_llt*2,
         tresh_pos_dlt_z2 = moving_median_dlt + mad_dlt*2,
         tresh_pos_tvt_z3_5 = moving_median_tvt + mad_tvt*3.5,
         tresh_pos_llt_z3_5 = moving_median_llt + mad_llt*3.5,
         tresh_pos_dlt_z3_5 = moving_median_dlt + mad_dlt*3.5,
         tresh_pos_tvt_z5 = moving_median_tvt + mad_tvt*5,
         tresh_pos_llt_z5 = moving_median_llt + mad_llt*5,
         tresh_pos_dlt_z5 = moving_median_dlt + mad_dlt*5,
         tresh_neg_tvt_z1 = moving_median_tvt - mad_tvt*1,
         tresh_neg_llt_z1 = moving_median_llt - mad_llt*1,
         tresh_neg_dlt_z1 = moving_median_dlt - mad_dlt*1,
         tresh_neg_tvt_z1_75 = moving_median_tvt - mad_tvt*1.75,
         tresh_neg_llt_z1_75 = moving_median_llt - mad_llt*1.75,
         tresh_neg_dlt_z1_75 = moving_median_dlt - mad_dlt*1.75,
         tresh_neg_tvt_z2_5 = moving_median_tvt - mad_tvt*2.5,
         tresh_neg_llt_z2_5 = moving_median_llt - mad_llt*2.5,
         tresh_neg_dlt_z2_5 = moving_median_dlt - mad_dlt*2.5
         ) %>% 
  ungroup()

tvt_plot <- full_ds %>% 
  filter(!is.na(varve_thick)) %>% 
  ggplot() +
  geom_line(aes(x = age_CE, y = varve_thick), linewidth = 0.2) +
  geom_point(aes(x = age_CE, y = varve_thick)) +
  geom_line(aes(x = age_CE, y = moving_median_tvt), color = "red") +
  geom_line(aes(x = age_CE, y = tresh_pos_tvt_z2), color = "orange") +
  geom_line(aes(x = age_CE, y = tresh_pos_tvt_z3_5), color = "darkorange2") +
  geom_line(aes(x = age_CE, y = tresh_pos_tvt_z5), color = "darkorange4") +
  geom_line(aes(x = age_CE, y = tresh_neg_tvt_z1), color = "lightblue") +
  geom_line(aes(x = age_CE, y = tresh_neg_tvt_z1_75), color = "blue") +
  geom_line(aes(x = age_CE, y = tresh_neg_tvt_z2_5), color = "darkblue") +
  annotate("rect",
           xmin = 1816 - 7, xmax = 1816 + 7, ymin = -Inf, ymax = Inf,
           alpha = 0.1, fill = "blue") +
  geom_vline(xintercept = 1816, color = "blue") +
  facet_wrap(.~ lake_name, scales = "free_y")
 
n_of_out_tvt <- full_ds %>% 
  filter(!is.na(varve_thick)) %>% 
  group_by(lake_name) %>% 
  summarise(n_pos_z2 = sum(varve_thick > tresh_pos_tvt_z2),
            n_pos_z3_5 = sum(varve_thick > tresh_pos_tvt_z3_5),
            n_pos_z5 = sum(varve_thick > tresh_pos_tvt_z5),
            n_neg_z1 = sum(varve_thick < tresh_neg_tvt_z1),
            n_neg_z1_75 = sum(varve_thick < tresh_neg_tvt_z1_75),
            n_neg_z2_5 = sum(varve_thick < tresh_neg_tvt_z2_5)) %>% 
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

n_of_out_tvt_plot <- ggplot(n_of_out_tvt,
                            aes(x = treshold, y = n_of_extremes, fill = z)) +
  geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap(.~ lake_name)

scales_out_tvt <- full_ds %>% 
  filter(!is.na(varve_thick)) %>% 
  filter(varve_thick < tresh_neg_tvt_z2_5 | varve_thick > tresh_pos_tvt_z5) %>%
  dplyr::select(lake_name, age_CE, tresh_pos_tvt_z5,
         tresh_neg_tvt_z2_5, varve_thick, moving_median_tvt, mad_tvt) %>% 
  mutate(scaled_magnitude = (varve_thick - moving_median_tvt)/mad_tvt,
         scaled_magnitude_sign = as.factor(ifelse(scaled_magnitude > 0, "positive", "negative")))

scales_out_tvt_plot <- ggplot(scales_out_tvt, aes(x = age_CE, y = scaled_magnitude, fill = scaled_magnitude_sign)) +
  geom_col() +
  annotate("rect",
           xmin = 1816 - 7, xmax = 1816 + 7, ymin = -Inf, ymax = Inf,
           alpha = 0.1, fill = "blue") +
  geom_vline(xintercept = 1816, color = "blue") +
  facet_wrap(.~ lake_name, scales = "free_y") +
  theme(legend.position = "bottom")
  
