library(tidyverse)
library(ggrepel)

#plot varve thickness time series -------

varda_long_red <- read_csv("data/varda_long_red.csv")

varve_thick_plot <- ggplot(varda_long_red, aes(x = age_CE, y = varve_thick)) +
  geom_line() +
  geom_point(size = 0.4) +
  geom_vline(xintercept = 1816, color = "blue", alpha = 0.8) +
  facet_wrap(.~ lake_name, scales = "free_y")

varve_thick_plot

varve_thick_plot + xlim(c(1770, 1870))

varve_thick_plot + xlim(c(1816-50, 1816+50)) +
  geom_vline(xintercept = 1783, color = "green", alpha = 0.8) + #Laki, Iceland
  geom_vline(xintercept = 1809, color = "green", alpha = 0.8) #undiff

dark_thick_plot <- varda_long_red %>% 
  filter(!is.na(dark_lamin_thick)) %>% 
  ggplot(aes(x = age_CE, y = dark_lamin_thick)) + 
  geom_line() +
  geom_point(size = 0.4) +
  geom_vline(xintercept = 1816, color = "blue", alpha = 0.8) +
  facet_wrap(.~ lake_name, scales = "free_y")

dark_thick_plot

dark_thick_plot + xlim(c(1770, 1870))

dark_thick_plot + xlim(c(1816-50, 1816+50)) +
  geom_vline(xintercept = 1783, color = "green", alpha = 0.8) + #Laki, Iceland
  geom_vline(xintercept = 1809, color = "green", alpha = 0.8) #undiff

light_thick_plot <- varda_long_red %>% 
  filter(!is.na(light_lamin_thick)) %>% 
  ggplot(aes(x = age_CE, y = light_lamin_thick)) + 
  geom_line() +
  geom_point(size = 0.4) +
  geom_vline(xintercept = 1816, color = "blue", alpha = 0.8) +
  facet_wrap(.~ lake_name, scales = "free_y")

light_thick_plot

light_thick_plot + xlim(c(1770, 1870))

light_thick_plot + xlim(c(1816-50, 1816+50)) +
  geom_vline(xintercept = 1783, color = "green", alpha = 0.8) + #Laki, Iceland
  geom_vline(xintercept = 1809, color = "green", alpha = 0.8) #undiff

#plot peak detection results ---------
##total vt-----
#add labels to peaks so that the corresponding year is visible
pd_df_vt_total_mgcv <- read_csv("data/pd_df_vt_total_mgcv.csv")

pd_df_vt_total_mgcv_plot <- ggplot(pd_df_vt_total_mgcv, aes(x = age_CE, y = varve_thick)) +
  geom_line() +
  geom_point(size = 0.4) +
  geom_point(data = filter(pd_df_vt_total_mgcv, peaks_pos == 1),
             shape = 8, color = "red") +
  geom_point(data = filter(pd_df_vt_total_mgcv, peaks_neg == 1), 
             shape = 8, color = "blue") +
  geom_vline(xintercept = 1816, color = "blue", alpha = 0.8) +
  geom_text_repel(data = filter(pd_df_vt_total_mgcv,
                                (peaks_pos == 1 | peaks_neg == 1) & age_CE >= 1805 & age_CE <= 1825),
            aes(label = as.character(round(age_CE, 0))),
             vjust = -0.5) +
  facet_wrap(.~ lake_name, scales = "free_y")



##light laminae thickness-------
pd_df_vt_light_mgcv <- read_csv("data/pd_df_vt_light_mgcv.csv")

pd_df_vt_light_mgcv_plot <- ggplot(pd_df_vt_light_mgcv, aes(x = age_CE, y = light_lamin_thick)) +
  geom_line() +
  geom_point(size = 0.4) +
  geom_point(data = filter(pd_df_vt_light_mgcv, peaks_pos == 1),
             shape = 8, color = "red") +
  geom_point(data = filter(pd_df_vt_light_mgcv, peaks_neg == 1), 
             shape = 8, color = "blue") +
  geom_vline(xintercept = 1816, color = "blue", alpha = 0.8) +
  geom_text_repel(data = filter(pd_df_vt_light_mgcv,
                                (peaks_pos == 1 | peaks_neg == 1) & age_CE >= 1805 & age_CE <= 1825),
                  aes(label = as.character(round(age_CE, 0))),
                  vjust = -0.5) +
  facet_wrap(.~ lake_name, scales = "free_y")

pd_df_vt_light_mgcv + xlim(c(1800, 1840))

##dark laminae thickness-------
pd_df_vt_dark_mgcv <- read_csv("data/pd_df_vt_dark_mgcv.csv")

pd_df_vt_dark_mgcv_plot <- ggplot(pd_df_vt_dark_mgcv, aes(x = age_CE, y = dark_lamin_thick)) +
  geom_line() +
  geom_point(size = 0.4) +
  geom_point(data = filter(pd_df_vt_dark_mgcv, peaks_pos == 1),
             shape = 8, color = "red") +
  geom_point(data = filter(pd_df_vt_dark_mgcv, peaks_neg == 1), 
             shape = 8, color = "blue") +
  geom_vline(xintercept = 1816, color = "blue", alpha = 0.8) +
  geom_text_repel(data = filter(pd_df_vt_dark_mgcv,
                                (peaks_pos == 1 | peaks_neg == 1) & age_CE >= 1805 & age_CE <= 1825),
                  aes(label = as.character(round(age_CE, 0))),
                  vjust = -0.5) +
  facet_wrap(.~ lake_name, scales = "free_y")

pd_df_vt_dark_mgcv + xlim(c(1800, 1840))