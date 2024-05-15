library(tidyverse)
library(ggrepel)
library(maps)

varda_index_red <- read_csv("data/varda_index_red.csv") %>% 
  distinct(dataset.core.lake.name, .keep_all = TRUE)

lakes_distr <- ggplot() +
  borders("world", fill = "gray80", colour = "black", size = 0.5) +
  coord_fixed(ratio = 1.25) +
  geom_point(data = varda_index_red, aes(x = dataset.core.lake.longitude, y = dataset.core.lake.latitude), color = "red", size = 3) +
  geom_text_repel(data = varda_index_red, aes(x = dataset.core.lake.longitude, y = dataset.core.lake.latitude, label = dataset.core.lake.name), size = 4, color = "darkgreen")