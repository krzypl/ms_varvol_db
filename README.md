This repository contains the code necessary to reproduce all analyses and figures for the paper: "Is the ‘Year Without a Summer’ imprinted in continental varve thickness records?" by Krzysztof Pleskot and Bernd Zolitschka, published in Quaternary Science Reviews.

The varve thickness time series analyzed in this study were obtained either directly from public data repositories or shared by the authors of the original publications. These data are stored in the "data" folder of this repository.

The data necessary for calculating site-specific climate anomalies and reproducing climate anomaly maps can be obtained from https://www.wdc-climate.de/ui/entry?acronym=EKF400_ens_mem_Mean_v2.0 upon registration.


## R Session Information

```r
R version 4.2.1 (2022-06-23 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default

locale:
[1] LC_COLLATE=Polish_Poland.utf8  LC_CTYPE=Polish_Poland.utf8    LC_MONETARY=Polish_Poland.utf8
[4] LC_NUMERIC=C                   LC_TIME=Polish_Poland.utf8    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] gridExtra_2.3           patchwork_1.1.2         abind_1.4-5             rnaturalearthdata_1.0.0 rnaturalearth_1.0.1    
 [6] tmap_3.3-3              sf_1.0-14               raster_3.6-23           ncdf4_1.21              maps_3.4.2             
[11] ggrepel_0.9.2           sp_2.1-3                pangaear_1.1.0          lubridate_1.9.2         forcats_1.0.0          
[16] stringr_1.5.1           dplyr_1.1.4             purrr_1.0.1             readr_2.1.4             tidyr_1.3.1            
[21] tibble_3.2.1            ggplot2_3.5.1           tidyverse_2.0.0        

loaded via a namespace (and not attached):
 [1] oai_0.4.0          bit64_4.0.5        RColorBrewer_1.1-3 httr_1.4.7         tools_4.2.1        utf8_1.2.4        
 [7] R6_2.5.1           KernSmooth_2.23-20 DBI_1.1.3          colorspace_2.1-0   withr_3.0.0        tidyselect_1.2.1  
[13] leaflet_2.1.2      bit_4.0.5          curl_4.3.2         compiler_4.2.1     textshaping_0.3.6  leafem_0.2.0      
[19] cli_3.6.2          xml2_1.3.3         labeling_0.4.3     triebeard_0.4.1    scales_1.3.0       classInt_0.4-9    
[25] proxy_0.4-27       rappdirs_0.3.3     systemfonts_1.0.4  digest_0.6.31      svglite_2.1.1      base64enc_0.1-3   
[31] dichromat_2.0-0.1  pkgconfig_2.0.3    htmltools_0.5.4    fastmap_1.1.0      htmlwidgets_1.6.2  rlang_1.1.3       
[37] rstudioapi_0.15.0  httpcode_0.3.0     generics_0.1.3     farver_2.1.1       jsonlite_1.8.7     crosstalk_1.2.0   
[43] vroom_1.6.3        magrittr_2.0.3     s2_1.1.4           Rcpp_1.0.11        munsell_0.5.1      fansi_1.0.6       
[49] lifecycle_1.0.4    terra_1.7-39       stringi_1.8.3      leafsync_0.1.0     tmaptools_3.1-1    plyr_1.8.8        
[55] grid_4.2.1         parallel_4.2.1     crayon_1.5.2       lattice_0.20-45    stars_0.6-0        hms_1.1.3         
[61] pillar_1.9.0       pkgload_1.3.3      codetools_0.2-18   crul_1.4.0         wk_0.7.3           XML_3.99-0.13     
[67] glue_1.7.0         hoardr_0.5.3       vctrs_0.6.5        png_0.1-8          tzdb_0.4.0         urltools_1.7.3    
[73] gtable_0.3.5       lwgeom_0.2-10      e1071_1.7-13       ragg_1.2.5         class_7.3-20       viridisLite_0.4.2 
[79] units_0.8-2        timechange_0.2.0  
