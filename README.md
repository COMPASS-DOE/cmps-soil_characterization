## Transition zones at the changing coastal terrestrial-aquatic interface

This repository contains data and code for the manuscript "Transition zones at the changing coastal terrestrial-aquatic interface" by Patel et al. 2025.

---

### Abstract
Coastal soils are a significant but highly uncertain component of global biogeochemical cycles. 
These systems experience spatial and temporal variability in biogeochemical processes, driven by marsh-to-upland gradients and hydrological fluctuations. 
These fluctuations make it difficult to understand and predict biogeochemical processes in these highly dynamic systems. 
We studied coastal soil biogeochemistry and its variability (a) at regional scales and (b) across transects from upland forest to marsh, in two contrasting regions — 
Lake Erie, a freshwater lacustrine system, and Chesapeake Bay, a saltwater estuarine system. 
Salinity-related analytes were a key source of variability in soil biogeochemistry, not just in the saltwater system, but surprisingly, also in the freshwater system. 
We had hypothesized linear trends in biogeochemical parameters along the TAI – 
however, contrary to expectations, transition soils were not consistently intermediate between upland and marsh endmembers; 
the non-monotonic trends of C, P, Fe along our transects suggest that these do not behave as expected and may be difficult to model and predict – 
thus these are key analytes to study in our regions. 
Rapidly changing soil factors across coastal gradients (e.g., Ca, K, CEC, and TS) may act as precursors to ecosystem shifts. 
Our comprehensive soil characterization represents a snapshot of a single timepoint of surface soils and provides essential data for mechanistic modeling of ecosystem dynamics across coastal transects.


<img align = "center" height = "400" src="https://github.com/COMPASS-DOE/cmps-soil_characterization/blob/main/3-reports/manuscript_figures_files/figure-gfm/map-1.png">

---

### Directory structure

```

home
|
|----- 1-data/
|         |----- raw/
|         |----- processed/
|         |----- soil_moisture_sensors/
|         |----- sample_key.csv
|         |----- analysis_key.csv
|         |----- synoptic_elevations.csv
|
|----- 2-code/
|
|----- 3-reports/
|
|----- cb_2018_us_state_5m/
|
|----- cmps-soil_characterization.Rproj
|----- README.md
|----- _targets.R
|----- run.R

```

We used the `{targets}` package for reproducible analytical workflows ([Landau 2021](https://doi.org/10.21105/joss.02959)). 
The workflow contains targets stored in `_targets.R`, which is run though `run.R`, both in the parent directory. 
The `_targets.R` file pulls data from `1-data/` and functions from `2-code/`, and the final graphs are rendered in the `3-reports/` subdirectory. 

---

## Funding
This work was supported through the Field, Measurements, and Experiments (FME) component of the 
Coastal Observations, Mechanisms, and Predictions Across Systems and Scales (COMPASS) program (https://compass.pnnl.gov/). 
COMPASS-FME is a multi-institutional project supported by the US Department of Energy, Office of Science, Biological and Environmental Research 
as part of the Environmental System Science Program.

A portion of this research was performed on project awards ([60602](https://www.osti.gov/award-doi-service/biblio/10.46936/cpcy.proj.2022.60602/60008672), 
[60511](https://www.osti.gov/award-doi-service/biblio/10.46936/cpcy.proj.2022.60511/60008517), [61038](https://www.osti.gov/award-doi-service/biblio/10.46936/cont.proj.2023.61038/60012321)) 
from the Environmental Molecular Sciences Laboratory, 
a DOE Office of Science User Facility sponsored by the Biological and Environmental Research program under Contract No. DE-AC05-76RL01830.

---

## Session Info

<details>
<summary>
Session Info
</summary>

    ## R version 4.5.0 (2025-04-11)
    ## Platform: aarch64-apple-darwin20
    ## Running under: macOS Sequoia 15.6.1
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRblas.0.dylib 
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## time zone: America/Los_Angeles
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] multcomp_1.4-28     TH.data_1.1-3       MASS_7.3-65        
    ##  [4] survival_3.8-3      mvtnorm_1.3-3       vegan_2.7-1        
    ##  [7] permute_0.9-7       ggConvexHull_0.1.0  factoextra_1.0.7   
    ## [10] ggspatial_1.1.9     sf_1.0-21           ggh4x_0.3.1        
    ## [13] ggbiplot_0.55       furrr_0.3.1         future_1.58.0      
    ## [16] googledrive_2.1.1   beepr_2.0           tictoc_1.2.1       
    ## [19] cowplot_1.1.3       parsedate_1.3.2     janitor_2.2.1      
    ## [22] pacman_0.5.1        googlesheets4_1.1.1 soilpalettes_0.1.0 
    ## [25] PNWColors_0.1.0     magrittr_2.0.3      lubridate_1.9.4    
    ## [28] forcats_1.0.0       stringr_1.5.1       dplyr_1.1.4        
    ## [31] purrr_1.0.4         readr_2.1.5         tidyr_1.3.1        
    ## [34] tibble_3.3.0        ggplot2_3.5.2       tidyverse_2.0.0    
    ## [37] tarchetypes_0.13.1  targets_1.11.3     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rdpack_2.6.4       DBI_1.2.3          gridExtra_2.3      s2_1.1.8          
    ##  [5] sandwich_3.1-1     rlang_1.1.6        snakecase_0.11.1   e1071_1.7-16      
    ##  [9] compiler_4.5.0     mgcv_1.9-1         callr_3.7.6        vctrs_0.6.5       
    ## [13] reshape2_1.4.4     pkgconfig_2.0.3    wk_0.9.4           fastmap_1.2.0     
    ## [17] backports_1.5.0    labeling_0.4.3     rmarkdown_2.29     tzdb_0.5.0        
    ## [21] nloptr_2.2.1       ps_1.9.1           xfun_0.52          broom_1.0.8       
    ## [25] parallel_4.5.0     prettyunits_1.2.0  cluster_2.1.8.1    R6_2.6.1          
    ## [29] stringi_1.8.7      RColorBrewer_1.1-3 boot_1.3-31        parallelly_1.45.1 
    ## [33] car_3.1-3          cellranger_1.1.0   Rcpp_1.0.14        knitr_1.50        
    ## [37] zoo_1.8-14         audio_0.1-11       Matrix_1.7-3       splines_4.5.0     
    ## [41] igraph_2.1.4       timechange_0.3.0   tidyselect_1.2.1   rstudioapi_0.17.1 
    ## [45] abind_1.4-8        yaml_2.3.10        codetools_0.2-20   processx_3.8.6    
    ## [49] listenv_0.9.1      lattice_0.22-6     plyr_1.8.9         withr_3.0.2       
    ## [53] evaluate_1.0.3     units_0.8-7        proxy_0.4-27       pillar_1.10.2     
    ## [57] ggpubr_0.6.0       carData_3.0-5      KernSmooth_2.23-26 reformulas_0.4.1  
    ## [61] generics_0.1.3     hms_1.1.3          scales_1.4.0       minqa_1.2.8       
    ## [65] globals_0.18.0     base64url_1.4      class_7.3-23       glue_1.8.0        
    ## [69] tools_4.5.0        data.table_1.17.0  lme4_1.1-37        ggsignif_0.6.4    
    ## [73] fs_1.6.6           grid_4.5.0         rbibutils_2.3      nlme_3.1-168      
    ## [77] Formula_1.2-5      cli_3.6.5          gargle_1.5.2       gtable_0.3.6      
    ## [81] ggcorrplot_0.1.4.1 rstatix_0.7.2      digest_0.6.37      classInt_0.4-11   
    ## [85] ggrepel_0.9.6      farver_2.1.2       htmltools_0.5.8.1  lifecycle_1.0.4   
    ## [89] secretbase_1.0.5

</details>
