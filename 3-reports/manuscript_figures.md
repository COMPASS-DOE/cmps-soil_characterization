Synpotic Soil Characterization
================

------------------------------------------------------------------------

    ## [1] "df created: `data_combined_wide`, `data_wide_PCA`"

------------------------------------------------------------------------

## Site Map

![](manuscript_figures_files/figure-gfm/map-1.png)<!-- -->

------------------------------------------------------------------------

## 0. Correlations

![](manuscript_figures_files/figure-gfm/gg_corr-1.png)<!-- -->

Highly correlated variables include:

- (+) TC & WEOC, TN
- (+) TS & Chloride, Sulfate, Na, CEC
- (+) CEC & TS, Na, K, Mg, SpConductance
- (+) Na & SpConductance, TS, K, Mg
- (+) Chloride & Sulfate, TS, CEC, SpConductance, Na
- (-) pH & WEOC, Al
- (-) Ca and Al

------------------------------------------------------------------------

# VERSION 1: ALL ANALYTES

<details>
<summary>
Click to Open
</summary>

## 1. PCAs

------------------------------------------------------------------------

### Overall PCAs

### PCA with clusters

![](manuscript_figures_files/figure-gfm/cluster_pca-1.png)<!-- -->

![](manuscript_figures_files/figure-gfm/pca_gg_regions-1.png)<!-- -->

### Drivers and loadings

![](manuscript_figures_files/figure-gfm/pc_gg-1.png)<!-- -->

------------------------------------------------------------------------

## 1b. PERMANOVA

    ## Permutation test for adonis under reduced model
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = (data_wide_PCA %>% dplyr::select(where(is.numeric)) %>% drop_na()) ~ (region + transect + horizon + site)^2, data = data_wide_PCA %>% drop_na)
    ##           Df SumOfSqs      R2      F Pr(>F)    
    ## Model     17  16.8403 0.82363 31.041  0.001 ***
    ## Residual 113   3.6062 0.17637                  
    ## Total    130  20.4464 1.00000                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## 2. Each analyte

### Normalized values - v2

![](manuscript_figures_files/figure-gfm/scaled_gg2-1.png)<!-- -->

![](manuscript_figures_files/figure-gfm/scaled_gg2_split-1.png)<!-- -->

### GWI only

![](manuscript_figures_files/figure-gfm/scaled_gg2_gwi-1.png)<!-- -->

</details>

# VERSION 2: DROPPING CORRELATED ANALYTES

### Overall PCAs

### PCA with clusters

![](manuscript_figures_files/figure-gfm/v2-cluster_pca-1.png)<!-- -->

------------------------------------------------------------------------

## PCA by region

![](manuscript_figures_files/figure-gfm/v2-pca_gg_regions-1.png)<!-- -->

### Drivers and loadings

![](manuscript_figures_files/figure-gfm/v2-pc_gg-1.png)<!-- -->

## 1b. PERMANOVA

    ## Permutation test for adonis under reduced model
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = (data_wide_PCA2 %>% dplyr::select(where(is.numeric)) %>% drop_na()) ~ (region + transect + horizon + site)^2, data = data_wide_PCA2 %>% drop_na)
    ##           Df SumOfSqs      R2      F Pr(>F)    
    ## Model     17  17.1096 0.80912 28.676  0.001 ***
    ## Residual 115   4.0362 0.19088                  
    ## Total    132  21.1459 1.00000                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## 2. Each analyte

### Normalized values

### only select analytes

![](manuscript_figures_files/figure-gfm/v2-scaled_gg2_split-NEW-1.png)<!-- -->

# Specific analytes

![](manuscript_figures_files/figure-gfm/analytesx-3-1.png)<!-- -->

## Chesapeake-only plots for GWI

![](manuscript_figures_files/figure-gfm/analytes-gwi-1.png)<!-- -->

## Water retention curves

![](manuscript_figures_files/figure-gfm/gg_hyprop-1.png)<!-- -->

## Elevation

![](manuscript_figures_files/figure-gfm/gg_elevation-1.png)<!-- -->

------------------------------------------------------------------------

## SOIL MOISTURE SENSORS

Processed data are available here in this repository. Raw data are
available on Google Drive (access needed).

### Soil water content for 2022-23

![](manuscript_figures_files/figure-gfm/gg_vwc-1.png)<!-- -->

### Summarized soil water content

| region     | site | transect   | mean_vwc | sd_vwc | median_vwc |
|:-----------|:-----|:-----------|---------:|-------:|-----------:|
| Erie       | CRC  | upland     |    27.31 |   9.90 |      28.53 |
| Erie       | CRC  | transition |    37.15 |   9.74 |      40.66 |
| Erie       | CRC  | marsh      |    48.33 |   4.94 |      49.43 |
| Erie       | PTR  | upland     |    24.81 |  10.44 |      25.35 |
| Erie       | PTR  | transition |    27.49 |   9.97 |      27.90 |
| Erie       | PTR  | marsh      |    39.09 |  13.14 |      43.38 |
| Erie       | OWC  | upland     |    32.74 |   2.96 |      33.66 |
| Erie       | OWC  | transition |    39.47 |   6.48 |      38.67 |
| Erie       | OWC  | marsh      |    47.66 |   1.86 |      47.53 |
| Chesapeake | GCW  | transition |    50.89 |   2.57 |      52.22 |
| Chesapeake | GCW  | marsh      |    57.96 |   0.79 |      57.79 |
| Chesapeake | MSM  | upland     |    41.57 |   2.40 |      41.65 |
| Chesapeake | MSM  | transition |    48.43 |   2.98 |      47.93 |
| Chesapeake | MSM  | marsh      |    62.49 |   1.01 |      62.46 |
| Chesapeake | GWI  | upland     |    30.41 |   3.67 |      30.12 |
| Chesapeake | GWI  | transition |    54.47 |   3.36 |      54.77 |
| Chesapeake | GWI  | marsh      |    61.86 |   0.94 |      62.06 |

### Water level in soil

When water level is positive (above surface), it means the soil is
flooded.

![](manuscript_figures_files/figure-gfm/gg_water_level-1.png)<!-- -->

### Water table summary

| region     | site | transect   | mean_wl | sd_wl | median_wl | min_wl | max_wl |
|:-----------|:-----|:-----------|--------:|------:|----------:|-------:|-------:|
| Chesapeake | MSM  | upland     |   -0.19 |  0.15 |     -0.13 |  -0.62 |   0.01 |
| Chesapeake | MSM  | transition |   -0.11 |  0.07 |     -0.10 |  -0.79 |   0.23 |
| Chesapeake | MSM  | marsh      |   -0.11 |  0.09 |     -0.10 |  -0.61 |   0.34 |
| Chesapeake | GWI  | upland     |   -0.49 |  0.15 |     -0.47 |  -1.00 |   0.03 |
| Chesapeake | GWI  | transition |   -0.22 |  0.18 |     -0.14 |  -0.93 |   0.38 |
| Chesapeake | GWI  | marsh      |   -0.06 |  0.10 |     -0.03 |  -0.42 |   0.54 |
| Erie       | CRC  | upland     |   -2.49 |  1.52 |     -2.79 |  -4.91 |  -0.04 |
| Erie       | CRC  | transition |   -0.63 |  0.52 |     -0.43 |  -1.38 |   0.42 |
| Erie       | CRC  | marsh      |   -0.12 |  0.17 |     -0.03 |  -0.38 |   0.66 |
| Erie       | PTR  | upland     |   -3.06 |  1.16 |     -3.36 |  -5.07 |  -0.29 |
| Erie       | PTR  | transition |   -1.26 |  0.69 |     -1.11 |  -2.55 |  -0.02 |
| Erie       | PTR  | marsh      |   -0.46 |  0.33 |     -0.29 |  -0.97 |   0.27 |
| Erie       | OWC  | marsh      |    0.03 |  0.14 |      0.00 |  -0.36 |   0.69 |

### Calculating time flooded (% of the year)

| region     | site | transect   | percent_flooded |
|:-----------|:-----|:-----------|----------------:|
| Chesapeake | MSM  | upland     |            0.06 |
| Chesapeake | MSM  | transition |            2.64 |
| Chesapeake | MSM  | marsh      |            8.24 |
| Chesapeake | GWI  | upland     |            0.02 |
| Chesapeake | GWI  | transition |            1.04 |
| Chesapeake | GWI  | marsh      |            8.34 |
| Erie       | CRC  | upland     |            0.00 |
| Erie       | CRC  | transition |            0.73 |
| Erie       | CRC  | marsh      |           32.59 |
| Erie       | PTR  | upland     |            0.00 |
| Erie       | PTR  | transition |            0.00 |
| Erie       | PTR  | marsh      |            0.44 |
| Erie       | OWC  | marsh      |           51.03 |

------------------------------------------------------------------------

## Session Info

<details>
<summary>
Session Info
</summary>

Date run: 2025-09-08

    ## R version 4.5.0 (2025-04-11)
    ## Platform: aarch64-apple-darwin20
    ## Running under: macOS Sequoia 15.6
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
