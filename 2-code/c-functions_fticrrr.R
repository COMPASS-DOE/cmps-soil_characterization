
## These functions have been modified from the `fticrrr` package and workflow: 
## https://github.com/kaizadp/fticrrr

################################################## #

# `fticrrr-functions.R`

################################################## #



################################################## #
################################################## #

import_fticr_data = function(FILEPATH){
  
  df <- 
    list.files(path=FILEPATH, full.names = TRUE) %>% 
    lapply(read_csv) %>% 
    bind_rows
  
}





# 1. PROCESSING FUNCTIONS -------------------------------------------------
## LEVEL I FUNCTIONS -------------------------------------------------------
## for metadata file
apply_filter_report = function(report){
  report %>% 
    # filter appropriate mass range
    filter(Mass>200 & Mass<800) %>% 
    # remove isotopes
    filter(C13==0) %>% 
    # remove peaks without C assignment
    filter(C>0)
}
compute_indices = function(dat){
  dat %>% 
    dplyr::select(Mass, C:P) %>% 
    dplyr::mutate(AImod = round((1+C-(0.5*O)-S-(0.5*(N+P+H)))/(C-(0.5*O)-S-N-P),4),
                  NOSC =  round(4-(((4*C)+H-(3*N)-(2*O)-(2*S))/C),4),
                  HC = round(H/C,2),
                  OC = round(O/C,2),
                  DBE_AI = 1+C-O-S-0.5*(N+P+H),
                  DBE =  1 + ((2*C-H + N + P))/2,
                  DBE_C = round(DBE_AI/C,4)) %>% 
    dplyr::select(-c(C:P))
}
compute_mol_formula = function(dat){
  dat %>% 
    dplyr::select(Mass, C:P) %>% 
    dplyr::mutate(formula_c = if_else(C>0,paste0("C",C),as.character(NA)),
                  formula_h = if_else(H>0,paste0("H",H),as.character(NA)),
                  formula_o = if_else(O>0,paste0("O",O),as.character(NA)),
                  formula_n = if_else(N>0,paste0("N",N),as.character(NA)),
                  formula_s = if_else(S>0,paste0("S",S),as.character(NA)),
                  formula_p = if_else(P>0,paste0("P",P),as.character(NA)),
                  formula = paste0(formula_c,formula_h, formula_o, formula_n, formula_s, formula_p),
                  formula = str_replace_all(formula,"NA","")) %>% 
    dplyr::select(Mass, formula, C:P)
}
assign_class_seidel = function(meta_clean, meta_indices){
  meta_clean %>%
    left_join(meta_indices, by = "Mass") %>% 
    mutate(Class = case_when(AImod>0.66 ~ "condensed aromatic",
                             AImod<=0.66 & AImod > 0.50 ~ "aromatic",
                             AImod <= 0.50 & HC < 1.5 ~ "unsaturated/lignin",
                             HC >= 1.5 ~ "aliphatic"),
           Class = replace_na(Class, "other"),
           Class_detailed = case_when(AImod>0.66 ~ "condensed aromatic",
                                      AImod<=0.66 & AImod > 0.50 ~ "aromatic",
                                      AImod <= 0.50 & HC < 1.5 ~ "unsaturated/lignin",
                                      HC >= 2.0 & OC >= 0.9 ~ "carbohydrate",
                                      HC >= 2.0 & OC < 0.9 ~ "lipid",
                                      HC < 2.0 & HC >= 1.5 & N==0 ~ "aliphatic",
                                      HC < 2.0 & HC >= 1.5 & N > 0 ~ "aliphatic+N")) %>% 
    dplyr::select(Mass, Class, Class_detailed)
}

## for data file
compute_presence = function(dat){
  dat %>% 
    pivot_longer(-("Mass"), values_to = "presence", names_to = "analysis_ID") %>% 
    # convert intensities to presence==1/absence==0  
    dplyr::mutate(presence = if_else(presence>0,1,0)) %>% 
    # keep only peaks present
    filter(presence>0)
}
apply_replication_filter = function(data_long_key, ...){
  max_replicates = 
    data_long_key %>% 
    ungroup() %>% 
    group_by(...) %>% 
    distinct(sample_label) %>% 
    dplyr::summarise(reps = n())

  # second, join the `reps` file to the long_key file
  # and then use the replication filter  
  data_long_key %>% 
    group_by(formula, ...) %>% 
    dplyr::mutate(n = n()) %>% 
    left_join(max_replicates) %>% 
    ungroup() %>% 
    mutate(keep = n >= (2/3)*reps) %>% 
    filter(keep) %>% 
    dplyr::select(-keep, -reps)
  
}
  
  
## LEVEL II FUNCTIONS ------------------------------------------------------

make_fticr_meta = function(icr_report){
  # filter peaks
  fticr_report = (apply_filter_report(icr_report))
  
  meta_clean = 
    fticr_report %>% 
    # select only the relevant columns for the formula assignments
    dplyr::select(Mass, C, H, O, N, S, P, El_comp)
  
  # calculate indices and assign classes - Seidel 2014 and Seidel 2017
  meta_indices = compute_indices(meta_clean)
  meta_formula = compute_mol_formula(meta_clean)
  meta_class = assign_class_seidel(meta_clean, meta_indices)
  
  # output
  meta2 = meta_formula %>% 
    left_join(meta_class, by = "Mass") %>% 
    left_join(meta_indices, by = "Mass") %>% dplyr::select(-Mass) %>% distinct(.)
  
  list(meta2 = meta2,
       meta_formula = meta_formula)
}
make_fticr_data = function(icr_report, analysis_key, sample_key){
  
  # a. convert intensities to compute presence/absence
  # b. pull samples vs. blanks vs. instrument blanks
  # c. do a blank correction 
  # d. apply replication filter (keep only peaks seen in > 2/3 of all replicates)
  # e. outputs: 
  # -- (1) peaks per sample -- use for relative abundance and stats, 
  # -- (2) peaks per treatment -- use for Van Krevelen plots and unique peak analysis
  

  fticr_report = (apply_filter_report(icr_report))
  mass_to_formula = make_fticr_meta(icr_report)$meta_formula
  
  data_columns = fticr_report %>% dplyr::select(Mass, starts_with("DOC"))
  
  data_presence = 
    compute_presence(data_columns) %>% 
    left_join(mass_to_formula, by = "Mass") %>% 
    dplyr::select(formula, analysis_ID, presence) 
  
  # analysis_ID values are not consistent (we need DOC_CMPS_KFP_####)
  # so we clean
  clean_analysis_ID = function(data_presence, analysis_key){
    
    analysis_ID_cleaned = 
      data_presence %>% 
      distinct(analysis_ID) %>% 
      mutate(sample_type = case_when(grepl("DOC_[0-9]|DOC_CMPS_[0-9]", analysis_ID) ~ "sample",
                                     TRUE ~ "instrument blanks"))
    
    samples = 
      analysis_ID_cleaned %>% 
      filter(sample_type == "sample") %>% 
      mutate(number = parse_number(analysis_ID),
             number = str_pad(number, 4, "0", side = "left"),
             analysis_ID_clean = paste0("DOC_CMPS_KFP_", number)) %>% 
      dplyr::select(analysis_ID, analysis_ID_clean, sample_type)
    
    nonsamples = 
      analysis_ID_cleaned %>% 
      filter(!sample_type %in% "sample") %>% 
      mutate(analysis_ID_clean = analysis_ID)
    
   data_presence %>% 
      left_join(bind_rows(samples, nonsamples)) %>% 
      dplyr::select(-analysis_ID) %>% 
      rename(analysis_ID = analysis_ID_clean)
  }
  
  data_presence_samples = 
    data_presence %>% clean_analysis_ID(.)
  
  # get the analyis/sample key and apply to the data
  analysis_key_wsoc = 
    analysis_key %>% 
    filter(analysis_type == "WSOC") %>% 
    dplyr::select(analysis_ID, sample_label)
  
  data_long_key = data_presence_samples %>% left_join(analysis_key_wsoc) %>% left_join(sample_key)
  
  # apply replication filter (to the samples only)
  data_long_key_repfiltered = 
    data_long_key %>% 
    filter(!is.na(region)) %>% 
    apply_replication_filter(., region, site, transect, horizon)
  
  # blanks - filter and solution
  blanks = 
    data_long_key %>% 
    filter(grepl("blank", sample_label))
  
  # instrument blanks - raw blanks, etc.
  instrument_blanks = 
    data_long_key %>% 
    filter(grepl("blank", sample_type))
  
  # do a blank correction
  blank_peaks = 
    blanks %>% 
    dplyr::distinct(formula) %>% 
    mutate(blank = TRUE)
  
  data_long_blank_corrected = 
    data_long_key_repfiltered %>% 
    left_join(blank_peaks) %>% 
    filter(is.na(blank)) %>% 
    dplyr::select(-blank)
  
  # get summary of peaks by treatment
  data_long_trt = 
    data_long_blank_corrected %>% 
    distinct(formula, region, site, transect, horizon)
  
  list(data_long_trt = data_long_trt,
       data_long_blank_corrected = data_long_blank_corrected)
  
}

#
# 2.  ANALYSIS FUNCTIONS --------------------------------------------------

## Van Krevelen -----------------------------------------------------------

gg_vankrev <- function(data,mapping){
  ggplot(data,mapping) +
    # plot points
    geom_point(size=0.5, alpha = 0.5) + # set size and transparency
    # axis labels
    ylab("H/C") +
    xlab("O/C") +
    # axis limits
    xlim(0,1.25) +
    ylim(0,2.5) +
    # add boundary lines for Van Krevelen regions
    geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
    geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
    geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
    guides(colour = guide_legend(override.aes = list(alpha=1, size = 1)))
}

plot_vankrevelen = function(icr_data_trt, icr_meta){
  
  vk_domains = 
    icr_meta %>% 
    dplyr::select(formula, HC, OC, Class_detailed, Class) %>% 
    gg_vankrev(aes(x = OC, y = HC, color = Class))
  
  
  data_hcoc = 
    icr_data_trt %>% 
    filter(horizon != "B") %>% 
    left_join(icr_meta %>% dplyr::select(formula, HC, OC)) %>% 
    mutate(transect = recode(transect, "wc" = "wetland")) %>% 
    reorder_horizon() %>% reorder_transect()
  
  vk_wle = 
    data_hcoc %>% 
    filter(region == "WLE") %>% 
    gg_vankrev(aes(x = OC, y = HC, color = transect))+
    stat_ellipse(level = 0.90, show.legend = F)+
    scale_color_manual(values = pal_transect)+
    labs(color = "",
         title = "FTICR: all peaks (blank corrected)",
         subtitle = "WLE sites")+
    facet_wrap(~site+horizon)+
    NULL
  
  vk_cb = 
    data_hcoc %>% 
    filter(region == "CB" ) %>% 
    gg_vankrev(aes(x = OC, y = HC, color = transect))+
    stat_ellipse(level = 0.90, show.legend = F)+
    scale_color_manual(breaks = c("upland", "transition", "wte", "wetland"),
                       values = pal_transect)+
    labs(color = "",
         title = "FTICR: all peaks (blank corrected)",
         subtitle = "CB sites")+
    facet_wrap(~site+horizon)+
    NULL
  
  
  list(vk_domains = vk_domains,
       vk_wle = vk_wle,
       vk_cb = vk_cb)
}

plot_vankrevelen_unique = function(icr_data_trt, icr_meta){
  
  unique_hcoc = 
    icr_data_trt %>% 
    filter(horizon != "B") %>% 
    filter(!(site == "MSM" & horizon == "A")) %>% 
    group_by(formula, region, site, horizon) %>% 
    dplyr::mutate(n = n()) %>% 
    filter(n == 1) %>% 
    left_join(icr_meta %>% dplyr::select(formula, HC, OC)) %>% 
    mutate(transect = recode(transect, "wc" = "wetland")) %>% 
    reorder_horizon() %>% reorder_transect()
  
  
  vk_unique_wle = 
    unique_hcoc %>% 
    filter(region == "WLE") %>% 
    gg_vankrev(aes(x = OC, y = HC, color = transect))+
    stat_ellipse(level = 0.90, show.legend = F)+
    scale_color_manual(breaks = c("upland", "transition", "wte", "wetland"),
                       values = pal_transect)+
    labs(color = "")+
    facet_grid(site ~ horizon)+
    labs(title = "FTICR Unique Peaks",
         subtitle = "WLE sites")
  
  
  vk_unique_cb = 
    unique_hcoc %>% 
    filter(region == "CB") %>% 
    gg_vankrev(aes(x = OC, y = HC, color = transect))+
    stat_ellipse(level = 0.90, show.legend = F)+
    scale_color_manual(breaks = c("upland", "transition", "wte", "wetland"),
                       values = pal_transect)+
    labs(color = "")+
    facet_grid(site ~ horizon)+
    labs(title = "FTICR Unique Peaks",
         subtitle = "CB sites")
  
  
  list(vk_unique_wle = vk_unique_wle,
       vk_unique_cb = vk_unique_cb)
}


#
## Relative Abundance -----------------------------------------------------


compute_icr_relabund = function(icr_data_long, icr_meta){
  
  icr_data_long %>% 
    filter(horizon != "B") %>% 
    # add the Class column to the data
    left_join(dplyr::select(icr_meta, formula, Class), by = "formula") %>% 
    # calculate abundance of each Class as the sum of all counts
    group_by(sample_label, Class) %>%
    dplyr::summarise(abund = sum(presence)) %>%
    ungroup %>% 
    # create a new column for total counts per core assignment
    # and then calculate relative abundance  
    group_by(sample_label) %>% 
    dplyr::mutate(total = sum(abund),
                  relabund  = ((abund/total)*100))
}
## #icr_relabund_samples = compute_icr_relabund(icr_data_long, icr_meta)


## fticr_relabund_per_sample %>%
##   left_join(sample_key) %>% 
##   ggplot(aes(x = sample_label, y = relabund, fill = Class))+
##   geom_bar(stat = "identity")+
##   facet_wrap(~region, scales = "free_x")

## fticr_relabund_per_sample %>%
##   left_join(sample_key) %>% 
##   filter(region == "CB" & horizon != "B") %>% 
##   ggplot(aes(x = sample_label, y = relabund, fill = Class))+
##   geom_bar(stat = "identity")+
##   facet_wrap(~site + transect, scales = "free_x")

## fticr_relabund_per_sample %>%
##   left_join(sample_key) %>% 
##   filter(region == "WLE" & horizon != "B") %>% 
##   ggplot(aes(x = sample_label, y = relabund, fill = Class))+
##   geom_bar(stat = "identity")+
##   facet_wrap(~site + transect, scales = "free_x")


## y = fticr_relabund_per_sample %>% 
##   group_by(sample_label) %>% 
##   dplyr::summarise(total2 = sum(relabund))## 



## Statistics -------------------------------------------------------------

# pca functions -----------------------------------------------------------
library(ggbiplot)
library(vegan)
library(patchwork)


fit_pca_function = function(icr_relabund_samples, sample_key){
  relabund_pca =
    icr_relabund_samples %>% 
    left_join(sample_key) %>% 
    drop_na() %>% 
    ungroup %>% 
    dplyr::select(-c(abund, total)) %>% 
    spread(Class, relabund) %>% 
    filter(!is.na(region)) %>% 
    replace(.,is.na(.),0)
  
  num = 
    relabund_pca %>% 
    dplyr::select(where(is.numeric))
  
  grp = 
    relabund_pca %>% 
    dplyr::select(where(is.character)) %>% 
    dplyr::mutate(row = row_number()) %>% 
    reorder_horizon()
  
  pca_int = prcomp(num, scale. = T)
  
  list(num = num,
       grp = grp,
       pca_int = pca_int)
}
compute_icr_pca = function(icr_relabund_samples, sample_key){
  
  sample_key = 
    sample_key %>% 
    mutate(transect = recode(transect, "wc" = "wetland"))
  
  pca_overall = fit_pca_function(icr_relabund_samples, sample_key)
  pca_wle = fit_pca_function(icr_relabund_samples, sample_key %>% filter(region == "WLE"))
  pca_cb = fit_pca_function(icr_relabund_samples, sample_key %>% filter(region == "CB"))
  
  
  # PCA biplots
  biplot_all = 
    ggbiplot(pca_overall$pca_int, obs.scale = 1, var.scale = 1,
             groups = pca_overall$grp$region, 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 1,
               aes(shape = pca_overall$grp$transect,
                   color = groups))+
    scale_shape_manual(breaks = c("upland", "transition", "wte", "wetland"),
                       values = c(1,2,3,4))+
    xlim(-4,4)+
    ylim(-3.5,3.5)+
    labs(color = "", shape = "")+
    theme(legend.position = "top", legend.box = "vertical")+
    NULL
  
  biplot_wle = 
    ggbiplot(pca_wle$pca_int, obs.scale = 1, var.scale = 1,
             groups = pca_wle$grp$transect, 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 1,
               aes(shape = pca_wle$grp$site,
                   color = groups))+
    scale_color_manual(breaks = c("upland", "transition", "wte", "wetland"), 
                       values = pal_transect)+
    labs(title = "FTICR: WLE",
         color = "", shape = "")+
    xlim(-4,4)+
    ylim(-3.5,3.5)+
    theme(legend.position = "top", legend.box = "vertical")+
    NULL
  
  
  biplot_cb = 
    ggbiplot(pca_cb$pca_int, obs.scale = 1, var.scale = 1,
             groups = pca_cb$grp$transect, 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 1,
               aes(shape = pca_cb$grp$site,
                   color = groups))+
    scale_color_manual(breaks = c("upland", "transition", "wte", "wetland"), 
                       values = pal_transect)+
    labs(title = "FTICR: Chesapeake",
         color = "", shape = "")+
    xlim(-4,4)+
    ylim(-3.5,3.5)+
    theme(legend.position = "top", legend.box = "vertical")+
    NULL
  
  library(patchwork)
  biplot_regions = 
    biplot_wle + biplot_cb 
  
  list(biplot_all = biplot_all,
       biplot_regions = biplot_regions)
}

#
# permanova -----------------------------------------------------------

compute_permanova = function(icr_relabund_samples){
  relabund_wide = 
    icr_relabund_samples %>% 
    left_join(sample_key) %>% 
    filter(horizon != "B") %>% 
    ungroup %>% 
    dplyr::select(-c(abund, total)) %>% 
    spread(Class, relabund) %>% 
    filter(!is.na(region)) %>% 
    replace(.,is.na(.),0)
  
  permanova_fticr_all = 
    adonis(relabund_wide %>% dplyr::select(where(is.numeric)) ~ (region + transect + site + horizon)^2, 
           data = relabund_wide)
  broom::tidy(permanova_fticr_all$aov.tab)
}



