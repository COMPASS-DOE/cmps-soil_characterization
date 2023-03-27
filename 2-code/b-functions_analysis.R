
pal_transect = rev(soilpalettes::soil_palette("redox2", 4))
theme_set(theme_kp())


OLD_plots = function(){
  
  plot_moisture = function(moisture_processed, sample_key){
    
    moisture =
      moisture_processed %>% 
      left_join(sample_key) %>% 
      reorder_horizon() %>% reorder_transect()
    
    moisture %>% 
      ggplot(aes(x = site, y = gwc_perc, color = transect, shape = horizon))+
      geom_point(position = position_dodge(width = 0.4),
                 size = 2)+
      facet_wrap(region ~ ., scales = "free_x")+
      labs(y = "Gravimetric water, %")+
      scale_color_manual(values = pal_transect)
    
  }
  
  plot_loi = function(loi_processed, sample_key){
    
    loi = 
      loi_processed %>% 
      left_join(sample_key) %>% 
      reorder_horizon() %>% reorder_transect()  
    
    loi %>% 
      ggplot(aes(x = site, y = percent_om, color = transect, shape = horizon))+
      geom_point(position = position_dodge(width = 0.4),
                 size = 2)+
      facet_wrap(region ~ ., scales = "free_x")+
      labs(y = "Organic Matter, %")+
      scale_color_manual(values = pal_transect)
    
  }
  
  plot_pH = function(pH_processed, sample_key){
    
    pH =
      pH_processed %>% 
      left_join(sample_key) %>% 
      reorder_horizon() %>% reorder_transect()
    
    pH %>% 
      ggplot(aes(x = site, y = pH, color = transect, shape = horizon))+
      geom_point(position = position_dodge(width = 0.4),
                 size = 2)+
      facet_wrap(region ~ ., scales = "free_x")+
      labs(y = "pH")+
      scale_color_manual(values = pal_transect)
    
  }
  
  plot_sp_cond = function(pH_processed, sample_key){
    
    pH =
      pH_processed %>% 
      left_join(sample_key) %>% 
      reorder_horizon() %>% reorder_transect()
    
    pH %>% 
      ggplot(aes(x = site, y = specific_conductance_ms_cm, color = transect, shape = horizon))+
      geom_point(position = position_dodge(width = 0.4),
                 size = 2)+
      facet_wrap(region ~ ., scales = "free_x")+
      labs(y = "Specific conductance, mS/cm")+
      scale_color_manual(values = pal_transect)
    
  }
  
  plot_tctnts = function(tctnts_data_samples, sample_key){
    
    tctnts =
      tctnts_data_samples %>% 
      left_join(sample_key) %>% 
      reorder_horizon() %>% reorder_transect()
    
    gg_tc = 
      tctnts %>% 
      ggplot(aes(x = site, y = TC_perc, color = transect, shape = horizon))+
      geom_point(position = position_dodge(width = 0.4),
                 size = 2)+
      facet_wrap(region ~ ., scales = "free_x")+
      labs(y = "Total C, %")+
      scale_color_manual(values = pal_transect)
    
    gg_tn = 
      tctnts %>% 
      ggplot(aes(x = site, y = TN_perc, color = transect, shape = horizon))+
      geom_point(position = position_dodge(width = 0.4),
                 size = 2)+
      facet_wrap(region ~ ., scales = "free_x")+
      labs(y = "Total N, %")+
      scale_color_manual(values = pal_transect)
    
    gg_ts = 
      tctnts %>% 
      ggplot(aes(x = site, y = TS_perc, color = transect, shape = horizon))+
      geom_point(position = position_dodge(width = 0.4),
                 size = 2)+
      facet_wrap(region ~ ., scales = "free_x")+
      labs(y = "Total S, %")+
      scale_color_manual(values = pal_transect)
    
    list(gg_tc = gg_tc,
         gg_tn = gg_tn,
         gg_ts = gg_ts)
  }
  
  plot_weoc = function(weoc_processed, sample_key){
    
    weoc =
      weoc_processed %>% 
      left_join(sample_key) %>% 
      filter(!is.na(site)) %>% 
      reorder_horizon() %>% reorder_transect()
    
    weoc %>% 
      ggplot(aes(x = site, y = npoc_ug_g, color = transect, shape = horizon))+
      geom_point(position = position_dodge(width = 0.4),
                 size = 2)+
      facet_wrap(region ~ ., scales = "free_x")+
      labs(y = "Water extractable organic C, μg/g")+
      scale_color_manual(values = pal_transect)
    
  }
  
  plot_din = function(din_processed, sample_key){
    
    din =
      din_processed %>% 
      left_join(sample_key) %>% 
      reorder_horizon() %>% reorder_transect()
    
    gg_no3 = 
      din %>% 
      ggplot(aes(x = site, y = no3n_ug_g, color = transect, shape = horizon))+
      geom_point(position = position_dodge(width = 0.4),
                 size = 2)+
      facet_wrap(region ~ ., scales = "free_x")+
      labs(y = "Extractable nitrate-N, μg/g",
           caption = "NO3 was below-detect for most CB soils")+
      scale_color_manual(values = pal_transect)
    
    gg_nh4 = 
      din %>% 
      ggplot(aes(x = site, y = nh4n_ug_g, color = transect, shape = horizon))+
      geom_point(position = position_dodge(width = 0.4),
                 size = 2)+
      facet_wrap(region ~ ., scales = "free_x")+
      labs(y = "Extractable ammonium-N, μg/g")+
      scale_color_manual(values = pal_transect)
    
    list(gg_no3 = gg_no3,
         gg_nh4 = gg_nh4)
    
  }
  
  plot_icp = function(icp_processed, sample_key){
    
    icp = 
      icp_processed %>% 
      dplyr::select(sample_label, ends_with("_ug_g")) %>% 
      pivot_longer(-sample_label, names_to = "species", values_to = "ug_g") %>% 
      left_join(sample_key) %>% 
      filter(!is.na(site)) %>% 
      reorder_transect() %>% 
      reorder_horizon()
    
    #  icp_long_outliers = 
    #    icp_long %>% 
    #    group_by(region, site, transect, horizon, species) %>% 
    #    mutate(sd = sd(ppm),
    #           mean = mean(ppm),
    #           outlier = ppm > mean + (3*sd) | ppm < mean - (3*sd),
    #           outlier2 = case_when(outlier == TRUE ~ 1,
    #                                outlier == FALSE ~ 0))
    
    icp_labels = 
      icp %>% 
      mutate(group = case_when(grepl("Ca|Mg|Na|K", species) ~ "1-base cations",
                               grepl("Al|Fe|Mn", species) ~ "2-other cations",
                               grepl("S", species) ~ "3-others")) %>% 
      filter(!is.na(group))
    
    gg_base_cations = 
      icp_labels %>% 
      filter(grepl("base cations", group)) %>% 
      ggplot(aes(x = site, y = ug_g, color = transect, shape = horizon)) +
      geom_point(position = position_dodge(width = 0.3))+
      facet_grid(species ~ region, scales = "free")+
      scale_color_manual(values = pal_transect)
    
    gg_other_cations = 
      icp_labels %>% 
      filter(grepl("other cations", group)) %>% 
      ggplot(aes(x = site, y = ug_g, color = transect, shape = horizon)) +
      geom_point(position = position_dodge(width = 0.3))+
      facet_grid(species ~ region, scales = "free")+
      scale_color_manual(values = pal_transect)
    
    gg_others = 
      icp_labels %>% 
      filter(grepl("others", group)) %>% 
      ggplot(aes(x = site, y = ug_g, color = transect, shape = horizon)) +
      geom_point(position = position_dodge(width = 0.3))+
      facet_grid(species ~ region, scales = "free")+
      scale_color_manual(values = pal_transect)
    
    list(gg_base_cations = gg_base_cations,
         gg_other_cations = gg_other_cations,
         gg_others = gg_others)
  }
  
  plot_ferrozine = function(ferrozine_processed, sample_key){
    
    ferrozine = 
      ferrozine_processed %>% 
      mutate(Fe_2_3 = Fe2_ppm/Fe3_ppm) %>% 
      # dplyr::select(sample_label, ends_with("_ug_g")) %>% 
      #  pivot_longer(-sample_label, names_to = "species", values_to = "ug_g") %>% 
      left_join(sample_key) %>% 
      filter(!is.na(site)) %>% 
      reorder_transect() %>% 
      reorder_horizon()
    
    gg_fe_2_3 = 
      ferrozine %>% 
      filter(Fe_2_3 > 0) %>% 
      ggplot(aes(x = site, y = Fe_2_3, color = transect, shape = horizon)) +
      geom_point(position = position_dodge(width = 0.3))+
      facet_grid(. ~ region, scales = "free")+
      scale_color_manual(values = pal_transect)+
      labs(title = "Fe-II/III",
           subtitle = "ferrozine method")
    
    gg_fe_total = 
      ferrozine %>% 
      #filter(Fe_2_3 > 0) %>% 
      ggplot(aes(x = site, y = Fe_total_ug_g, color = transect, shape = horizon)) +
      geom_point(position = position_dodge(width = 0.3))+
      facet_grid(. ~ region, scales = "free")+
      scale_color_manual(values = pal_transect)+
      labs(title = "total Fe",
           subtitle = "ferrozine method")
    
    list(gg_fe_2_3 = gg_fe_2_3,
         gg_fe_total = gg_fe_total)
    
  }
  
  plot_mehlich = function(mehlich_processed, sample_key){
    
    mehlich = 
      mehlich_processed %>% 
      left_join(sample_key) %>% 
      filter(!is.na(site)) %>% 
      reorder_transect() %>% 
      reorder_horizon()
    
    #gg_mehlich = 
    mehlich %>% 
      ggplot(aes(x = site, y = mehlichp_ugg, color = transect, shape = horizon)) +
      geom_point(position = position_dodge(width = 0.3))+
      facet_grid(. ~ region, scales = "free")+
      scale_color_manual(values = pal_transect)+
      labs(title = "Mehlich-3 extractable P")
    
  }
  
  plot_ions = function(ions_processed, sample_key){
    
    ions = 
      ions_processed %>% 
      dplyr::select(sample_label, ends_with("_ug_g")) %>% 
      pivot_longer(-sample_label, names_to = "species", values_to = "ug_g") %>% 
      left_join(sample_key) %>% 
      filter(!is.na(site)) %>% 
      reorder_transect() %>% 
      reorder_horizon()
    
    #  icp_long_outliers = 
    #    icp_long %>% 
    #    group_by(region, site, transect, horizon, species) %>% 
    #    mutate(sd = sd(ppm),
    #           mean = mean(ppm),
    #           outlier = ppm > mean + (3*sd) | ppm < mean - (3*sd),
    #           outlier2 = case_when(outlier == TRUE ~ 1,
    #                                outlier == FALSE ~ 0))
    
    
    
    ions_subset = 
      ions %>% 
      mutate(species = str_remove(species, "_ug_g")) %>% 
      filter(species %in% c("Sodium", "Potassium", "Magnesium",
                            "Calcium", "Chloride", "Sulfate", "Nitrate", "Phosphate"))
    
    ions_subset %>% 
      ggplot(aes(x = site, y = ug_g, color = transect, shape = horizon)) +
      geom_point(position = position_dodge(width = 0.3))+
      facet_wrap(species ~ region, scales = "free")+
      scale_color_manual(values = pal_transect)
    
    
    
  }
  
  
  
  
}


compute_analysis_matrix = function(data_combined){

  #data_combined2 = 
  data_combined %>% 
    distinct(sample_label, analysis, region, site, transect, horizon) %>% 
    filter(!is.na(analysis)) %>% 
    filter(!is.na(region)) %>% 
    group_by(region, site, transect, horizon, analysis) %>% 
    dplyr::summarise(n = n()) %>% 
    pivot_wider(values_from = "n", names_from = "analysis") %>% 
    reorder_transect() %>% reorder_horizon() %>% 
    arrange(desc(region), site, transect, horizon)
  
}



compute_overall_pca = function(data_combined_wide, sample_key){
  library(ggbiplot)
  
  fit_pca_function = function(dat){
    
    dat %>% 
      drop_na()
    
    num = 
      dat %>%       
      dplyr::select(where(is.numeric)) %>%
      dplyr::mutate(row = row_number()) %>% 
      drop_na()
    
    num_row_numbers = num %>% dplyr::select(row)
    
    grp = 
      dat %>% 
      dplyr::select(where(is.character)) %>% 
      dplyr::mutate(row = row_number()) %>% 
      right_join(num_row_numbers)
    
    
    num = num %>% dplyr::select(-row)
    pca_int = prcomp(num, scale. = T)
    
    list(num = num,
         grp = grp,
         pca_int = pca_int)
  }
  
  combined_surface = 
    data_combined_wide %>% 
    filter(horizon != "B") %>% 
    dplyr::select(-"Bromide (IC)") %>% 
    filter(sample_label != "COMPASS_Dec2021_016") %>% 
    dplyr::select(-ends_with("(IC)"), -"S (ICP)", -"Fe (ICP)", -"P (ICP)") %>% 
    force() 
  
  combined_surface_no_ferrozine = 
    combined_surface %>% 
    dplyr::select(-ends_with("(Ferrozine)"))
  
  ## PCA input files ----
  pca_overall = fit_pca_function(combined_surface_no_ferrozine)
  pca_overall_wle = fit_pca_function(combined_surface_no_ferrozine %>% filter(region == "WLE") %>% dplyr::select(-ends_with("(IC)")))
  pca_overall_cb = fit_pca_function(combined_surface %>% filter(region == "CB") %>% dplyr::select(-ends_with("(IC)")))
  
  
  ## PCA plots overall ----
  gg_pca_overall = 
    ggbiplot(pca_overall$pca_int, obs.scale = 1, var.scale = 1,
             groups = as.character(pca_overall$grp$region), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 1,
               aes(#shape = pca_overall$grp$transect,
                 color = groups))+ 
    #scale_shape_manual(values = c(21, 19))+
    #scale_shape_manual(values = c(21, 21, 19), name = "", guide = "none")+
   # xlim(-4,20)+
   # ylim(-8,8)+
    labs(shape="",
         title = "Overall PCA, both regions",
         subtitle = "Surface horizons only")+
    theme_kp()+
    NULL
  
  
  gg_pca_wle = 
    ggbiplot(pca_overall_wle$pca_int, obs.scale = 1, var.scale = 1,
             groups = as.character(pca_overall_wle$grp$transect), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 1,
               aes(shape = pca_overall_wle$grp$site,
                   color = groups))+ 
    scale_color_manual(breaks = c("upland", "transition", "wte", "wc"), 
                       values = pal_transect)+
    #scale_shape_manual(values = c(21, 19))+
    #scale_shape_manual(values = c(21, 21, 19), name = "", guide = "none")+
    #xlim(-4,4)+
    #ylim(-3.5,3.5)+
    labs(shape="",
         title = "PCA: WLE",
         subtitle = "surface horizons")+
    theme_kp()+
    theme(legend.position = "top", legend.box = "vertical")+
    NULL
  
  gg_pca_cb = 
    ggbiplot(pca_overall_cb$pca_int, obs.scale = 1, var.scale = 1,
             groups = as.character(pca_overall_cb$grp$transect), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 1,
               aes(shape = pca_overall_cb$grp$site,
                   color = groups))+ 
    scale_color_manual(breaks = c("upland", "transition", "wte", "wc"), 
                       values = pal_transect)+
    #scale_shape_manual(values = c(21, 19))+
    #scale_shape_manual(values = c(21, 21, 19), name = "", guide = "none")+
    #xlim(-4,4)+
    #ylim(-3.5,3.5)+
    labs(shape="",
         title = "PCA: CB",
         subtitle = "surface horizons")+
    theme_kp()+
    theme(legend.position = "top", legend.box = "vertical")+
    NULL
  
  library(patchwork)
  gg_pca_regions = gg_pca_wle + gg_pca_cb
  
  list(gg_pca_overall = gg_pca_overall,
       gg_pca_regions = gg_pca_regions)
  
}

compute_correlations = function(data_combined_wide, TITLE){
  #library(corrplot)
  
  fit_correlations_function = function(dat, TITLE){
    num = 
      dat %>%       
      dplyr::select(where(is.numeric)) %>%
      drop_na()
    
    num_clean = 
      num %>% 
      rownames_to_column("row") %>% 
      pivot_longer(-row) %>% 
      separate(name, sep = "_", into = c("name")) %>% 
      pivot_wider() %>% 
      dplyr::select(-row)
    
    
    m = cor(num_clean)
    p.mat <- ggcorrplot::cor_pmat(num_clean)
    
 #   x = corrplot::corrplot(m, #method="color", 
 #                      type = "lower",
 #                      title = TITLE,
 #                      mar = c(0,0,1,0) 
 #                      #, order = "hclust"
 #                      )
                       
    ggcorrplot::ggcorrplot(m, type = "lower",
                           p.mat = p.mat,
                           outline.color = "black",
                           #   lab = TRUE, 
                           insig = "blank",
                           colors = c("#E46726", "white", "#6D9EC1"),
                           title = TITLE)

  }

  combined_surface = 
    data_combined_wide %>% 
    filter(horizon != "B") %>% 
    force() 
  
  combined_surface_no_ferrozine = 
    combined_surface %>% 
    dplyr::select(-ends_with("(Ferrozine)"))
  
  
  
  corr_all = fit_correlations_function(dat = combined_surface_no_ferrozine%>% dplyr::select(-"Bromide (IC)"), TITLE = "all")
  corr_wle = fit_correlations_function(dat = combined_surface_no_ferrozine %>% filter(region == "WLE")%>% dplyr::select(-"Bromide (IC)"), TITLE = "WLE" )
  corr_cb = fit_correlations_function(dat = combined_surface %>% filter(region == "CB"), TITLE = "CB")
  
  list(corr_all = corr_all,
       corr_regions = corr_wle + corr_cb)

}



########################
# plots for each analyte
## there are different functions for different types of graphs
## the function names describe the variable on the x-axis


plot_transect_as_x = function(data, YLAB, TITLE = "", SUBTITLE = "", SCALES = "free_x"){
  
  ggplot(data,
         aes(x = transect,
             y = value,
             group= site,
             color = horizon,
             shape = site,
             #alpha = horizon
         )) +
    # plot points
    geom_point(size = 2, stroke = 1,
               position = position_dodge(width = 0.4))+
    scale_alpha_manual(values = c(1, 0.3))+
    scale_color_manual(values = c("grey60", "black"))+
    scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,3,4,5,6))+
    facet_wrap(~region, scales = SCALES,
               labeller = as_labeller(c("CB" = "Chesapeake Bay", "WLE" = "Lake Erie")))+
    theme_kp()+
    labs(x = "",
         y = YLAB,
         title = TITLE,
         subtitle = SUBTITLE,
         shape = "Site",
         alpha = "Horizon",
         color = "Horizon")+
    theme(legend.position = "right", legend.box = "vertical")+
    NULL
  
}
plot_region_as_x = function(data, YLAB){
  
  ggplot(data,
         aes(x = region,
             y = value,
             group= transect,
             color = transect,
             shape = site,
             #alpha = horizon
         )) +
    # plot points
    geom_point(size = 2, stroke = 1,
               position = position_dodge(width = 0.4))+
    scale_alpha_manual(values = c(1, 0.3))+
    scale_color_manual(values  = pal_transect)+
    scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,3,4,5,6))+
    facet_wrap(~horizon, ncol = 2, scales = "free_x")+
    theme_kp()+
    labs(x = "",
         y = YLAB,
         shape = "Site",
         alpha = "Horizon",
         color = "Horizon")+
    theme(legend.position = "right", legend.box = "vertical")+
    NULL
  
}
plot_site_as_x = function(data, YLAB, TITLE = "", SUBTITLE = ""){
  
  data %>% 
    ggplot(aes(x = site, 
               y = value, 
               color = transect, shape = horizon))+
    geom_point(position = position_dodge(width = 0.4),
               size = 2, stroke = 1)+
    facet_wrap(region ~ ., scales = "free_x")+
    theme_kp()+
    labs(y = YLAB,
         x = "",
         title = TITLE,
         subtitle = SUBTITLE)+
    scale_color_manual(values = pal_transect)+
    theme(legend.position = "right", legend.box = "vertical")+
    NULL
  
}

make_graphs_by_transect = function(data_combined){
  
  combined_surface = 
    data_combined %>% 
    filter(horizon != "B") %>% 
    force() 
  
  gwc <- combined_surface %>% filter(analysis == "GWC")
  gg_gwc <- plot_transect_as_x(data = gwc, YLAB = "Gravimetric water, %", TITLE = "Gravimetric Water Content")
  
  pH <- combined_surface %>% filter(analysis == "PH")
  gg_ph <- plot_transect_as_x(data = pH %>% filter(name == "pH"), YLAB = "pH", TITLE = "pH")
  gg_sp_conduc <- plot_transect_as_x(data = pH %>% filter(grepl("specific_conductance", name)), 
                                     YLAB = "Specific Conductance μS/cm", TITLE = "Specific Conductance")
  
  tctnts <- combined_surface %>% filter(analysis == "TCTNTS")
  gg_tc <- plot_transect_as_x(data = tctnts %>% filter(name == "TC_perc"), YLAB = "Total C, %", TITLE = "Elemental Analysis")
  gg_tn <- plot_transect_as_x(data = tctnts %>% filter(name == "TN_perc"), YLAB = "Total N, %", TITLE = "Elemental Analysis")
  gg_ts <- plot_transect_as_x(data = tctnts %>% filter(name == "TS_perc"), YLAB = "Total S, %", TITLE = "Elemental Analysis")

  tctn <- tctnts %>% 
    filter(name == c("TC_perc", "TN_perc")) %>% 
    pivot_wider(names_from = "name", values_from = "value") %>% 
    mutate(value = TC_perc/TN_perc)
  gg_cn <- plot_transect_as_x(data = tctn, YLAB = "C:N", TITLE = "Elemental Analysis")
  
  loi <- combined_surface %>% filter(analysis == "LOI")
  gg_loi <- plot_transect_as_x(data = loi, YLAB = "% OM", TITLE = "Loss on ignition")
  
  weoc <- combined_surface %>% filter(analysis == "NPOC")
  gg_weoc <- plot_transect_as_x(data = weoc %>% filter(name == "npoc_ug_g"), YLAB = "WEOC, μg/g", TITLE = "Water Extractable Organic Carbon")
  
  din <- combined_surface %>% filter(analysis == "DIN")
  gg_din_nh4n <- plot_transect_as_x(data = din %>% filter(name == "nh4n_ug_g"), YLAB = "NH4-N, μg/g", TITLE = "Extractable Inorganic N", SUBTITLE = "extracted with 2M KCl")
  gg_din_no3n <- plot_transect_as_x(data = din %>% filter(name == "no3n_ug_g"), YLAB = "NO3-N, μg/g", TITLE = "Extractable Inorganic N", SUBTITLE = "extracted with 2M KCl")

  icp <- combined_surface %>% filter(analysis == "ICP")
  gg_icp_ca <- plot_transect_as_x(data = icp %>% filter(name == "Ca_meq100g"), YLAB = "Ca, μg/g", TITLE = "ICP-OES", SUBTITLE = "extracted with 1M NH4Cl")
  gg_icp_mg <- plot_transect_as_x(data = icp %>% filter(name == "Mg_meq100g"), YLAB = "Mg, μg/g", TITLE = "ICP-OES", SUBTITLE = "extracted with 1M NH4Cl")
  gg_icp_na <- plot_transect_as_x(data = icp %>% filter(name == "Na_meq100g"), YLAB = "Na, μg/g", TITLE = "ICP-OES", SUBTITLE = "extracted with 1M NH4Cl",
                                  SCALES = "free")
  gg_icp_k <- plot_transect_as_x(data = icp %>% filter(name == "K_meq100g"), YLAB = "K, μg/g", TITLE = "ICP-OES", SUBTITLE = "extracted with 1M NH4Cl")
  
  gg_icp_al <- plot_transect_as_x(data = icp %>% filter(name == "Al_meq100g"), YLAB = "Al, μg/g", TITLE = "ICP-OES", SUBTITLE = "extracted with 1M NH4Cl",
                                  SCALES = "free")
 # gg_icp_fe <- plot_transect_as_x(data = icp %>% filter(name == "Fe_ug_g"), YLAB = "Fe, μg/g", TITLE = "ICP-OES", SUBTITLE = "extracted with 1M NH4Cl")
#  gg_icp_mn <- plot_transect_as_x(data = icp %>% filter(name == "Mn_ug_g"), YLAB = "Mn, μg/g", TITLE = "ICP-OES", SUBTITLE = "extracted with 1M NH4Cl")
  
  gg_icp_cec <- plot_transect_as_x(data = icp %>% filter(name == "cec_meq100g"), YLAB = "CEC, meq/100g", TITLE = "ICP-OES", SUBTITLE = "extracted with 1M NH4Cl")
  
  gg_p_mehlich <- plot_transect_as_x(data = combined_surface %>% filter(name == "mehlichp_ugg"), YLAB = "P, μg/g", TITLE = "Mehlich-3 extractable P", 
                                     SUBTITLE = "measured colorimetrically (molybdate/ascorbic acid method)")
  
  ferrozine <- combined_surface %>% filter(analysis == "Ferrozine")
 # ferrozine_23 <- ferrozine %>% 
 #   filter(grepl("Fe2|Fe3", name)) %>% 
 #   pivot_wider() %>% 
 #   mutate(value = Fe2_ug_g/Fe3_ug_g)
  gg_ferr_fetotal <- plot_transect_as_x(data = ferrozine %>% filter(name == "Fe_ugg"), YLAB = "Fe, μg/g", 
                                        TITLE = "Total Fe", 
                                        SUBTITLE = "extracted with 0.5M HCl, measured colorimetrically using ferrozine")
 # gg_ferr_fe23 <- plot_transect_as_x(data = ferrozine_23, YLAB = "Fe-II/Fe-III", 
 #                                       TITLE = "Fe-II/III", 
 #                                       SUBTITLE = "extracted with 0.5M HCl, measured colorimetrically using ferrozine")+ylim (0, 5)
  
  ions <- combined_surface %>% filter(analysis == "IC")
  gg_ions_ca <- plot_transect_as_x(data = ions %>% filter(name == "Calcium_meq100g"), YLAB = "Ca, μg/g", TITLE = "IC", SUBTITLE = "extracted with MQ water")
  gg_ions_na <- plot_transect_as_x(data = ions %>% filter(name == "Sodium_meq100g"), YLAB = "Na, μg/g", TITLE = "IC", SUBTITLE = "extracted with MQ water",
                                   SCALES = "free")
  gg_ions_mg <- plot_transect_as_x(data = ions %>% filter(name == "Magnesium_meq100g"), YLAB = "Mg, μg/g", TITLE = "IC", SUBTITLE = "extracted with MQ water",
                                   SCALES = "free")
  gg_ions_k <- plot_transect_as_x(data = ions %>% filter(name == "Potassium_meq100g"), YLAB = "K, μg/g", TITLE = "IC", SUBTITLE = "extracted with MQ water")
  gg_ions_cl <- plot_transect_as_x(data = ions %>% filter(name == "Chloride_meq100g"), YLAB = "Cl, μg/g", TITLE = "IC", SUBTITLE = "extracted with MQ water",
                                   SCALES = "free")
  gg_ions_so4 <- plot_transect_as_x(data = ions %>% filter(name == "Sulfate_meq100g"), YLAB = "SO4, μg/g", TITLE = "IC", SUBTITLE = "extracted with MQ water",
                                    SCALES = "free")
  gg_ions_no3 <- plot_transect_as_x(data = ions %>% filter(name == "Nitrate_meq100g"), YLAB = "NO3, μg/g", TITLE = "IC", SUBTITLE = "extracted with MQ water")
  gg_ions_po4 <- plot_transect_as_x(data = ions %>% filter(name == "Phosphate_meq100g"), YLAB = "PO4, μg/g", TITLE = "IC", SUBTITLE = "extracted with MQ water")
  
  
  list(gg_gwc = gg_gwc,
       gg_ph = gg_ph,
       gg_sp_conduc = gg_sp_conduc,
       gg_tc =  gg_tc,
       gg_tn = gg_tn,
       gg_cn = gg_cn,
       gg_ts = gg_ts,
       gg_loi = gg_loi,
       gg_weoc = gg_weoc,
       gg_din_nh4n = gg_din_nh4n,
       gg_din_no3n = gg_din_no3n,
       gg_icp_ca = gg_icp_ca, 
       gg_icp_mg = gg_icp_mg, 
       gg_icp_na = gg_icp_na, 
       gg_icp_k = gg_icp_k , 
       gg_icp_al = gg_icp_al, 
       #gg_icp_fe = gg_icp_fe, 
       #gg_icp_mn = gg_icp_mn, 
       gg_icp_cec = gg_icp_cec,
       gg_p_mehlich = gg_p_mehlich,
       gg_ferr_fetotal = gg_ferr_fetotal, 
       #gg_ferr_fe23 = gg_ferr_fe23,
       gg_ions_ca = gg_ions_ca, 
       gg_ions_na = gg_ions_na, 
       gg_ions_mg = gg_ions_mg, 
       gg_ions_k = gg_ions_k,
       gg_ions_cl = gg_ions_cl, 
       gg_ions_so4 = gg_ions_so4,
       gg_ions_no3 = gg_ions_no3,
       gg_ions_po4 = gg_ions_po4
  )
}
make_graphs_by_site = function(data_combined){
  
  combined_surface = 
    data_combined 
  
  
 # combined_surface = 
 #   data_combined %>% 
 #   filter(horizon != "B") %>% 
 #   force() 
  
  gwc <- combined_surface %>% filter(analysis == "GWC")
  gg_gwc <- plot_site_as_x(data = gwc, YLAB = "Gravimetric water, %", TITLE = "Gravimetric Water Content")
  
  pH <- combined_surface %>% filter(analysis == "PH")
  gg_ph <- plot_site_as_x(data = pH %>% filter(name == "pH"), YLAB = "pH", TITLE = "pH")
  gg_sp_conduc <- plot_site_as_x(data = pH %>% filter(grepl("specific_conductance", name)), 
                                     YLAB = "Specific Conductance μS/cm", TITLE = "Specific Conductance")
  
  tctnts <- combined_surface %>% filter(analysis == "TCTNTS")
  gg_tc <- plot_site_as_x(data = tctnts %>% filter(name == "TC_perc"), YLAB = "Total C, %", TITLE = "Elemental Analysis")
  gg_tn <- plot_site_as_x(data = tctnts %>% filter(name == "TN_perc"), YLAB = "Total N, %", TITLE = "Elemental Analysis")
  gg_ts <- plot_site_as_x(data = tctnts %>% filter(name == "TS_perc"), YLAB = "Total S, %", TITLE = "Elemental Analysis")
  
  loi <- combined_surface %>% filter(analysis == "LOI")
  gg_loi <- plot_site_as_x(data = loi, YLAB = "% OM", TITLE = "Loss on ignition")
  
  weoc <- combined_surface %>% filter(analysis == "NPOC")
  gg_weoc <- plot_site_as_x(data = weoc %>% filter(name == "npoc_ug_g"), YLAB = "WEOC, μg/g", TITLE = "Water Extractable Organic Carbon")
  
  din <- combined_surface %>% filter(analysis == "DIN")
  gg_din_nh4n <- plot_site_as_x(data = din %>% filter(name == "nh4n_ug_g"), YLAB = "NH4-N, μg/g", TITLE = "Extractable Inorganic N", SUBTITLE = "extracted with 2M KCl")
  gg_din_no3n <- plot_site_as_x(data = din %>% filter(name == "no3n_ug_g"), YLAB = "NO3-N, μg/g", TITLE = "Extractable Inorganic N", SUBTITLE = "extracted with 2M KCl")
  
  icp <- combined_surface %>% filter(analysis == "ICP")
  gg_icp_ca <- plot_site_as_x(data = icp %>% filter(name == "Ca_meq100g"), YLAB = "Ca, μg/g", TITLE = "ICP-OES", SUBTITLE = "extracted with 1M NH4Cl")
  gg_icp_mg <- plot_site_as_x(data = icp %>% filter(name == "Mg_meq100g"), YLAB = "Mg, μg/g", TITLE = "ICP-OES", SUBTITLE = "extracted with 1M NH4Cl")
  gg_icp_na <- plot_site_as_x(data = icp %>% filter(name == "Na_meq100g"), YLAB = "Na, μg/g", TITLE = "ICP-OES", SUBTITLE = "extracted with 1M NH4Cl")
  gg_icp_k <- plot_site_as_x(data = icp %>% filter(name == "K_meq100g"), YLAB = "K, μg/g", TITLE = "ICP-OES", SUBTITLE = "extracted with 1M NH4Cl")
  
  gg_icp_al <- plot_site_as_x(data = icp %>% filter(name == "Al_meq100g"), YLAB = "Al, μg/g", TITLE = "ICP-OES", SUBTITLE = "extracted with 1M NH4Cl")
  #gg_icp_fe <- plot_site_as_x(data = icp %>% filter(name == "Fe_ug_g"), YLAB = "Fe, μg/g", TITLE = "ICP-OES", SUBTITLE = "extracted with 1M NH4Cl")
  #gg_icp_mn <- plot_site_as_x(data = icp %>% filter(name == "Mn_ug_g"), YLAB = "Fe, μg/g", TITLE = "ICP-OES", SUBTITLE = "extracted with 1M NH4Cl")
  gg_icp_cec <- plot_site_as_x(data = icp %>% filter(name == "cec_meq100g"), YLAB = "CEC, meq/100g", TITLE = "ICP-OES", SUBTITLE = "extracted with 1M NH4Cl")
  
  gg_p_mehlich <- plot_site_as_x(data = combined_surface %>% filter(name == "mehlichp_ugg"), YLAB = "P, μg/g", TITLE = "Mehlich-3 extractable P", 
                                     SUBTITLE = "measured colorimetrically (molybdate/ascorbic acid method)")
  
  ferrozine <- combined_surface %>% filter(analysis == "Ferrozine")
  gg_ferr_fetotal <- plot_site_as_x(data = ferrozine %>% filter(name == "Fe_ugg"), YLAB = "Fe, μg/g", 
                                        TITLE = "Total Fe", 
                                        SUBTITLE = "extracted with 0.5M HCl, measured colorimetrically using ferrozine")
  
  ions <- combined_surface %>% filter(analysis == "IC")
  gg_ions_ca <- plot_site_as_x(data = ions %>% filter(name == "Calcium_meq100g"), YLAB = "Ca, μg/g", TITLE = "IC", SUBTITLE = "extracted with MQ water")
  gg_ions_na <- plot_site_as_x(data = ions %>% filter(name == "Sodium_meq100g"), YLAB = "Na, μg/g", TITLE = "IC", SUBTITLE = "extracted with MQ water")
  gg_ions_mg <- plot_site_as_x(data = ions %>% filter(name == "Magnesium_meq100g"), YLAB = "Mg, μg/g", TITLE = "IC", SUBTITLE = "extracted with MQ water")
  gg_ions_k <- plot_site_as_x(data = ions %>% filter(name == "Potassium_meq100g"), YLAB = "K, μg/g", TITLE = "IC", SUBTITLE = "extracted with MQ water")
  gg_ions_cl <- plot_site_as_x(data = ions %>% filter(name == "Chloride_meq100g"), YLAB = "Cl, μg/g", TITLE = "IC", SUBTITLE = "extracted with MQ water")
  gg_ions_so4 <- plot_site_as_x(data = ions %>% filter(name == "Sulfate_meq100g"), YLAB = "SO4, μg/g", TITLE = "IC", SUBTITLE = "extracted with MQ water")
  gg_ions_no3 <- plot_site_as_x(data = ions %>% filter(name == "Nitrate_meq100g"), YLAB = "NO3, μg/g", TITLE = "IC", SUBTITLE = "extracted with MQ water")
  gg_ions_po4 <- plot_site_as_x(data = ions %>% filter(name == "Phosphate_meq100g"), YLAB = "PO4, μg/g", TITLE = "IC", SUBTITLE = "extracted with MQ water")
  
  
  list(gg_gwc = gg_gwc,
       gg_ph = gg_ph,
       gg_sp_conduc = gg_sp_conduc,
       gg_tc =  gg_tc,
       gg_tn = gg_tn,
       gg_ts = gg_ts,
       gg_loi = gg_loi,
       gg_weoc = gg_weoc,
       gg_din_nh4n = gg_din_nh4n,
       gg_din_no3n = gg_din_no3n,
       gg_icp_ca = gg_icp_ca, 
       gg_icp_mg = gg_icp_mg, 
       gg_icp_na = gg_icp_na, 
       gg_icp_k = gg_icp_k , 
       gg_icp_al = gg_icp_al, 
       #gg_icp_fe = gg_icp_fe, 
       #gg_icp_mn = gg_icp_mn, 
       gg_icp_cec = gg_icp_cec,
       gg_p_mehlich = gg_p_mehlich,
       gg_ferr_fetotal = gg_ferr_fetotal, 
       gg_ions_ca = gg_ions_ca, 
       gg_ions_na = gg_ions_na, 
       gg_ions_mg = gg_ions_mg, 
       gg_ions_k = gg_ions_k,
       gg_ions_cl = gg_ions_cl, 
       gg_ions_so4 = gg_ions_so4,
       gg_ions_no3 = gg_ions_no3,
       gg_ions_po4 = gg_ions_po4
  )
}

# xrd ----

plot_xrd = function(xrd_processed){
  
  xrd_summary = 
    xrd_processed %>% 
    dplyr::select(-c(tree_number, File)) %>% 
    pivot_longer(-c(sample_label, region, site, transect, horizon),
                 names_to = "mineral",
                 values_to = "percentage") %>% 
    reorder_horizon() %>% 
    reorder_transect()
  
  xrd_summary %>% 
    ggplot(aes(x = sample_label, y = percentage, fill = mineral))+
    geom_bar(stat = "identity")+
    facet_grid(horizon ~ region + site + transect, scales = "free_x")+
    theme_kp()
    
  
}

plot_ions_piper = function(dic_processed, ions_processed){
  
  #tic = read_tsv("1-data/tic_owc.txt", skip = 10) %>% janitor::clean_names()

  tic_processed = 
    tic %>% 
    dplyr::select(sample_name, result_ic) %>% 
    rename(analysis_ID = sample_name,
           tic_mgL = result_ic) %>% 
    # keep only sampple rows 
    filter(grepl("DOC_", analysis_ID)) %>% 
    # join the analysis key to get the sample_label
    left_join(analysis_key %>% dplyr::select(analysis_ID, sample_label)) %>% 
    mutate(Bicarbonate_ppm = tic_mgL * 5) %>% 
    dplyr::select(sample_label, Bicarbonate_ppm) %>% 
    left_join(sample_key)
  
  carbonate = 
    dic_processed %>% 
    dplyr::select(sample_label, dic_ug_g) %>% 
    filter(!is.na(dic_ug_g)) %>% 
    mutate(Carbonate_ugg = dic_ug_g * 5,
           Carbonate_meq100g =  Carbonate_ugg * 2/60 * 100/1000) %>% 
    dplyr::select(sample_label, Carbonate_meq100g)
  
    
  
  
  ions_long = 
    ions_processed_meq %>% 
    dplyr::select(sample_label, ends_with("meq100g")) %>% 
    left_join(carbonate) %>% 
    pivot_longer(-sample_label,
                 names_to = "ion", 
                 values_to = "meq_100g") %>% 
    mutate(group = case_when(grepl("Calcium", ion) ~ "Calcium",
                             grepl("Magnesium", ion) ~ "Magnesium",
                             grepl("Sodium|Potassium|Ammoni", ion) ~ "Sodium",
                             grepl("Chloride|Nitrate|Phosphate", ion) ~ "Chloride",
                             grepl("Carbonate|Bicarbonate", ion) ~ "Bicarbonate",
                             grepl("Sulfate", ion) ~ "Sulfate")) %>% 
    mutate(cation_anion = case_when(grepl("Calcium|Magnesium|Sodium", group) ~ "cation",
                                    grepl("Chloride|Bicarbonate|Sulfate", group) ~ "anion")) %>% 
    filter(!is.na(group)) %>% 
    group_by(sample_label, cation_anion, group) %>% 
    dplyr::summarise(meq_100g = sum(meq_100g)) %>% 
    group_by(sample_label, cation_anion) %>% 
    dplyr::mutate(total = sum(meq_100g),
                     percent = (meq_100g/total) * 100)

  ions_wide = ions_long %>% 
    ungroup() %>% 
    dplyr::select(-c(cation_anion, meq_100g, total)) %>% 
    pivot_wider(names_from = "group", values_from = "percent") 
  
  ions_wide2 = 
    ions_wide %>% 
    rename(Ca = Calcium,
           Mg = Magnesium,
           Cl = Chloride,
           SO4 = Sulfate)
  
  
  #remotes::install_github("https://github.com/USGS-R/smwrBase")
  #remotes::install_github("USGS-R/smwrGraphs")
  
# smwrGraphs::piperPlot(xCat = ions_wide$Calcium, yCat = ions_wide$Magnesium, zCat = ions_wide$Sodium, 
#           xAn = ions_wide$Chloride, yAn = ions_wide$Bicarbonate, zAn = ions_wide$Sulfate, 
#           Plot = list(name = "", what =
#                         "points", type = "solid", width = "standard", symbol = "circle", filled =
#                         TRUE, size = 0.09, color = "black"), axis.range = c(0, 100),
#           num.labels = 6, ticks = FALSE, grids = !ticks,
#           xCat.title = "Calcium", yCat.title = "Magnesium",
#           zCat.title = "Sodium plus Potassium",
#           xAn.title = "Chloride, Fluoride, Nitrite plus Nitrate",
#           yAn.title = "Carbonate plus Bicarbonate", zAn.title = "Sulfate",
#           x.yCat.title = "Calcium plus Magnesium",
#           x.zAn.title = "Sulfate plus Chloride", units.title = "Percent",
#           caption = "", margin = c(NA, NA, NA, NA))

  
  ggplot_piper <- function() {
    library(ggplot2)
    grid1p1 <<- data.frame(x1 = c(20,40,60,80), x2= c(10,20,30,40),y1 = c(0,0,0,0), y2 = c(17.3206,34.6412,51.9618, 69.2824))
    grid1p2 <<- data.frame(x1 = c(20,40,60,80), x2= c(60,70,80,90),y1 = c(0,0,0,0), y2 = c(69.2824, 51.9618,34.6412,17.3206))
    grid1p3 <<- data.frame(x1 = c(10,20,30,40), x2= c(90,80,70,60),y1 = c(17.3206,34.6412,51.9618, 69.2824), y2 = c(17.3206,34.6412,51.9618, 69.2824))
    grid2p1 <<- grid1p1
    grid2p1$x1 <- grid2p1$x1+120
    grid2p1$x2 <- grid2p1$x2+120
    grid2p2 <<- grid1p2
    grid2p2$x1 <- grid2p2$x1+120
    grid2p2$x2 <- grid2p2$x2+120
    grid2p3 <<- grid1p3
    grid2p3$x1 <- grid2p3$x1+120
    grid2p3$x2 <- grid2p3$x2+120
    grid3p1 <<- data.frame(x1=c(100,90, 80, 70),y1=c(34.6412, 51.9618, 69.2824, 86.603), 
                           x2=c(150, 140, 130, 120), y2=c(121.2442,138.5648,155.8854,173.2060))
    grid3p2 <<- data.frame(x1=c(70, 80, 90, 100),y1=c(121.2442,138.5648,155.8854,173.2060), 
                           x2=c(120, 130, 140, 150), y2=c(34.6412, 51.9618, 69.2824, 86.603))
    
    p <- ggplot() +
      ## left hand ternary plot
      geom_segment(aes(x=0,y=0, xend=100, yend=0)) +
      geom_segment(aes(x=0,y=0, xend=50, yend=86.603)) +
      geom_segment(aes(x=50,y=86.603, xend=100, yend=0)) +
      ## right hand ternary plot
      geom_segment(aes(x=120,y=0, xend=220, yend=0)) +
      geom_segment(aes(x=120,y=0, xend=170, yend=86.603)) +
      geom_segment(aes(x=170,y=86.603, xend=220, yend=0)) +
      ## Upper diamond
      geom_segment(aes(x=110,y=190.5266, xend=60, yend=103.9236)) +
      geom_segment(aes(x=110,y=190.5266, xend=160, yend=103.9236)) +
      geom_segment(aes(x=110,y=17.3206, xend=160, yend=103.9236)) +
      geom_segment(aes(x=110,y=17.3206, xend=60, yend=103.9236)) +
      ## Add grid lines to the plots
      geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p1, linetype = "dashed", size = 0.25, colour = "grey50") +
      geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p2, linetype = "dashed", size = 0.25, colour = "grey50") +
      geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid1p3, linetype = "dashed", size = 0.25, colour = "grey50") +
      geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p1, linetype = "dashed", size = 0.25, colour = "grey50") +
      geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p2, linetype = "dashed", size = 0.25, colour = "grey50") +
      geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid2p3, linetype = "dashed", size = 0.25, colour = "grey50") +
      geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p1, linetype = "dashed", size = 0.25, colour = "grey50") +
      geom_segment(aes(x=x1, y=y1, yend=y2, xend=x2), data=grid3p2, linetype = "dashed", size = 0.25, colour = "grey50") +
      ### Labels and grid values
      #geom_text(aes(50,-10, label="Ca^2"), parse=T, size=4) + # Commented out, as parse=TRUE can cause issues
      
      geom_text(aes(c(20,40,60,80),c(-5,-5,-5,-5), label=c(80, 60, 40, 20)), size=3) +
      geom_text(aes(c(35,25,15,5),grid1p2$y2, label=c(80, 60, 40, 20)), size=3) +
      geom_text(aes(c(95,85,75,65),grid1p3$y2, label=c(80, 60, 40, 20)), size=3) +
      # geom_text(aes(17,50, label="Mg^2"), parse=T, angle=60, size=4) +
      coord_equal(ratio=1)+  
      geom_text(aes(17,50, label="Mg^2"), angle=60, size=4, parse=TRUE) +  
      geom_text(aes(82.5,50, label="Na + K"), angle=-60, size=4) +
      geom_text(aes(50,-10, label="Ca^2"), size=4, parse=TRUE) +
      
      
      geom_text(aes(170,-10, label="Cl^-phantom()"), size=4, parse=TRUE) +
      geom_text(aes(205,50, label="SO4"), angle=-60, size=4, parse=TRUE) +
      geom_text(aes(137.5,50, label="Alkalinity~as~CO3"), angle=60, size=4, parse=TRUE) +
      geom_text(aes(72.5,150, label="SO4~+~Cl^-phantom()"), angle=60, size=4, parse=TRUE) +
      geom_text(aes(147.5,150, label="Ca^2~+~Mg^2"), angle=-60, size=4, parse=TRUE) + 
      
      geom_text(aes(c(155,145,135,125),grid2p2$y2, label=c(20, 40, 60, 80)), size=3) +
      geom_text(aes(c(215,205,195,185),grid2p3$y2, label=c(20, 40, 60, 80)), size=3) +
      geom_text(aes(c(140,160,180,200),c(-5,-5,-5,-5), label=c(20, 40, 60, 80)), size=3) +
      geom_text(aes(grid3p1$x1-5,grid3p1$y1, label=c(80, 60, 40, 20)), size=3) +
      geom_text(aes(grid3p1$x2+5,grid3p1$y2, label=c(20, 40, 60, 80)), size=3) +
      geom_text(aes(grid3p2$x1-5,grid3p2$y1, label=c(20, 40, 60, 80)), size=3) +
      geom_text(aes(grid3p2$x2+5,grid3p2$y2, label=c(80, 60, 40, 20)), size=3) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(), axis.ticks = element_blank(),
            axis.text.x = element_blank(), axis.text.y = element_blank(),
            axis.title.x = element_blank(), axis.title.y = element_blank())
    return(p)
  }
  
  
  transform_piper_data <- function(Mg, Ca, Cl, SO4, name=NULL){
    if(is.null(name)){
      name = rep(1:length(Mg),3)
    } else {
      name = rep(name,3)
    }
    y1 <- Mg * 0.86603
    x1 <- 100*(1-(Ca/100) - (Mg/200))
    y2 <- SO4 * 0.86603
    x2 <-120+(100*Cl/100 + 0.5 * 100*SO4/100)
    new_point <- function(x1, x2, y1, y2, grad=1.73206){
      b1 <- y1-(grad*x1)
      b2 <- y2-(-grad*x2)
      M <- matrix(c(grad, -grad, -1,-1), ncol=2)
      intercepts <- as.matrix(c(b1,b2))
      t_mat <- -solve(M) %*% intercepts
      data.frame(x=t_mat[1,1], y=t_mat[2,1])
    }
    np_list <- lapply(1:length(x1), function(i) new_point(x1[i], x2[i], y1[i], y2[i]))
    npoints <- do.call("rbind",np_list)
    data.frame(observation=name,x=c(x1, x2, npoints$x), y=c(y=y1, y2, npoints$y))
  }
  
  data=as.data.frame(list("Ca"=c(43,10,73,26,32),"Mg"=c(30,50,3,14,12),Cl=c(24,10,12,30,43),"SO4"=c(24,10,12,30,43),"WaterType"=c(2,2,1,2,3)),row.names=c("A","B","C","D","E"))
  piper_data <- transform_piper_data(Ca=data$Ca, Mg = data$Mg, Cl=data$Cl, SO4= data$SO4, name=data$WaterType)
  
  
  piper_data2 = 
    transform_piper_data(Ca = ions_wide$Calcium, Mg = ions_wide$Magnesium, 
                         SO4 = ions_wide$Sulfate, Cl = ions_wide$Chloride,
                                     name = ions_wide$sample_label) %>% 
    rename(sample_label = observation) %>% 
    left_join(sample_key) %>% 
    filter(horizon != "B") %>% 
    reorder_horizon() %>% 
    reorder_transect()
  
  #ggplot_piper() + geom_point(aes(x,y), data=piper_data)
  ggplot_piper() + 
    geom_point(aes(x,y, 
                   color = transect, shape = site), 
               size = 3, stroke = 1,
               data=piper_data2 %>% filter(region == "WLE"))+
    scale_color_manual(values = pal_transect)+
    scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,3,4,5,6))+
    labs(title = "Piper Plot using water-extractable ions",
         subtitle = "WLE region")
  
  ggplot_piper() + 
    geom_point(aes(x,y, 
                   color = transect, shape = site), 
               size = 3, stroke = 1,
               data=piper_data2 %>% filter(region == "CB"))+
    scale_color_manual(values = rev(soilpalettes::soil_palette("redox2", 3)))+
    scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,3,4,5,6))+
    labs(title = "Piper Plot using water-extractable ions",
         subtitle = "CB region")

    
  # pca ----
  # 
  ions_pca = fit_pca_function(ions_processed %>% dplyr::select(sample_label, ends_with("ppm"), -Bromide_ppm) %>% left_join(sample_key) %>% filter(region == "WLE"))

  ggbiplot(ions_pca$pca_int, obs.scale = 1, var.scale = 1,
           groups = as.character(ions_pca$grp$transect), 
           ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 1,
               aes(shape = ions_pca$grp$region,
                   color = groups))+ 
    scale_color_manual(breaks = c("upland", "transition", "wte", "wc"), 
                       values = pal_transect)+
    labs(shape="",
         title = "PCA: CB",
         subtitle = "surface horizons")+
    theme_kp()+
    theme(legend.position = "top", legend.box = "vertical")+
    NULL
  
  }




# soil physics ----
## water retention curves

plot_wrc = function(wrc_processed){
  
  wrc_processed %>% 
    filter(kpa_fit >= 0 | kpa_eval >= 0) %>% 
    mutate(region = factor(region, levels = c("WLE", "CB"))) %>% 
    ggplot(aes(y = water_content_vol_percent, color = transect))+
    geom_line(aes(x = kpa_fit))+
    geom_point(aes(x = kpa_eval), shape = 1)+
    scale_x_log10(labels = scales::comma)+
    scale_color_manual(values = rev(soilpalettes::soil_palette("redox2", 3)))+
    labs(color = "")+
    facet_wrap(~region + site)+
    theme_kp()
  
}

## texture

plot_texture = function(texture_processed){

  # plot the soil texture triangle
  library(ggtern) 
  data(USDA)
  
  gg_texture = 
    ggtern(data = texture_processed,
           aes(x = percent_sand, y = percent_clay, z = percent_silt)) +
    geom_polygon(data = USDA, 
                 aes(x = Sand, y = Clay, z = Silt, group = Label),
                 fill = NA, size = 0.3, alpha = 0.5, color = "grey30")+
    geom_point(aes(color = site, shape = transect),
               size = 3)+
    theme_bw()+
    theme_showarrows()+
    theme_hidetitles()+
    theme_clockwise() 
  
  USDA_text <- 
    USDA  %>% 
    group_by(Label) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE) %>%
    ungroup()
  
  #textures_names<-
  gg_texture +
    geom_text(data = USDA_text,
              aes(x = Sand, y = Clay, z = Silt, label = Label),
              size = 3, color = "grey30")
}

########################

# make summary tables
make_summary_tables <- function(data_combined){
  
  data_combined_summary <- 
    data_combined %>% 
    filter(horizon != "B") %>% 
    group_by(analysis, name, region, transect, horizon) %>% 
    dplyr::summarise(mean = mean(value),
                     mean = round(mean, 2),
                     sd = sd(value),
                     se = sd/sqrt(n()),
                     se = round(se, 2)) %>% 
    mutate(region_transect = paste0(region, "_", transect),
           mean_se = paste(mean, "\u00b1", se)) %>% 
    ungroup() %>% 
    dplyr::select(-c(region, transect, mean, sd, se)) %>% 
    pivot_wider(names_from = region_transect, values_from = mean_se)
  
  
  
}