
pal_transect = rev(soilpalettes::soil_palette("redox2", 4))
theme_set(theme_kp())

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