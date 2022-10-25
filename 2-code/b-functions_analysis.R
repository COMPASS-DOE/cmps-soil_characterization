
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
    labs(y = "Extractable nitrate-N, μg/g")+
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
