

require(pacman)
p_load(tidyverse,
       janitor,
       parsedate,
       lubridate,
       cowplot, 
       purrr,
       readr, 
       tictoc,
       beepr,
       googledrive, 
       furrr,
       PNWColors,
       googlesheets4)



SENSOR_PATH = "TEMP/csvs_to_process"

## read in TEROS/TROLL data. gives df_trim
process_teros = function(SENSOR_PATH){
  
  to_read <- list.files(SENSOR_PATH, full.names = T)
  
  df_raw <- to_read %>% 
    map(read_csv) %>% 
    bind_rows() %>% 
    dplyr::select(-instrument_id)
  
  ## Summary of start dates gives 3/22 to 4/22 coinciding with synoptic installs
  ## I'm setting 4/22 - 4/23 as a consistent(ish) time-frame for all sites
  df_trim <- df_raw %>% 
    filter(datetime_est > "2022-04-01") %>% 
    filter(datetime_est < "2023-04-01")
  
  ## Monster 68M dataset, free up memory
  rm(df_raw)
  
  df_trim %>% 
    rename(transect = plot) %>% 
    mutate(region = case_when(site %in% c("GCW", "MSM", "GWI") ~ "Chesapeake",
                       site %in% c("CRC", "PTR", "OWC") ~ "Erie"),
           transect = case_match(transect, "UP" ~ "upland", "TR" ~ "transition", "W" ~ "wetland"))
}
df_trim = process_teros(SENSOR_PATH)



# summarize VWC means
get_vwc_data = function(df_trim){
  
  tic("bin teros") # only 5s!
  vwc_data <- df_trim %>% 
    filter(research_name == "soil_vwc_10cm") %>% 
    group_by(datetime_est, region, site, transect) %>% 
    summarize(value = mean(value, na.rm = T),
              value = value * 100) %>% 
    drop_na() 
    
  toc()
  vwc_data
}
vwc_data = get_vwc_data(df_trim)

summarize_vwc = function(vwc_data){

  vwc_means <- 
    vwc_data %>% 
    ungroup() %>% 
    group_by(region, site, transect) %>% 
    summarize(mean_vwc = round(mean(value, na.rm = T), 2),
              sd_vwc = round(sd(value, na.rm = T), 2)) %>% 
    reorder_site() %>% 
    reorder_transect()
  
} 
vwc_means = summarize_vwc(vwc_data)

# plot vwc
library(ggh4x)

vwc_data %>% 
  filter(value > 0) %>% 
  reorder_site() %>% 
  reorder_transect() %>% 
  ggplot(aes(x = datetime_est, y = value, color = transect))+
  geom_line()+
#  facet_wrap(~ site + transect)+
  facet_nested_wrap(~region + site,
                    nest_line = element_line(colour = "grey")) +
  scale_color_manual(values = pal_transect)+
  labs(x = "Date", y = "Water content (% v/v)")+
  theme_kp()+
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        panel.grid = element_blank())



## WATER LEVELS


calculate_water_levels = function(df_trim){
  
  tic("pivot troll")
  troll_raw_unbinned <- 
    df_trim %>% 
    filter(grepl("gw", research_name)) %>% 
    group_by(datetime_est, region, site, transect) %>% 
    pivot_wider(names_from = "research_name", 
                values_from = "value", 
                values_fn = mean) 
  toc()
  
  tic("summarize troll")
  troll_raw <- 
    troll_raw_unbinned %>% 
    drop_na() %>% 
    summarize(gw_density = mean(gw_density, na.rm = T), 
              gw_pressure = mean(gw_pressure, na.rm = T))
  toc()
  

  # Calculate water levels
  
  ## Second, read in well dimensions. These are measurements made when installing
  ## sensors, and are used to calculate the distance below-ground that the 
  well_dimensions <- 
    read_sheet("https://docs.google.com/spreadsheets/d/1O4sHvj2FO7EcWEm3WpKEZhFubGn8HCcUsz9EFXhQTXM/edit#gid=0") %>% 
    mutate(transect = tolower(transect_location), 
           ground_to_sensor_cm = ring_to_pressure_sensor_cm - (well_top_to_ground_cm - bolt_to_cap_cm)) %>% 
    dplyr::select(site, transect, ground_to_sensor_cm)
  
  ## Add well dimensions to dataset: inner_join ensures no uncorrected WL data makes it through
  troll_raw_depths <- 
    inner_join(troll_raw, well_dimensions, by = c("site", "transect")) %>% 
    filter(!is.na(gw_pressure))
  
  ## Correct water levels for pressure, density, and well dimensions
  ## 0.98 based on lower density of freshwater based on L1 readme
  ## 1.05 is upper bound
  troll <- 
    troll_raw_depths %>% 
    mutate(density_gcm3_cor = ifelse(gw_density >= 0.98 & gw_density <= 1.05, gw_density, 1), 
           pressure_mbar = ifelse(gw_pressure == -99999, 0, gw_pressure), 
           pressurehead_m = (pressure_mbar * 100) / (density_gcm3_cor * 1000 * 9.80665), 
           wl_below_surface_m = pressurehead_m - (ground_to_sensor_cm / 100)) %>% 
    ## This is a weird work-around: need to flag here so that we can easily match to spreadsheet
    mutate(flag_out_of_water = ifelse(wl_below_surface_m < ((ground_to_sensor_cm/100) * -1), TRUE,FALSE)) %>% 
    ## there are some outliers, those need to be removed
    ## do this using rolling means, if the value is more than 1 unit from the rolling mean, it is an outlier
    ## this gets rid of most of the crazy outliers
    arrange(region, site, transect, datetime_est) %>% 
    group_by(region, site, transect) %>% 
    mutate(rollmean = zoo::rollmean(wl_below_surface_m, 70, fill = NA),
           outlier = case_when(abs(wl_below_surface_m - rollmean) > 1 ~ "outlier")) %>% 
    filter(is.na(outlier)) %>% 
    ## GCW is weird, so converting those to NAs
    mutate(rollmean = case_when(site == "GCW" ~ NA, .default = rollmean)) %>% 
    reorder_site() %>% 
    reorder_transect()
    
}
troll = calculate_water_levels(df_trim)

calculate_percent_flooded = function(){
  
  percent_flooded <- troll %>% 
    mutate(flooded = ifelse(wl_below_surface_m >= 0, "flooded", "not")) %>% 
    ungroup() %>% 
    group_by(region, site, transect) %>% 
    summarize(percent_flooded = sum(flooded == "flooded") / n() * 100) %>% 
    filter(!(site == "GCW"))
  
  # write_csv(percent_flooded, paste0(temp_storage, "/250620_kaizad_synoptic_troll_percent_flooded.csv"))  
  
}

 
  
  ##  group_by(site, plot) %>% 
  ##  dplyr::summarise(mean_wl = mean(wl_below_surface_m),
  ##                   median_wl = median(wl_below_surface_m),
  ##                   min_wl = min(wl_below_surface_m),
  ##                   max_wl = max(wl_below_surface_m))
  
  
  library(ggh4x) # for nested facets
  troll %>% 
    #filter(!site == "GCW") %>%
  #  mutate(rollmean = case_when(site == "GCW" ~ NA, .default = rollmean)) %>% 
  #  filter(is.na(outlier2)) %>% 
    ggplot(aes(x = datetime_est, y = wl_below_surface_m, color = transect))+
    geom_hline(yintercept = 0, alpha = 0.8)+
    # geom_point(aes(color = outlier2))+
    geom_line(aes(y = rollmean), linewidth = 1)+
    #  facet_wrap(~site)+
    #  facet_grid(site ~ plot)+ 
    facet_nested_wrap(~region + site,
                      nest_line = element_line(colour = "grey"))+ 
    scale_color_manual(values = pal_transect)+
    labs(x = "",
         y = "Water level below surface (m)",
         color = "")+
    theme_kp()+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size = 14, angle = 45, hjust = 1))

