

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



## read in TEROS/TROLL data. gives sensor_df_trim
process_teros = function(SENSOR_PATH){
  
  to_read <- list.files(SENSOR_PATH, full.names = T)
  
  df_raw <- to_read %>% 
    map(read_csv) %>% 
    bind_rows() %>% 
    dplyr::select(-instrument_id)
  
  ## Summary of start dates gives 3/22 to 4/22 coinciding with synoptic installs
  ## I'm setting 4/22 - 4/23 as a consistent(ish) time-frame for all sites
  sensor_df_trim1 <- df_raw %>% 
    rename(transect = plot) %>% 
  #  filter(datetime_est > "2023-01-01") %>% 
  #  filter(datetime_est < "2023-12-31") %>% 
    force()
  
  ## Monster 68M dataset, free up memory
#  rm(df_raw)
  
  sensor_df_trim = sensor_df_trim1 %>% 
    mutate(region = case_when(site %in% c("GCW", "MSM", "GWI") ~ "Chesapeake",
                       site %in% c("CRC", "PTR", "OWC") ~ "Erie"),
           transect = case_match(transect, "UP" ~ "upland", "TR" ~ "transition", "W" ~ "wetland")) %>% 
    filter(!is.na(transect)) %>% 
    filter(!is.na(value))
}

# summarize VWC means
get_vwc_data = function(sensor_df_trim){
  
  tic("bin teros") # only 5s!
  vwc_data <- sensor_df_trim %>% 
    filter(research_name == "soil-vwc-10cm") %>% 
    group_by(datetime_est, region, site, transect) %>% 
    summarize(value = mean(value, na.rm = T),
              value = value * 100) %>% 
    drop_na() 
    
  toc()
  vwc_data
}

summarize_vwc = function(vwc_data){

  vwc_means <- 
    vwc_data %>% 
    filter(datetime_est > "2023-01-01") %>% 
    filter(datetime_est < "2023-12-31") %>% 
    ungroup() %>% 
    group_by(region, site, transect) %>% 
    summarize(mean_vwc = round(mean(value, na.rm = T), 2),
              sd_vwc = round(sd(value, na.rm = T), 2),
              median_vwc = round(median(value, na.rm = T), 2)) %>% 
    reorder_site() %>% 
    reorder_transect() %>% 
    arrange(site, transect)
  
} 

## WATER LEVELS

calculate_water_levels = function(sensor_df_trim){
  
  tic("pivot troll")
  troll_raw_unbinned <- 
    sensor_df_trim %>% 
    filter(grepl("gw", research_name)) %>% 
    filter(!is.na(value)) %>% 
    group_by(datetime_est, region, site, transect) %>% 
    pivot_wider(names_from = "research_name", 
                values_from = "value", 
                values_fn = mean) %>% 
    rename(gw_density = `gw-density`,
           gw_pressure = `gw-pressure`)
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
   # mutate(rollmean = case_when(site == "GCW" ~ NA, .default = rollmean)) %>% 
    reorder_site() %>% 
    reorder_transect()
    
}

calculate_percent_flooded = function(troll){
  
  percent_flooded <- 
    troll %>% 
    filter(datetime_est > "2023-01-01") %>% 
    filter(datetime_est < "2023-12-31") %>% 
    mutate(flooded = ifelse(wl_below_surface_m >= 0, "flooded", "not")) %>% 
    ungroup() %>% 
    group_by(region, site, transect) %>% 
    summarize(percent_flooded = sum(flooded == "flooded") / n() * 100) %>% 
    mutate_if(is.numeric, round, 2) %>% 
  #  filter(!(site == "GCW")) %>% 
    force()

}

summarize_water_table = function(troll){
  
  troll %>% 
    filter(datetime_est > "2023-01-01") %>% 
    filter(datetime_est < "2023-12-31") %>% 
    group_by(region, site, transect) %>% 
    dplyr::summarise(mean_wl = mean(wl_below_surface_m),
                     sd_wl = sd(wl_below_surface_m),
                     median_wl = median(wl_below_surface_m),
                     min_wl = min(wl_below_surface_m),
                     max_wl = max(wl_below_surface_m)) %>% 
    mutate_if(is.numeric, round, 2) %>% 
    #   filter(!site %in% "GCW") %>% 
    reorder_site() %>% 
    reorder_transect()

}
