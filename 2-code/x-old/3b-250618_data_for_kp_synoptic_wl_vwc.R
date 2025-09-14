

## This script pulls data from 2021-2023 for 6 (not SWH) synoptic sites, looking
## for a full year (ish) of data across our sites, and calculating 
## 1) the % of time that each site-location is under water, and 
## 2) the average VWC at 5 cm
##
## Peter Regier
## 2025-06-18
##
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## Load packages with pacman::p_load
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

# Set ggplot theme
theme_set(theme_bw())

common_tz = "Etc/GMT+5"


sites_to_pull = "GWI|GCW|MSM|PTR|CRC|OWC"
years_to_pull = "2022|2023|2024"

temp_storage = "TEMP/"


# 2. Pull L1 data --------------------------------------------------------------

## We are pulling lots of data, let's figure out how to do that programmatically
l2_folders <- drive_ls("https://drive.google.com/drive/folders/14F8IH0UBC9Yn4YsHhLixtnacjUvcKtta") %>% 
  filter(grepl(sites_to_pull, name)) %>% 
  filter(grepl(years_to_pull, name)) #%>% 
#slice(3:n())


## Make functions to find and download needed csvs
download_l2_csvs <- function(i){
  
  folder <- l2_folders %>% slice(i)
  
  folder_name <- folder$name
  
  message(paste("downloading data from", folder_name))
  
  folder_id <- folder$id
  
  y <- googledrive::drive_ls(folder_id, pattern = ".parquet$") %>% 
    filter(!(grepl("_OW_", name))) %>% 
    filter(grepl("soil-vwc-10cm|gw-pressure|gw-density", name))
  
  # Download files only if they aren't already present
  for (j in seq_along(y$id)) {
    file_path <- paste0(temp_storage, "raw_files/", y$name[[j]])
    
    if (!file.exists(file_path)) {
      message(paste("Downloading", y$name[[j]], "to", file_path))
      googledrive::drive_download(y$id[[j]],
                                  overwrite = FALSE,
                                  path = file_path)
    } else {
      message(paste("File already exists:", y$name[[j]], "Skipping download."))
    }
  }
  
  
  
  
old_function = function(){  
  
  ## NOT WORKING. KEEPS ADDING CRC TO EACH EXPORT
  files_to_read <- list.files(paste0(temp_storage, "raw_files"), full.names = T)
  
  #  variables_to_keep = c("gw_pressure", "gw_density", "soil_vwc_10cm")
  
  read_file <- function(file){
    arrow::read_parquet(file) %>%
      clean_names() %>%
      mutate(datetime_est = force_tz(timestamp, tzone = common_tz)) %>% 
      mutate(instrument_id = as.character(instrument_id)) %>% 
      dplyr::select(datetime_est, site, plot, research_name, instrument_id, value) %>% 
      force()
  }
  
  raw_data <- files_to_read %>%
    map(read_file) %>%
    bind_rows() %>% 
    filter(!is.na(value))
  
  write_csv(raw_data, paste0(temp_storage, "csvs_to_process/", folder_name, ".csv"))
  
}

}

process_l2_csvs = function(){
  
  
  read_and_combine = function(PATTERN, FOLDER_NAME){
    
    files_to_read <- list.files(paste0(temp_storage, "raw_files"), pattern = PATTERN, full.names = T)
    
    #  variables_to_keep = c("gw_pressure", "gw_density", "soil_vwc_10cm")
    
    read_file <- function(file){
      arrow::read_parquet(file) %>%
        clean_names() %>%
        mutate(datetime_est = force_tz(timestamp, tzone = common_tz)) %>% 
        mutate(instrument_id = as.character(instrument_id)) %>% 
        dplyr::select(datetime_est, site, plot, research_name, instrument_id, value) %>% 
        force()
    }
    
    raw_data <- files_to_read %>%
      map(read_file) %>%
      bind_rows() %>% 
      filter(!is.na(value))
    
    write_csv(raw_data, paste0(temp_storage, "csvs_to_process/", FOLDER_NAME, ".csv"))
    
    
  }
  
  read_and_combine(PATTERN = c("GCW", "2022"), FOLDER_NAME = "GCW_2022")
  read_and_combine(PATTERN = c("GCW", "2023"), FOLDER_NAME = "GCW_2023")
  read_and_combine(PATTERN = c("GCW", "2024"), FOLDER_NAME = "GCW_2024")
  
  read_and_combine(PATTERN = c("MSM", "2022"), FOLDER_NAME = "MSM_2022")
  read_and_combine(PATTERN = c("MSM", "2023"), FOLDER_NAME = "MSM_2023")
  read_and_combine(PATTERN = c("MSM", "2024"), FOLDER_NAME = "MSM_2024")
  
  read_and_combine(PATTERN = c("GWI", "2022"), FOLDER_NAME = "GWI_2022")
  read_and_combine(PATTERN = c("GWI", "2023"), FOLDER_NAME = "GWI_2023")
  read_and_combine(PATTERN = c("GWI", "2024"), FOLDER_NAME = "GWI_2024")
  
  read_and_combine(PATTERN = c("CRC", "2022"), FOLDER_NAME = "CRC_2022")
  read_and_combine(PATTERN = c("CRC", "2023"), FOLDER_NAME = "CRC_2023")
  read_and_combine(PATTERN = c("CRC", "2024"), FOLDER_NAME = "CRC_2024")
  
  read_and_combine(PATTERN = c("PTR", "2022"), FOLDER_NAME = "PTR_2022")
  read_and_combine(PATTERN = c("PTR", "2023"), FOLDER_NAME = "PTR_2023")
  read_and_combine(PATTERN = c("PTR", "2024"), FOLDER_NAME = "PTR_2024")
  
  read_and_combine(PATTERN = c("OWC", "2022"), FOLDER_NAME = "OWC_2022")
  read_and_combine(PATTERN = c("OWC", "2023"), FOLDER_NAME = "OWC_2023")
  read_and_combine(PATTERN = c("OWC", "2024"), FOLDER_NAME = "OWC_2024")
  

}


## Clean out directory
#file.remove(list.files(paste0(temp_storage, "raw_files/"), full.names = T))

## Download and format data 
tic("read all relevant L1 data")
1:nrow(l2_folders) %>%
  map(download_l2_csvs)
toc()

process_l2_csvs()


file.remove(list.files(paste0(temp_storage, "raw_files/"), full.names = T))


    ####    # 3. Read in datasets ----------------------------------------------------------
    ####    
    ####    to_read <- list.files(paste0(temp_storage, "csvs_to_process"), 
    ####                          full.names = T)
    ####    
    ####    df_raw <- to_read %>% 
    ####      map(read_csv) %>% 
    ####      bind_rows() %>% 
    ####      dplyr::select(-instrument_id)
    ####    
    ####    ## Summary of start dates gives 3/22 to 4/22 coinciding with synoptic installs
    ####    ## I'm setting 4/22 - 4/23 as a consistent(ish) time-frame for all sites
    ####    df_trim <- df_raw %>% 
    ####      filter(datetime_est > "2022-04-01") %>% 
    ####      filter(datetime_est < "2023-04-01")
    ####    
    ####    ## Monster 68M dataset, free up memory
    ####    rm(df_raw)
    ####    
    ####    
    ####    tic("bin teros") # only 5s!
    ####    teros_raw <- df_trim %>% 
    ####      filter(research_name == "soil_vwc_10cm") %>% 
    ####      group_by(datetime_est, site, plot) %>% 
    ####      summarize(value = mean(value, na.rm = T)) %>% 
    ####      drop_na()
    ####    toc()
    ####    
    ####    tic("pivot troll")
    ####    troll_raw_unbinned <- df_trim %>% 
    ####      filter(grepl("gw", research_name)) %>% 
    ####      group_by(datetime_est, site, plot) %>% 
    ####      pivot_wider(names_from = "research_name", 
    ####                  values_from = "value", 
    ####                  values_fn = mean) 
    ####    toc()
    ####    
    ####    tic("summarize troll")
    ####    troll_raw <- troll_raw_unbinned %>% 
    ####      drop_na() %>% 
    ####      summarize(gw_density = mean(gw_density, na.rm = T), 
    ####                gw_pressure = mean(gw_pressure, na.rm = T))
    ####    toc()
    ####    
    ####    
    ####    
    ####    rm(df_trim)
    ####    
    ####    
    ####    # 4. Teros stats - mean VWC by 
    ####    
    ####    vwc_means <- teros_raw %>% 
    ####      ungroup() %>% 
    ####      group_by(site, plot) %>% 
    ####      summarize(mean_vwc = mean(value, na.rm = T),
    ####                sd_vwc = sd(value, na.rm = T))
    ####    
    ####    write_csv(vwc_means, paste0(temp_storage, "/250620_kaizad_synoptic_vwc_10cm_means.csv"))
    ####    
    ####    vwc_means %>% 
    ####      mutate(plot = factor(plot, levels = c("W", "TR", "UP"))) %>% 
    ####      ggplot(aes(x = plot, color = site)) + 
    ####      geom_line(aes(y = mean_vwc, group = site), lwd = 0.8) + 
    ####      geom_point(aes(y = mean_vwc), size = 2) + 
    ####      geom_errorbar(aes(ymin = mean_vwc - sd_vwc, 
    ####                        ymax = mean_vwc + sd_vwc), 
    ####                    width = 0.05)
    ####    
    ####    
    ####    # 5. Calculate water levels ----------------------------------------------------
    ####    
    ####    ## Second, read in well dimensions. These are measurements made when installing
    ####    ## sensors, and are used to calculate the distance below-ground that the 
    ####    well_dimensions <- read_sheet("https://docs.google.com/spreadsheets/d/1O4sHvj2FO7EcWEm3WpKEZhFubGn8HCcUsz9EFXhQTXM/edit#gid=0") %>% 
    ####      mutate(location = case_when(transect_location == "Upland" ~ "UP", 
    ####                                  transect_location == "Transition" ~ "TR", 
    ####                                  transect_location == "Wetland" ~ "W"), 
    ####             ground_to_sensor_cm = ring_to_pressure_sensor_cm - (well_top_to_ground_cm - bolt_to_cap_cm)) %>% 
    ####      dplyr::select(site, location, ground_to_sensor_cm)
    ####    
    ####    ## Add well dimensions to dataset: inner_join ensures no uncorrected WL data makes it through
    ####    troll_raw_depths <- inner_join(troll_raw, well_dimensions, by = c("site" = "site", 
    ####                                                                      "plot" = "location")) %>% 
    ####      filter(!is.na(gw_pressure))
    ####    
    ####    ## Correct water levels for pressure, density, and well dimensions
    ####    ## 0.98 based on lower density of freshwater based on L1 readme
    ####    ## 1.05 is upper bound
    ####    troll <- troll_raw_depths %>% 
    ####      mutate(density_gcm3_cor = ifelse(gw_density >= 0.98 & gw_density <= 1.05, gw_density, 1), 
    ####             pressure_mbar = ifelse(gw_pressure == -99999, 0, gw_pressure), 
    ####             pressurehead_m = (pressure_mbar * 100) / (density_gcm3_cor * 1000 * 9.80665), 
    ####             wl_below_surface_m = pressurehead_m - (ground_to_sensor_cm / 100)) %>% 
    ####      ## This is a weird work-around: need to flag here so that we can easily match to spreadsheet
    ####      mutate(flag_out_of_water = ifelse(wl_below_surface_m < ((ground_to_sensor_cm/100) * -1), TRUE,FALSE))
    ####    
    ####    percent_flooded <- troll %>% 
    ####      mutate(flooded = ifelse(wl_below_surface_m >= 0, "flooded", "not")) %>% 
    ####      ungroup() %>% 
    ####      group_by(site, plot) %>% 
    ####      summarize(percent_flooded = sum(flooded == "flooded") / n() * 100) %>% 
    ####      filter(!(site == "GCW" & plot == "UP"))
    ####    
    ####    write_csv(percent_flooded, paste0(temp_storage, "/250620_kaizad_synoptic_troll_percent_flooded.csv"))
    ####    
    ####    
    ####    troll_outlier = 
    ####      troll %>% 
    ####      arrange(site, plot, datetime_est) %>% 
    ####      group_by(site, plot) %>% 
    ####      mutate(rollmean = zoo::rollmean(wl_below_surface_m, 70, fill = NA),
    ####             lag = - wl_below_surface_m + lag(wl_below_surface_m),
    ####             outlier = case_when(abs(lag) >= 0.3 ~ "outlier"),
    ####             outlier2 = case_when(abs(wl_below_surface_m - rollmean) > 1 ~ "outlier"),
    ####             plot = factor(plot, levels = c("UP", "TR", "W")),
    ####             region = case_when(site %in% c("GCW", "MSM", "GWI") ~ "Chesapeake",
    ####                                site %in% c("CRC", "PTR", "OWC") ~ "Erie"))
    ####    
    ####    ##  group_by(site, plot) %>% 
    ####    ##  dplyr::summarise(mean_wl = mean(wl_below_surface_m),
    ####    ##                   median_wl = median(wl_below_surface_m),
    ####    ##                   min_wl = min(wl_below_surface_m),
    ####    ##                   max_wl = max(wl_below_surface_m))
    ####    
    ####    
    ####    troll_outlier %>% 
    ####      filter(!(site == "GCW" & plot == "UP")) %>% 
    ####      ggplot(aes(x = datetime_est, y = wl_below_surface_m))+
    ####      geom_point(aes(color = outlier2))+
    ####      geom_line(aes(y = rollmean))+
    ####      facet_grid(site ~ plot)
    ####    
    ####    library(ggh4x) # for nested facets
    ####    troll_outlier %>% 
    ####      #filter(!site == "GCW") %>%
    ####      mutate(rollmean = case_when(site == "GCW" ~ NA, .default = rollmean)) %>% 
    ####      filter(is.na(outlier2)) %>% 
    ####      ggplot(aes(x = datetime_est, y = wl_below_surface_m, color = plot))+
    ####      geom_hline(yintercept = 0, alpha = 0.8)+
    ####      # geom_point(aes(color = outlier2))+
    ####      geom_line(aes(y = rollmean), linewidth = 1)+
    ####      #  facet_wrap(~site)+
    ####      #  facet_grid(site ~ plot)+ 
    ####      facet_nested_wrap(~region + site,
    ####                        nest_line = element_line(colour = "grey"))+ 
    ####      theme_kp()+
    ####      theme(panel.grid = element_blank())
    ####    
    ####    
    ####    
    ####    
    ####    
    ####    
    ####    
    ####    #######################################
    ####    #######################################
    ####    
    ####    
    ####    
    ####    
    ####    
    ####    ## This script pulls data from 2021-2023 for 6 (not SWH) synoptic sites, looking
    ####    ## for a full year (ish) of data across our sites, and calculating 
    ####    ## 1) the % of time that each site-location is under water, and 
    ####    ## 2) the average VWC at 5 cm
    ####    ##
    ####    ## Peter Regier
    ####    ## 2025-06-18
    ####    ##
    ####    # ########### #
    ####    # ########### #
    ####    
    ####    
    ####    # 1. Setup ---------------------------------------------------------------------
    ####    
    ####    ## Load packages with pacman::p_load
    ####    require(pacman)
    ####    p_load(tidyverse,
    ####           janitor,
    ####           parsedate,
    ####           lubridate,
    ####           cowplot, 
    ####           purrr,
    ####           readr, 
    ####           tictoc,
    ####           beepr,
    ####           googledrive, 
    ####           furrr,
    ####           PNWColors,
    ####           googlesheets4)
    ####    
    ####    # Set ggplot theme
    ####    theme_set(theme_bw())
    ####    
    ####    common_tz = "Etc/GMT+5"
    ####    
    ####    
    ####    sites_to_pull = "PTR|CRC|OWC|MSM|GCW|GWI"
    ####    years_to_pull = "2022|2023"
    ####    
    ####    temp_storage = "TEMP/"
    ####    
    ####    
    ####    # 2. Pull L1 data --------------------------------------------------------------
    ####    
    ####    ## We are pulling lots of data, let's figure out how to do that programmatically
    ####    l1_folders <- drive_ls("https://drive.google.com/drive/folders/1sDB3Q3ZXGvXkniWw03CHY1E0KV5GdN8j") %>% 
    ####      filter(grepl(sites_to_pull, name)) %>% 
    ####      filter(grepl(years_to_pull, name)) #%>% 
    ####      #slice(3:n())
    ####    
    ####    
    ####    ## Make functions to find and download needed csvs
    ####    download_l1_csvs <- function(i){
    ####      
    ####      folder <- l1_folders %>% slice(i)
    ####      
    ####      folder_name <- folder$name
    ####      
    ####      message(paste("downloading data from", folder_name))
    ####      
    ####      folder_id <- folder$id
    ####      
    ####      y <- googledrive::drive_ls(folder_id, pattern = ".csv$") %>% 
    ####        filter(!(grepl("_OW_", name)))
    ####      
    ####      # for(i in 1:length(y$id)){
    ####      # #for(i in 1:1){
    ####      #   googledrive::drive_download(y$id[[i]],
    ####      #                               overwrite = TRUE,
    ####      #                               path = paste0(temp_storage, "raw_files/", y$name[[i]]))
    ####      # }
    ####      
    ####      # Download files only if they aren't already present
    ####      for (j in seq_along(y$id)) {
    ####        file_path <- paste0(temp_storage, "raw_files/", y$name[[j]])
    ####        
    ####        if (!file.exists(file_path)) {
    ####          message(paste("Downloading", y$name[[j]], "to", file_path))
    ####          googledrive::drive_download(y$id[[j]],
    ####                                      overwrite = FALSE,
    ####                                      path = file_path)
    ####        } else {
    ####          message(paste("File already exists:", y$name[[j]], "Skipping download."))
    ####        }
    ####      }
    ####      
    ####      files_to_read <- list.files(paste0(temp_storage, "raw_files"), full.names = T)
    ####      
    ####      variables_to_keep = c("gw_pressure", "gw_density", "soil_vwc_10cm")
    ####      
    ####      read_file <- function(file){
    ####        read_csv(file) %>%
    ####          clean_names() %>%
    ####          filter(research_name %in% variables_to_keep) %>% 
    ####          mutate(datetime_est = force_tz(timestamp, tzone = common_tz)) %>% 
    ####          mutate(instrument_id = as.character(instrument_id)) %>% 
    ####          dplyr::select(datetime_est, site, plot, research_name, instrument_id, value) 
    ####      }
    ####    
    ####      raw_data <- files_to_read %>%
    ####        map(read_file) %>%
    ####        bind_rows()
    ####      
    ####      write_csv(raw_data, paste0(temp_storage, "csvs_to_process/", folder_name, ".csv"))
    ####    }
    ####    
    ####    ## Clean out directory
    ####    #file.remove(list.files(paste0(temp_storage, "raw_files/"), full.names = T))
    ####    
    ####    ## Download and format data 
    ####    tic("read all relevant L1 data")
    ####    1:nrow(l1_folders) %>%
    ####      map(download_l1_csvs)
    ####    toc()
    ####    
    ####    file.remove(list.files(paste0(temp_storage, "raw_files/"), full.names = T))
    ####    
    ####    
    ####    # 3. Read in datasets ----------------------------------------------------------
    ####    
    ####    to_read <- list.files(paste0(temp_storage, "csvs_to_process"), 
    ####                          full.names = T)
    ####    
    ####    df_raw <- to_read %>% 
    ####      map(read_csv) %>% 
    ####      bind_rows() %>% 
    ####      dplyr::select(-instrument_id)
    ####    
    ####    ## Summary of start dates gives 3/22 to 4/22 coinciding with synoptic installs
    ####    ## I'm setting 4/22 - 4/23 as a consistent(ish) time-frame for all sites
    ####    df_trim <- df_raw %>% 
    ####      filter(datetime_est > "2022-04-01") %>% 
    ####      filter(datetime_est < "2023-04-01")
    ####    
    ####    ## Monster 68M dataset, free up memory
    ####    rm(df_raw)
    ####    
    ####    
    ####    tic("bin teros") # only 5s!
    ####    teros_raw <- df_trim %>% 
    ####      filter(research_name == "soil_vwc_10cm") %>% 
    ####      group_by(datetime_est, site, plot) %>% 
    ####      summarize(value = mean(value, na.rm = T)) %>% 
    ####      drop_na()
    ####    toc()
    ####    
    ####    tic("pivot troll")
    ####    troll_raw_unbinned <- df_trim %>% 
    ####      filter(grepl("gw", research_name)) %>% 
    ####      group_by(datetime_est, site, plot) %>% 
    ####      pivot_wider(names_from = "research_name", 
    ####                  values_from = "value", 
    ####                  values_fn = mean) 
    ####    toc()
    ####    
    ####    tic("summarize troll")
    ####    troll_raw <- troll_raw_unbinned %>% 
    ####      drop_na() %>% 
    ####      summarize(gw_density = mean(gw_density, na.rm = T), 
    ####                gw_pressure = mean(gw_pressure, na.rm = T))
    ####    toc()
    ####    
    ####    
    ####    
    ####    rm(df_trim)
    ####    
    ####    
    ####    # 4. Teros stats - mean VWC by 
    ####    
    ####    vwc_means <- teros_raw %>% 
    ####      ungroup() %>% 
    ####      group_by(site, plot) %>% 
    ####      summarize(mean_vwc = mean(value, na.rm = T),
    ####                sd_vwc = sd(value, na.rm = T))
    ####      
    ####    write_csv(vwc_means, paste0(temp_storage, "/250620_kaizad_synoptic_vwc_10cm_means.csv"))
    ####    
    ####    vwc_means %>% 
    ####      mutate(plot = factor(plot, levels = c("W", "TR", "UP"))) %>% 
    ####      ggplot(aes(x = plot, color = site)) + 
    ####      geom_line(aes(y = mean_vwc, group = site), lwd = 0.8) + 
    ####      geom_point(aes(y = mean_vwc), size = 2) + 
    ####      geom_errorbar(aes(ymin = mean_vwc - sd_vwc, 
    ####                        ymax = mean_vwc + sd_vwc), 
    ####                    width = 0.05)
    ####    
    ####    
    ####    # 5. Calculate water levels ----------------------------------------------------
    ####    
    ####    ## Second, read in well dimensions. These are measurements made when installing
    ####    ## sensors, and are used to calculate the distance below-ground that the 
    ####    well_dimensions <- read_sheet("https://docs.google.com/spreadsheets/d/1O4sHvj2FO7EcWEm3WpKEZhFubGn8HCcUsz9EFXhQTXM/edit#gid=0") %>% 
    ####      mutate(location = case_when(transect_location == "Upland" ~ "UP", 
    ####                                  transect_location == "Transition" ~ "TR", 
    ####                                  transect_location == "Wetland" ~ "W"), 
    ####             ground_to_sensor_cm = ring_to_pressure_sensor_cm - (well_top_to_ground_cm - bolt_to_cap_cm)) %>% 
    ####      dplyr::select(site, location, ground_to_sensor_cm)
    ####    
    ####    ## Add well dimensions to dataset: inner_join ensures no uncorrected WL data makes it through
    ####    troll_raw_depths <- inner_join(troll_raw, well_dimensions, by = c("site" = "site", 
    ####                                                                      "plot" = "location")) %>% 
    ####      filter(!is.na(gw_pressure))
    ####    
    ####    ## Correct water levels for pressure, density, and well dimensions
    ####    ## 0.98 based on lower density of freshwater based on L1 readme
    ####    ## 1.05 is upper bound
    ####    troll <- troll_raw_depths %>% 
    ####      mutate(density_gcm3_cor = ifelse(gw_density >= 0.98 & gw_density <= 1.05, gw_density, 1), 
    ####             pressure_mbar = ifelse(gw_pressure == -99999, 0, gw_pressure), 
    ####             pressurehead_m = (pressure_mbar * 100) / (density_gcm3_cor * 1000 * 9.80665), 
    ####             wl_below_surface_m = pressurehead_m - (ground_to_sensor_cm / 100)) %>% 
    ####      ## This is a weird work-around: need to flag here so that we can easily match to spreadsheet
    ####      mutate(flag_out_of_water = ifelse(wl_below_surface_m < ((ground_to_sensor_cm/100) * -1), TRUE,FALSE))
    ####    
    ####    percent_flooded <- troll %>% 
    ####      mutate(flooded = ifelse(wl_below_surface_m >= 0, "flooded", "not")) %>% 
    ####      ungroup() %>% 
    ####      group_by(site, plot) %>% 
    ####      summarize(percent_flooded = sum(flooded == "flooded") / n() * 100) %>% 
    ####      filter(!(site == "GCW" & plot == "UP"))
    ####    
    ####    write_csv(percent_flooded, paste0(temp_storage, "/250620_kaizad_synoptic_troll_percent_flooded.csv"))
    ####    
    ####    
    ####    troll_outlier = 
    ####      troll %>% 
    ####      arrange(site, plot, datetime_est) %>% 
    ####      group_by(site, plot) %>% 
    ####      mutate(rollmean = zoo::rollmean(wl_below_surface_m, 70, fill = NA),
    ####             lag = - wl_below_surface_m + lag(wl_below_surface_m),
    ####             outlier = case_when(abs(lag) >= 0.3 ~ "outlier"),
    ####             outlier2 = case_when(abs(wl_below_surface_m - rollmean) > 1 ~ "outlier"),
    ####             plot = factor(plot, levels = c("UP", "TR", "W")),
    ####             region = case_when(site %in% c("GCW", "MSM", "GWI") ~ "Chesapeake",
    ####                                site %in% c("CRC", "PTR", "OWC") ~ "Erie"))
    ####      
    ####    ##  group_by(site, plot) %>% 
    ####    ##  dplyr::summarise(mean_wl = mean(wl_below_surface_m),
    ####    ##                   median_wl = median(wl_below_surface_m),
    ####    ##                   min_wl = min(wl_below_surface_m),
    ####    ##                   max_wl = max(wl_below_surface_m))
    ####    
    ####    
    ####    troll_outlier %>% 
    ####      filter(!(site == "GCW" & plot == "UP")) %>% 
    ####      ggplot(aes(x = datetime_est, y = wl_below_surface_m))+
    ####      geom_point(aes(color = outlier2))+
    ####      geom_line(aes(y = rollmean))+
    ####      facet_grid(site ~ plot)
    ####    
    ####    library(ggh4x) # for nested facets
    ####    troll_outlier %>% 
    ####      #filter(!site == "GCW") %>%
    ####      mutate(rollmean = case_when(site == "GCW" ~ NA, .default = rollmean)) %>% 
    ####      filter(is.na(outlier2)) %>% 
    ####      ggplot(aes(x = datetime_est, y = wl_below_surface_m, color = plot))+
    ####      geom_hline(yintercept = 0, alpha = 0.8)+
    ####     # geom_point(aes(color = outlier2))+
    ####      geom_line(aes(y = rollmean), linewidth = 1)+
    ####    #  facet_wrap(~site)+
    ####    #  facet_grid(site ~ plot)+ 
    ####      facet_nested_wrap(~region + site,
    ####                        nest_line = element_line(colour = "grey"))+ 
    ####      theme_kp()+
    ####      theme(panel.grid = element_blank())
    ####    
    ####    
    ####    
    ####    
    ####    ###############
    ####    ##############
    ####    
    ####    
    ####    files_to_read <- list.files(paste0(temp_storage, "raw_files"), pattern = "OWC", full.names = T)
    ####    
    ####    #  variables_to_keep = c("gw_pressure", "gw_density", "soil_vwc_10cm")
    ####    
    ####    read_file <- function(file){
    ####      arrow::read_parquet(file) %>%
    ####        clean_names() %>%
    ####        mutate(datetime_est = force_tz(timestamp, tzone = common_tz)) %>% 
    ####        mutate(instrument_id = as.character(instrument_id)) %>% 
    ####        dplyr::select(datetime_est, site, plot, research_name, instrument_id, value) %>% 
    ####        force()
    ####    }
    ####    
    ####    raw_data <- files_to_read %>%
    ####      map(read_file) %>%
    ####      bind_rows()
