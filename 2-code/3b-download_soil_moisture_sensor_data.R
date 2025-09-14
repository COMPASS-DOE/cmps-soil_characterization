

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