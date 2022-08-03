


# import function(s) ------------------------------------------------------
## import meta-data/data files from Google Drive 

import_gsheet = function(dat){
  googlesheets4::read_sheet(dat) %>% mutate_all(as.character)
}


#

# refactor/reorder functions ----------------------------------------------
## functions to set the order of factors

reorder_horizon = function(dat){
  dat %>% 
    mutate(horizon = factor(horizon, levels = c("O", "A", "B")))
}

reorder_transect = function(dat){
  dat %>% 
    mutate(transect = factor(transect, levels = c("upland", "transition", "wte", "wc")))
}


# process data ------------------------------------------------------------
# Moisture
process_moisture = function(moisture_dat){
#  moisture_processed = 
    moisture_dat %>% 
    # keep only relevant columns
    dplyr::select(sample_label, starts_with("wt")) %>% 
    # make all (most) columns numeric 
    mutate_at(vars(-sample_label), as.numeric)  %>% 
    filter(!is.na(sample_label)) %>% 
    # calculate GWC
    # ((wet-dry)/dry) * 100
    mutate(gwc_perc = 100 * (wt_tray_moist_soil_g - wt_tray_dry_soil_g)/(wt_tray_dry_soil_g - wt_tray_g),
           gwc_perc = round(gwc_perc, 2)) %>% 
    dplyr::select(sample_label, gwc_perc)
  
  
  #
}


# pH
process_pH = function(pH_data){
#pH_samples = 
  pH_data %>% 
  dplyr::select(sample_label, pH, specific_conductance_ms_cm) %>% 
  mutate_at(vars(-sample_label), as.numeric)  %>% 
  filter(grepl("COMPASS", sample_label)) %>% 
  filter(!is.na(pH) | !is.na(specific_conductance_ms_cm))
}


# TCTN-TS
import_tctn_data = function(FILEPATH){
  
  filePaths_tctn <- list.files(path = FILEPATH, pattern = "CN", full.names = TRUE)
  tctn_dat <- do.call(bind_rows, lapply(filePaths_tctn, function(path) {
    df <- read.csv(path, header=TRUE, check.names = F)
    df %<>%
      dplyr::select(Name, `C  [%]`, `N  [%]`, Memo) %>% 
      rename(TC_perc = `C  [%]`,
             TN_perc = `N  [%]`) %>% 
      ## this is a ONE-TIME CORRECTION because of the way the sample names were entered
      mutate(Name = str_replace(Name, "TCTN_CMPS_KFP_0", "COMPASS_Dec2021_")) %>% 
      #mutate(source = rep(path, nrow(.))) %>% 
      force()
    df}))
  
}
import_ts_data = function(FILEPATH){
  
  filePaths_ts <- list.files(path = FILEPATH, pattern = "totalS", full.names = TRUE)
  ts_dat <- do.call(bind_rows, lapply(filePaths_ts, function(path) {
    df <- read.csv(path, header=TRUE, check.names = F)
    df %<>%
      dplyr::select(Name, `S  [%]`, Memo) %>% 
      rename(TS_perc = `S  [%]`) %>% 
      mutate(Name = str_replace(Name, "DEC", "Dec")) %>% 
      #mutate(source = rep(path, nrow(.))) %>% 
      force()
    df}))
  
}

# tctn_data = import_tctn_data(FILEPATH = "1-data/tctnts")
# ts_data = import_ts_data(FILEPATH = "1-data/tctnts")

process_tctnts = function(tctn_data, ts_data){
  
  tctn_samples = 
    tctn_data %>% 
    filter(grepl("COMPASS_", Name) & !Memo %in% c("skip")) %>% 
    dplyr::select(-Memo)
  
  ts_samples = 
    ts_data %>% 
    filter(grepl("COMPASS_", Name)) %>% 
    dplyr::select(-Memo)
  
  # tctnts_samples = 
    tctn_samples %>% 
    left_join(ts_samples) %>% 
    rename(sample_label = Name) %>% 
      arrange(sample_label)
  
}


# WEOC
import_weoc_data = function(FILEPATH, PATTERN){
  
  filePaths_weoc <- list.files(path = FILEPATH, pattern = PATTERN, full.names = TRUE)
  weoc_dat <- do.call(bind_rows, lapply(filePaths_weoc, function(path) {
    df <- read_tsv(path, skip = 10)
    df}))
  
  
}
process_weoc = function(weoc_data, analysis_key, moisture_processed, subsampling){
  
  npoc_processed = 
    weoc_data %>% 
    # remove skipped samples
    filter(!`Sample ID` %in% "skip") %>% 
    # keep only relevant columns and rename them
    dplyr::select(`Sample Name`, `Result(NPOC)`) %>% 
    rename(analysis_ID = `Sample Name`,
           npoc_mgL = `Result(NPOC)`) %>% 
    # keep only sampple rows 
    filter(grepl("DOC_", analysis_ID)) %>% 
    # join the analysis key to get the sample_label
    left_join(analysis_key %>% dplyr::select(analysis_ID, sample_label, NPOC_dilution)) %>%
    # do blank/dilution correction
    mutate(blank_mgL = case_when(sample_label == "blank-filter" ~ npoc_mgL)) %>% 
    fill(blank_mgL, .direction = c("up")) %>% 
    mutate(NPOC_dilution = as.numeric(NPOC_dilution),
           npoc_corr_mgL = (npoc_mgL) * NPOC_dilution) %>% 
    # join gwc and subsampling weights to normalize data to soil weight
    left_join(moisture_processed) %>% 
    left_join(subsampling %>% dplyr::select(notes, sample_label, WSOC_g)) %>% 
    rename(fm_g = WSOC_g) %>% 
    mutate(od_g = fm_g/((gwc_perc/100)+1),
           soilwater_g = fm_g - od_g,
           npoc_ug_g = npoc_corr_mgL * ((40 + soilwater_g)/od_g),
           npoc_ug_g = round(npoc_ug_g, 2)) %>% 
    dplyr::select(sample_label, npoc_corr_mgL, npoc_ug_g, notes)
  
  npoc_samples = 
    npoc_processed %>% 
    filter(grepl("COMPASS", sample_label))
  
  npoc_samples
}


# DIN
process_din = function(din_data, analysis_key, moisture_processed, subsampling){
  
  din_processed = 
    din_data %>% 
    rename(analysis_ID = `Customer ID #`,
           NO3_mgL = `NO3-N (ppm raw extract)`) %>% 
    mutate(analysis_ID = str_pad(analysis_ID, 4, pad = "0"),
           analysis_ID = paste0("NIT_CMPS_KFP_", analysis_ID),
           NO3_mgL = as.numeric(NO3_mgL)) %>% 
    left_join(analysis_key %>% dplyr::select(analysis_ID, sample_label)) %>%
    mutate(blank_mgL = case_when(sample_label == "blank-filter" ~ NO3_mgL))
  
  din_samples = 
    din_processed %>% 
    filter(grepl("COMPASS", sample_label)) %>% 
    dplyr::select(sample_label, NO3_mgL) %>% 
    
    # join gwc and subsampling weights to normalize data to soil weight
    left_join(moisture_processed) %>% 
    left_join(subsampling %>% dplyr::select(notes, sample_label, NH4_NO3_g)) %>% 
    rename(fm_g = NH4_NO3_g) %>% 
    mutate(od_g = fm_g/((gwc_perc/100)+1),
           soilwater_g = fm_g - od_g,
           no3_ug_g = NO3_mgL * ((25 + soilwater_g)/od_g),
           no3_ug_g = round(no3_ug_g, 2)) %>% 
    dplyr::select(sample_label, NO3_mgL, no3_ug_g) %>% 
    arrange(sample_label)
  
  din_samples
}