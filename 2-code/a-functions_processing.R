


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


# loss on ignition
process_loi = function(loi_data){
  
  #loi = 
  loi_data %>%
    mutate_at(vars(contains("wt_")), as.numeric) %>% 
  mutate(percent_om = ((wt_tray_drysoil_g - wt_tray_combustedsoil_g)/(wt_tray_drysoil_g - wt_tray_g))*100,
         percent_om = round(percent_om, 2))  
  
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
    dplyr::select(-Memo) %>% 
    mutate(Name = str_replace(Name, "MAY_2022", "May22"))
  
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
import_din_data = function(FILEPATH){
  
  filePaths_din <- list.files(path = FILEPATH, pattern = "din", full.names = TRUE)
  din_dat <- do.call(bind_rows, lapply(filePaths_din, function(path) {
    df <- read_csv(path)
    df}))
  
  
}
process_din = function(din_data, analysis_key, moisture_processed, subsampling){
  
  din_processed = 
    din_data %>% 
    rename(analysis_ID = `Customer ID #`,
           NO3N_mgL = `NO3-N (ppm)`,
           NH4N_mgL = `NH4-N (ppm)`) %>% 
    mutate(analysis_ID = str_pad(analysis_ID, 4, pad = "0"),
           analysis_ID = paste0("NIT_CMPS_KFP_", analysis_ID),
           NO3N_mgL = as.numeric(NO3N_mgL),
           NH4N_mgL = as.numeric(NH4N_mgL)) %>% 
    left_join(analysis_key %>% dplyr::select(analysis_ID, sample_label)) %>%
    mutate(blank_NO3N_mgL = case_when(sample_label == "blank-filter" ~ NO3N_mgL),
           blank_NH4N_mgL = case_when(sample_label == "blank-filter" ~ NH4N_mgL))
  
  din_samples = 
    din_processed %>% 
    filter(grepl("COMPASS", sample_label)) %>% 
    dplyr::select(sample_label, NO3N_mgL, NH4N_mgL) %>% 
    
    # join gwc and subsampling weights to normalize data to soil weight
    left_join(moisture_processed) %>% 
    left_join(subsampling %>% dplyr::select(notes, sample_label, NH4_NO3_g)) %>% 
    rename(fm_g = NH4_NO3_g) %>% 
    mutate(od_g = fm_g/((gwc_perc/100)+1),
           soilwater_g = fm_g - od_g,
           no3n_ug_g = NO3N_mgL * ((25 + soilwater_g)/od_g),
           no3n_ug_g = round(no3n_ug_g, 2),
           nh4n_ug_g = NH4N_mgL * ((25 + soilwater_g)/od_g),
           nh4n_ug_g = round(nh4n_ug_g, 2)) %>% 
    dplyr::select(sample_label, NO3N_mgL, NH4N_mgL, no3n_ug_g, nh4n_ug_g) %>% 
    arrange(sample_label)
  
  din_samples
}

# ICP
import_icp_data = function(FILEPATH){
  
  filePaths_icp <- list.files(path = FILEPATH, pattern = "csv", full.names = TRUE)
  icp_dat <- do.call(bind_rows, lapply(filePaths_icp, function(path) {
    df <- read_csv(path, skip = 3)
    df}))
  
}

process_icp = function(icp_data, moisture_processed, subsampling){
  
  icp_processed = 
    icp_data %>% 
    rename(sample = `...2`) %>% 
    dplyr::select(-starts_with("...")) %>% 
    filter(grepl("PNNL", sample)) %>% 
    mutate(sample = str_remove(sample, "PNNL "),
           sample = str_pad(sample, 4, pad = "0"),
           analysis_ID = paste0("CAT_CMPS_KFP_", sample)) %>% 
    dplyr::select(-sample) %>% 
    arrange(analysis_ID) %>% 
    left_join(analysis_key %>% dplyr::select(analysis_ID, sample_label)) %>% 
    relocate(analysis_ID, sample_label) 
  
  icp_samples = 
    icp_processed %>% 
    pivot_longer(cols = c(Na:S), names_to = "species", values_to = "ppm") %>% 
    left_join(moisture_processed) %>% 
    left_join(subsampling %>% dplyr::select(sample_label, base_cations_g)) %>% 
    rename(fm_g = base_cations_g) %>% 
    mutate(ppm = as.numeric(ppm),
           od_g = fm_g/((gwc_perc/100)+1),
           soilwater_g = fm_g - od_g,
           ug_g = ppm * ((25 + soilwater_g)/od_g),
           ug_g = round(ug_g, 2)) %>% 
    dplyr::select(sample_label, species, ppm, ug_g) %>% 
    arrange(sample_label) %>% 
    pivot_longer(-c(sample_label, species)) %>% 
    mutate(name = paste0(species, "_", name)) %>% 
    dplyr::select(-species) %>% 
    filter(!grepl("blank", sample_label)) %>% 
    pivot_wider()

  icp_samples
}




FILEPATH = "1-data/icp"
analysis_key = read.csv("1-data/analysis_key.csv")
sample_key = read.csv("1-data/sample_key.csv")
