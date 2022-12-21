


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
    dplyr::select(-Memo) %>% 
    mutate(Name = str_replace(Name, "MAY22", "May22"),
           Name = str_replace(Name, "AUG22", "Aug22"))
  
  ts_samples = 
    ts_data %>% 
    filter(grepl("COMPASS_", Name) & !Memo %in% c("skip")) %>% 
    dplyr::select(-Memo) %>% 
    mutate(Name = str_replace(Name, "MAY_2022", "May22"),
           Name = str_replace(Name, "AUG22", "Aug22"),
           Name = str_remove(Name, "KFP_"))
  
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
           blank_NH4N_mgL = case_when(sample_label == "blank-filter" ~ NH4N_mgL)) %>% 
    # some samples were below-detect (0.2 ppm)
    # set these values to half-detect
    mutate(no3_flag = case_when(is.na(NO3N_mgL) ~ "below dl"),
           NO3N_mgL = case_when(is.na(NO3N_mgL) ~ 0.10, TRUE ~ NO3N_mgL))
  
  din_samples = 
    din_processed %>% 
    filter(grepl("COMPASS", sample_label)) %>% 
    dplyr::select(sample_label, NO3N_mgL, NH4N_mgL, no3_flag) %>% 
    
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
    dplyr::select(sample_label, NO3N_mgL, NH4N_mgL, no3n_ug_g, nh4n_ug_g, no3_flag) %>% 
    arrange(sample_label)
  
  din_samples
}

# ICP
import_icp_data = function(FILEPATH){
  
  filePaths_icp <- list.files(path = FILEPATH, pattern = "csv", full.names = TRUE)
  icp_dat <- do.call(bind_rows, lapply(filePaths_icp, function(path) {
    df <- read_csv(path, skip = 3) %>% mutate_all(as.character)
    df}))
  
}

process_icp = function(icp_data, analysis_key, moisture_processed, subsampling){
  
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


# Iron - ferrozine
import_iron = function(FILEPATH){
  
  # import map
  cb = read_sheet("1pzvGUvjK6qV8BVYSfoc2cid88dx3v7ZOD7TnYFgyWbc", sheet = "cb") %>% mutate_all(as.character) %>% mutate(region = "CB")
  wle = read_sheet("1pzvGUvjK6qV8BVYSfoc2cid88dx3v7ZOD7TnYFgyWbc", sheet = "wle") %>% mutate_all(as.character) %>% mutate(region = "WLE")
  ferrozine_map = bind_rows(cb, wle)
  
  # import data files (plate reader)
  filePaths_ferrozine <- list.files(path = FILEPATH, pattern = "csv", full.names = TRUE, recursive = TRUE)
  ferrozine_data <- do.call(bind_rows, lapply(filePaths_ferrozine, function(path) {
    df <- read.csv(path, skip = 24, header = TRUE) %>% mutate_all(as.character) %>% janitor::clean_names()
    df = df %>% mutate(source = basename(path))
    df}))
  
  list(ferrozine_map = ferrozine_map,
       ferrozine_data = ferrozine_data)
  
}


process_iron = function(ferrozine_map, ferrozine_data, moisture_processed, subsampling){
  
  # clean the map
  map_processed = 
    ferrozine_map %>% 
    mutate(tray_number = parse_number(tray_number)) %>% 
    filter(!is.na(sample_name) & !is.na(tray_number)) %>% 
    rename(sample_label = sample_name)
  
  # clean the data
  data_processed = 
    ferrozine_data %>% 
    mutate_all(na_if,"") %>% 
    dplyr::select(-x) %>% 
    fill(x_1) %>% 
    filter(x_2 == "562") %>% 
    dplyr::select(-x_2) %>% 
    pivot_longer(-c(source, x_1), values_to = "absorbance_562") %>% 
    mutate(name = str_remove(name, "x"),
           well_position = paste0(x_1, name),
           region = str_extract(source, regex("cb|wle", ignore_case = TRUE)),
           region = toupper(region),
           tray_number = str_extract(source, "tray|plate[1-9]+"),
           tray_number = parse_number(tray_number),
           absorbance_562 = as.numeric(absorbance_562)) %>% 
    dplyr::select(region, tray_number, well_position, absorbance_562) %>% 
    right_join(map_processed, by = c("region", "tray_number", "well_position")) %>% 
    filter(!notes %in% "skip")
  
  calibrate_ferrozine_data = function(data_processed){
    standards = 
      data_processed %>% 
      filter(grepl("standard", sample_label)) %>% 
      dplyr::select(region, tray_number, absorbance_562, standard_ppm) %>% 
      mutate(standard_ppm = as.numeric(standard_ppm))
    
    standards %>% 
      ggplot(aes(x = standard_ppm, y = absorbance_562, color = as.character(tray_number)))+
      geom_point()+
      geom_smooth(method = "lm", se = F)+
      facet_wrap(~region + tray_number)
    
    calibration_coef = 
      standards %>% 
      dplyr::group_by(region, tray_number) %>% 
      dplyr::summarize(slope = lm(absorbance_562 ~ standard_ppm)$coefficients["standard_ppm"], 
                       intercept = lm(absorbance_562 ~ standard_ppm)$coefficients["(Intercept)"])
      
      # y = mx + c
      # abs = m*ppm + c
      # ppm = abs-c/m
      
    # data_processed2 = 
      data_processed %>% 
      left_join(calibration_coef) %>% 
      mutate(ppm_calculated = ((absorbance_562 - intercept) / slope))
      
    }

    samples = 
      calibrate_ferrozine_data(data_processed) %>% 
      filter(grepl("COMPASS", sample_label)) %>% 
      dplyr::select(region, sample_label, analysis, ppm_calculated) %>% 
      mutate(analysis = recode(analysis, "Fe2 (water only)" = "Fe2", "total-Fe (ascorbic)" = "Fe_total")) %>% 
      pivot_wider(names_from = "analysis", values_from = "ppm_calculated") %>% 
      mutate(across(where(is.numeric), round, 2)) %>% 
      mutate(Fe3 = Fe_total - Fe2)
      
    samples2 = 
      samples %>% 
      dplyr::select(sample_label, starts_with("Fe")) %>% 
      pivot_longer(cols = starts_with("Fe"), names_to = "species", values_to = "ppm") %>% 
      left_join(moisture_processed) %>% 
      left_join(subsampling %>% dplyr::select(sample_label, iron_g)) %>% 
      rename(fm_g = iron_g) %>% 
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
    
    samples2
  }

# Water retention curves
import_wrc_data = function(FILEPATH){
  
  filePaths_wrc <- list.files(path = FILEPATH, pattern = "xlsx", full.names = TRUE, recursive = FALSE)
  wrc_data <- do.call(bind_rows, lapply(filePaths_wrc, function(path) {
    
    # importing both, the evaluated values and the fitted values
    df_eval <- readxl::read_excel(path, sheet = "Evaluation-Retention Θ(pF)") %>% mutate_all(as.character) %>% janitor::clean_names()
    df_eval = df_eval %>% mutate(source = basename(path)) %>% dplyr::select(p_f, water_content_vol_percent, source) %>% rename(pf_eval = p_f)
    
    df_fit <- readxl::read_excel(path, sheet = "Fitting-Retention Θ(pF)") %>% mutate_all(as.character) %>% janitor::clean_names()
    df_fit = df_fit %>% mutate(source = basename(path)) %>% dplyr::select(p_f, water_content_vol_percent, source) %>% rename(pf_fit = p_f)
    
    df <- full_join(df_eval, df_fit)
    df
  }
  
  ))

}
#wrc_data = import_wrc_data(FILEPATH)

process_wrc = function(wrc_data){
  
  wrc_processed <- 
    wrc_data %>% 
    mutate(source = str_remove(source, ".xlsx")) %>% 
    separate(source, sep = "_", into = c("EC", "kit", "site", "transect")) %>% 
    dplyr::select(site, transect, water_content_vol_percent, starts_with("pf")) %>% 
    mutate_at(vars(starts_with("pf")), as.numeric) %>% 
    mutate_at(vars(starts_with("water")), as.numeric) %>% 
    mutate(transect = tolower(transect),
           kpa_eval = round((10^pf_eval)/10,2),
           kpa_fit = round((10^pf_fit)/10,2))
  
  
 wrc_processed %>% 
    filter(pf_fit >= 0 | pf_eval >= 0) %>% 
    ggplot(aes(y = water_content_vol_percent))+
    geom_line(aes(x = pf_fit))+
    geom_point(aes(x = pf_eval), shape = 1)+
    facet_wrap(~site+transect)
 
 
 wrc_processed %>% 
   filter(pf_fit >= 0 | pf_eval >= 0) %>% 
   ggplot(aes(y = water_content_vol_percent, color = transect))+
   geom_line(aes(x = kpa_fit/1000))+
   geom_point(aes(x = kpa_eval/1000), shape = 1)+
   scale_x_log10(labels = scales::comma)+
   facet_wrap(~site)
  
  
  
  
  
}

# Mehlich-P
import_mehlich = function(FILEPATH){

  mehlich_map = read_sheet("1qBXwZhjwc4ane7Wq8dfqu7ioVbnU4c_nEHwIAqRayPs", na = "") %>% mutate_all(as.character) 
  mehlich_map[mehlich_map == "NULL"] <- NA
  
  filePaths_mehlich <- list.files(path = FILEPATH, pattern = "csv", full.names = TRUE, recursive = TRUE)
  mehlich_data <- do.call(bind_rows, lapply(filePaths_mehlich, function(path) {
    df <- read.csv(path, skip = 22, header = TRUE) %>% mutate_all(as.character) %>% janitor::clean_names()
    df = df %>% mutate(source = basename(path))
    df}))
  
  
  list(mehlich_map = mehlich_map,
       mehlich_data = mehlich_data)
}
#mehlich_map = import_mehlich(FILEPATH)$mehlich_map
#mehlich_data = import_mehlich(FILEPATH)$mehlich_data

process_mehlich = function(mehlich_map, mehlich_data, moisture_processed, subsampling){
  
  # clean the map
  map_processed = 
    mehlich_map %>% 
    fill(date_run, plate) %>% 
    mutate(plate = parse_number(plate)) %>% 
    dplyr::select(-notes) %>% 
    pivot_longer(-c(date_run, plate, letter), names_to = "number", values_to = "sample_label") %>% 
    drop_na() %>% 
    filter(letter != "letter") %>% 
    mutate(sample_type = case_when(grepl("ppm", sample_label) ~ "standard",
                                   grepl("blank", sample_label) ~ "blank",
                                   TRUE ~ "sample"),
           well_position = paste0(letter, number)) %>%
    rename(tray_number = plate) %>% 
    dplyr::select(tray_number, well_position, sample_label, sample_type)
  
  
  # clean the data
  data_processed = 
    mehlich_data %>% 
    dplyr::select(-x) %>% 
    fill(x_1) %>% 
    filter(x_2 == "880") %>% 
    dplyr::select(-x_2) %>% 
    pivot_longer(-c(source, x_1), values_to = "absorbance_880") %>% 
    mutate(name = str_remove(name, "x"),
           well_position = paste0(x_1, name),
           tray_number = str_extract(source, "tray|plate[0-9]+"),
           tray_number = parse_number(tray_number),
           absorbance_880 = as.numeric(absorbance_880)) %>% 
    dplyr::select(tray_number, well_position, absorbance_880) %>% 
    right_join(map_processed, by = c("tray_number", "well_position")) 

  calibrate_mehlich_data = function(data_processed){
    standards = 
      data_processed %>% 
      filter(grepl("standard", sample_type)) %>% 
      dplyr::select(tray_number, absorbance_880, sample_label) %>%
      mutate(standard_ppm = parse_number(sample_label))
    
    standards %>% 
      filter(standard_ppm < 2) %>% 
      ggplot(aes(x = standard_ppm, y = absorbance_880, color = as.character(tray_number)))+
      geom_point()+
      geom_smooth(method = "lm", se = F)+
      facet_wrap(~tray_number)
    
    calibration_coef123 = 
      standards %>% 
      dplyr::group_by(tray_number) %>% 
      dplyr::summarize(slope = lm(absorbance_880 ~ standard_ppm)$coefficients["standard_ppm"], 
                       intercept = lm(absorbance_880 ~ standard_ppm)$coefficients["(Intercept)"],
                       rsquared = summary(lm(absorbance_880 ~ standard_ppm))$adj.r.squared)
    
    calibration_coef0 = 
      standards %>% 
      #dplyr::group_by(tray_number) %>% 
      dplyr::summarize(slope = lm(absorbance_880 ~ standard_ppm)$coefficients["standard_ppm"], 
                       intercept = lm(absorbance_880 ~ standard_ppm)$coefficients["(Intercept)"],
                       rsquared = summary(lm(absorbance_880 ~ standard_ppm))$adj.r.squared) %>% 
      mutate(tray_number = 0)
    
    calibration_coef = 
      rbind(calibration_coef123, calibration_coef0)
    
    # y = mx + c
    # abs = m*ppm + c
    # ppm = abs-c/m
    
    # data_processed2 = 
    data_processed %>% 
      left_join(calibration_coef) %>% 
      mutate(ppm_calculated = ((absorbance_880 - intercept) / slope))
    
  }
  
  samples = 
    calibrate_mehlich_data(data_processed) %>% 
    filter(grepl("COMPASS", sample_label)) %>% 
    dplyr::select(sample_label, ppm_calculated) %>% 
    force()
  
  samples2 = 
    samples %>% 
    left_join(moisture_processed) %>% 
    left_join(subsampling %>% dplyr::select(sample_label, starts_with("mehlich"))) %>% 
    rename(fm_g = mehlichp_g,
           extractant_mL = mehlichp_mL) %>% 
    mutate(ppm = as.numeric(ppm_calculated),
           od_g = fm_g/((gwc_perc/100)+1),
           soilwater_g = fm_g - od_g,
           ug_g = ppm * ((extractant_mL + soilwater_g)/od_g),
           ug_g = round(ug_g, 2)) %>% 
    dplyr::select(sample_label, ppm, ug_g) %>% 
    rename(mehlichp_ppm = ppm,
           mehlichp_ugg = ug_g) %>% 
    arrange(sample_label) %>% 
   force()
  
  samples2
  
}

# Ions (IC)
import_ions = function(FILEPATH){
  
  anions <- 
    list.files(path=FILEPATH, pattern = c("anion", ".csv"), full.names = TRUE) %>% 
    lapply(read_csv, skip = 5, id = "source") %>% 
    bind_rows %>% 
    rename(analysis_ID = "...3")
  
  cations <- 
    list.files(path=FILEPATH, pattern = c("cation", ".csv"), full.names = TRUE) %>% 
    lapply(read_csv, skip = 5, id = "source") %>% 
    bind_rows %>% 
    rename(analysis_ID = "...3")
  
  ions <- 
    full_join(cations, anions, by = "analysis_ID")
  
  ions
}
#ions_data = import_ions(FILEPATH = "1-data/ions")

process_ions = function(ions_data, analysis_key, sample_key, moisture_processed, subsampling){
  
  ions = 
    ions_data %>% 
    mutate_all(as.character) %>% 
    pivot_longer(-c(analysis_ID, source.x, source.y)) %>% 
    mutate(date_run = str_extract(source.x, "[0-9]{8}"),
           date_run = lubridate::ymd(date_run)) %>% 
    dplyr::select(-starts_with("source")) %>% 
    filter(!grepl("...[0-9]", name)) %>% 
    filter(!name %in% c("Nitrate", "Nitrite")) %>% 
    mutate(name = str_remove(name, " UV"),
           value = as.numeric(value)) %>% 
    filter(!is.na(value)) %>% 
    rename(ppm = value,
           ion = name)
  
  samples = 
    ions %>% 
    filter(grepl("DOC_CMPS", analysis_ID))
  
  standards = 
    ions %>% 
    filter(grepl("x", analysis_ID))
  
  standards_max = 
    standards %>% 
    group_by(ion) %>% 
    dplyr::summarise(max = max(ppm))

  high_samples = 
    samples %>% 
    left_join(standards_max) %>% 
    mutate(HIGH = ppm > max) %>% 
    filter(HIGH)
    
#  high_samples %>% write.csv("high.csv")

  dilutions = 
    read_sheet("1s82bKl85AmqWerNpvGQv-ddgFQpjyFoMeRW9QdZmRZw", sheet = "dilutions") %>% 
    mutate_all(as.character) %>% 
    dplyr::select(sample_label, dilution_factor) %>% 
    mutate(dilution_factor = as.numeric(dilution_factor))
  
  samples2 = 
    samples %>% 
    left_join(analysis_key %>% dplyr::select(analysis_ID, sample_label)) %>%
    filter(grepl("COMPASS_", sample_label)) %>% 
    # do blank/dilution correction
    left_join(dilutions, by = "sample_label") %>% 
    mutate(dilution_factor = replace_na(dilution_factor, 1),
           ppm_dil_corrected = ppm * dilution_factor,
           ppm_dil_corrected = round(ppm_dil_corrected, 2)) %>% 
    # join gwc and subsampling weights to normalize data to soil weight
    left_join(moisture_processed) %>% 
    left_join(subsampling %>% dplyr::select(notes, sample_label, WSOC_g)) %>% 
    rename(fm_g = WSOC_g) %>% 
    mutate(od_g = fm_g/((gwc_perc/100)+1),
           soilwater_g = fm_g - od_g,
           ug_g = ppm_dil_corrected * ((40 + soilwater_g)/od_g),
           ug_g = round(ug_g, 2)) %>% 
    dplyr::select(sample_label, ion, ppm_dil_corrected, ug_g) %>% 
    rename(ppm = ppm_dil_corrected) %>% 
    pivot_longer(-c(sample_label, ion)) %>% 
    mutate(ion = paste0(ion, "_", name)) %>% 
    dplyr::select(sample_label, ion, value) %>% 
    pivot_wider(names_from = "ion")
    
  samples2
  }

