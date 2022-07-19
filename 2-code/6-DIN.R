# SOIL CHARACTERIZATION FOR WLE AND CB SITES

## 6-DIN.R
## This script will import and clean data for extractable/dissolved inorganic N (NH4 + NO3)

## KFP, July 2022

######################## ####
######################## ####

# load packages -----------------------------------------------------------
source("2-code/0-packages.R")

#
# import data -------------------------------------------------------------
din_data = readxl::read_xlsx("1-data/223013_Patel_Preliminary_NO3.xlsx", sheet = "NO3-N data")
gwc = read.csv("1-data/processed/gravimetric_water.csv")
subsampling = read.csv("1-data/subsampling_weights.csv")
analysis_key = read.csv("1-data/analysis_key.csv")
sample_key = read.csv("1-data/sample_key.csv")

#

# process data ------------------------------------------------------------
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
  left_join(gwc) %>% 
  left_join(subsampling %>% dplyr::select(notes, sample_label, NH4_NO3_g)) %>% 
  rename(fm_g = NH4_NO3_g) %>% 
  mutate(od_g = fm_g/((gwc_perc/100)+1),
         soilwater_g = fm_g - od_g,
         no3_ug_g = NO3_mgL * ((25 + soilwater_g)/od_g),
         no3_ug_g = round(no3_ug_g, 2)) %>% 
  dplyr::select(sample_label, NO3_mgL, no3_ug_g) %>% 
  arrange(sample_label)


#

# graphs ------------------------------------------------------------------

din_samples_key = 
  din_samples %>% 
  left_join(sample_key) %>% 
  filter(!is.na(no3_ug_g))

din_samples_key %>% 
  ggplot(aes(x = site, y = no3_ug_g, color = transect, shape = horizon))+
  geom_point(position = position_dodge(width = 0.4), size = 2)+
  facet_wrap(region ~ ., scales = "free_x")

#
# export ------------------------------------------------------------------
din_samples %>% write.csv("1-data/processed/inorganic_n_2022-07-18.csv", row.names = F, na = "")

