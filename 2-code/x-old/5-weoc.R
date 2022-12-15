# SOIL CHARACTERIZATION FOR WLE AND CB SITES

## 5-weoc.R
## This script will import and clean data for water extractable organic carbon (WEOC), measured as NPOC (non-purgeable organic carbon)

## KFP, July 2022

######################## ####
######################## ####

# load packages -----------------------------------------------------------
source("2-code/0-packages.R")

#
# import data -------------------------------------------------------------
npoc_data_wle = read_tsv("1-data/npoc/20220616_Summary_Raw_COMPASS_toledo.txt", skip = 10)
npoc_data_cb = read_tsv("1-data/npoc/20220720_Summary_Raw_COMPASS_cb.txt", skip = 10)
gwc = read.csv("1-data/processed/gravimetric_water.csv")
subsampling = read.csv("1-data/subsampling_weights.csv")
analysis_key = read.csv("1-data/analysis_key.csv")
sample_key = read.csv("1-data/sample_key.csv")
#

# process data ------------------------------------------------------------
npoc_processed = 
  npoc_data_wle %>%
  bind_rows(npoc_data_cb) %>% 
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
  left_join(gwc) %>% 
  left_join(subsampling %>% dplyr::select(notes, sample_label, WSOC_g)) %>% 
  rename(fm_g = WSOC_g) %>% 
  mutate(od_g = fm_g/((gwc_perc/100)+1),
         soilwater_g = fm_g - od_g,
         npoc_ug_g = npoc_corr_mgL * ((40 + soilwater_g)/od_g),
         npoc_ug_g = round(npoc_ug_g, 2)) %>% 
  dplyr::select(sample_label, npoc_mgL, npoc_ug_g, notes)
  
npoc_samples = 
  npoc_processed %>% 
  filter(grepl("COMPASS", sample_label))

#

# graphs ------------------------------------------------------------------
npoc_samples_key = 
  npoc_samples %>% 
  left_join(sample_key, by = "sample_label") %>% 
    filter(!is.na(npoc_ug_g))

npoc_samples_key %>% 
  ggplot(aes(x = site, y = npoc_ug_g, color = transect, shape = horizon))+
  geom_point(position = position_dodge(width = 0.4), size = 2)+
  facet_wrap(region ~ ., scales = "free_x")


#
# export ------------------------------------------------------------------
npoc_samples %>% write.csv("1-data/processed/weoc.csv", row.names = F, na = "")
