# SOIL CHARACTERIZATION FOR WLE AND CB SITES

## 1-initial_processing.R
## Use this script to download data and metadata files from Google Drive,
## and process/clean the data for further processing and analysis.

## KFP, July 2022

######################## ####
######################## ####

# load packages -----------------------------------------------------------
source("2-code/0-packages.R")

#

# download sample data files ----------------------------------------------
## sample key --------------------------------------------------------------
## load and clean sample key
sample_key = read_sheet("1ThSoogME7LvBl5U5FSi7sm601WvlV2fKrBsi43g7kvk") %>% mutate_all(as.character)
sample_key_subset = 
  sample_key %>% 
  dplyr::select(sample_label, region, site, transect, tree_number, horizon) %>% 
  filter(!is.na(sample_label)) %>% 
  mutate(transect = tolower(transect),
         transect = recode(transect, "wc" = "wetland"),
         site = recode(site, "PR" = "PTR", "CC" = "CRC", "GCREW" = "GCW"),
         region = recode(region, "WLE" = "Erie", "CB" = "Chesapeake"))
sample_key_subset[sample_key_subset == "NULL"] <- NA

## export
sample_key_subset %>% write.csv("1-data/sample_key.csv", row.names = FALSE, na = "")

#
## subsampling weights -----------------------------------------------------
## load and clean subsampling weight
subsampling_weights = read_sheet("12mTWytgKryo5CkKVOba32XWhwzhvsADsaXp2A25wyA0", sheet = "subsampling_weights") %>% mutate_all(as.character)
subsampling_weights_subset = 
  subsampling_weights %>% 
  filter(!is.na(site)) %>% 
  dplyr::select(sample_label, ends_with(c("_g", "mL")), notes) %>% 
  mutate_at(vars(ends_with("_g")), as.numeric)

## export
subsampling_weights_subset %>% write.csv("1-data/subsampling_weights.csv", row.names = F, na = "")

#
## analysis key ------------------------------------------------------------
## load and clean analysis key
analysis_key = read_sheet("12mTWytgKryo5CkKVOba32XWhwzhvsADsaXp2A25wyA0", sheet = "analysis_key") %>% mutate_all(as.character)
analysis_key[analysis_key == "NULL"] <- NA

## export
analysis_key %>% write.csv("1-data/analysis_key.csv", row.names = F, na = "")

#