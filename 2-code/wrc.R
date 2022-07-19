# SOIL CHARACTERIZATION FOR WLE AND CB SITES

## wrc.R
## This script will import and clean data for water retention curves

## KFP, July 2022

######################## ####
######################## ####

# load packages -----------------------------------------------------------
source("2-code/0-packages.R")

#
# import data -------------------------------------------------------------
import_wrc_data = function(FILEPATH){
  
  filePaths_wrc <- list.files(path = FILEPATH, pattern = "*.xlsx", full.names = TRUE)
  wrc_dat <- do.call(bind_rows, lapply(filePaths_wrc, function(path) {
    df <- readxl::read_xlsx(path, sheet = "Fitting-Retention Θ(pF)")
    df %<>%
      rename(pF = `pF [-]`,
             vol_water_perc = `Water Content [Vol%]`) %>% 
      mutate(source = rep(path, nrow(.))) %>% 
      force()
    df}))
  
}

wrc_data = import_wrc_data("1-data/wrc")

#
# process data ------------------------------------------------------------
wrc_processed = 
  wrc_data %>% 
  separate(source, sep = "_EC1_", into = c("delete", "name")) %>% 
  separate(name, sep = "_", into = c("EC_kit", "site", "transect")) %>% 
  mutate(transect = str_remove(transect, ".xlsx"),
         kPa = round((10^pF)/10,2)) %>% 
  dplyr::select(-delete)

#

# graphs ------------------------------------------------------------------
wrc_processed %>% 
  filter(EC_kit != "K008") %>% 
  filter(EC_kit == c("K004", "K011")) %>% 
  ggplot(aes(x = kPa/1000, y = vol_water_perc, colour = transect))+
  geom_line()+
  scale_x_log10()+
  facet_wrap(~site)

#

# export ------------------------------------------------------------------
wrc_processed %>% write.csv("1-data/processed/wrc_2022-07-18.csv", row.names = F, na = "")

