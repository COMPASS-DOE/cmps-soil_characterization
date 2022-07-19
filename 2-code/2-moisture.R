# SOIL CHARACTERIZATION FOR WLE AND CB SITES

## 2-moisture.R
## This script will import Gravimetric Water Content data from Google Drive
## and compute gravimetric water content.

## KFP, July 2022

######################## ####
######################## ####

# load packages -----------------------------------------------------------
source("2-code/0-packages.R")

#
# import and process moisture data ----------------------------------------
moisture_dat = read_sheet("1nYzExPmpv01IYalqo2L3pmDsBDvhjdvHYMJo31N5_MA") %>% mutate_all(as.character)

moisture_processed = 
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
# graphs ------------------------------------------------------------------
# join sample key
sample_key = read.csv("1-data/sample_key.csv")

moisture =
  moisture_processed %>% 
  left_join(sample_key)

moisture %>% 
  ggplot(aes(x = site, y = gwc_perc, color = transect, shape = horizon))+
  geom_point(position = position_dodge(width = 0.4),
             size = 2)+
  facet_wrap(region ~ ., scales = "free_x")

#
# export gwc data ---------------------------------------------------------
moisture_processed %>% write.csv("1-data/processed/gravimetric_water.csv", row.names = FALSE, na = "")
