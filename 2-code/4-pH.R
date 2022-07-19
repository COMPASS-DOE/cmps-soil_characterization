# SOIL CHARACTERIZATION FOR WLE AND CB SITES

## 4-pH.R
## This script will import and clean data for pH and specific conductance

## KFP, July 2022

######################## ####
######################## ####

# load packages -----------------------------------------------------------
source("2-code/0-packages.R")

#
# import data -------------------------------------------------------------
pH_data = read_sheet("177ZR7O9JpsgVmFfLh72LfJ72oM-oTc6k_r1qE0XHL64") %>% mutate_all(as.character())

#
# process data ------------------------------------------------------------
pH_samples = 
  pH_data %>% 
  dplyr::select(sample_label, pH, specific_conductance_ms_cm) %>% 
  mutate_at(vars(-sample_label), as.numeric)  %>% 
  filter(grepl("COMPASS", sample_label)) %>% 
  filter(!is.na(pH) | !is.na(specific_conductance_ms_cm))

#
# graphs ------------------------------------------------------------------

sample_key = read.csv("1-data/sample_key.csv")

data_with_key =
  pH_samples %>% 
  left_join(sample_key)


data_with_key %>% 
  ggplot(aes(x = site, y = pH, color = transect, shape = horizon))+
  geom_point(position = position_dodge(width = 0.4),
             size = 2)+
  facet_wrap(region ~ ., scales = "free_x")


data_with_key %>% 
  ggplot(aes(x = site, y = specific_conductance_ms_cm, color = transect, shape = horizon))+
  geom_point(position = position_dodge(width = 0.4),
             size = 2)+
  facet_wrap(region ~ ., scales = "free_x")


#
# export ------------------------------------------------------------------
pH_samples %>% write.csv("1-data/processed/pH_sp_cond.csv", row.names = F, na = "")


