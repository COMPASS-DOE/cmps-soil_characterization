# SOIL CHARACTERIZATION FOR WLE AND CB SITES

## 3-tctnts.R
## This script will import and clean data for total C/N/S

## KFP, July 2022

######################## ####
######################## ####

# load packages -----------------------------------------------------------
source("2-code/0-packages.R")

#

# import data -------------------------------------------------------------
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
    df <- read.csv(path, header=TRUE, check.names = F, sep= "\t", skip = 1)
    df %<>%
      dplyr::select(Name, `S  [%]`, Memo) %>% 
      rename(TS_perc = `S  [%]`) %>% 
      mutate(Name = str_replace(Name, "DEC", "Dec")) %>% 
      #mutate(source = rep(path, nrow(.))) %>% 
      force()
    df}))
  
}

tctn_data = import_tctn_data(FILEPATH = "1-data/tctnts")
ts_data = import_ts_data(FILEPATH = "1-data/tctnts")

#
# process data ------------------------------------------------------------
tctn_samples = 
  tctn_data %>% 
  filter(grepl("COMPASS_", Name) & !Memo %in% c("skip")) %>% 
  dplyr::select(-Memo)

ts_samples = 
  ts_data %>% 
  filter(grepl("COMPASS_", Name)) %>% 
  dplyr::select(-Memo)


tctnts_samples = 
  tctn_samples %>% 
  left_join(ts_samples) %>% 
  rename(sample_label = Name)

#

# graphs ------------------------------------------------------------------
sample_key = read.csv("1-data/sample_key.csv")

data_with_key =
  tctnts_samples %>% 
  left_join(sample_key)


data_with_key %>% 
  ggplot(aes(x = site, y = TC_perc, color = transect, shape = horizon))+
  geom_point(position = position_dodge(width = 0.4),
             size = 2)+
  facet_wrap(region ~ ., scales = "free_x")


data_with_key %>% 
  ggplot(aes(x = site, y = TN_perc, color = transect, shape = horizon))+
  geom_point(position = position_dodge(width = 0.4),
             size = 2)+
  facet_wrap(region ~ ., scales = "free_x")

data_with_key %>% 
  ggplot(aes(x = site, y = TS_perc, color = transect, shape = horizon))+
  geom_point(position = position_dodge(width = 0.4),
             size = 2)+
  facet_wrap(region ~ ., scales = "free_x")
#

# export ------------------------------------------------------------------
tctnts_samples %>% write.csv("1-data/processed/total_CNS.csv", row.names = F, na = "")

