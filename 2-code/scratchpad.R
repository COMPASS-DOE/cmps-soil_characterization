

loi %>% 
  filter(percent_om < 25) %>% 
  group_by(site, transect) %>% 
  dplyr::summarise(mean = mean(percent_om))

moisture %>% 
  filter(site == "OWC") %>% 
  group_by(site, transect, horizon) %>% 
  dplyr::summarise(mean = mean(gwc_perc))

pH %>% 
  filter(site == "OWC") %>% 
  group_by(site, transect, horizon) %>% 
  dplyr::summarise(mean = mean(pH))


tctnts %>% 
  filter(site == "OWC") %>% 
  group_by(site, transect, horizon) %>% 
  dplyr::summarise(mean = mean(TC_perc))

tctnts %>% 
  filter(site == "OWC") %>% 
  filter(TN_perc < 5) %>% 
  group_by(site, transect, horizon) %>% 
  dplyr::summarise(mean = mean(TN_perc))

tctnts %>% 
  filter(site == "OWC") %>% 
  group_by(site, transect, horizon) %>% 
  dplyr::summarise(mean = mean(TS_perc))




icp_data= import_icp_data(FILEPATH = "1-data/icp")







FILEPATH = "1-data/icp"
analysis_key = read.csv("1-data/analysis_key.csv")
sample_key = read.csv("1-data/sample_key.csv")





ferrozine =   
  samples %>% 
  left_join(sample_key)


ferrozine %>% 
  mutate(Fe_2_3 = Fe2/Fe3) %>% 
  filter(Fe_2_3 > 0) %>% 
  ggplot(aes(x = site, y = Fe_2_3, color = transect, shape = horizon))+
  geom_point(position = position_dodge(width = 0.4),
             size = 2)+
  facet_wrap(~region, scales = "free_x")


ferrozine %>% 
  
  ggplot(aes(x = site, y = Fe_total, color = transect, shape = horizon))+
  geom_point(position = position_dodge(width = 0.4),
             size = 2)+
  facet_wrap(~region, scales = "free_x")



# -------------------------------------------------------------------------
## ICR

source("2-code/c-fticrrr-functions.R")
icr_report= import_fticr_data(FILEPATH = "1-data/icr")
icr_meta= make_fticr_meta(icr_report)$meta2
icr_data_long= make_fticr_data(icr_report, analysis_key, sample_key)$data_long_blank_corrected
icr_data_trt= make_fticr_data(icr_report, analysis_key, sample_key)$data_long_trt



# -------------------------------------------------------------------------

# mehlich calibrations

mehlich = readxl::read_excel("1-data/phosphorus-mehlich/20221220_COMPASS_Mehlich_recalibration.xlsx", skip = 25)
ppm = readxl::read_excel("1-data/phosphorus-mehlich/20221220_COMPASS_Mehlich_recalibration.xlsx", sheet = "ppm")

mehlich2 = 
  mehlich %>% 
  rename(letter = `...1`) %>% 
  janitor::clean_names() %>% 
  fill(letter) %>% 
  filter(x14 == "880") %>% 
  dplyr::select(-x14) %>% 
  pivot_longer(-letter) %>% 
  mutate(name = str_remove(name, "x"),
         well = paste0(letter, name),
         value = as.numeric(value)) %>% 
  left_join(ppm %>% mutate(name = as.character(name)))

mehlich2 %>% 
  filter(ppm < 5 & ppm > 0.1) %>% 
  ggplot(aes(x = ppm, y = value, color = letter))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~letter)





  mehlich2 %>% 
    filter(ppm < 5 & ppm > 0.1) %>% 
  dplyr::group_by(letter) %>% 
  dplyr::summarize(slope = lm(value ~ ppm)$coefficients["ppm"], 
                   intercept = lm(value ~ ppm)$coefficients["(Intercept)"],
                   rsquared = summary(lm(value ~ ppm))$adj.r.squared)

  
  
  data_combined_clean_surface %>% 
    filter(`Fe (ICP)` > 0) %>% 
    ggplot(aes(x = `Fe (ICP)`, y = `Fe3 (Ferrozine)`))+
    geom_point()




  

# OWC data ----------------------------------------------------------------

owc_data = 
    data_combined %>% 
    filter(site == "OWC")
owc_data %>% write.csv("owc_data_2022-12-28.csv", row.names = F, na = "")  
