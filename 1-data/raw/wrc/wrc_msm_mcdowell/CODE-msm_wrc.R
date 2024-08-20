library(tidyverse)
options(scipen = 9999)
theme_set(theme_bw(base_size = 14))

#R code created on January 19, 2024, by Roberta Peixoto. 

# calculating soil water potential based on VWC and water retential curves https://www.nature.com/articles/s41598-019-54449-8
#van Genuchten
# SWC from continuous TEROS data and coefs from water release curves = Kaizad
# unities conversion to give us SWP (hPa or cm(H2O)) - SWC have to be in % , but our TEROS values are in m3/m3

calculate_SWP <- function( mean_VWC, location ) {
  if (location  == "UP") {
    alpha <- 0.1996                 # cm-1
    n <- 1.316                      # admensionless
    theta_r <- 0 * 100              # converting cm3/cm3 to %
    theta_s <- 0.497 * 100          # converting cm3/cm3 to %
    m <- 1-(1/1.316)                  # m = 1-(1/n) #van Genuchten
  } else if (location  == "TR") {
    alpha <- 0.021                   # cm-1
    n <- 1.542                      # admensionless
    theta_r <- 0 * 100               # converting cm3/cm3 to %
    theta_s <- 0.782 * 100           # converting cm3/cm3 to %
    m <- 1-(1/1.542)                  # m = 1-(1/n) #van Genuchten
  }
  
  # Calculate SWP
  SWP <-  ((( (  (theta_s - theta_r) /  (mean_VWC*100  - theta_r) )  ^ (1/m))- 1) /(alpha^n)) ^(1 / n)
  return(SWP)
}



# KFP code ----------------------------------------------------------------
# January 2024
# Kaizad F. Patel

# wrc parameters ----
# parameters for all 6 synoptic sites
parameters = read.csv("1-data/wrc/wrc_msm_mcdowell/wrc_parameters_all_sites.csv")

# recreating the wrc with the parameters
# (reverse checking to make sure it looks correct)
testing = 
  runif(200, min = 0, max = 0.9) %>% 
  as_tibble() %>% 
  rename(vwc_percent = value) %>% 
  cross_join(parameters) %>% 
  filter(vwc_percent < theta_s) %>% 
  mutate(theta_r = 0) %>% 
  mutate(    m = abs(m),
             term_a = (theta_s - theta_r)/(vwc_percent - theta_r),
             term_b = term_a^(1/m),
             numerator = (term_b - 1)^(1/n),
             recalc_cm = numerator/alpha,
             potential_cm = ((((theta_s - theta_r)/(vwc_percent - theta_r))^(1/m) - 1)^(1/n))/alpha,
         potential_kpa = potential_cm/10)

testing %>% 
  ggplot(aes(x = potential_kpa, y = vwc_percent, color = location))+
  geom_point()+
  facet_wrap(~site)+
  scale_x_log10(limits = c(0.05, 1000000))
  
#
# MSM TEROS data ----
df_MSM = read.csv("1-data/wrc/wrc_msm_mcdowell/df_msm_TEROS.csv")

df_processed = 
  df_MSM %>% 
  filter(location %in% c("UP", "TR")) %>% 
  dplyr::select(site, location, depth_cm, date, mean_VWC) %>% 
  mutate(date = as.Date(date)) %>%  # cm3/cm3
  left_join(parameters) %>% 
  mutate(potential_cm = ((((theta_s - theta_r)/(mean_VWC - theta_r))^(1/m) - 1)^(1/n))/alpha,
         potential_pF = log10(potential_cm),
         potential_kPa = potential_cm / 10,
         potential_MPa = potential_kPa/1000)

df_processed_units = 
  df_MSM %>% 
  filter(location %in% c("UP", "TR")) %>% 
  dplyr::select(site, location, depth_cm, date, mean_VWC) %>% 
  mutate(date = as.Date(date)) %>%  # cm3/cm3
  group_by(site, location, depth_cm) %>% 
  mutate(max_min = case_when(mean_VWC == min(mean_VWC) ~ "lowest moisture",
                             mean_VWC == max(mean_VWC) ~ "highest moisture")) %>% 
  filter(!is.na(max_min)) %>% 
  left_join(parameters) %>% 
  mutate(potential_cm = ((((theta_s - theta_r)/(mean_VWC - theta_r))^(1/m) - 1)^(1/n))/alpha,
         potential_hPa = potential_cm,
         potential_kPa = potential_hPa/10
  )



gg_potential = 
  df_processed %>% 
  ggplot(aes(x = date, y = potential_kPa, color = as.character(depth_cm)))+
  geom_line()+
  facet_wrap(~location)+
  theme_bw()+
  labs(title = "MSM water potential",
       color = "depth, cm")

gg_vwc = 
  df_processed %>% 
  ggplot(aes(x = date, y = mean_VWC, color = as.character(depth_cm)))+
  geom_line()+
  facet_wrap(~location)+
  theme_bw()+
  labs(title = "MSM water content",
       color = "depth, cm")

cowplot::plot_grid(gg_potential, gg_vwc, nrow = 2)


#
# MSM wrc ----
# checking with the wrc

wrc = read.csv("1-data/wrc/wrc_msm_mcdowell/wrc_msm_curves.csv")
wrc_processed = 
  wrc %>% 
  janitor::clean_names() %>% 
  mutate(mean_VWC = water_content_vol/100,
         location = case_match(zone, "upland" ~ "UP", "transition" ~ "TR")) %>% 
  # recalculating kPa values from
  left_join(parameters %>% filter(site == "MSM")) %>% 
  mutate(
    term_a = (theta_s - theta_r)/(mean_VWC - theta_r),
    term_b = term_a^(1/m),
    numerator = (term_b - 1)^(1/n),
    recalc_cm = numerator/alpha,
    potential_cm = ((((theta_s - theta_r)/(mean_VWC - theta_r))^(1/m) - 1)^(1/n))/alpha,
    cm = k_pa * 10) %>% 
  dplyr::select(zone, location, type, mean_VWC, p_f, k_pa, cm, recalc_cm, potential_cm) %>% 
  mutate(recalc_pF =  log10(recalc_cm),
         recalc_kPa_from_pF = (10^recalc_pF)/10,
         recalc_kPa_from_cm = recalc_cm/10
)

# overlaying TEROS data onto wrc 

gg1 = wrc_processed %>% 
  filter(type == "fitted") %>% 
  ggplot()+
  geom_line(aes(x = (recalc_kPa_from_cm), y = mean_VWC), color = "blue")+
  geom_point(data = df_processed, aes(x = (potential_kPa), y = mean_VWC, color = as.character(depth_cm)), 
             shape = 21, size = 4, alpha = 0.5)+
  facet_wrap(~location)+
  scale_x_log10(labels = scales::comma)+
  labs(x = "kPa",
       y = "VWC, %",
       title = "MSM data plotted onto water retention curves",
       color = "depth, cm")+
  theme_bw()

gg2 = wrc_processed %>% 
  filter(type == "fitted") %>% 
  ggplot()+
  geom_line(aes(x = (recalc_kPa_from_cm), y = mean_VWC), color = "blue")+
  geom_point(data = df_processed, aes(x = (potential_kPa), y = mean_VWC, color = as.character(depth_cm)), 
             shape = 21, size = 4, alpha = 0.5)+
  facet_wrap(~location)+
  xlim(0, 20)+
  #scale_x_log10(labels = scales::comma)+
  labs(x = "kPa",
       y = "VWC, %",
       title = "MSM data plotted onto water retention curves (zoomed on x-axis)",
       color = "depth, cm")+
  theme_bw()

cowplot::plot_grid(gg1, gg2, nrow = 2)
# the solid line represents the entire water retention curve, from saturation to the dry end
# the dots represent values from the field (TEROS vwc data, calculated into potential)
# the measured data fit onto the wrc
