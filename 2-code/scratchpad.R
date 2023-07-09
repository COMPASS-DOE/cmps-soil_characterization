

# -------------------------------------------------------------------------


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


# -------------------------------------------------------------------------

# comparing different analyses
# 
data_combined_long <- 
  data_combined_clean_surface %>% 
  pivot_longer(-c(sample_label, region, site, transect, tree_number, horizon))


data_combined_clean_surface %>% 
  filter(site != "GCREW") %>% 
  # filter(region != "CB") %>% 
  # filter(grepl("Calcium|Ca ", name)) %>% 
  ggplot(aes(x = `Na (ICP)`, y = `Sodium (IC)`,
             color = transect, shape = region))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", aes(group= 1))
ylim(0, 200)+xlim(0, 10)


# -------------------------------------------------------------------------

# combining NH4N and NO3N graphs

tar_load(data_combined)
combined_surface = 
  data_combined %>% 
  filter(horizon != "B") %>% 
  force() 

din <- combined_surface %>% filter(analysis == "DIN")

gg_din_nh4n <- 
  ggplot(data = din %>% filter(name == "nh4n_ug_g"),
         aes(x = transect,
             y = value,
             group= site,
             color = horizon,
             shape = site,
             #alpha = horizon
         )) +
  # plot points
  geom_point(size = 2, stroke = 1,
             position = position_dodge(width = 0.4))+
  scale_alpha_manual(values = c(1, 0.3))+
  scale_color_manual(values = c("grey60", "black"))+
  scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,3,4,5,6))+
  facet_wrap(~region, scales = "free_x",
             labeller = as_labeller(c("CB" = "Chesapeake Bay", "WLE" = "Lake Erie")))+
  theme_kp()+
  labs(x = "",
       y = "NH4-N, μg/g \n",
       # title = TITLE,
       #subtitle = SUBTITLE,
       shape = "Site",
       alpha = "Horizon",
       color = "Horizon")+
  theme(plot.background = element_rect(fill = NA, color = "black"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.spacing.x = unit(2, "lines"))+
  theme(legend.position = "right", legend.box = "vertical")+
  NULL


gg_din_no3n <- 
  ggplot(data = din %>% filter(name == "no3n_ug_g"),
         aes(x = transect,
             y = value,
             group= site,
             color = horizon,
             shape = site,
             #alpha = horizon
         )) +
  # plot points
  geom_point(size = 2, stroke = 1,
             position = position_dodge(width = 0.4))+
  scale_alpha_manual(values = c(1, 0.3))+
  scale_color_manual(values = c("grey60", "black"))+
  scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,3,4,5,6))+
  facet_wrap(~region, scales = "free_x",
             labeller = as_labeller(c("CB" = "Chesapeake Bay", "WLE" = "Lake Erie")))+
  theme_kp()+
  labs(x = "",
       y = "NO3-N, μg/g \n",
       # title = TITLE,
       #subtitle = SUBTITLE,
       shape = "Site",
       alpha = "Horizon",
       color = "Horizon")+
  theme(plot.background = element_rect(fill = NA, color = "black"),
        panel.background = element_blank(),
        #panel.border = element_blank(),
        panel.spacing.x = unit(2, "lines")
  )+
  theme(legend.position = "right", legend.box = "vertical")+
  NULL


library(patchwork)
gg_din_nh4n / gg_din_no3n + plot_layout(guides = "collect") & theme(legend.position = "right")


#
# #############

mehlich = data = combined_surface %>% filter(name == "mehlichp_ugg")
mehlich_mixing = 
  mehlich %>% 
  group_by(region, site, transect, horizon) %>% 
  dplyr::summarise(mean = mean(value)) %>% 
  pivot_wider(names_from = "transect", values_from = "mean") %>% 
  dplyr::select(-wte) %>% 
  mutate(prop_upland = (transition - wetland)/(upland - wetland),
         prop_wetland = (transition - upland)/(wetland - upland))


nitrate <- combined_surface %>% filter(name == "no3n_ug_g")
nitrate_mixing <- 
  nitrate %>% 
  group_by(region, site, transect, horizon) %>% 
  dplyr::summarise(mean = mean(value)) %>% 
  pivot_wider(names_from = "transect", values_from = "mean") %>% 
  dplyr::select(-wte) %>% 
  mutate(prop_upland = (transition - wetland)/(upland - wetland),
         prop_wetland = (transition - upland)/(wetland - upland))


ions <- combined_surface %>% filter(analysis == "IC")
sulfate <- combined_surface %>% filter(name == "Sulfate_ug_g")
sulfate_mixing <- 
  sulfate %>% 
  group_by(region, site, transect, horizon) %>% 
  dplyr::summarise(mean = mean(value)) %>% 
  pivot_wider(names_from = "transect", values_from = "mean") %>% 
  dplyr::select(-wte) %>% 
  mutate(prop_upland = (transition - wetland)/(upland - wetland),
         prop_wetland = (transition - upland)/(wetland - upland))



# testing graphs ---- 

compute_aov = function(dat){
  
  a = aov(value ~ site, data = dat) 
  broom::tidy(a) %>% 
    filter(term == "site") %>% 
    rename(p_value = `p.value`) %>% 
    mutate(label = case_when(p_value <= 0.05 ~ "*"),
           p_value = round(p_value, 3)) %>% 
    dplyr::select(term, p_value) %>% 
    force()
}
compute_hsd = function(dat){
  a = aov(value ~ site, data = dat)
  h = agricolae::HSD.test(a, "site")
  h$groups %>% as.data.frame() %>% rownames_to_column("site")
}

# mehlich 
mehlich_wle <- 
  data_combined %>% 
  filter(analysis == "Mehlich3") %>% 
  filter(horizon != "B" & region == "WLE" & transect != "wte")

mehlich_aov = 
  mehlich_wle %>% 
  group_by(transect, horizon) %>% 
  do(compute_aov(.))

mehlich_hsd = 
  mehlich_wle %>% 
  group_by(transect, horizon) %>% 
  do(compute_hsd(.))


## BOXPLOTS FOR ALL
mehlich_wle %>% 
  ggplot(aes(x = transect, y = value, 
             color = site, group = site)) +
  geom_boxplot(aes(group = interaction(transect, site),
                   fill = site), 
               color = "black", alpha = 0.4,
               outlier.alpha = 0,
               width = 0.4, 
               position = position_dodge(width = 0.5))+
  geom_point(size = 2, stroke = 1, color = "black",
             position = position_dodge(width = 0.5)
  )+
  geom_text(data = mehlich_hsd, aes(y = 670, label = groups),
            color = "black", position = position_dodge(width = 0.5))+
  scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,5,4,5,6))+
  scale_color_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(7,2,5,4,5,6))+
  scale_fill_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(7,2,5,4,5,6))+
  facet_wrap(~region, #scales = SCALES,
             labeller = as_labeller(c("CB" = "Chesapeake Bay", "WLE" = "Lake Erie")))+
  theme_kp()+
  labs(x = "", y = "P, ug/g",
       title = "WLE-P",
       subtitle = "option 1: boxplot for all",
       shape = "Site")+
  theme(legend.position = "right", legend.box = "vertical")+
  NULL

## BOXPLOTS FOR SIG ONLY
mehlich_wle2 <- 
  mehlich_wle %>% 
  left_join(mehlich_aov) %>% 
  mutate(GROUP = case_when(p_value <= 0.05 ~ site,
                           TRUE ~ NA_character_))

mehlich_wle2 %>% 
  ggplot(aes(x = transect, y = value, 
             color = site, group = site)) +
  geom_boxplot(aes(group = interaction(transect, GROUP),
                   fill = GROUP), 
               color = "black", alpha = 0.4,
               outlier.alpha = 0,
               width = 0.4, 
               position = position_dodge(width = 0.5))+
  geom_point(size = 2, stroke = 1, color = "black",
             position = position_dodge(width = 0.5)
  )+
  geom_text(data = mehlich_hsd, aes(y = 670, label = groups),
            color = "black", position = position_dodge(width = 0.5))+
  scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,5,4,5,6))+
  scale_color_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(7,2,5,4,5,6))+
  scale_fill_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(7,2,5,4,5,6))+
  facet_wrap(~region, #scales = SCALES,
             labeller = as_labeller(c("CB" = "Chesapeake Bay", "WLE" = "Lake Erie")))+
  theme_kp()+
  labs(x = "", y = "P, ug/g",
       title = "WLE-P",
       subtitle = "option 2: boxplot only if sites are significant",
       shape = "Site")+
  theme(legend.position = "right", legend.box = "vertical")+
  NULL


##

mehlich_wle %>% 
  ggplot(aes(x = transect, y = value, 
             color = site, shape = site)) +
  geom_boxplot(aes(group = transect),
               outlier.alpha = 0,
               width = 0.1, 
               position = position_nudge(x = -0.1)
               )+
  geom_point(size = 2, stroke = 1,
            # position = position_nudge(x = 0.1),
             position = position_dodge(width = 0.5)
  )+
  scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,3,4,5,6))+
  facet_wrap(~region, #scales = SCALES,
             labeller = as_labeller(c("CB" = "Chesapeake Bay", "WLE" = "Lake Erie")))+
  theme_kp()+
  labs(x = "", y = "P, ug/g",
       title = "WLE-P",
       shape = "Site")+
  theme(legend.position = "right", legend.box = "vertical")+
  NULL

## JITTER + MEAN-SE
mehlich_wle %>% 
  ggplot(aes(x = transect, y = value, 
             color = site, shape = site))+
  stat_summary(fun = mean, geom = "point", size = 3, stroke = 1,
               position = position_nudge(x = 0.2), show.legend = F)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, linewidth = 1,
               position = position_nudge(x = 0.2), show.legend = F) +
  geom_jitter(width = 0.05, size = 2, stroke = 1)+
  labs(title = "P - WLE",
       subtitle = "A horizon",
       x = "",
       y = "P, ug/g")+
  scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,5,4,5,6))+
  scale_color_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(7,2,5,4,5,6))+
  NULL
  
# 
## BOXPLOTS FOR ALL, WITH OVERALL STATS
mehlich_aov_overall = aov(value ~ transect, data = mehlich_wle)
mehlich_hsd_overall = agricolae::HSD.test(mehlich_aov_overall, "transect")
mehlich_lme_overall = nlme::lme(value ~ transect, random = ~1|site, data = mehlich_wle) %>% anova(.)

mehlich_wle %>% 
  ggplot(aes(x = transect, y = value, 
             color = site, group = site)) +
  geom_boxplot(aes(group = interaction(transect, site),
                   fill = site), 
               color = "black", alpha = 0.4,
               outlier.alpha = 0,
               width = 0.4, 
               position = position_dodge(width = 0.5))+
  geom_point(size = 2, stroke = 1, color = "black",
             position = position_dodge(width = 0.5)
  )+
  geom_text(data = mehlich_hsd, aes(y = 670, label = groups),
            color = "black", position = position_dodge(width = 0.5))+
  scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,5,4,5,6))+
  scale_color_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(7,2,5,4,5,6))+
  scale_fill_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(7,2,5,4,5,6))+
  facet_wrap(~region, #scales = SCALES,
             labeller = as_labeller(c("CB" = "Chesapeake Bay", "WLE" = "Lake Erie")))+
  theme_kp()+
  labs(x = "", y = "P, ug/g",
       title = "WLE-P",
       subtitle = "option 1: boxplot for all",
       shape = "Site")+
  theme(legend.position = "right", legend.box = "vertical")+
  NULL






# CEC
cec_wle <- 
  data_combined %>% 
  filter(analysis == "ICP") %>% 
  filter(horizon != "B" & region == "WLE" & transect != "wte")  %>% 
  filter(grepl("Ca|Na|K", name))

cec_aov = 
  cec_wle %>% 
  group_by(transect, horizon, name) %>% 
  do(compute_aov(.))

cec_hsd = 
  cec_wle %>% 
  group_by(transect, horizon, name) %>% 
  do(compute_hsd(.))

# JITTER + MEAN-SE
cec_wle %>% 
  ggplot(aes(x = transect, y = value, 
             color = site, shape = site))+
  stat_summary(fun = mean, geom = "point", size = 3, stroke = 1,
               position = position_nudge(x = 0.2), show.legend = F)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, linewidth = 1,
               position = position_nudge(x = 0.2), show.legend = F) +
  
  geom_jitter(width = 0.05, size = 2, stroke = 1)+
  scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,5,4,5,6))+
  scale_color_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,5,4,5,6))+
  facet_wrap(~name, scales = "free_y")+
  NULL

## BOXPLOT FOR ALL
cec_wle %>% 
  ggplot(aes(x = transect, y = value, 
             color = site, group = site)) +
  geom_boxplot(aes(group = interaction(transect, site),
                   fill = site), 
               color = "black", alpha = 0.4,
               outlier.alpha = 0,
               width = 0.4, 
               position = position_dodge(width = 0.5))+
  geom_point(size = 2, stroke = 1, color = "black",
             position = position_dodge(width = 0.5)
  )+
  geom_text(data = cec_hsd, aes(y = 200, label = groups),
            color = "black", position = position_dodge(width = 0.5))+
  scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,5,4,5,6))+
  scale_color_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(7,2,5,4,5,6))+
  scale_fill_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(7,2,5,4,5,6))+
  facet_wrap(~name, scales = "free_y", ncol = 2)+
  theme_kp()+
  labs(x = "", y = "ug/g",
       title = "WLE-Cations",
       subtitle = "option 1: boxplot for all",
       shape = "Site")+
  theme(legend.position = "right", legend.box = "vertical")+
  NULL

## BOXPLOTS FOR SIG ONLY
cec_wle2 <- 
  cec_wle %>% 
  left_join(cec_aov) %>% 
  mutate(GROUP = case_when(p_value <= 0.05 ~ site,
                           TRUE ~ NA_character_))

cec_wle2 %>% 
  ggplot(aes(x = transect, y = value, 
             color = site, group = site)) +
  geom_boxplot(aes(group = interaction(transect, GROUP),
                   fill = GROUP), 
               color = "black", alpha = 0.4,
               outlier.alpha = 0,
               width = 0.4, 
               position = position_dodge(width = 0.5))+
  geom_point(size = 2, stroke = 1, color = "black",
             position = position_dodge(width = 0.5)
  )+
  geom_text(data = cec_hsd, aes(y = 200, label = groups),
            color = "black", position = position_dodge(width = 0.5))+
  scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,5,4,5,6))+
  scale_color_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(7,2,5,4,5,6))+
  scale_fill_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(7,2,5,4,5,6))+
  facet_wrap(~name, scales = "free_y", ncol = 2)+
  theme_kp()+
  labs(x = "", y = "ug/g",
       title = "WLE-Cations",
       subtitle = "option 2: boxplot only if sites are significant",
       shape = "Site")+
  theme(legend.position = "right", legend.box = "vertical")+
  NULL

###
###

# CEC-CB
cec_cb <- 
  data_combined %>% 
  filter(analysis == "ICP") %>% 
  filter(horizon != "B" & region == "CB" & transect != "wte")  %>% 
  filter(grepl("Ca|Na|K", name))

cec_cb_aov = 
  cec_cb %>% 
  filter(horizon == "O") %>% 
  group_by(transect, horizon, name) %>% 
  do(compute_aov(.))

cec_hsd = 
  cec_wle %>% 
  group_by(transect, horizon, name) %>% 
  do(compute_hsd(.))

# JITTER + MEAN-SE
cec_wle %>% 
  ggplot(aes(x = transect, y = value, 
             color = site, shape = site))+
  stat_summary(fun = mean, geom = "point", size = 3, stroke = 1,
               position = position_nudge(x = 0.2), show.legend = F)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, linewidth = 1,
               position = position_nudge(x = 0.2), show.legend = F) +
  
  geom_jitter(width = 0.05, size = 2, stroke = 1)+
  scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,5,4,5,6))+
  scale_color_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,5,4,5,6))+
  facet_wrap(~name, scales = "free_y")+
  NULL

## BOXPLOT FOR ALL
cec_cb %>% 
  ggplot(aes(x = transect, y = value, 
             color = site, group = site)) +
  geom_boxplot(aes(group = interaction(transect, site),
                   fill = site), 
               color = "black", alpha = 0.4,
               outlier.alpha = 0,
               width = 0.4, 
               position = position_dodge(width = 0.5))+
  geom_point(size = 2, stroke = 1, color = "black",
             position = position_dodge(width = 0.5)
  )+
  scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,5,4,5,6))+
  scale_color_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(7,2,5,4,5,6))+
  scale_fill_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(7,2,5,4,5,6))+
  #facet_wrap(~name, scales = "free_y", ncol = 2)+
  facet_grid(name ~ horizon, scales = "free_y")+
  theme_kp()+
  labs(x = "", y = "ug/g",
       title = "CB-Cations",
       subtitle = "option 1: boxplot for all, separate panels for horizons",
       shape = "Site")+
  theme(legend.position = "right", legend.box = "vertical")+
  NULL

## BOXPLOTS FOR SIG ONLY
cec_wle2 <- 
  cec_wle %>% 
  left_join(cec_aov) %>% 
  mutate(GROUP = case_when(p_value <= 0.05 ~ site,
                           TRUE ~ NA_character_))

cec_wle2 %>% 
  ggplot(aes(x = transect, y = value, 
             color = site, group = site)) +
  geom_boxplot(aes(group = interaction(transect, GROUP),
                   fill = GROUP), 
               color = "black", alpha = 0.4,
               outlier.alpha = 0,
               width = 0.4, 
               position = position_dodge(width = 0.5))+
  geom_point(size = 2, stroke = 1, color = "black",
             position = position_dodge(width = 0.5)
  )+
  geom_text(data = cec_hsd, aes(y = 200, label = groups),
            color = "black", position = position_dodge(width = 0.5))+
  scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,5,4,5,6))+
  scale_color_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(7,2,5,4,5,6))+
  scale_fill_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(7,2,5,4,5,6))+
  facet_wrap(~name, scales = "free_y", ncol = 2)+
  theme_kp()+
  labs(x = "", y = "ug/g",
       title = "WLE-Cations",
       subtitle = "option 2: boxplot only if sites are significant",
       shape = "Site")+
  theme(legend.position = "right", legend.box = "vertical")+
  NULL

## sites as color

plot_site_as_color = function(data, YLAB = "", TITLE = "", SUBTITLE = "", SCALES = "free_x"){
  
  ggplot(data,
         aes(x = transect,
             y = value,
             group= site,
             color = site,
             #shape = horizon,
             #alpha = horizon
         )) +
    # plot points
    geom_point(size = 2.5, stroke = 1,
               position = position_dodge(width = 0.4))+
    scale_alpha_manual(values = c(1, 0.3))+
#    scale_color_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c('#002c82', '#4a9eba', '#82dac2', '#ffab71', '#db5843', '#93003a'))+
    scale_color_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c('#ED6e85', '#ffc115', '#7f4420', '#1fee9f', '#9cbd4d', '#0268b6'))+
    #scale_shape_manual(breaks = c("CC", "PR", "OWC", "MSM", "GWI", "GCREW"), values = c(1,2,3,4,5,6))+
    facet_wrap(~region, scales = SCALES,
               labeller = as_labeller(c("CB" = "Chesapeake Bay", "WLE" = "Lake Erie")))+
    theme_kp()+
    labs(x = "",
         y = YLAB,
         title = TITLE,
         subtitle = SUBTITLE,
         shape = "Site",
         alpha = "Horizon",
         color = "Site")+
    theme(legend.position = "right", legend.box = "vertical")+
    NULL
  
}


combined_surface = 
  data_combined %>% 
  filter(transect != "wte") %>% 
  filter(horizon != "B") %>% 
  force() 

gwc <- combined_surface %>% filter(analysis == "GWC")
(gg_gwc <- plot_site_as_color(data = gwc, YLAB = "Gravimetric water, %", TITLE = "Gravimetric Water Content"))

pH <- combined_surface %>% filter(analysis == "PH")
(gg_ph <- plot_site_as_color(data = pH %>% filter(name == "pH"), YLAB = "pH", TITLE = "pH"))
(gg_sp_conduc <- plot_site_as_color(data = pH %>% filter(grepl("spConductance", name)), 
                                   YLAB = "Specific Conductance μS/cm", TITLE = "Specific Conductance"))

nlme::lme(value ~ transect, random =~1|site, data = combined_surface %>% 
            filter(grepl("spConductance_mscm", name) & region == "CB" & transect != "wetland")) %>% 
  anova()


lmer = lme4::lmer(value ~ transect + (1|site), data = combined_surface %>% 
             filter(grepl("spConductance_mscm", name) & region == "CB"))


library(multcomp)
summary(glht(lmer, linfct = mcp(transect = "Tukey")), test = adjusted("holm"))

library(emmeans)
library(multcompView)
inter.test1 <- emmeans(lmer, "transect")
cld(inter.test1, Letter="abcdefg")

##
##

#gg_p_wle = 
  data_combined %>% 
  filter(region == "WLE") %>% 
  filter(analysis == "Mehlich3") %>% 
  plot_site_as_color(YLAB = "P, ug/g")+
  geom_text(data = lme_for_graphs %>% mutate(site = "OWC") %>% filter(analysis == "Mehlich3" & region == "WLE"), aes(y = y, label = label), color = "black")

gg_icp = 
  data_combined %>% 
  filter(region == "WLE") %>% 
  filter(analysis == "ICP") %>% 
  filter(grepl("cec|ca|k_|na", name, ignore.case = T)) %>% 
  plot_site_as_color(TITLE = "WLE")+facet_wrap(~ name, scales = "free_y")

remotes::install_github('kevinblighe/PCAtools')
library(PCAtools)
plotloadings(pca_overall)


##
calcium_saturation = 
  data_combined_subset %>% 
  filter(analysis == "ICP") %>% 
  filter(grepl("cec|Ca|Na", name)) %>% 
  pivot_wider(names_from = name, values_from = "value") %>% 
  mutate(ca_saturation = (Ca_meq100g/cec_meq100g)*100,
         na_saturation = (Na_meq100g/cec_meq100g)*100)

#
# extracting ICP and IC data for all sites ----
icp_long = 
  icp_samples %>% 
  pivot_longer(-c("sample_label", "analysis"))
ic_long = 
  ions_processed %>% 
  pivot_longer(-c("sample_label", "analysis"))
icp_ic_long = 
  bind_rows(icp_long, ic_long) %>% 
  drop_na() %>% 
  left_join(sample_key)
icp_ic_long %>% write.csv("1-data/processed/synoptic_ICP_IC_all_horizons.csv", row.names = F)
