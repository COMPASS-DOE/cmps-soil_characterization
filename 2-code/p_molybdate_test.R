library(tidyverse)
theme_set(theme_bw())
data = readxl::read_xlsx("1-data/P_molybdate_test_2022-07-12.xlsx", skip = 27)
map = readxl::read_xlsx("1-data/P_molybdate_test_2022-07-12.xlsx", sheet = "Map")

# process data
data2 = 
  data %>% 
  rename(letter = `...1`,
         measurement = `...14`) %>% 
  fill(letter)

data_long = 
  data2 %>% 
  pivot_longer(-c(letter, measurement)) %>% 
  mutate(name = str_pad(name, 2, pad = "0"),
         well = paste0(letter, name),
         value = as.numeric(value),
         value = round(value, 4)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(well, measurement, value) %>% 
  filter(grepl("650", measurement)) %>% 
  mutate(measurement = recode(measurement, "Corrected [650]" = "650_corrected")) %>% 
  pivot_wider(names_from = "measurement", values_from = "value")

# process map
map2 = 
  map %>% 
  rename(letter = `...1`,
         notes = `...14`) %>% 
  pivot_longer(-c(letter, notes)) %>% 
  mutate(name = str_pad(name, 2, pad = "0"),
         well = paste0(letter, name)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(well, value, notes) %>% 
  rename(sample = value) %>% 
  mutate(std_conc_ppm = case_when(sample == "0-std" ~ 0,
                                  sample == "1-std" ~ 0.1,
                                  sample == "2-std" ~ 0.25,
                                  sample == "3-std" ~ 0.5,
                                  sample == "4-std" ~ 0.75,
                                  sample == "5-std" ~ 1.0,
                                  sample == "6-std" ~ 1.2))


data_full = 
  data_long %>% 
  left_join(map2) %>% 
  pivot_longer(cols = c(`650`, `650_corrected`), 
               names_to = "measurement", values_to = "intensity")


# graphs

data_full %>% 
  filter(!is.na(std_conc_ppm)) %>% 
  ggplot(aes(x = std_conc_ppm, y = `intensity`, color = notes))+
  geom_point()+
  geom_line()+
  facet_wrap(~measurement, scales = "free_y")


data_full %>% 
  filter(is.na(std_conc_ppm) & !is.na(sample)) %>% 
  ggplot(aes(x = sample, y = intensity, color = notes))+
  geom_point(size = 2)+
  facet_wrap(~measurement, scales = "free_y")

