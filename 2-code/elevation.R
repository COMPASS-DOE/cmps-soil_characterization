library(tidyverse)

elevation = read.csv("synoptic-distances.csv", na = "")


elevation %>% 
  filter(site_id != "Sweet Hall Marsh") %>% 
  filter(!is.na(region)) %>% 
 # filter(site_id != "Crane Creek") %>% 
  ggplot(aes(x = dist_corrected, y = elev, color = zone))+
  geom_line(color = "black")+
  geom_point(size = 4)+
  facet_wrap(region ~ site_id, 
             scales = "free_y"
             )


elevation %>% 
  filter(site_id != "Sweet Hall Marsh") %>% 
  filter(!is.na(region)) %>% 
  # filter(site_id != "Crane Creek") %>% 
  ggplot(aes(x = `distance.to.water`, y = elev, color = zone))+
  geom_line(color = "black")+
  geom_point(size = 4)+
  facet_wrap(region ~ site_id, 
             scales = "free_y"
  )
