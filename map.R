
library(sf) # for map
library(ggspatial) # for north arrow


## Set CRS
common_crs <- 4326

## Set map size and point size
point_size <- 2
map_width = 9
map_height = 6

## Set regional and WLE/CB (inset) bounding boxes
us_bbox <- c(xmin = -125, xmax = -60, ymin = 20, ymax = 50)
region_bbox <- c(xmin = -98, xmax = -60, ymin = 32, ymax = 50)
cb_bbox <- c(xmin = -77.8, xmax = -74.5, ymin = 36.8, ymax = 40.5)
wle_bbox <- c(xmin = -84, xmax = -82, ymin = 41, ymax = 42)

## Make US states map cropped to GL/CB region
us <- 
  read_sf("cb_2018_us_state_5m/cb_2018_us_state_5m.shp") %>% 
  st_transform(., crs = common_crs) %>% 
  st_crop(., y = us_bbox)

region <- st_crop(us, y = region_bbox)

## Further crop states to WLE/CB region
cb_states <- st_crop(region, y = cb_bbox)
wle_states <- st_crop(region, y = wle_bbox)

## Get state labels
st_labels = st_centroid(us) %>% 
  filter(!STUSPS %in% c("TN", "NC", "DC")) # remove state labels that mess with the graph

## create a df with site coordinates and labels
sites = data.frame(
  y = c(41.61524, 41.50148, 41.37618,
        38.43085, 37.21927, 38.874445),
  x = c(-83.22889, -83.04611, -82.50685,
        -76.22622, -76.40851, -76.551667),
  region = c("Erie", "Erie", "Erie",
             "Chesapeake", "Chesapeake", "Chesapeake"),
  site = c("CRC", "PTR", "OWC",
           "MSM", "GWI", "GCW"),
  label = c("1", "2", "3", 
            "5", "6", "4"))

## Make the base map
base_plot <- 
  ggplot() + 
  geom_sf(data = region) + 
  #   geom_sf_text(data = st_labels, aes(label = STUSPS))+
  geom_point(
    data = sites, aes(x, y),
    size = 2, color = "black")+
  # CB inset rectangle
  geom_rect(aes(xmin = -77.8, xmax = -74.5, ymin = 36.8, ymax = 40.5), 
            fill = NA, color = "black", lwd = 0.75) +
  geom_segment(aes(x = -74.5, xend = -70, y = 38, yend = 38), 
               color = "black", lwd = 0.75) + 
  # WLE inset rectangle
  geom_rect(aes(xmin = -84, xmax = -82, ymin = 41, ymax = 42), 
            fill = NA, color = "black", lwd = 0.75) +
  geom_segment(aes(x = -83, xend = -83, y = 42, yend = 44), 
               color = "black", lwd = 0.75) + 
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  xlim(-92, -67)+
  ylim(35, 49)+
  theme(legend.background = element_rect(fill = alpha("white", 0.0)), 
        legend.key = element_rect(fill = "transparent"), 
        legend.position = c(0.85, 0.1)) + 
  labs(x = "", y = "")+
  NULL

## Make the inset map with just WLE sites
inset_plot_wle <- 
  ggplot() + 
  geom_sf(data = wle_states) + 
  geom_point(
    data = sites %>% filter(region == "Erie"), aes(x, y),
    size = 5, color = "black")+
  geom_text(data = sites %>% filter(region == "Erie"), aes(x, y, label = label), nudge_x = 0, nudge_y = -0.10, size = 7)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  cowplot::theme_map() + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#56cfe1"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5),
        axis.text = element_blank()) 

## Make the inset map with just CB sites
inset_plot_cb <- 
  ggplot() + 
  geom_sf(data = cb_states) + 
  geom_point(
    data = sites %>% filter(region == "Chesapeake"), aes(x, y),
    size = 5, color = "black")+
  geom_text(data = sites %>% filter(region == "Chesapeake"), aes(x, y, label = label), nudge_x = -0.25, nudge_y = -0.00, size = 7)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  cowplot::theme_map() + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#48bfe3"),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5),
        axis.text = element_blank()) 

## Combine into single figure
base_plot + 
  annotation_custom(
    ggplotGrob(inset_plot_wle), 
    xmin = -90, xmax = -80, ymin = 42, ymax = 50)+
  annotation_custom(
    ggplotGrob(inset_plot_cb), 
    xmin = -75, xmax = -65, ymin = 35, ymax = 43)+
  theme_kp()

# save figure 1300 x 950


