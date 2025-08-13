## PCA on C-normalized data

```{r}
data_wide_PCA2 = 
  data_wide_PCA %>% 
  pivot_longer(cols = c(where(is.numeric), -TC)) %>% 
  mutate(value = value/TC) %>% 
  dplyr::select(-TC) %>% 
  pivot_wider()

pca_all_c = fit_pca_function(data_wide_PCA2)
pca_overall_wle = fit_pca_function(dat = data_wide_PCA2 %>% filter(region == "WLE"))
pca_overall_cb = fit_pca_function(data_wide_PCA2 %>% filter(region == "CB"))


ggbiplot(pca_all_c$pca_int, obs.scale = 1, var.scale = 1,
         groups = as.character(pca_all_c$grp$region), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 1) +
  geom_point(size=3,stroke=1, alpha = 1,
             aes(#shape = pca_all_c$grp$site,
               color = groups))+ 
  scale_color_manual(#breaks = c("upland", "transition", "wte", "wc"), 
    values = pal_transect)+
  #scale_shape_manual(values = c(21, 19))+
  #scale_shape_manual(values = c(21, 21, 19), name = "", guide = "none")+
  xlim(-6,6)+
  ylim(-4.5,4.5)+
  labs(shape="",
       title = "PCA: all, normalized to C",
       subtitle = "surface horizons")+
  theme_kp()+
  theme(legend.position = "top", legend.box = "vertical")+
  NULL


ggbiplot(pca_overall_wle$pca_int, obs.scale = 1, var.scale = 1,
         groups = as.character(pca_overall_wle$grp$transect), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 1) +
  geom_point(size=3,stroke=1, alpha = 1,
             aes(shape = pca_overall_wle$grp$site,
               color = groups))+ 
  scale_color_manual(breaks = c("upland", "transition", "wte", "wc"), 
    values = pal_transect)+
  #scale_shape_manual(values = c(21, 19))+
  #scale_shape_manual(values = c(21, 21, 19), name = "", guide = "none")+
  xlim(-6,6)+
  ylim(-4.5,4.5)+
  labs(shape="",
       title = "PCA: all, normalized to C",
       subtitle = "surface horizons")+
  theme_kp()+
  theme(legend.position = "top", legend.box = "vertical")+
  NULL


ggbiplot(pca_overall_cb$pca_int, obs.scale = 1, var.scale = 1,
         groups = as.character(pca_overall_cb$grp$transect), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 1) +
  geom_point(size=3,stroke=1, alpha = 1,
             aes(shape = pca_overall_cb$grp$site,
               color = groups))+ 
  scale_color_manual(breaks = c("upland", "transition", "wte", "wc"), 
                     values = pal_transect)+
  #scale_shape_manual(values = c(21, 19))+
  #scale_shape_manual(values = c(21, 21, 19), name = "", guide = "none")+
  xlim(-6,6)+
  ylim(-4.5,4.5)+
  labs(shape="",
       title = "PCA: all, normalized to C",
       subtitle = "surface horizons")+
  theme_kp()+
  theme(legend.position = "top", legend.box = "vertical")+
  NULL


####
####

df = data_wide_PCA2 %>%   drop_na() %>% dplyr::select(where(is.numeric))
grp = data_wide_PCA2 %>% drop_na() %>% dplyr::select(!where(is.numeric))

fviz_nbclust(df, FUN = hcut, method = "wss")
fviz_nbclust(df, FUN = hcut, method = "silhouette")
# three clusters

d <- dist(df, method = "euclidean")
hc1 <- hclust(d, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)
hc5 <- hclust(d, method = "ward.D2" )
sub_grp <- cutree(hc5, k = 3)
table(sub_grp)
plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 3, border = 2:5)

df_cl <- mutate(df, cluster = sub_grp) %>% cbind(grp) %>% mutate(cluster = as.character(cluster))
count(df_cl,cluster)


library(ggConvexHull)

pca_cluster = fit_pca_function(df_cl) 

ggbiplot::ggbiplot(pca_cluster$pca_int, obs.scale = 1, var.scale = 1,
                   groups = as.character(pca_cluster$grp$cluster), 
                   ellipse = F, circle = FALSE, var.axes = TRUE, alpha = 0) +
  geom_convexhull(aes(group = pca_cluster$grp$cluster,
                      fill = pca_cluster$grp$cluster), alpha = 0.2)+
  geom_point(size=3,stroke=1, alpha = 1,
             aes(shape = pca_cluster$grp$site,
                 color = groups))+ 
  # scale_color_manual(values = c("#16879C", "#BB281E" ))+
  #scale_shape_manual(values = c(21, 19))+
  #scale_shape_manual(values = c(21, 21, 19), name = "", guide = "none")+
  # xlim(-4,20)+
  # ylim(-8,8)+
  labs(fill="cluster",
       color = "cluster",
       shape = "region",
       title = "Overall PCA, both regions",
       subtitle = "Surface horizons only")+
  theme_kp()+
  NULL


df_cl %>% 
  ggplot(aes(x = transect, y = cluster, color = site))+
  geom_jitter(width = 0.1, height = 0.1, size = 3)+
  facet_wrap(~region)+
  theme_kp()

###
###

## drivers

x = data_wide_PCA2 %>% group_by(region, transect) %>% do(do_pca(.))

x %>% 
  ggplot(aes(x = PC1, y = PC2, label = variable))+
  geom_point()+geom_text()+
  facet_grid(transect ~ region)


pc_overall = get_pc_values(data_wide_PCA2) %>% mutate(region = "combined")
pc_wle = get_pc_values(data_wide_PCA2 %>% filter(region == "WLE")) %>% mutate(region = "WLE")
pc_cb = get_pc_values(data_wide_PCA2 %>% filter(region == "CB")) %>% mutate(region = "CB")

pc_all = 
  bind_rows(pc_overall, pc_wle, pc_cb) %>% 
  mutate(variable = fct_reorder(variable, (PC1)))

pc_all %>% 
  mutate(variable = fct_reorder(variable, (PC1))) %>% 
  filter(region != "combined") %>% 
  ggplot(aes(x = PC1, y = variable, color = region))+
  geom_point(size = 5)+
  scale_colour_manual(values = c("#81C0A7", "#1A421D"))+
  xlim(-0.4, 0.4)+
  geom_vline(xintercept = 0)+
  labs(y = "")+
  theme_kp()

pc_all %>% 
  mutate(variable = fct_reorder(variable, (PC1))) %>% 
  filter(region != "combined") %>% 
  ggplot(aes(x = abs(PC1), y = variable, color = region))+
  geom_point(size = 5)+
  scale_colour_manual(values = c("#81C0A7", "#1A421D"))+
  # xlim(0, 0.4)+
  #geom_vline(xintercept = 0)+
  labs(y = "",
       x = "|PC1|")+
  theme_kp()

###
###


data_normalized <- 
  data_wide_PCA2 %>% 
  dplyr::select(where(is.numeric)) %>% 
  scale() %>% 
  bind_cols(data_wide_PCA2 %>% dplyr::select(where(is.character))) %>%
  filter(transect != "wte") %>% 
  pivot_longer(cols = where(is.numeric)) 

data_normalized_summary <- 
  data_normalized %>% 
  group_by(region, site, transect, horizon, name) %>% 
  dplyr::summarise(value = median(value, na.rm = T))


data_normalized_summary %>% 
  ggplot(aes(x = value, y = name, color = transect))+
  geom_point(size = 3)+
  #facet_wrap(~region)
  facet_wrap(~region+site)


data_normalized %>% 
  ggplot(aes(x = value, y = name, color = transect))+
  # geom_violin()+
  geom_point()+
  facet_wrap( ~ region)

data_normalized_summary2 <- 
  data_normalized_summary %>% 
  pivot_wider(names_from = "transect") %>% 
  mutate(distance_transition = transition - transition,
         distance_upland = upland - transition,
         distance_wetland = wc - transition) %>% 
  dplyr::select(region, site, horizon, name, starts_with("distance")) %>% 
  pivot_longer(cols = c(distance_transition, distance_upland, distance_wetland),
               names_to = "transect")




data_combined_subset %>% 
  filter(name == "mehlichp_ugg") %>% 
  ggplot(aes(x = transect, y = value, color = site))+
  geom_jitter(width = 0.1)+
  facet_wrap(~region)

data_combined_subset %>% 
  filter(name == "Fe_ugg") %>% 
  ggplot(aes(x = transect, y = value, color = site))+
  geom_jitter(width = 0.1)+
  facet_wrap(~region)


# euclidean distance ----
library(vegan)
library(ape)

compute_euclidean_distance <- function(data_normalized){
  
  wide = 
    data_normalized %>% 
    #dplyr::select(-sample_label, -tree_number, -horizon) %>% 
    pivot_wider(names_from = "transect", values_from = "value") 
  
  upland = wide %>% pull(upland) %>% na.omit()
  transition = wide %>% pull(transition) %>% na.omit()
  wetland = wide %>% pull(wc) %>% na.omit()
  
  
  
  tibble(
    euclidean_dist_up_tr = dist(rbind(upland, transition)),
    euclidean_dist_wc_tr = dist(rbind(wetland, transition)),
    median_tr = mean(transition),
    median_up = mean(upland),
    median_wc = mean(wetland)
  ) %>% 
    mutate(euclidean_dist_up_tr = as.numeric(euclidean_dist_up_tr),
           euclidean_dist_wc_tr = as.numeric(euclidean_dist_wc_tr),
           sign_up = if_else(median_up - median_tr <= 0, -1, 1),
           sign_wc = if_else(median_wc - median_tr <= 0, -1, 1),
           euclidean_dist_up_tr = sign_up * euclidean_dist_up_tr,
           euclidean_dist_wc_tr = sign_wc * euclidean_dist_wc_tr,
           euclidean_dist_tr = 0)
  
  
  
  
}

euc <- 
  data_normalized %>% 
  filter(name != "percentOM") %>% 
  group_by(region, site, name) %>% 
  do(compute_euclidean_distance(.))





euc %>% 
  pivot_longer(cols = c(euclidean_dist_up_tr, euclidean_dist_wc_tr, euclidean_dist_tr), names_to = "transect", values_to = "distance") %>% 
  ggplot(aes(x = distance, y = name, color = transect))+
  geom_segment(aes(xend = 0, yend = name))+
  geom_point(size = 3)+
  labs(x = "Euclidean distance")+
  facet_wrap(~region + site)+
  theme_kp()


###


###
###




###
###

library(corrr)
# data<-read.csv("all_mean_location2.csv") ## keep rownames
simple <- data_wide_PCA2 %>% 
  dplyr::select(where(is.numeric))
res.cor<-correlate(na.omit(simple))
res.cor %>% network_plot(min_cor = .4, colors=c("#8073ac","white","#e08214"))

simple_wle <- 
  data_wide_PCA2 %>% 
  filter(region == "WLE") %>% 
  dplyr::select(where(is.numeric))
res.cor <-correlate(na.omit(simple_wle))
res.cor %>% network_plot(min_cor = .4, colors=c("#8073ac","white","#e08214"))

simple_cb <- 
  data_wide_PCA2 %>% 
  filter(region == "CB") %>% 
  dplyr::select(where(is.numeric))
res.cor <-correlate(na.omit(simple_cb))
res.cor %>% network_plot(min_cor = .4, colors=c("#8073ac","white","#e08214"),
                         curved = T)



###
###
wetlands_only = data_wide_PCA %>% filter(transect == "wc")
pca_wetland = fit_pca_function(wetlands_only) 

ggbiplot(pca_wetland$pca_int, obs.scale = 1, var.scale = 1,
         groups = as.character(pca_wetland$grp$region), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
  geom_point(size=3,stroke=1, alpha = 1,
             aes(shape = pca_wetland$grp$site,
               color = groups))+ 
  scale_color_manual(values = c("#16879C", "#BB281E" ))+
  #scale_shape_manual(values = c(21, 19))+
  #scale_shape_manual(values = c(21, 21, 19), name = "", guide = "none")+
  # xlim(-4,20)+
  # ylim(-8,8)+
  labs(shape="",
       title = "Overall PCA, both regions",
       subtitle = "Surface horizons only")+
  theme_kp()+
  NULL
