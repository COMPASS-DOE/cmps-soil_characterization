

# hierarchical clusters ----

library(cluster)
library(factoextra)

data(iris)
scale(iris)

df <- USArrests 

df <- na.omit(df)
df <- scale(df)




df = iris %>%  dplyr::select(where(is.numeric))

d <- dist(df, method = "euclidean")
hc1 <- hclust(d, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)
hc5 <- hclust(d, method = "ward.D2" )
sub_grp <- cutree(hc5, k = 3)
table(sub_grp)
plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 5, border = 2:5)

fviz_cluster(list(data = df, cluster = sub_grp))


df = data_combined_wide %>% filter(region == "CB") %>%  dplyr::select(where(is.numeric))
grp = data_combined_wide %>%  filter(region == "CB") %>% dplyr::select(!where(is.numeric))

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

# plot
set.seed(12345)
gg = 
  df_cl %>% 
  ggplot(aes(y = transect, x = as.character(cluster), color = site))+
  geom_point(aes(group = site, text = sample_label), 
             size = 4, 
             position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1, 
                                             dodge.width = 0.5))

plotly::ggplotly(gg)


# pca
library(ggbiplot)

pca_wle_clusters = fit_pca_function(df_cl %>% dplyr::select(-dic)) 

ggbiplot(pca_wle_clusters$pca_int, obs.scale = 1, var.scale = 1,
           groups = as.character(pca_wle_clusters$grp$cluster), 
           ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
  geom_point(size=3,stroke=1, alpha = 1,
             aes(shape = pca_wle_clusters$grp$site,
               color = pca_wle_clusters$grp$transect)) 
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
















df_cl %>% 
#  filter(site == "CPCRW") %>% 
  ggplot(aes(y = cluster, x = transect, fill = site))+
  geom_dotplot(binaxis = "y", stackdir = "center",
               dotsize = 0.4)+
  facet_wrap(~site, nrow = 1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(ggpubr)

df_cl %>% 
  ggdotplot(y = "cluster", x = "transect", fill = "site", shape = "transect",
            position = position_jitter(0.05), dotsize = 1)+
  #  scale_fill_manual(values = c("pink", "red", "lightblue", "blue"))+
  facet_wrap(~region, nrow = 1)

#  ggplot(aes(y = cluster, x = length, fill = length))+
#  geom_dotplot(binaxis = "y", stackdir = "centerwhole",
#               dotsize = 0.4)+
#  facet_wrap(~depth + saturation + drying, nrow = 1)+
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))


  


  geom_jitter(y = "cluster", x = "transect", fill = "site", shape = "transect",
            position = position_jitter(0.05), dotsize = 1)+
  #  scale_fill_manual(values = c("pink", "red", "lightblue", "blue"))+
  facet_wrap(~region, nrow = 1)


#
#


#install.packages("ggbeeswarm")
library(ggbeeswarm)


y <- round(rnorm(200), 1)

df <- data.frame(y = y,
                 group = sample(c("G1", "G2", "G3"),
                                size = 200,
                                replace = TRUE))
ggplot(df, aes(x = group, y = y)) +
  geom_beeswarm()





# Beeswarm plot in ggplot2
ggplot(df_cl, aes(y = region, x = as.character(cluster), color = site)) +
  geom_beeswarm(cex = 2)







df_cl = df_cl %>% mutate(cluster = as.character(cluster))
df_cl_pca = fit_pca_function(df_cl %>% filter(site == "SR"))


ggbiplot(df_cl_pca$pca_int, obs.scale = 1, var.scale = 1,
         groups = as.character(df_cl_pca$grp$cluster), 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
  geom_point(size=3,stroke=1, alpha = 1,
             aes(shape = df_cl_pca$grp$depth,
                 color = groups))+ 
  #scale_color_manual(values = c("#16879C", "#BB281E" ))+
  scale_shape_manual(values = c(21, 19))+
  #scale_shape_manual(values = c(21, 21, 19), name = "", guide = "none")+
  # xlim(-4,20)+
  # ylim(-8,8)+
  labs(shape="",
       #title = "Overall PCA, both regions",
       #subtitle = "Surface horizons only"
  )+
  theme_kp()+
  NULL



#



#
