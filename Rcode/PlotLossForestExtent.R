library(rworldmap)
library(dplyr)
library(ggplot2)
library(geosphere)
library(gpclib)
library(viridis)
library(tidyverse)
library(sf)
# World map


Loss <- st_read('Data/SHP/ZenodoData/ValidationResultsLoss.shp')
Forest <- st_read('Data/SHP/ZenodoData/ValidationResultsForest.shp')


Loss2 <- Loss%>% st_set_crs('null')  %>% st_set_crs("EPSG:3857") %>% st_transform(crs=4326)
Loss2 <- st_centroid(Loss2)

ForestCoord <- as.data.frame(Forest %>%st_coordinates())
Forest$X <-ForestCoord$X
Forest$Y <-ForestCoord$Y

Loss2Coord <- as.data.frame(Loss2 %>%st_coordinates())
Loss2$X <-Loss2Coord$X
Loss2$Y <-Loss2Coord$Y



world <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))


ggplot() +
  geom_sf(data = world) +
  geom_point(data = Forest, aes(x = X, y = Y, color = 'Forest'), size = .5               ) + #color = "darkgreen"
  geom_point(data = Loss2, aes(x = X, y = Y, color = 'Loss'), size = .5 
             ) + #color = "darkred"
  # scale_color_manual(values = c("Forest" = "#3399ff","Loss"= "#ff00ff"))+
  coord_sf(xlim = c(8, 33), ylim = c(55, 71), expand = FALSE)+
  xlab("") + ylab("")+
  theme_bw()+
    labs( color = "Legend:") +
  scale_color_manual(values = c("darkgreen", "darkred"),
                     labels = c("Forest", "Loss"))

ggsave("PlotValidation.png", width = 12, height = 25, units = "cm",dpi=300,path = 'figures' )
