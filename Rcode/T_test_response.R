library(rworldmap)

library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(gdalUtils)
library(devtools)
library(rasterVis)

library(RColorBrewer)

library(ggplot2)
library(rgdal)           
library(zoo)
library(snow)
library(scales)

library(dplyr)

library(raster)
library(sp)
library(ggplot2)
library(RColorBrewer)
library(reshape2)

######## welch test ( t test)
library(broom)
library(dplyr)
library(broom)
library(sf)



Data <- read.csv('Data/OriginalData.csv')


# filter 2013
Data <- Data %>%filter(Year > 2012)

Data$period <- rep(c('f','f','f','s','s','s'), 26)

### aggregate big_data_EU_4 for 2 periods for each country

# big_data_EU_4_2p <- big_data_EU_4 %>% 
#   group_by(Country,period) %>%
#   # filter(Year <= 2015) %>%
#   summarize(FirstP = mean(harvest,na.rm=T))

big_data_EU_4_2p = Data %>% 
  group_by(Country) %>% 
  do(tidy(t.test(Loss~period, data=.,var.equal=FALSE)))%>% 
  dplyr::select('Country', 'p.value')



# big_data_EU_4 <- left_join(Data,big_data_EU_4_2p,'Country')

big_data_EU_4 <- big_data_EU_4_2p %>%
  mutate(P = cut(p.value, breaks = c(0,0.05,0.1,0.2,1), 
                 right = TRUE,  labels = c('0','0.05', '0.1', '0.2')))  
# mutate(P = case_when(p.value<= 0.05 ~ 1, 
#                      p.value> 0.05 ~0))



vpath <- 'Data/GeographyEarth/'
# prepare map 
world <- sf::st_read(paste0('Data/GeographyEarth/','ne_50m_land.shp'), quiet = TRUE)
laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

world <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))%>%
  st_transform(laes_prj)


EU_C <- big_data_EU_4$Country


EU <- world %>% filter(ID %in% EU_C)
ggplot() + 
  geom_sf(data = EU)##, aes(x=long, y = lat, group = group)) + 

colnames(big_data_EU_4)[1] <- 'ID'

EU_T <- dplyr::left_join(EU, big_data_EU_4, by = "ID")

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

sc <- scale_fill_distiller(type = "div", palette = "RdBu", direction=-1,limits=c(-100,100), oob=squish,na.value="transparent") # space = "Lab", na.value = "grey50",
sc <- scale_fill_gradient2(low=("blue"), high=("red"))##,limits=c(-20,150), oob=squish)

sc <- scale_fill_gradient2(low=("blue"), high=("red"),
                           guide = guide_colorbar(title.position = "top", 
                                                  direction = "horizontal")) 

sc <- scale_fill_viridis(guide = guide_colorbar(title.position = "top", 
                                                direction = "horizontal"), option = 'cividis') 



europe_laea <- sf::st_intersection(world,st_set_crs(st_as_sf(as(raster::extent(-25, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
  st_transform(laes_prj)
EU_C <- sf::st_read(paste0(vpath,'ne_50m_coastline.shp'), quiet = TRUE)
EU_B <- sf::st_read(paste0(vpath,'ne_10m_admin_0_boundary_lines_land.shp'), quiet = TRUE)



library(viridis)


# ggsave("IncreasePercentageRel.png", width = 32, height = 25, units = "cm",dpi=300)




p1 <- ggplot(EU_T) +
  geom_sf(data=europe_laea,fill='grey90')+
  geom_sf(data=EU_T, aes( fill = P, geometry = geom))+
  geom_sf(data=EU_B,fill='grey80')+
  geom_sf(data=EU_C,fill='grey80')+
  xlab("") + ylab("")+
  theme_bw()+ 
  theme_void() +
  # labs(fill = "Forest Loss Per Year [km^2] ")+
  labs(fill = "Harvested Forest [%]")+##coord_equal() +
  # theme_Publication()+
  theme(legend.position = 'top', 
        legend.key = element_blank()) + 
  
  scale_fill_manual(values = rev(viridis(5)[2:5]),#viridis(5)[2:5],
                    # scale_fill_manual(values = viridis(7), na.translate = F,#viridis(5)[2:5],
                    guide = guide_legend(nrow = 1, direction = 'horizontal', 
                                         label.hjust = 0, label.position = 'bottom', 
                                         keywidth = 7.51, keyheight = 0.75, title = ""))+ 
  coord_sf(xlim=c(2.65e6,5.8e6),ylim=c(1.6e6,5.2e6))+
  labs(title = expression(atop(
    "T-test Harvest Intensity Country level" )))
    # subtitle = "t-test")+
   # theme(legend.position = "none")
    # caption = "Map by Ceccherini et al.")




p1

############################################################





Final_loss <- stack('Data/FinalLoss_at_20km_2018.tif') ##~/Documents/Forest_management_EU/Data/
Forest <- stack('Data/Forest2000_at_20kmEffisexl.tif') ##~/Documents/Forest_management_EU/Data/


beginCluster()
ccc <- c('f','f','f','s','s','s')
f1<- function(x) { if (length(which(is.na(x)))>0){ NA } else {
  
  
  m = t.test(x~ccc,var.equal=FALSE)
  m$p.value}} # slope
Final_loss_areaTot.slope_parallel <- clusterR(Final_loss[[13:18]], calc, args=list(fun=f1), export='ccc')
endCluster()



laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
europe_laea <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))%>%
  st_transform(laes_prj)


Forest_area <- projectRaster(Final_loss_areaTot.slope_parallel, crs=laes_prj,method='ngb')

## plot with ggplot2 library
Final_loss_area_dat = as.data.frame(as(Forest_area, 'SpatialPixelsDataFrame'))
plotData = melt(Final_loss_area_dat, id.vars = c('x','y'))



plotData2 <-  plotData[complete.cases(plotData), ]

plotData22 <- plotData2  %>% mutate(value2 = cut(value, breaks = c(0,0.05,0.1,0.2,1), 
                                                 right = TRUE,  labels = c('0','0.05', '0.1', '0.2')))  





p2 <- ggplot(plotData22) +
  geom_sf(data=europe_laea,fill='grey80')+
  geom_raster(aes(x, y, fill = value2)) +
  # sc2  +
  coord_sf(xlim=c(2.5e6,6e6),ylim=c(1.5e6,5.4e6))+
  xlab("") + ylab("")+
  theme_bw()+ 
  theme_void() +
  # labs(fill = "Forest Loss Per Year [km^2] ")+
  labs(fill = "Harvested Forest [%]")+##coord_equal() +
  # theme_Publication()+
  theme(legend.position = 'top', 
        legend.key = element_blank()) + 
  scale_fill_manual(values = rev(viridis(5)[2:5]),#viridis(5)[2:5],
                    # scale_fill_manual(values = viridis(7), na.translate = F,#viridis(5)[2:5],
                    guide = guide_legend(nrow = 1, direction = 'horizontal', 
                                         label.hjust = 0, label.position = 'bottom', 
                                         keywidth = 7.51, keyheight = 0.75, title = ""))+ 
  labs(title = expression(atop(
    "T-test Harvest Intensity Pixel level" )))
    # subtitle = "t-test")
  # caption = "Map by Ceccherini et al.")


p2
# setwd("/data/cecchgu/Forest_management_EU/Figures4Publication/Codes")
# ggsave("TTestRaster.png", width = 25, height = 25, units = "cm",dpi=300)



Landsat <- stack('Data/CoverageLandsats.tif')



##### to save time uncomment and use the Raster file LandsatObsTTest.tif instead


# 
# # L5 <- Landsat[[1:8]]
# 
# L7 <- Landsat [[9:23]]
# 
# L8 <- Landsat [[24:29]]
# 
# 
# # L57 <- L5+L7[[1:8]]
# 
# L78 <- L8 + L7[[10:15]]
# 
# 
# L78 <- stack(L78)
# 
# beginCluster()
# ccc <- c('f','f','f','s','s','s')
# f1<- function(x) { 
#   if (length(which(is.na(x)))>0){ NA } 
#   else if (sum(x)<1){ 1 } 
#   
#   else if(length(unique(x))< 4){1}
#   # 
#   else {
#   
#     
#     m = t.test(x~ccc)
#     m$p.value}} # slope
# Final_loss_areaTot.slope_parallel <- clusterR((L78), calc, args=list(fun=f1), export='ccc')
# endCluster()

# writeRaster(Final_loss_areaTot.slope_parallel, 'Data/LandsatObsTTest.tif')

Final_loss_areaTot.slope_parallel <- raster('Data/LandsatObsTTest.tif')

ForestchangedRes <- resample(Forest, Final_loss_areaTot.slope_parallel, method="bilinear")


Final_loss_areaTot.slope_parallel [!is.finite(ForestchangedRes)] <- NA

library(sf)

laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
europe_laea <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))%>%
  st_transform(laes_prj)


Forest_area <- projectRaster(Final_loss_areaTot.slope_parallel, crs=laes_prj,method='ngb')

## plot with ggplot2 library
Final_loss_area_dat = as.data.frame(as(Forest_area, 'SpatialPixelsDataFrame'))
plotData = melt(Final_loss_area_dat, id.vars = c('x','y'))



plotData2 <-  plotData[complete.cases(plotData), ]

plotData22 <- plotData2  %>% mutate(value2 = cut(value, breaks = c(0,0.05,0.1,0.2,1), 
                                                 right = TRUE,  labels = c('0','0.05', '0.1', '0.2')))  


library(viridis)


p3 <- ggplot(plotData22) +
  geom_sf(data=europe_laea,fill='grey80')+
  geom_raster(aes(x, y, fill = value2)) +
  # sc2  +
  coord_sf(xlim=c(2.5e6,6e6),ylim=c(1.5e6,5.4e6))+
  xlab("") + ylab("")+
  theme_bw()+ 
  theme_void() +
  # labs(fill = "Forest Loss Per Year [km^2] ")+
  labs(fill = "Landsat coverage")+##coord_equal() +
  # theme_Publication()+
  theme(legend.position = 'top', 
        legend.key = element_blank()) + 
  scale_fill_manual(values = rev(viridis(5)[2:5]),#viridis(5)[2:5],
                    # scale_fill_manual(values = viridis(7), na.translate = F,#viridis(5)[2:5],
                    guide = guide_legend(nrow = 1, direction = 'horizontal', 
                                         label.hjust = 0, label.position = 'bottom', 
                                         keywidth = 7.51, keyheight = 0.75, title = ""))+ 
  labs(title = expression(atop(
    "T-test Landsat 8 Observations" )),
    tag = 'b')+
theme(plot.tag = element_text(size = 18, face = "bold"))
  
    # subtitle = "t-test")+
  # theme(legend.position = "none")
# caption = "Map by Ceccherini et al.")


p3
# setwd("/data/cecchgu/Forest_management_EU/Figures4Publication/Codes")
ggsave("TTestRasterL78.png", width = 16, height = 15, units = "cm",dpi=300,path = 'figures' )











library(grid)
fig.name <- 'fig_test'
fig.width <- 14; fig.height <- 10; 
fig.fmt <- 'png' ##'png'
fig.fullfname <- paste0('figures', '/',  fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

grid.newpage()
# Create layout : nrow = 3, ncol = 2
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol =2)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 
# Arrange the plots
print(p3, vp = define_region(row = 1, col = 1))   # Span over two columns
# print(p2, vp = define_region(row = 1, col = 2))
print(p1, vp = define_region(row = 1, col = 2))

# print(p1, vp = viewport(width = 1, height = 1, x = 0, y = 0, just=c(0,0)))
# print(p2, vp = viewport(width = 1, height = 1, x = 0.5, y = 0, just=c(0,0)))

grid.text(expression(bold("a")), x = unit(0.02, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("b")), x = unit(0.52, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))
# grid.text(expression(bold("c")), x = unit(0.76, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))

dev.off()


# 
# 
# library(grid)
# fig.name <- 'fig_test'
# fig.width <- 10; fig.height <- 16; 
# fig.fmt <- 'png' ##'png'
# fig.fullfname <- paste0('Figures', '/',  fig.name, '.', fig.fmt)
# if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
# if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}
# 
# grid.newpage()
# # Create layout : nrow = 3, ncol = 2
# pushViewport(viewport(layout = grid.layout(nrow = 3, ncol =1)))
# # A helper function to define a region on the layout
# define_region <- function(row, col){
#   viewport(layout.pos.row = row, layout.pos.col = col)
# } 
# # Arrange the plots
# print(p1, vp = define_region(row = 1, col = 1))   # Span over two columns
# print(p2, vp = define_region(row = 2:3, col = 1))
# 
# # print(p1, vp = viewport(width = 1, height = 1, x = 0, y = 0, just=c(0,0)))
# # print(p2, vp = viewport(width = 1, height = 1, x = 0.5, y = 0, just=c(0,0)))
# 
# grid.text(expression(bold("a")), x = unit(0.02, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))
# grid.text(expression(bold("b")), x = unit(0.02, "npc"), y = unit(0.64, "npc"), gp = gpar(fontsize = 18))
# dev.off()
# 
# 
# 
# 
# library(grid)
# fig.name <- 'fig_test'
# fig.width <- 12; fig.height <- 16; 
# fig.fmt <- 'eps' ##'png'
# fig.fullfname <- paste0('Figures', '/',  fig.name, '.', fig.fmt)
# if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
# if(fig.fmt == 'eps'){cairo_pdf(fig.fullfname, width = fig.width, height = fig.height)}
# 
# grid.newpage()
# # Create layout : nrow = 3, ncol = 2
# pushViewport(viewport(layout = grid.layout(nrow = 3, ncol =1)))
# # A helper function to define a region on the layout
# define_region <- function(row, col){
#   viewport(layout.pos.row = row, layout.pos.col = col)
# } 
# # Arrange the plots
# print(p1, vp = define_region(row = 1, col = 1))   # Span over two columns
# print(p2, vp = define_region(row = 2:3, col = 1))
# 
# # print(p1, vp = viewport(width = 1, height = 1, x = 0, y = 0, just=c(0,0)))
# # print(p2, vp = viewport(width = 1, height = 1, x = 0.5, y = 0, just=c(0,0)))
# 
# grid.text(expression(bold("a")), x = unit(0.02, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))
# grid.text(expression(bold("b")), x = unit(0.02, "npc"), y = unit(0.64, "npc"), gp = gpar(fontsize = 18))
# dev.off()