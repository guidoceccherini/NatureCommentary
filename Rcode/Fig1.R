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
library(viridis)
Data_EU = list()

  
# i = 1
list_countries <- c('AU','HU','PL','IT','FR', 'LO','GM','EZ','RO','HR','SW','FI','UK','EI','SP','PO',
                    'LH','LG','EN','GR','BU','BE', 'LU','SI','DA','NL')
list_countries_REAL <- c('Austria','Hungary','Poland','Italy','France', 'Slovakia','Germany',
                         'Czech Republic','Romania','Croatia','Sweden','Finland','UK','Ireland','Spain','Portugal',
                         'Lithuania','Latvia','Estonia','Greece','Bulgaria','Belgium', 
                         'Luxembourg','Slovenia', 'Denmark','Netherlands')
for (i in 1: 26){
  Forest_CL <- read.csv(paste0('Data/CompactLosses/Country_Forest_Change_TOTAL_loss_',list_countries[i],'.csv' ))
  Forest_18 <- read.csv(paste0('Data/CompactLosses/Country_Forest_Change_Compact_loss22_',list_countries[i],'.csv' ))
  
  
  
  Data_EU_DB<- data.frame(Year =  rep(NA, 18))
  Data_EU_DB$Year <-  Forest_CL$year
  Data_EU_DB$TotLoss <-  Forest_CL$Forest.Loss.Total/ 10000000
  Data_EU_DB$TotLoss18 <-  Forest_18$Forest.Loss.Total/ 10000000
  
  Data_EU_DB$Country <- as.character(list_countries_REAL[i])
  Data_EU[[i]] <- Data_EU_DB # add it to your list
  
}

big_data_EU = do.call(rbind, Data_EU)





big_data_EU <- big_data_EU  %>% filter(Year > 2003)  ### ///%>% select(Year,Forest, Country)

big_data_EU_2 <- big_data_EU

big_data_EU_2$FragLoss18 <- big_data_EU_2$TotLoss -big_data_EU_2$TotLoss18

big_data_EU_2$Harvest <- big_data_EU_2$TotLoss18

big_data_EU_2 <- big_data_EU_2[,c(1,4,5,6)]

big_data_EU_3 = reshape2::melt(big_data_EU_2, id.vars = c(1,2))##melt(r_df, id.vars = c('x','y'))




levels(big_data_EU_3$variable)[levels(big_data_EU_3$variable)=="Harvest"] <- "Large Loss > 2 ha"
levels(big_data_EU_3$variable)[levels(big_data_EU_3$variable)=="FragLoss18"] <- "Small Loss <= 2 ha"


##### explore bigdata_stats

ggplot(big_data_EU_3, aes(x=Year, y=value, fill =variable))+##, color=CompLoss))+
  geom_bar(stat="identity", position=position_dodge())+ ##, position=position_dodge())
  facet_wrap(Country ~ ., scales = "free")


library(wesanderson)
# pal <- wes_palette("Darjeeling1")



big_data_EU_3 <- big_data_EU_3[order(big_data_EU_3$Country),]


ggplot(big_data_EU_3, aes(x=Year, y=value, fill =variable))+##, color=CompLoss))+
  geom_bar(stat="identity", position="stack")+## position_dodge())+ ##, position=position_dodge())
  facet_wrap(Country ~ ., scales = "free")+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  theme_minimal()+ #### , scales="free"
  # ggtitle('Mean LAI')+ 
  xlab("Year ") +
  ylab(" Harvested Forest Area [1,000 ha]") +##["~km^2~"]") +
  labs(fill=expression(atop("Type of Harvested Forest")))+ 
  theme(panel.grid.minor=element_blank(), strip.text = element_text(size = 10),
        # legend.key.height = unit(1.75,'cm'),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        legend.key.width = unit(1.75,'cm'),
        legend.position = "bottom",
        legend.direction = "horizontal"   )


# ggsave("CountryStatsCompactVsFragmentedLoss2ha.png", width = 33, height = 35, units = "cm",dpi=300,path = 'figures' )







y <- big_data_EU_2 %>% group_by(Country) %>% summarize(y = mean(FragLoss18/(Harvest +FragLoss18) ))

y2 <- big_data_EU_2 %>% summarize(y = mean(FragLoss18/(Harvest +FragLoss18) ))


#### compute weighted average

WeightAvg_Frag <- sum(big_data_EU_2$FragLoss18)/ (sum(big_data_EU_2$FragLoss18)+ sum(big_data_EU_2$Harvest)) *100


Corr_TS <- y
Corr_TS$y <- y$y*100

mean(Corr_TS$y, na.rm = T)



dat_text <- Corr_TS$y

dat_text<- round(dat_text,1)

dat_text <- as.character(dat_text )

dat_text <- paste0(dat_text, ' %')

Corr_TS$COR <- dat_text
Corr_TS$Year <- rep(2004.5,26)

MM  <- big_data_EU_3 %>% group_by(Country) %>% summarize(M = max(value, na.rm=T)*0.8)
Corr_TS$y <- MM$M

dat_text <- Corr_TS

ggplot(big_data_EU_3, aes(x=Year, y=value))+##, color=CompLoss))+
  geom_bar(aes(fill =variable), stat="identity", position="stack")+## position_dodge())+ ##, position=position_dodge())
  facet_wrap(Country ~ ., scales = "free")+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  theme_minimal()+ #### , scales="free"
  # ggtitle('Mean LAI')+ 
  xlab("Year ") +
  ylab(" Harvested Forest Area [1000 ha]") +##["~km^2~"]") +
  labs(fill=expression(atop("Type of Harvested Forest")))+ 
  theme(panel.grid.minor=element_blank(), strip.text = element_text(size = 10),
        # legend.key.height = unit(1.75,'cm'),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        legend.key.width = unit(1.75,'cm'),
        legend.position = "bottom",
        legend.direction = "horizontal"   )+
  theme(axis.title.x = element_text(size = 15, hjust=-.2)) +
  theme(axis.title.y = element_text(size = 15, hjust=0.3))+ 
  geom_text(data    = dat_text,
            mapping = aes(x = Year, y = y, label = COR),size =5,
            hjust   = -0.1,
            vjust   = -.1
  )




big_data_EU_4 <- left_join(big_data_EU_3,dat_text,'Country')



big_data_EU_4$Country <- paste0(big_data_EU_4$Country,'  (', big_data_EU_4$COR,')')





big_data_EU_2_g <- group_by(big_data_EU_2, Year)
counts_EU <- summarize(big_data_EU_2_g, Harvest = sum(Harvest),SmallLoss = sum(FragLoss18))%>%
  mutate(Harvest = ((Harvest /max(Harvest, na.rm=T))  ))%>%
  mutate(SmallLoss = ((SmallLoss /max(SmallLoss, na.rm=T))  ))

#%>% select(Year,Harvest)
head(counts_EU)
counts_EU_2 = reshape2::melt(counts_EU, id.vars = c(1))##melt(r_df, id.vars = c('x','y'))


levels(counts_EU_2$variable)[levels(counts_EU_2$variable)=="Harvest"] <- "Large Loss > 2 ha"
levels(counts_EU_2$variable)[levels(counts_EU_2$variable)=="SmallLoss"] <- "Small Loss <= 2 ha"


p1 <- ggplot(counts_EU_2)+##//forcats::fct_rev(variable)))+##, color=CompLoss))+
  geom_bar(aes(x=Year, y=value, fill = forcats::fct_rev(variable)),stat="identity", position=position_dodge())+ ##, position=position_dodge())
  theme_minimal()+ #### , scales="free"
  scale_x_continuous(limits=c(2003.4,2018.6),breaks = c(2004, 2009, 2014,2018))+##,labels=c("2004","2017"))+
  # scale_fill_manual(values = rev(wes_palette("Royal1",2)))+ ##("Royal1")
  # scale_fill_manual(values = c( "#4DBBD5FF","#E64B35FF","#00A087FF"))+
  # scale_fill_manual(values = rev(colorblind_pal()(6)))+
  
  # scale_fill_npg(palette = rev("nrc"))+
  # ggtitle('Mean LAI')+ 
  xlab("Year") +
  ylab(" Normalized Harvested Area") +##["~km^2~"]") +
  labs( linetype = "Biomass",
        # fill=expression('  '))+
        fill=expression(atop("Forest Loss Area with:")))+
  theme(panel.grid.minor=element_blank(), 
        strip.text = element_text(size = 10),
        # legend.key.height = unit(1.75,'cm'),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        axis.text.x = element_text(size = 20),
        axis.title.x =  element_text(size = 18),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        # legend.key.width = unit(1.75,'cm'),
        # legend.position = "bottom",
        # legend.direction = "horizontal"  ,
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20),
        legend.key.width = unit(1.75,'cm'),
        legend.position = "bottom",
        legend.direction = "horizontal"   )+
  guides(fill=guide_legend(nrow=3, title.vjust =-1))+ 
  # labs(tag ='a')+
  theme(plot.tag = element_text(size = 18, face = "bold"))+
  theme(plot.tag.position = c(0.01, 1.05))+
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))
# ggsave("TotalAreaGrowth.png", width = 23, height = 15, units = "cm",dpi=300,path = 'figures' )
p1




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






Final_loss <- stack('Data/FinalLoss_at_20km_2018.tif') ##~/Documents/Forest_management_EU/Data/
Forest <- stack('Data/Forest2000_at_20kmEffisexl.tif') ##~/Documents/Forest_management_EU/Data/

Landsat <- stack('Data/CoverageLandsats.tif')




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
                                         keywidth = 7.51, keyheight = 0.75, title = "")) +
  labs(title = expression(atop(
    "T-test Landsat 8 Observations" )))
  # theme(plot.tag = element_text(size = 18, face = "bold"))

# subtitle = "t-test")+
# theme(legend.position = "none")
# caption = "Map by Ceccherini et al.")


p3
# setwd("/data/cecchgu/Forest_management_EU/Figures4Publication/Codes")
# ggsave("TTestRasterL78.png", width = 16, height = 15, units = "cm",dpi=300,path = 'figures' )









library(grid)
fig.name <- 'MA_fig_1'
fig.width <- 14; fig.height <- 10; 
fig.fmt <- 'pdf' ##'png'
fig.fullfname <- paste0('figures', '/',  fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 300)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}
if(fig.fmt == 'eps'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

grid.newpage()
# Create layout : nrow = 3, ncol = 2
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol =2)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 
# Arrange the plots
print(p1, vp = define_region(row = 1, col = 1))   # Span over two columns
# print(p2, vp = define_region(row = 1, col = 2))
print(p3, vp = define_region(row = 1, col = 2))

# print(p1, vp = viewport(width = 1, height = 1, x = 0, y = 0, just=c(0,0)))
# print(p2, vp = viewport(width = 1, height = 1, x = 0.5, y = 0, just=c(0,0)))

grid.text(expression(bold("a")), x = unit(0.02, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("b")), x = unit(0.52, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))
# grid.text(expression(bold("c")), x = unit(0.76, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))

dev.off()

