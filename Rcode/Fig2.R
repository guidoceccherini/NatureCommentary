library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(datasets)
library(ggrepel)
library(readxl)
library(sf)
library(dplyr)
library(raster)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(gdalUtils)
library(devtools)
library(rasterVis)
##### OPEN xlsx files



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

library(ggplot2)











Data_OBS <- c(#2, 0.36,0.37,97.27,
  3.26, 0.61,0.66,95.47,
  2.68,0.89,0.5,95.92)
Data_CLASS1 <- rep(c('Loss', 'Loss', 'Forest','Forest'),2)
Data_CLASS2 <- rep(c('Loss', 'Forest', 'Loss','Forest'),2)
Data_CLASS3 <- rep(c(#######'First Period (2013-15)Landsat 8 Era',
  'First Period (2011-15)',
  'Second Period (2016-18)'),each=4)

mydata = data.frame(Data_OBS,Data_CLASS1,Data_CLASS2,Data_CLASS3);


mydata$Data_CLASS2 <- factor(mydata$Data_CLASS2, levels=c('Loss', 'Forest'))

# Pixel-based Area (GFC)', 'Sample-based Area

p1 <- ggplot( mydata, aes( x=as.factor(Data_CLASS2), y=as.factor(Data_CLASS1), fill=Data_OBS ) ) +
  # geom_tile( aes( fill = Data_OBS ) ) +
  geom_tile(  fill = 'white', colour = 'grey') +
  geom_text( aes( fill = Data_OBS, label = round( Data_OBS, 2 )  ) ,size = 12) +
  scale_x_discrete(position = "top") +
  facet_wrap( ~Data_CLASS3, ncol=3, scales="free_x" )+
  
  xlab("Area-Corrected Proportions [%]") + ylab("GFC")+
  theme_bw()+
  theme_minimal()+
  
  # theme_void() +
  # labs(fill = "Forest Loss Per Year [km^2] ")+
  scale_fill_viridis(trans = 'reverse') +
  theme(legend.position = "none")+
  theme(panel.grid.minor=element_blank(),
        strip.text = element_text(size = 14),
        # legend.key.height = unit(1.75,'cm'),
        axis.text.x = element_text(size = 14),
        axis.title.x =  element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        # legend.key.width = unit(1.75,'cm'),
        # legend.position = "bottom",
        # legend.direction = "horizontal"  ,
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        legend.key.width = unit(1.75,'cm'),
        legend.position = "bottom",
        legend.direction = "horizontal"   )+
  labs( tag = 'b')+
  theme(plot.tag = element_text(size = 18, face = "bold"))+
  theme(strip.placement = "outside",
        axis.title.x.top = element_text(hjust=0),  #left align axis title on top
        axis.title.x.bottom = element_blank(),#remove title on x axis/bottom
        axis.ticks.length.x.top = unit(0, units="cm")) #remove axis ticks on top
# axis.text.y.left = element_blank(), #remove axis label on left side
# axis.title.y.right = element_blank(), #remove axis title on right side
# axis.ticks.length.y.left = unit(0, units="cm")) #remo

# subtitle = "t-test")+
# theme(legend.position = "none")
# caption = "Map by Ceccherini et al.")



p1



######################################################


Data_OBS <- c(#84.6, 84.8, 99.6, 99.6, 
  83.1, 84.3, 99.4,99.3, 
  84.3, 75.1, 99.1, 99.5)
Data_CLASS1 <- rep(c('Producer Acc.', 'User Acc.'),4)
Data_CLASS2 <- rep(c('Loss', 'Loss', 'Forest','Forest'),2)
Data_CLASS3 <- rep(c(###'First Period (2013-15)Landsat 8 EraOverall Acc.99.2%',
  'First Period (2011-15)\nOverall Acc.98.7%',
  'Second Period (2016-18)\nOverall Acc.98.8%'),each=4)

mydata2 = data.frame(Data_OBS,Data_CLASS1,Data_CLASS2,Data_CLASS3);


mydata2$Data_CLASS2 <- factor(mydata2$Data_CLASS2, levels=c('Loss', 'Forest'))


p2 <- ggplot( mydata2, aes( x=Data_CLASS2, y=Data_OBS, fill =Data_CLASS1) ) + 
  geom_col( position = position_dodge()) +
  geom_text(
    aes(label =Data_OBS, y = Data_OBS ),
    position = position_dodge(0.9),
    vjust = 0 ,
    size = 5
  )+
  # geom_text( label = round( Data_OBS, 1 )  )  +
  facet_wrap( ~Data_CLASS3, ncol=3, scales="free_x" )+
  xlab("Validation") + 
  ylab("GFC")+
  scale_y_continuous(limits = c(0, 120))+
  theme_bw()+ 
  theme_minimal()+
  labs(fill = "")+
  scale_fill_manual(values = rev(viridis(3)[1:2])) +#option = "plasma"
  theme(legend.title =  element_blank())+
  theme(panel.grid.minor=element_blank(),
        strip.text = element_text(size = 14),
        # legend.key.height = unit(1.75,'cm'),
        axis.text.x = element_text(size = 14),
        axis.title.x =  element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        # legend.key.width = unit(1.75,'cm'),
        # legend.position = "bottom",
        # legend.direction = "horizontal"  ,
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        legend.key.width = unit(1.75,'cm'),
        legend.position = "bottom",
        legend.direction = "horizontal"   )+
  labs( tag = 'c')+
  theme(plot.tag = element_text(size = 18, face = "bold"))+
  theme(legend.position = 'bottom', 
        legend.key = element_blank()) +
  theme(legend.title = element_blank())

# subtitle = "t-test")+
# theme(legend.position = "none")
# caption = "Map by Ceccherini et al.")



p2




######################################################


Data_OBS <- c(#1148, 1152,  47426, 47422, 
  1906, 1934,  47426, 47398, 
  1761, 1568, 47426, 47619)

Data_CI <- c(#####'','(CI=114)','','(CI=114)',
  '','(CI=136)','','(CI=136)',
  '', '(CI=152)','', '(CI=152)')
Data_CLASS1 <- rep(c('Pixel-based Area (GFC)', 'Sample-based Area'),4)
Data_CLASS2 <- rep(c('Loss', 'Loss', 'Forest','Forest' ),2)
Data_CLASS3 <- rep(c(####'First Period (2013-15)Landsat 8 Era',
  'First Period (2011-15)',
  'Second Period (2016-18)'),each=4)

mydata3 = data.frame(Data_OBS,Data_CLASS1,Data_CLASS2,Data_CLASS3,Data_CI);


mydata3$Data_CLASS2 <- factor(mydata3$Data_CLASS2, levels=c('Loss', 'Forest'))



p3 <- ggplot( mydata3, aes( x=Data_CLASS2, y=Data_OBS, fill =Data_CLASS1) ) + 
  geom_col( position = position_dodge()) +
  #  geom_errorbar(aes(ymin=Data_OBS-Data_CI, ymax=Data_OBS+Data_CI), width=.2,
  #                position=position_dodge(.9)) +
  # scale_y_log10()+
  geom_text(
    aes(label =Data_OBS, y = Data_OBS + 8000),
    position = position_dodge(0.9),
    vjust = 0,
    size = 5
  )+
  geom_text(
    aes(label =Data_CI, y = Data_OBS + 0.05),
    position = position_dodge(0.9),
    vjust = 0,
    size = 4.5
  )+
  theme_bw()+ 
  theme_minimal()+
  # geom_text(  label = round( Data_OBS, 1 )  )  +
  facet_wrap( ~Data_CLASS3, ncol=3, scales="free_x" )+
  scale_y_continuous(limits = c(0, 59500))+
  theme(legend.title =  element_blank())+
  theme(panel.grid.minor=element_blank(),
        strip.text = element_text(size = 14),
        # legend.key.height = unit(1.75,'cm'),
        axis.text.x = element_text(size = 14),
        axis.title.x =  element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        # legend.key.width = unit(1.75,'cm'),
        # legend.position = "bottom",
        # legend.direction = "horizontal"  ,
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        legend.key.width = unit(1.75,'cm'),
        legend.position = "bottom",
        legend.direction = "horizontal"   )+
  xlab("Classes") + 
  ylab("Area (1000 ha)")+
  
  # labs(fill = "Forest Loss Per Year [km^2] ")+
  scale_fill_manual(values = rev(viridis(3, option = "A")[1:2])) +#option = "plasma"
  theme(legend.title =  element_blank())+
  labs( tag = 'a')+
  theme(plot.tag = element_text(size = 18, face = "bold"))+
  theme(legend.position = 'bottom', 
        legend.key = element_blank()) 

# subtitle = "t-test")+
# theme(legend.position = "none")
# caption = "Map by Ceccherini et al.")



p3





# xls files

my_data <- read_excel("Data/SHP/LossD2020/TOTLoss.xlsx") #%>% 

##select columns
my_data <- my_data[,c(1,2,3,5,6,7,8,9)]


#remove dubious
my_data <- my_data %>% 
  filter(CLASS !=0)%>%
  filter(CLASS !=3)%>% 
  filter(CLASS !=4)

#rename
my_data <- my_data %>% 
  rename( DateGEP = 'Date GEP') %>% 
  rename( YearHansen = 'Year Hansen') %>% 
  rename( DateLandsat = 'Date Landsat') %>% 
  rename( Country = '...3')



my_data <- my_data %>% 
  mutate(CLASS2 = case_when(CLASS == 1 & FLAG == 0 ~ 1,
                            CLASS == 1 & FLAG == 1 ~ 1,
                            CLASS == 1 & FLAG == 2 ~ 2,
                            CLASS == 2 & DateGEP > (YearHansen) ~ 2, #####
                            CLASS == 2 &  DateGEP <= (YearHansen) ~ 0,
                            TRUE ~ 0))%>% 
  mutate(TimeSpan = case_when(YearHansen <= 2015 ~ 1, 
                              YearHansen >= 2016 ~ 2,
                              TRUE ~ 0))%>% 
  mutate(TimeSpan2 = case_when(YearHansen <= 2015 & YearHansen >= 2013 ~ 1, 
                               YearHansen >= 2016 ~ 2,
                               TRUE ~ 0))


my_data<- my_data %>% 
  filter(CLASS2 !=0)

my_data %>% count(TimeSpan)


######summary
Summary1 <- dplyr::group_by(my_data, .dots = c('CLASS2')) %>%
  dplyr::summarize(counts = n())

Summary2 <-dplyr::group_by(my_data, .dots = c('CLASS2','TimeSpan')) %>%
  dplyr::summarize(counts = n())

Summary3 <-dplyr::group_by(my_data, .dots = c('CLASS2','TimeSpan2')) %>%
  dplyr::summarize(counts = n())

### Sweden
Summary1S <- dplyr::group_by(my_data  %>% 
                               filter(Country =='Sweden'), .dots = c('CLASS2')) %>%
  dplyr::summarize(counts = n())

Summary2S <-dplyr::group_by(my_data  %>% 
                              filter(Country =='Sweden'), .dots = c('CLASS2','TimeSpan')) %>%
  dplyr::summarize(counts = n())

Summary3S <-dplyr::group_by(my_data  %>% 
                              filter(Country =='Sweden'), .dots = c('CLASS2','TimeSpan2')) %>%
  dplyr::summarize(counts = n())

### Finland
Summary1F <- dplyr::group_by(my_data  %>% 
                               filter(Country =='Finland'), .dots = c('CLASS2')) %>%
  dplyr::summarize(counts = n())

Summary2F <-dplyr::group_by(my_data  %>% 
                              filter(Country =='Finland'), .dots = c('CLASS2','TimeSpan')) %>%
  dplyr::summarize(counts = n())

Summary3F <-dplyr::group_by(my_data  %>% 
                              filter(Country =='Finland'), .dots = c('CLASS2','TimeSpan2')) %>%
  dplyr::summarize(counts = n())




#####analysis loss classes

my_data2<- my_data %>% 
  filter( CLASS ==1)  %>% 
  filter( YearHansen >=2011) 


my_data2<- my_data2 %>%
  mutate(TimeSpan = case_when(TimeSpan == 1 ~ '2011-2015',
                              TimeSpan == 2 ~ '2016-2018'))
# ,

# creating a dataframe
dff <- dplyr::group_by(my_data2, .dots = c('TimeSpan','FLAG')) %>%
  dplyr::summarize(counts = n()) %>%
  dplyr::mutate(perc = (counts / sum(counts)) * 100) %>%
  dplyr::arrange(desc(perc))







# preparing the plot
pie <- ggplot2::ggplot(dff, aes('', counts)) +
  geom_col(
    position = 'fill',
    color = 'black',
    width = 1,
    aes(fill = factor(FLAG))
  ) +
  facet_grid( ~ TimeSpan)+
  geom_label_repel(
    aes(label = paste0(round(perc), "%"), group = factor(FLAG)),
    position = position_fill(vjust = 0.5),
    color = 'black',
    size = 5,
    show.legend = FALSE
  ) +
  coord_polar(theta = "y")+
  scale_fill_viridis(discrete=TRUE,
                     option="C",
                     name="Categories",
                     labels=c(
                       # "Unknown Harvest\n(No Loss date)",
                       "100% Loss", '> 50%','<50%'))




# make a pie

pie
# add labels
p5 <- pie +  ##theme_minimal()+
  theme(
    axis.text.y=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    # legend.position="none",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

p5 <- p5+ theme(strip.text.x = element_text(size = 10))+
  ggtitle("Validation Points \n by Loss") +
  labs( tag = 'e')+
  theme(plot.tag = element_text(size = 18, face = "bold"))


p5
# ggsave("LossAnalysis11.png", width = 27, height = 20, units = "cm",dpi=300,path = 'figures' )








# Script to plot the time series of MODIS and Landsat values for
# a series of points. 
# 
# Used as visual validation of forest disturbances 
# (mostly for clear cuts)
#
# G. Duveiller - Aug.2020
################################################################


library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(patchwork)

dat.path <- paste0('Data/ExtractionL78/')
fig.path <- 'figures/ts/'

fig.format <- 'png'

# prepare L8 + L7
dat_landsat8 <- read_csv(file = paste0(dat.path,'/landsat8NDVIprofiles.csv'), col_names = T, skip = 0)
dat_landsat7 <- read_csv(file = paste0(dat.path,'/landsat7NDVIprofiles.csv'), col_names = T, skip = 0)

dat_L_clean0 <- bind_rows(
  dat_landsat8 %>% mutate(platform = 'L8'),
  dat_landsat7 %>% mutate(platform = 'L7')) %>%
  select(ID, imageId, mean, platform) %>%
  rename(ndvi = mean) %>%
  mutate(time = as.Date(substr(imageId, 13, 20), format = '%Y%m%d'))

# dat_L_clean0 <- dat_landsat8 %>% 
#   select(ID, imageId, mean) %>%
#   rename(ndvi = mean) %>%
#   mutate(time = as.Date(substr(imageId, 13, 20), format = '%Y%m%d'))


# prepare MODIS
dat_modis <- read_csv(file = paste0(dat.path,'/modisNDVIprofiles.csv'), col_names = T, skip = 0)

dat_M_clean0 <- dat_modis %>% 
  select(ID, imageId, mean) %>%
  mutate(time = as.Date(imageId, format = '%Y_%m_%d'),
         ndvi = mean/10000,
         DoY = format(time, '%j')) %>%
  select(-mean)

dat_M_clean1 <- dat_M_clean0 %>%
  group_by(DoY, ID) %>%
  summarize(ndvi_clim = mean(ndvi)) %>%
  right_join(dat_M_clean0, by = c('DoY', 'ID'))






dir.create(fig.path, showWarnings = F, recursive = T)

pts_list <- unique(dat_M_clean1$ID)



######plot point 41
iPts <- 41

g.ts.1 <- ggplot(dat_M_clean1 %>% filter(ID == iPts)) +
  geom_line(aes(x = time, y = ndvi_clim), 
            colour = 'grey60', size = 0.2) +
  geom_line(aes(x = time, y = ndvi), 
            colour = 'cornflowerblue') +
  geom_point(data = dat_L_clean0 %>% filter(ID == iPts), 
             aes(x = time, y = ndvi, fill = platform),
             colour = 'grey20', 
             shape = 21, size = 1.5) +
  scale_x_date(name = '', date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(name = 'NDVI') +
  scale_fill_manual(values = c('L8' = 'olivedrab2', 
                               'L7' = 'olivedrab4')) +
  coord_cartesian(ylim = c(-0.1, 0.9)) +
  labs(title = paste0('Landsat/MODIS NDVI profiles'),
       subtitle = '[Landsat in green (L8 light, L7 dark), MODIS (MYD13) in blue, climatology in grey]') +
  theme(legend.position = 'none', 
        plot.subtitle = element_text(color = "grey40", size = 8))+
  labs( tag = 'd')+
  theme(plot.tag = element_text(size = 12, face = "bold"))

# 
# g.ts.2 <- ggplot(dat_M_clean1 %>% filter(ID == iPts)) +
#   geom_bar(aes(x = time, y = ndvi - ndvi_clim), stat ='identity',
#             fill = 'cornflowerblue') +
#   geom_hline(yintercept = 0, colour = 'grey20') +
#   scale_x_date(name = '', date_breaks = "1 year", date_labels = "%Y") +
#   scale_y_continuous(name = 'NDVI anomalies') +
#   coord_cartesian(ylim = c(-0.4, 0.4)) +
#   ggtitle(paste0('MODIS NDVI anomalies (ndvi - ndvi_clim) for point ', iPts))

p4 <- g.ts.1 

# ggsave(filename = paste0('ProvaPaper','.', fig.format), 
#        plot = g.all, path = fig.path, width = 9, height = 3)


library(grid)
fig.name <- 'MA_fig_2'
fig.width <- 14; fig.height <- 20; 
fig.fmt <- 'eps' ##'png'
fig.fullfname <- paste0('figures', '/',  fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 300)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}
if(fig.fmt == 'eps'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

grid.newpage()
# Create layout : nrow = 3, ncol = 2
pushViewport(viewport(layout = grid.layout(nrow = 5, ncol =1)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 
# Arrange the plots
print(p3, vp = define_region(row = 1, col = 1))   # Span over two columns
print(p1, vp = define_region(row = 2, col = 1))
print(p2, vp = define_region(row = 3, col = 1))
print(p4, vp = define_region(row = 4, col = 1))
print(p5, vp = define_region(row = 5, col = 1))

# print(p1, vp = viewport(width = 1, height = 1, x = 0, y = 0, just=c(0,0)))
# print(p2, vp = viewport(width = 1, height = 1, x = 0.5, y = 0, just=c(0,0)))
# 
# grid.text(expression(bold("a")), x = unit(0.02, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))
# grid.text(expression(bold("b")), x = unit(0.52, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))
# grid.text(expression(bold("c")), x = unit(0.76, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))

dev.off()