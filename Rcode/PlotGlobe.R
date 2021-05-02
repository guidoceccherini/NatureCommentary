library(rworldmap)
library(dplyr)
library(ggplot2)
library(geosphere)
library(gpclib)
library(viridis)
library(tidyverse)
library(sf)
# World map
worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id

world.df <- world.points[,c("long","lat","group", "region")]

worldmap <- ggplot() + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45)

worldmap


countries <- c("Canada", "Russia", "United States of America","China" ,
               'Austria','Hungary','Poland','Italy','France', 'Slovakia','Germany',
               'Czech Republic','Romania','Croatia','Sweden','Finland','United Kingdom','Ireland','Spain','Portugal',
               'Lithuania','Latvia','Estonia','Greece','Bulgaria','Belgium', 
               'Luxembourg','Slovenia', 'Denmark','Netherlands')

EU <- c('Austria','Hungary','Poland','Italy','France', 'Slovakia','Germany',
               'Czech Republic','Romania','Croatia','Sweden','Finland','United Kingdom','Ireland','Spain','Portugal',
               'Lithuania','Latvia','Estonia','Greece','Bulgaria','Belgium', 
               'Luxembourg','Slovenia', 'Denmark','Netherlands')

world.df2 <- world.df
world.df2$fill <- NA

for(country in countries){
  world.df2[world.df$region == country, "fill"] <- country
}

world.df2 <- world.df2 %>% mutate(fill2 = ifelse(fill %in% EU, 'EU', fill))

  

worldmap <- ggplot() + 
  geom_polygon(data = world.df2, aes(x = long, y = lat, group = group, fill = fill2)) +
  # geom_text(data = world.df2, aes(x = long, y = lat, group = group, label = fill2))+
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45)

sc2 <- scale_fill_viridis(discrete = TRUE, option = "D",na.value = "grey80") ## option = "inferno"

P <- worldmap + 
  
  coord_map("ortho", orientation=c(65, -4, 0)) +
  xlab("") + ylab("")+
  theme_bw()+
  sc2+
   theme(legend.position = "none") +
  # labs(fill = "Forest Loss Per Year [km^2] ")+
 
  theme(panel.grid.minor=element_blank(),
        strip.text = element_text(size = 20),
        legend.key.width = unit(1.5,'cm'),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        # legend.position = "bottom",
        # legend.direction = "horizontal",
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20)
  )

  # scale_fill_manual("",
  #                   values = c("Canada" = "#ffcccc", "zcircleCanada" = "#cc0000",
  #                              "Russia" = "#7fc1ff", "zcircleRussia" = "#004e99",
  #                              "Greenland" = "#66CC98", "zcircleGreenland" = "#009966"),
  #                   na.value = "grey80",
                    # guide = FALSE)

P

ggsave("PlotGlobe.png", width = 25, height = 25, units = "cm",dpi=300,path = 'figures' )



datalist = list()


################################# GFW

list_countries <- c('US','CA','CH','RU')
list_countries_REAL <- c('US', 'Canada','China','Russia')

###### get areas 

ii <- 1
for (i in 1:NROW(list_countries)){
  #### read 
  expression2find <- paste('*',list_countries[i],'*csv', sep ='')
  file.ls_2 <- list.files(path='Data/GFW/',
                          pattern=glob2rx(expression2find))  
  
  
  Forest_L <- read.csv(paste0('Data/GFW/treecover_loss__ha_',list_countries[i],'.csv' ))
  
  
  Forest_L <- Forest_L %>%  filter(tsc_tree_cover_loss_drivers__type == 'Forestry')
  
  
  # if (NROW(Forest_L) == 1){
  Data_EU_DB<- data.frame(Year =  rep(NA, 19),.geo = rep(NA,19))
  Data_EU_DB$Year <-  Forest_L$umd_tree_cover_loss__year
  Data_EU_DB$AreaTotalLoss <-  Forest_L$umd_tree_cover_loss__ha / 1000
  
  Data_EU_DB <- arrange(Data_EU_DB, Year)
  
  
  Data_EU_DB$Country <- as.character(list_countries[i])
  
  datalist[[i]] <- Data_EU_DB # add it to your list
  
  
}
HansenArea = do.call(rbind, datalist)


HansenArea$Country <- as.character(HansenArea$Country)


HansenArea <- HansenArea %>%select(-.geo)


HansenArea <- HansenArea %>%  
  # mutate(Country = replace(Country,Country == 'RS', 'RU'))%>%  
  mutate(Country = replace(Country,Country == 'CH', 'CN'))


EU <- read_csv('Data/GFW/EU_GFW.csv')
# EU <- as.tibble(EU)
# 
# colnames(EU$V1) <- 'Year'
# colnames(EU$V2) <- 'Harvest'

EU <- EU %>% select(Year,Harvest)%>%
  rename(AreaTotalLoss = Harvest)%>%
  mutate(AreaTotalLoss = AreaTotalLoss/1000)

EU$Country <- 'EU26'

HansenArea <- rbind(HansenArea, EU)

HansenArea$Country <- as.factor(HansenArea$Country)


HansenArea$Country <- factor(HansenArea$Country, levels = c("CA","CN","EU26", 'RU',"US"))


counts_EU2 <- HansenArea
# counts_EU_2 = reshape2::melt(counts_EU2, id.vars = c(1,3))##melt(r_df, id.vars = c('x','y'))
# 
counts_EU_2 <- HansenArea %>%  filter(Year >= 2011)


# Biomass <- read_csv('/data/cecchgu/Forest_management_EU/Figure_NC/Fig_Biomass_S1/Data/Biomass.csv')
# Biomass <- as.data.frame(Biomass)


ggplot(counts_EU_2)+##//forcats::fct_rev(variable)))+##, color=CompLoss))+
  geom_bar(aes(x=Year, y=AreaTotalLoss, fill = Country),stat="identity")+
  facet_wrap(Country ~ ., scales = "free")+
  ##, position=position_dodge())+ ##, position=position_dodge())
  theme_bw()+ 
  # theme_void() +
  scale_x_continuous(limits=c(2010.4,2019.6),breaks = c( 2011, 2014,2017,2019))+##,labels=c("2004","2017"))+
  # scale_fill_manual(values = rev(wes_palette("Royal1",2)))+ ##("Royal1")
  # scale_fill_manual(values = c( "#4DBBD5FF","#E64B35FF","#00A087FF"))+
  # scale_fill_manual(values = rev(colorblind_pal()(6)))+
 scale_fill_viridis(discrete = TRUE, option = "D",na.value = "grey80")+ ## option = "inferno"

  # scale_fill_npg(palette = rev("nrc"))+
  # ggtitle('Mean LAI')+ 
  xlab("Year") +
  ylab(" Harvested Area [1,000 ha]") +##["~km^2~"]") +
  labs( linetype = "Biomass",
        # fill=expression('  '))+
        fill=expression(atop("Forest Loss Area from:")))+
  # theme_Publication()+
  theme(panel.grid.minor=element_blank(),
        strip.text = element_text(size = 10),
        # legend.key.height = unit(1.75,'cm'),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        axis.text.x = element_text(size = 10),
        axis.title.x =  element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        # legend.key.width = unit(1.75,'cm'),
        # legend.position = "bottom",
        # legend.direction = "horizontal"  ,
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        legend.key.width = unit(1.75,'cm'),
        legend.position = "bottom",
        legend.direction = "horizontal"   )+
  # guides(fill=guide_legend(nrow=3, title.vjust =-1))+ 
  # # labs(tag ='a')+
  # # theme(plot.tag = element_text(size = 24, face = "bold"))+
  # # theme(plot.tag.position = c(0.01, 1.05))+
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))

ggsave("PlotGFW_5countriesA.png", width = 25, height = 15, units = "cm",dpi=300,path = 'figures' )


######## welch test ( t test)
library(broom)
library(dplyr)
library(broom)

counts_EU_2$period <- rep(c('f','f','f','f','f','s','s','s','s'), 5)

### aggregate big_data_EU_4 for 2 periods for each country

# big_data_EU_4_2p <- big_data_EU_4 %>% 
#   group_by(Country,period) %>%
#   # filter(Year <= 2015) %>%
#   summarize(FirstP = mean(harvest,na.rm=T))

counts_EU_2p = counts_EU_2 %>% 
  group_by(Country) %>% 
  do(tidy(t.test(AreaTotalLoss~period, data=.,var.equal=FALSE)))%>% 
  dplyr::select('Country', 'p.value')



counts_EU_4 <- left_join(counts_EU_2,counts_EU_2p,'Country')

counts_EU_4 <- counts_EU_4 %>%
  mutate(P = case_when(p.value<= 0.05 ~ ' * ', 
                       p.value> 0.05 ~ ' '))


Increase1 <- counts_EU_4 %>%
  group_by(Country) %>% 
  filter(period == 'f')%>% 
  summarize(Mean1=mean(AreaTotalLoss,na.rm = T))

Increase2 <- counts_EU_4 %>%
  group_by(Country) %>% 
  filter(period == 's')%>% 
  summarize(Mean1=mean(AreaTotalLoss,na.rm = T))

Increase <- round(((Increase2$Mean1-Increase1$Mean1)/Increase1$Mean1)*100)

Increase1$Mean1 <- Increase

counts_EU_4 <- left_join(counts_EU_4,Increase1,'Country')



counts_EU_4$Country2 <- paste0(counts_EU_4$Country, counts_EU_4$P,' ', counts_EU_4$Mean1,'%')

# counts_EU_4$Country <- paste0(counts_EU_4$Country, Increase1$Mean1,'%')

counts_EU_4$Country <- factor(counts_EU_4$Country, levels = c("CA","CN","EU26", 'RU',"US"))

    
    ggplot(counts_EU_4)+##//forcats::fct_rev(variable)))+##, color=CompLoss))+
      geom_bar(aes(x=Year, y=AreaTotalLoss, fill = Country2),stat="identity")+
      facet_wrap(Country2 ~ ., scales = "free")+
      ##, position=position_dodge())+ ##, position=position_dodge())
      theme_bw()+ 
      # theme_void() +
      scale_x_continuous(limits=c(2010.4,2019.6),breaks = c( 2011, 2014,2017,2019))+##,labels=c("2004","2017"))+
      # scale_fill_manual(values = rev(wes_palette("Royal1",2)))+ ##("Royal1")
      # scale_fill_manual(values = c( "#4DBBD5FF","#E64B35FF","#00A087FF"))+
      # scale_fill_manual(values = rev(colorblind_pal()(8)))+
      scale_fill_viridis(discrete = TRUE, option = "D",na.value = "grey80")+ ## option = "inferno"
      
      # scale_fill_npg(palette = "nrc")+
      # ggtitle('Mean LAI')+ 
      xlab("Year") +
      ylab(" Harvested Area [1,000 ha]") +##["~km^2~"]") +
      labs( linetype = "Biomass",
            # fill=expression('  '))+
            fill=expression(atop("Forest Loss Area from:")))+
      # theme_Publication()+
      theme(panel.grid.minor=element_blank(),
            strip.text = element_text(size = 10),
            # legend.key.height = unit(1.75,'cm'),
            axis.line.y = element_line(),
            axis.line.x = element_line(),
            axis.text.x = element_text(size = 10),
            axis.title.x =  element_text(size = 10),
            axis.text.y = element_text(size = 10),
            axis.title.y = element_text(size = 10),
            # legend.key.width = unit(1.75,'cm'),
            # legend.position = "bottom",
            # legend.direction = "horizontal"  ,
            legend.title=element_text(size=10),
            legend.text=element_text(size=10),
            legend.key.width = unit(1.75,'cm'),
            legend.position = "bottom",
            legend.direction = "horizontal"   )+
      # guides(fill=guide_legend(nrow=3, title.vjust =-1))+ 
      # # labs(tag ='a')+
      # # theme(plot.tag = element_text(size = 24, face = "bold"))+
      # # theme(plot.tag.position = c(0.01, 1.05))+
      scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                     scientific = FALSE)) + 
      theme(legend.title = element_blank())+ theme(legend.position = "none") +
      geom_vline(xintercept=2015.5, linetype="dashed", 
                 color = "red", size=1)
    
    ggsave("PlotGFW_5countriesB.png", width = 25, height = 15, units = "cm",dpi=300,path = 'figures' )

    
    
    fig.format <- 'png'
    fig.path <- 'figures/'
    pts_list <- unique(counts_EU_4$Country)
    pts_list <- factor(pts_list, levels = c("CA","CN","EU26", 'RU',"US"))
    pts_list <- c("CA","CN","EU26", 'RU',"US")
    
    nCol <- 5
    myCol <- viridis(n = nCol)
     i <- 1 
    for(iPts in pts_list){
      
      g.ts.1 <- ggplot(counts_EU_4 %>% filter(Country == iPts))+
        geom_bar(aes(x=Year, y=AreaTotalLoss, fill = Country2),stat="identity", fill =myCol[i]) +
        theme_bw()+ 
        facet_wrap(Country2 ~ ., scales = "free")+
        scale_x_continuous(limits=c(2010.4,2019.6),breaks = c( 2011, 2014,2017,2019))+##,labels=c("2004","2017"))+
       # scale_fill_viridis(discrete = TRUE, option = "D",na.value = "grey80")+ ## option = "inferno"
        xlab("Year") +
        ylab(" Harvested Area [1,000 ha]") +##["~km^2~"]") +
        labs( linetype = "Biomass",
              fill=expression(atop("Forest Loss Area from:")))+
        theme(panel.grid.minor=element_blank(),
              strip.text = element_text(size = 10),
              # legend.key.height = unit(1.75,'cm'),
              axis.line.y = element_line(),
              axis.line.x = element_line(),
              axis.text.x = element_text(size = 10),
              axis.title.x =  element_text(size = 10),
              axis.text.y = element_text(size = 10),
              axis.title.y = element_text(size = 10),
              legend.title=element_text(size=10),
              legend.text=element_text(size=10),
              legend.key.width = unit(1.75,'cm'),
              legend.position = "bottom",
              legend.direction = "horizontal"   )+
        
        scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                       scientific = FALSE)) + 
        theme(legend.title = element_blank())+ theme(legend.position = "none") +
        geom_vline(xintercept=2015.5, linetype="dashed", 
                   color = "red", size=0.5)
                # ggtitle(paste0('Harvest ', iPts))
      
      g.all <- g.ts.1 
      
      ggsave(filename = paste0('Country_', iPts, '.', fig.format), 
             plot = g.all, path = fig.path, width = 3, height = 2)
      
       i <- i+1
    }