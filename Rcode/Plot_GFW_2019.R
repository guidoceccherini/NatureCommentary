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
  
  library("ggsci")
  library("ggplot2")
  library("gridExtra")
  library(tidyverse)
  
  
  theme_Publication <- function(base_size=14, base_family="helvetica") {
    library(grid)
    library(ggthemes)
    (theme_foundation(base_size=base_size, base_family=base_family)
      + theme(plot.title = element_text(face = "bold",
                                        size = rel(1.2), hjust = 0.5),
              text = element_text(),
              panel.background = element_rect(colour = NA),
              plot.background = element_rect(colour = NA),
              panel.border = element_rect(colour = NA),
              axis.title = element_text(face = "bold",size = rel(1)),
              axis.title.y = element_text(angle=90,vjust =2),
              axis.title.x = element_text(vjust = -0.2),
              axis.text = element_text(), 
              axis.line = element_line(colour="black"),
              axis.ticks = element_line(),
              panel.grid.major = element_line(colour="#f0f0f0"),
              panel.grid.minor = element_blank(),
              legend.key = element_rect(colour = NA),
              legend.position = "bottom",
              legend.direction = "horizontal",
              legend.key.size= unit(0.2, "cm"),
              legend.margin = unit(0, "cm"),
              legend.title = element_text(face="italic"),
              plot.margin=unit(c(10,5,5,5),"mm"),
              strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
              strip.text = element_text(face="bold"),
              # legend.key.width = unit(1.75,'cm'),
              # legend.position = "bottom",
              # legend.direction = "horizontal"
      ))
    
  }
  
  datalist = list()
  
  
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
  
  EU <- read_csv('Data/GFW/EU_agg.csv')
  
  EU <- EU %>% select(Year,Harvest)%>%
    rename(AreaTotalLoss = Harvest)
  
  EU$Country <- 'EU26'
  
  HansenArea <- rbind(HansenArea, EU)
  
  HansenArea$Country <- as.factor(HansenArea$Country)
  
  
  HansenArea$Country <- factor(HansenArea$Country, levels = c("CN", "CA", "US", 'RU',"EU26"))
  
  
  
  library(tidyverse)
  
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
    
    # scale_fill_npg(palette = rev("nrc"))+
    # ggtitle('Mean LAI')+ 
    xlab("Year") +
    ylab(" Harvested Area [1,000 ha]") +##["~km^2~"]") +
    labs( linetype = "Biomass",
          # fill=expression('  '))+
          fill=expression(atop("Forest Loss Area from:")))+
    theme_Publication()+
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
  
  
  ggsave("Plot2019US_CH_RS.png", width = 23, height = 15, units = "cm",dpi=300,path = 'figures' )
  
  
  
  
  ######## welch test ( t test)
  library(broom)
  library(dplyr)
  library(broom)
  
  counts_EU_2$period <- rep(c('f','f','f','f','f','s','s','s','s'), 4)
  
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
  
  
  
  counts_EU_4$Country <- paste0(counts_EU_4$Country, counts_EU_4$P,' ', counts_EU_4$Mean1,'%')
  
  # counts_EU_4$Country <- paste0(counts_EU_4$Country, Increase1$Mean1,'%')
  
  
  
  ggplot(counts_EU_4)+##//forcats::fct_rev(variable)))+##, color=CompLoss))+
    geom_bar(aes(x=Year, y=AreaTotalLoss, fill = forcats::fct_rev(Country)),stat="identity")+
    facet_wrap(Country ~ ., scales = "free")+
    ##, position=position_dodge())+ ##, position=position_dodge())
    theme_bw()+ 
    # theme_void() +
    scale_x_continuous(limits=c(2010.4,2019.6),breaks = c( 2011, 2014,2017,2019))+##,labels=c("2004","2017"))+
    # scale_fill_manual(values = rev(wes_palette("Royal1",2)))+ ##("Royal1")
    # scale_fill_manual(values = c( "#4DBBD5FF","#E64B35FF","#00A087FF"))+
    # scale_fill_manual(values = rev(colorblind_pal()(8)))+
    
    # scale_fill_npg(palette = "nrc")+
    # ggtitle('Mean LAI')+ 
    xlab("Year") +
    ylab(" Harvested Area [1,000 ha]") +##["~km^2~"]") +
    labs( linetype = "Biomass",
          # fill=expression('  '))+
          fill=expression(atop("Forest Loss Area from:")))+
    theme_Publication()+
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
               color = "red", size=1.25)
  
  
  ggsave("Plot2019US_CH_RS.png", width = 23, height = 15, units = "cm",dpi=300,path = 'figures' )
  
  
  
  
  
  ############ stats 2018
  
  counts_EU_2b <- counts_EU_2%>% filter(Year<=2018)
  counts_EU_2b$period <- rep(c('f','f','f','f','f','s','s','s'), 4)
  
  ### aggregate big_data_EU_4 for 2 periods for each country
  
  # big_data_EU_4_2p <- big_data_EU_4 %>% 
  #   group_by(Country,period) %>%
  #   # filter(Year <= 2015) %>%
  #   summarize(FirstP = mean(harvest,na.rm=T))
  
  counts_EU_2p = counts_EU_2b %>% 
    group_by(Country) %>% 
    do(tidy(t.test(AreaTotalLoss~period, data=.,var.equal=FALSE)))%>% 
    dplyr::select('Country', 'p.value')
  
  
  
  counts_EU_4 <- left_join(counts_EU_2b,counts_EU_2p,'Country')
  
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
  
  
  
  counts_EU_4$Country <- paste0(counts_EU_4$Country, counts_EU_4$P,' ', counts_EU_4$Mean1,'%')
  
  # counts_EU_4$Country <- paste0(counts_EU_4$Country, Increase1$Mean1,'%')
  
  
  
  ggplot(counts_EU_4)+##//forcats::fct_rev(variable)))+##, color=CompLoss))+
    geom_bar(aes(x=Year, y=AreaTotalLoss, fill = forcats::fct_rev(Country)),stat="identity")+
    facet_wrap(Country ~ ., scales = "free")+
    ##, position=position_dodge())+ ##, position=position_dodge())
    theme_bw()+ 
    # theme_void() +
    scale_x_continuous(limits=c(2010.4,2018.6),breaks = c( 2011, 2013,2015,2018))+##,labels=c("2004","2017"))+
    # scale_fill_manual(values = rev(wes_palette("Royal1",2)))+ ##("Royal1")
    # scale_fill_manual(values = c( "#4DBBD5FF","#E64B35FF","#00A087FF"))+
    # scale_fill_manual(values = rev(colorblind_pal()(8)))+
    
    # scale_fill_npg(palette = "nrc")+
    # ggtitle('Mean LAI')+ 
    xlab("Year") +
    ylab(" Harvested Area [1,000 ha]") +##["~km^2~"]") +
    labs( linetype = "Biomass",
          # fill=expression('  '))+
          fill=expression(atop("Forest Loss Area from:")))+
    theme_Publication()+
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
               color = "red", size=1.25)
  
  
  ggsave("Plot2018US_CH_RS.png", width = 23, height = 15, units = "cm",dpi=300,path = 'figures' )
